(* ::Package:: *)

(* PersonalSite`SessionStore`
   --------------------------------------------------------------------------
   Almacenamiento de sesiones con firma HMAC-SHA256 (RFC 2104) y dos capas:
     Capa 1: PersonalSite`Cache` (en memoria, per-kernel, sub-milisegundo)
     Capa 2: SQLite / PostgreSQL (tabla `sessions`, persiste entre kernels)

   Formato de token (URL-safe, opaco para el cliente):
       <sessionId>.<ts_hex>.<hmac>
   donde:
       sessionId  = UUID v4 (CreateUUID[])
       ts_hex     = AbsoluteTime[] en hex (anti-replay window)
       hmac       = HMAC-SHA256(SESSION_SECRET, sessionId|userId|ts)

   HMAC-SHA256 implementado en WL puro (RFC 2104) sin dependencias externas.

   Variables de entorno:
     SESSION_SECRET   secreto HMAC (minimo 32 chars; generado por proceso si falta)
     SESSION_TTL      TTL en segundos (default 3600)
   -------------------------------------------------------------------------- *)

Begin["PersonalSite`SessionStore`Private`"];

(* ── Configuracion ────────────────────────────────────────────────────── *)
(*   $secret se calcula UNA SOLA VEZ al cargar el modulo (= no :=).
     Con SESSION_SECRET en el entorno el valor es deterministico entre
     reinicios del kernel. Sin el, se genera por proceso (solo para dev).   *)

$secret =
  Module[{v = PersonalSite`Config`value["SESSION_SECRET", ""]},
    If[v =!= "", v,
      "dev:" <> Hash[
        ToString[$ProcessID] <> ToString[AbsoluteTime[]], "SHA256", "HexString"]]];

$ttl :=
  With[{n = Quiet @ ToExpression @ PersonalSite`Config`value["SESSION_TTL", "3600"]},
    If[IntegerQ[n] && n > 0, n, 3600]];

(* ── HMAC-SHA256 RFC 2104 en WL puro ─────────────────────────────────── *)
(*   Implementacion completa: ipad/opad XOR, doble hash SHA-256.
     Resiste length-extension: no usa SHA-256 naively sobre (key||msg).   *)

hmacSha256[keyStr_String, msgStr_String] :=
  Module[{blockSize = 64,
          key = ToCharacterCode[keyStr, "UTF8"],
          msg = ToCharacterCode[msgStr, "UTF8"],
          ipad, opad, inner},

    (* 1: si |key| > blockSize, reemplazar por SHA-256(key) *)
    If[Length[key] > blockSize,
      key = IntegerDigits[
              Hash[ByteArray[key], "SHA256"], 256, 32]];

    (* 2: pad con 0x00 hasta blockSize bytes *)
    key = PadRight[key, blockSize, 0];

    (* 3: ipad = key XOR 0x36 (repetido); opad = key XOR 0x5C *)
    ipad = BitXor[key, ConstantArray[16^^36, blockSize]];
    opad = BitXor[key, ConstantArray[16^^5C, blockSize]];

    (* 4: inner = SHA-256(ipad || msg) -> bytes *)
    inner = IntegerDigits[
              Hash[ByteArray[Join[ipad, msg]], "SHA256"], 256, 32];

    (* 5: HMAC = SHA-256(opad || inner) -> 64-char hex string *)
    IntegerString[
      Hash[ByteArray[Join[opad, inner]], "SHA256"], 16, 64]
  ];

(* ── Token build / parse ─────────────────────────────────────────────── *)

buildToken[sessionId_String, userId_String, ts_Integer] :=
  Module[{mac},
    mac = hmacSha256[$secret,
            sessionId <> "|" <> userId <> "|" <> ToString[ts]];
    sessionId <> "." <> IntegerString[ts, 16] <> "." <> mac
  ];

parseToken[token_String] :=
  Module[{parts = StringSplit[token, ".", 3]},
    If[Length[parts] =!= 3, Return[$Failed]];
    If[StringLength[parts[[3]]] =!= 64, Return[$Failed]];
    <|"sessionId" -> parts[[1]],
      "ts"        -> Quiet @ Check[FromDigits[parts[[2]], 16], $Failed],
      "mac"       -> parts[[3]]|>
  ];

(* ── Helpers de fecha ────────────────────────────────────────────────── *)

isoNow[] :=
  DateString[Now, {"Year","-","Month","-","Day"," ",
                   "Hour24",":","Minute",":","Second"}];

isoPlus[seconds_Integer] :=
  DateString[DatePlus[Now, {seconds, "Second"}],
    {"Year","-","Month","-","Day"," ","Hour24",":","Minute",":","Second"}];

(* ── Capa DB ─────────────────────────────────────────────────────────── *)

dbWrite[sid_String, uid_String, role_Integer, state_String,
        perms_List, meta_Association, expAt_String] :=
  Quiet @ Check[
    PersonalSite`Database`execute[
      "INSERT OR REPLACE INTO sessions \
(session_id,user_id,role,state,permissions,meta,expires_at,last_seen) \
VALUES (?,?,?,?,?,?,?,datetime('now'))",
      {sid, uid, role, state,
       Developer`WriteRawJSONString[perms],
       Developer`WriteRawJSONString[meta],
       expAt}],
    $Failed];

dbRead[sid_String] :=
  Module[{rows},
    rows = Quiet @ Check[
      PersonalSite`Database`execute[
        "SELECT user_id,role,state,permissions,meta,\
created_at,expires_at,last_seen \
FROM sessions WHERE session_id=? AND expires_at>datetime('now')",
        {sid}],
      {}];
    If[!ListQ[rows] || Length[rows] === 0,
      Return[$Failed]];
    With[{r = First[rows]},
      <|"sessionId"   -> sid,
        "userId"      -> r[[1]],
        "role"        -> If[IntegerQ[r[[2]]], r[[2]], 1],
        "state"       -> r[[3]],
        "permissions" -> Quiet @ Check[ImportString[r[[4]],"RawJSON"], {}],
        "meta"        -> Quiet @ Check[ImportString[r[[5]],"RawJSON"], <||>],
        "createdAt"   -> r[[6]],
        "expiresAt"   -> r[[7]],
        "lastSeen"    -> r[[8]]|>]
  ];

dbUpdateExpiry[sid_String, newExp_String] :=
  Quiet @ Check[
    PersonalSite`Database`execute[
      "UPDATE sessions SET last_seen=datetime('now'),expires_at=? \
WHERE session_id=?",
      {newExp, sid}],
    $Failed];

dbUpdateState[sid_String, newState_String, newPerms_List] :=
  Quiet @ Check[
    PersonalSite`Database`execute[
      "UPDATE sessions SET state=?,permissions=? WHERE session_id=?",
      {newState, Developer`WriteRawJSONString[newPerms], sid}],
    $Failed];

dbDelete[sid_String] :=
  Quiet @ Check[
    PersonalSite`Database`execute[
      "DELETE FROM sessions WHERE session_id=?", {sid}],
    $Failed];

dbGc[] :=
  Quiet @ Check[
    PersonalSite`Database`execute[
      "DELETE FROM sessions WHERE expires_at<=datetime('now')", {}],
    $Failed];

(* ── Cache key helper ────────────────────────────────────────────────── *)
ckey[sid_String] := "session:" <> sid;

(* ── API publica ─────────────────────────────────────────────────────── *)

PersonalSite`SessionStore`createSession[
    userId_String,
    role_Integer : 1,
    meta : _Association : <||>] :=
  Module[{sid, ts, token, expAt, perms, session},
    sid   = CreateUUID[];
    ts    = Round[AbsoluteTime[]];
    token = buildToken[sid, userId, ts];
    expAt = isoPlus[$ttl];
    perms = PersonalSite`SessionFSM`derivePermissions[role];

    session = <|
      "sessionId"   -> sid,
      "userId"      -> userId,
      "role"        -> role,
      "state"       -> "active",
      "permissions" -> perms,
      "meta"        -> meta,
      "createdAt"   -> isoNow[],
      "expiresAt"   -> expAt,
      "lastSeen"    -> isoNow[]|>;

    PersonalSite`Cache`set[ckey[sid], session];
    dbWrite[sid, userId, role, "active", perms, meta, expAt];

    <|"sessionId" -> sid, "token" -> token, "expiresAt" -> expAt|>
  ];

PersonalSite`SessionStore`validateToken[token_String] :=
  Module[{parts, sid, ts, mac, session, expected},
    parts = parseToken[token];
    If[parts === $Failed, Return[$Failed]];
    {sid, ts, mac} = {parts["sessionId"], parts["ts"], parts["mac"]};
    If[ts === $Failed, Return[$Failed]];

    session = PersonalSite`SessionStore`getSession[sid];
    If[session === $Failed, Return[$Failed]];

    (* Verificar HMAC: usa userId de la sesion almacenada (no del token) *)
    expected = hmacSha256[$secret,
      sid <> "|" <> session["userId"] <> "|" <> ToString[ts]];
    If[mac =!= expected, Return[$Failed]];

    (* Verificar estado activo *)
    If[!MemberQ[{"active", "elevated"}, session["state"]], Return[$Failed]];

    (* Renovar last_seen silenciosamente *)
    PersonalSite`SessionStore`refreshSession[sid];
    session
  ];

(* verifyTokenIdentity: verifica HMAC + existencia, sin chequear estado.
   Permite operar sobre sesiones suspended/expired (resume, destroy, etc.) *)
PersonalSite`SessionStore`verifyTokenIdentity[token_String] :=
  Module[{parts, sid, ts, mac, session, expected},
    parts = parseToken[token];
    If[parts === $Failed, Return[$Failed]];
    {sid, ts, mac} = {parts["sessionId"], parts["ts"], parts["mac"]};
    If[ts === $Failed, Return[$Failed]];
    session = PersonalSite`SessionStore`getSession[sid];
    If[session === $Failed, Return[$Failed]];
    expected = hmacSha256[$secret,
      sid <> "|" <> session["userId"] <> "|" <> ToString[ts]];
    If[mac =!= expected, Return[$Failed]];
    session
  ];

PersonalSite`SessionStore`getSession[sessionId_String] :=
  Module[{cached = PersonalSite`Cache`get[ckey[sessionId]]},
    If[!MissingQ[cached], Return[cached]];
    With[{s = dbRead[sessionId]},
      If[s =!= $Failed,
        PersonalSite`Cache`set[ckey[sessionId], s]];
      s]
  ];

PersonalSite`SessionStore`refreshSession[sessionId_String] :=
  Module[{s = PersonalSite`SessionStore`getSession[sessionId], newExp},
    If[s === $Failed, Return[$Failed]];
    newExp  = isoPlus[$ttl];
    s       = <|s, "expiresAt" -> newExp, "lastSeen" -> isoNow[]|>;
    PersonalSite`Cache`set[ckey[sessionId], s];
    dbUpdateExpiry[sessionId, newExp];
    s
  ];

PersonalSite`SessionStore`applyTransition[sessionId_String, event_String] :=
  Module[{s, result, newSession, reason},
    s = PersonalSite`SessionStore`getSession[sessionId];
    If[s === $Failed, Return[{$Failed, "session_not_found"}]];
    {result, reason} = PersonalSite`SessionFSM`transition[s, event];
    If[result === $Failed, Return[{$Failed, reason}]];
    newSession = result;
    PersonalSite`Cache`set[ckey[sessionId], newSession];
    dbUpdateState[sessionId, newSession["state"], newSession["permissions"]];
    {newSession, reason}
  ];

PersonalSite`SessionStore`destroySession[sessionId_String] :=
  (PersonalSite`Cache`clear[];   (* TODO: eviction puntual si Cache agrega delete[] *)
   dbDelete[sessionId];
   True);

PersonalSite`SessionStore`gcSessions[] :=
  (dbGc[];
   (* Sincronizar cache invalidando todo: las sesiones vigentes se
      recargaran desde DB en el proximo acceso. *)
   PersonalSite`Cache`clear[];
   True);

PersonalSite`SessionStore`sessionStats[] :=
  Module[{rows},
    rows = Quiet @ Check[
      PersonalSite`Database`execute[
        "SELECT state,COUNT(*) FROM sessions \
WHERE expires_at>datetime('now') GROUP BY state", {}],
      {}];
    <|"byState"   -> Association[
                       If[ListQ[rows],
                          Map[Rule[#[[1]], #[[2]]] &, rows], {}]],
      "cacheStats" -> PersonalSite`Cache`stats[]|>
  ];

End[];
