let get (): Yojson.Safe.t =
  let db = Db.open_db () in
  let confirmed = Db.fetch_people db "confirmed" in
  let unknown = Db.fetch_people db "unknown" in
  let unlikely = Db.fetch_people db "unlikely" in
  ignore (Sqlite3.db_close db);
  let confirmed_json = `List (List.map Db.person_to_assoc confirmed) in
  let unknown_json = `List (List.map Db.person_to_assoc unknown) in
  let unlikely_json = `List (List.map Db.person_to_assoc unlikely) in
  `Assoc [
    ("confirmed", confirmed_json);
    ("unknown", unknown_json);
    ("unlikely", unlikely_json);
  ]

let put (name: string) (status: string): Yojson.Safe.t =
  let p: Db.person = { name; seen=false; flaker=false; status } in
  let db = Db.open_db () in
  Db.upsert_person db p;
  ignore (Sqlite3.db_close db);
  `Assoc [("status", `String "success")]

let delete (): Yojson.Safe.t =
  let db = Db.open_db () in
  Db.reset db;
  ignore (Sqlite3.db_close db);
  `Assoc [("status", `String "success")]
