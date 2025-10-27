let get (): Yojson.Safe.t =
  let db = Db.open_db () in
  let confirmed = Db.fetch_people db "confirmed" in
  let unknown = Db.fetch_people db "unknown" in
  let unlikely = Db.fetch_people db "unlikely" in
  ignore (Sqlite3.db_close db);
  let confirmed_json = `List (List.map (fun s -> `String s) confirmed) in
  let unknown_json = `List (List.map (fun s -> `String s) unknown) in
  let unlikely_json = `List (List.map (fun s -> `String s) unlikely) in
  `Assoc [
    ("confirmed", confirmed_json);
    ("unknown", unknown_json);
    ("unlikely", unlikely_json);
  ]

let put (names: string list) (status: string): Yojson.Safe.t =
  let db = Db.open_db () in
  List.iter (fun name -> Db.upsert_person db name status) names;
  ignore (Sqlite3.db_close db);
  `Assoc [("status", `String "success")]

let delete (): Yojson.Safe.t =
  let db = Db.open_db () in
  Db.reset db;
  ignore (Sqlite3.db_close db);
  `Assoc [("status", `String "success")]
