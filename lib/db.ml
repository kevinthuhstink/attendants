type person = {
  name: string;
  seen: bool;
  flaker: bool;
  status: string;
}
let person_to_assoc (p: person): Yojson.Safe.t =
  `Assoc [
    ("name", `String p.name);
    ("seen", `Bool p.seen);
    ("flaker", `Bool p.flaker);
    ("status", `String p.status);
  ]

let open_db (): Sqlite3.db =
  let db = Sqlite3.db_open "attendants.db" in
  let sql = {|
    CREATE TABLE IF NOT EXISTS people (
      name TEXT PRIMARY KEY,
      seen INTEGER NOT NULL DEFAULT 0 CHECK(seen IN (0, 1)),
      flaker INTEGER NOT NULL DEFAULT 0 CHECK(flaker IN (0, 1)),
      status TEXT NOT NULL CHECK(status IN ('confirmed', 'unknown', 'unlikely', 'absent'))
    )|} in
  match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> db
    | _ -> failwith (Printf.sprintf "server.ml open_db(): Unable to initialize attendants.db:\n%s" (Sqlite3.errmsg db))

let fetch_people (db: Sqlite3.db) (status: string): person list =
  let stmt = Sqlite3.prepare db "SELECT * FROM people WHERE status == ?" in
  ignore (Sqlite3.bind_text stmt 1 status);
  let rec loop acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let name = Sqlite3.column_text stmt 0 in
        let seen = Sqlite3.column_bool stmt 1 in
        let flaker = Sqlite3.column_bool stmt 2 in
        let status = Sqlite3.column_text stmt 3 in
        loop ({ name; seen; flaker; status } :: acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | _ -> failwith (Printf.sprintf "server.ml fetch_people(): attendants.db SELECT failure:\n%s" (Sqlite3.errmsg db))
  in
  let results = loop [] in
  ignore (Sqlite3.finalize stmt);
  results

let upsert_person (db: Sqlite3.db) (p: person): unit =
  let stmt = Sqlite3.prepare db "INSERT OR REPLACE INTO people (name, status) VALUES (?, ?)" in
  ignore (Sqlite3.bind_text stmt 1 p.name);
  ignore (Sqlite3.bind_text stmt 2 p.status);
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  ()

let reset (db: Sqlite3.db): unit =
  let stmt = Sqlite3.prepare db "UPDATE people SET status = 'absent', seen = 1" in
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  ()

