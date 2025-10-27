let open_db (): Sqlite3.db =
  let db = Sqlite3.db_open "attendants.db" in
  let sql = {|
    CREATE TABLE IF NOT EXISTS people (
      name TEXT PRIMARY KEY,
      status TEXT NOT NULL CHECK(status IN ('confirmed', 'unknown', 'unlikely', 'absent'))
    )|} in
  match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> db
    | _ -> failwith (Printf.sprintf "server.ml open_db(): Unable to initialize attendants.db:\n%s" (Sqlite3.errmsg db))

let fetch_people (db: Sqlite3.db) (status: string): string list =
  let stmt = Sqlite3.prepare db "SELECT name FROM people WHERE status == ?" in
  ignore (Sqlite3.bind_text stmt 1 status);
  let rec loop acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let name = Sqlite3.column_text stmt 0 in
        loop (name :: acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | _ -> failwith (Printf.sprintf "server.ml fetch_people(): attendants.db SELECT failure:\n%s" (Sqlite3.errmsg db))
  in
  let results = loop [] in
  ignore (Sqlite3.finalize stmt);
  results

let upsert_person (db: Sqlite3.db) (name: string) (status: string): unit =
  let stmt = Sqlite3.prepare db "INSERT OR REPLACE INTO people (name, status) VALUES (?, ?)" in
  ignore (Sqlite3.bind_text stmt 1 name);
  ignore (Sqlite3.bind_text stmt 2 status);
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  ()

let reset (db: Sqlite3.db): unit =
  let stmt = Sqlite3.prepare db "UPDATE people SET status = 'absent', seen = 1" in
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  ()

