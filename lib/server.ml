open Cohttp
open Cohttp_lwt_unix

type person = {
  name: string;
  seen: bool;
  flaker: bool;
  status: string;
}
let person_to_assoc (p: person): Yojson.Safe.t =
  `Assoc [
    ("person", `String p.name);
    ("seen", `Bool p.seen);
    ("flaker", `Bool p.flaker);
    ("status", `String p.status);
  ]

let open_db (): Sqlite3.db =
  let db = Sqlite3.db_open "attendants.db" in
  let sql = {|
    CREATE TABLE IF NOT EXISTS people (
      name TEXT PRIMARY KEY,
      seen INTEGER NOT NULL DEFAULT 0,
      flaker INTEGER NOT NULL DEFAULT 0,
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

let jsonify (body: Yojson.Safe.t) =
  let json_body = Yojson.Safe.to_string body in
  let headers = Header.init_with "Content-Type" "application/json" in
  Printf.printf "%s\n" json_body;
  flush stdout;
  Server.respond_string ~status:`OK ~headers ~body:json_body ()

let handle_request (meth: Code.meth) path _body =
  match (meth, path) with
  | (`GET, "/") ->
      let db = open_db () in
      let people = fetch_people db "confirmed" in
      let people_json = `List (List.map person_to_assoc people)
      ignore (Sqlite3.db_close db);
      jsonify (`Assoc [("confirmed", people_json)])
  | (`PUT, "/")
  | (`DELETE, "/") ->
      jsonify (`Assoc [("status", `String "success")])
  | _ ->
      Server.respond_string ~status:`Not_found ~body:"404 Not Found" ()

let start_server =
  let port = 
    match Sys.getenv_opt "ATTENDANTS_PORT" with
    | Some port -> int_of_string port
    | None -> 7921
  in
  let callback _conn req body =
    let meth = Request.meth req in
    let uri = Request.uri req in
    let path = Uri.path uri in
    handle_request meth path body
  in
  let config = Server.make ~callback () in
  Printf.printf "Server running on http://localhost:%d/\n" port;
  flush stdout;
  Server.create ~mode:(`TCP (`Port port)) config
