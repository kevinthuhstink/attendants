let pad3 (s: string) =
  let len = String.length s in
  if len >= 3
  then s
  else
    let padding = String.make (3 - len) ' ' in
    padding ^ s

let get (): string =
  let db = Db.open_db () in
  let confirmed = Db.fetch_people db "confirmed" in
  let unknown = Db.fetch_people db "unknown" in
  let unlikely = Db.fetch_people db "unlikely" in
  ignore (Sqlite3.db_close db);

  let namei_tostr = fun i name -> (pad3 (string_of_int (1 + i))) ^ " " ^ name in
  let confirmedi = List.mapi namei_tostr confirmed in
  let unknowni = List.mapi namei_tostr unknown in
  let unlikelyi = List.mapi namei_tostr unlikely in

  let newline_concat = fun acc x -> acc ^ x ^ "\n" in
  let confirmed_str = List.fold_left newline_concat "CONFIRMED\n" confirmedi in
  let unknown_str = List.fold_left newline_concat "UNKNOWN\n" unknowni in
  let unlikely_str = List.fold_left newline_concat "UNLIKELY\n" unlikelyi in
  confirmed_str ^ "\n" ^ unknown_str ^ "\n" ^ unlikely_str

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
