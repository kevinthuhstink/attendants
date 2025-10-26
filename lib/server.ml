open Lwt
open Cohttp
open Cohttp_lwt_unix

let jsonify (body: Yojson.Safe.t) =
  let json_body = Yojson.Safe.to_string body in
  let headers = Header.init_with "Content-Type" "application/json" in
  Server.respond_string ~status:`OK ~headers ~body:json_body ()

let handle_request (meth: Code.meth) path body =
  match (meth, path) with
  | (`GET, "/") -> jsonify (Handlers.get ())
  | (`PUT, "/") ->
      Cohttp_lwt.Body.to_string body >>= fun body_string ->
      let json = Yojson.Safe.from_string body_string in
      let name =
        match json with
        | `Assoc assoc ->
            (List.assoc "name" assoc |> function
             | `String s -> s
             | _ -> failwith "name must be a string")
        | _ -> failwith "Expected JSON object"
      in
      let status =
        match json with
        | `Assoc assoc ->
            (List.assoc "status" assoc |> function
             | `String s -> s
             | _ -> failwith "status must be a string")
        | _ -> failwith "Expected JSON object"
      in
      jsonify (Handlers.put name status)
  | (`DELETE, "/") -> jsonify (Handlers.delete ())
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
