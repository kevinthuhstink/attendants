open Cohttp
open Cohttp_lwt_unix

let json_success_response () =
  let body = `Assoc [("status", `String "success")] in
  let json_body = Yojson.Basic.to_string body in
  let headers = Header.init_with "Content-Type" "application/json" in
  Server.respond_string ~status:`OK ~headers ~body:json_body ()

let handle_request (meth : Code.meth) path _body =
  match (meth, path) with
  | (`GET, "/")
  | (`PUT, "/")
  | (`DELETE, "/") ->
      json_success_response ()
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

let () = Lwt_main.run start_server
