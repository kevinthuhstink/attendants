open Lwt
open Cohttp
open Cohttp_lwt_unix

let text_response (body: string) =
  let headers = Header.init_with "Content-Type" "text/plain" in
  Server.respond_string ~status:`OK ~headers ~body ()

let json_response (body: Yojson.Safe.t) =
  let json_body = Yojson.Safe.to_string body in
  let headers = Header.init_with "Content-Type" "application/json" in
  Server.respond_string ~status:`OK ~headers ~body:json_body ()

let json_unauthenticated =
  let json_body = Yojson.Safe.to_string (`Assoc [("status", `String "failure"); ("reason", `String "unauthenticated")]) in
  let headers = Header.init_with "Content-Type" "application/json" in
  Server.respond_string ~status:`Unauthorized ~headers ~body:json_body ()

let validate_auth headers =
  let auth = Sys.getenv "ATTENDANTS_AUTH" in
  match Header.get headers "Authorization" with
  | Some h -> String.equal auth h
  | _ -> false

let parse_put (json: Yojson.Safe.t): (string * string list) =
  let fail = fun (emsg: string) ->
    print_endline ("PUT /: " ^ emsg);
    failwith emsg
  in
  match json with
  | `Assoc fields ->
      let status =
        match List.assoc "status" fields with
        | `String s -> s
        | _ -> fail "field \"status\" must be a string"
        | exception Not_found -> fail "field \"status\" required"
      in
      begin
        match List.assoc "names" fields with
        | `List names ->
          let names_l = List.map (function
            | `String s -> s
            | _ -> fail "field \"names\" must be a string[]"
          ) names in (status, names_l)
        | exception Not_found ->
            begin
              match List.assoc "name" fields with
              | `String name -> (status, [name])
              | exception Not_found -> fail "field \"names\" or \"name\" is required"
              | _ -> fail "Expected either \"names\" string[] or \"name\" string"
            end
        | _ -> fail "field \"names\" must be a string[]"
      end
  | _ -> fail "Invalid JSON structure"

let handle_request (meth: Code.meth) path headers body =
  match (meth, path) with
  | (`GET, "/") -> text_response (Handlers.get ())
  | (`PUT, "/") ->
      if
        validate_auth headers
      then
        Cohttp_lwt.Body.to_string body >>= fun body_string ->
        let json = Yojson.Safe.from_string body_string in
        let status, names = parse_put json in
        json_response (Handlers.put names status)
      else
        json_unauthenticated
  | (`DELETE, "/") ->
      if
        validate_auth headers
      then
        json_response (Handlers.delete ())
      else
        json_unauthenticated
  | _ ->
      Server.respond_string ~status:`Not_found ~body:"404 Not Found" ()

let start_server =
  ignore (Sys.getenv "ATTENDANTS_AUTH");
  let port =
    match Sys.getenv_opt "ATTENDANTS_PORT" with
    | Some port -> int_of_string port
    | None -> 7921
  in
  let callback _conn req body =
    let meth = Request.meth req in
    let uri = Request.uri req in
    let path = Uri.path uri in
    let headers = Request.headers req in
    handle_request meth path headers body
  in
  let config = Server.make ~callback () in
  Printf.printf "Server running on http://localhost:%d/\n" port;
  flush stdout;
  Server.create ~mode:(`TCP (`Port port)) config
