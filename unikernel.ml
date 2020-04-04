open Lwt.Infix
module StringMap = Map.Make(String)
module S = Store

module Main (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct

  module C = Control.Make (TIME) (PClock)

  (*Data type for adjacency Matrix*)
  type adjacency_rec = {
    atomic_function: string;
    condition: bool;
  }

  (*Custom defined functions*)
  let f1 store =
    let n = S.to_int (store#get "count" (S.VInt 0)) in
    if n == 0 then begin
      let tstr = C.time () in
      Logs.info (fun m -> m "%s HELLO... (%i)" tstr n);
      store#set "count" (S.VInt (n+1));
      TIME.sleep_ns (Duration.of_sec 1) >>= fun () ->
      Lwt.return ()
    end else begin
      let tstr = C.time () in
      Logs.info (fun m -> m "%s AGAIN (%i)" tstr n);
      store#set "count" (S.VInt (n+1));
      TIME.sleep_ns (Duration.of_sec 1) >>= fun () ->
      Lwt.return ()
    end
  (*The function passed on termination*)
  let terminate store = 
    store#terminate
  
  (*Define the adjacency matrix here as a associative list*)
  let get_adjacency store =
    let assoc_adj_list = [
      ("f1", [
        {atomic_function = "f1"; condition = ((S.to_int (store#get "count" (S.VInt 0))) < 200)};
        {atomic_function = "terminate"; condition = ((S.to_int (store#get "count" (S.VInt 0))) >= 200)};
      ])
    ] in
    let amap = StringMap.of_seq (List.to_seq assoc_adj_list) in
    amap

  (*Define a mapping from function names to functions. Required for serialisation*)
  let get_function_map =
    let functions = [("f1", f1)] in
    let fmap = StringMap.of_seq (List.to_seq functions) in
    fmap

  (*These functions do not need to be edited*)
  let get_next_function_name store (curr: string) = 
    let adjacency = get_adjacency store in
    let adj_arr = StringMap.find curr adjacency in
    let rec inner = function
      | [] -> "terminate"
      | hd::tail -> if hd.condition == true then hd.atomic_function else inner tail
    in inner adj_arr

  let get_function name =
    let functions = get_function_map in
    match name with
      | "terminate" -> terminate
      | _ -> StringMap.find name functions

  let rec run store (client: OS.Xs.client) (curr: string) =
    C.read_shutdown_value client >>= fun status ->
    match status with
      | Control.Status.Resume -> begin
          let f = get_function curr in
          f store >>= fun () ->
          let fnext_name = get_next_function_name store curr in
          store#set "next" (S.VString fnext_name);
          run store client fnext_name 
        end
      | _ -> begin 
          let time = C.time () in 
          store#suspend time status
        end

  let start _time _pclock resolver conduit =
    let tstr = C.time () in
    Logs.info (fun m -> m "start-TS: %s" tstr);
    let token = Key_gen.token () in
    let repo = Key_gen.repo () in
    let migration = Key_gen.migration () in
    let id = Key_gen.id () in
    let host_id = Key_gen.hostid () in
    let store = new S.webStore conduit resolver repo token id host_id in
    let time = C.time () in 
    store#init time migration (C.steady) >>= fun _ ->
    let fct = (S.to_str (store#get "next" (S.VString "f1"))) in
    OS.Xs.make () >>= fun client ->
    run store client fct
end
