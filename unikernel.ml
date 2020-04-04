open Lwt.Infix

module S = Store

module Main (TIME: Mirage_time.S) (PClock: Mirage_clock.PCLOCK) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) = struct

  module C = Control.Make (TIME) (PClock)

  type adjacency_rec = {
    atomic_function: string;
    condition: bool;
  }

  let f1 store =
    let n = S.to_int (store#get "count" (S.VInt 0)) in
    let loop = function
    | 200 -> Lwt.return Control.Status.Terminate
    | n ->
      if n = 0 
        then 
          Logs.info (fun m -> m "HELLO... (%i)" n)
        else 
          Logs.info (fun m -> m "AGAIN (%i)" n);
      store#set "count" (S.VInt (n+1));
      TIME.sleep_ns (Duration.of_sec 1) >>= fun () ->
    in loop n

  let get_adjacency store =
    let assoc_adj_list = [
      ("f1", [
        {atomic_function = "f1"; condition = ((S.to_int (store#get "count" (S.VInt 0))) <= 250)};
        {atomic_function = "terminate"; condition = ((S.to_int (store#get "count" (S.VInt 0))) > 250)};
      ])
    ] in
    let amap = StringMap.of_seq (List.to_seq assoc_adj_list) in
    amap

  (*Define a mapping from function names to functions. Required for serialisation*)
  let get_function_map =
    let functions = [("f1", f1)] in
    let fmap = StringMap.of_seq (List.to_seq functions) in
    fmap
  
  let start _time _pclock res (ctx: CON.t) =
    let tstr = C.time () in
    Logs.info (fun m -> m "start-TS: %s" tstr);
    let token = Key_gen.token () in
    let repo = Key_gen.repo () in
    let migration = Key_gen.migration () in
    let id = Key_gen.id () in
    let host_id = Key_gen.hostid () in
    let store = new S.webStore conduit resolver repo token id host_id in
    let time = C.time (`) in 
    store#init time migration (C.steady) >>= fun _ ->
    let fct = (S.to_str (store#get "next" (S.VString "f1"))) in
    OS.Xs.make () >>= fun client ->
    run store client fct
end
