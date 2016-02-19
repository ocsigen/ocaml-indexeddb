(* Copyright (C) 2015, Thomas Leonard. See the LICENSE file for
   details. *)

open Lwt.Infix

let (>|?=) x f =
  match x with
  | None   -> None
  | Some x -> Some (f x)

module Make (C : Idb_sigs.Js_string_conv) = struct

  type db = Idb_js_api.database Js.t

  type db_upgrader = Idb_js_api.database Js.t

  type db_name = Js.js_string Js.t
  let db_name = Js.string

  type store_name = Js.js_string Js.t
  let store_name = Js.string

  type store = {
    db : db;
    store_name : store_name;

    (* We reuse transactions where possible for performance.

       This does mean that if any read fails then the others will
       hang, but we treat any read failing as a fatal error anyway. *)
    mutable ro_trans :
      (Idb_js_api.transaction Js.t * (exn -> unit) list ref) option;
  }

  type key = C.key
  type content = C.content

  let opt_string x ~if_missing =
    Js.Optdef.case x
      (fun () -> if_missing)
      (fun x  -> Js.to_string x)

  exception AbortError

  let idb_error typ
      (event:Idb_js_api.request Idb_js_api.errorEvent Js.t) =
    let failure msg =
      Failure
        (Printf.sprintf "IndexedDB operation (%s) failed: %s" typ msg)
    in
    Js.Opt.case (event##target)
      (fun () -> failure "(missing target on error event)")
      (fun target ->
         Js.Opt.case (target##error)
           (fun () -> failure "(missing error on request)")
           (fun error ->
              let name =
                opt_string (error##name)
                  ~if_missing:"(no name)"
              in
              let message =
                opt_string (error##message)
                  ~if_missing:"(no message)"
              in
              let code = Js.Optdef.get (error##code) (fun () -> 0) in
              if name = "AbortError" then
                AbortError
              else
                failure
                  (Printf.sprintf "%s: %s (error code %d)"
                     name message code)))

  let get_factory () =
    let factory : Idb_js_api.factory Js.t Js.Optdef.t =
      (Obj.magic Dom_html.window)##indexedDB
    in
    Js.Optdef.get factory
      (fun () -> failwith "IndexedDB not available")

  let make db_name ~version ~init =
    let factory = get_factory () in
    let request = factory##_open (db_name, version) in
    let t, set_t = Lwt.wait () in
    request##onblocked <- Dom.handler (fun _event ->
        print_endline
          "Waiting for other IndexedDB users to close \
           their connections before upgrading schema version.";
        Js._true
      );
    request##onupgradeneeded <- Dom.handler (fun _event ->
        try
          init (request##result);
          Js._true
        with ex ->
          (* Firefox throws the exception away and returns AbortError
             instead, so save it here. *)
          Lwt.wakeup_exn set_t ex;
          raise ex
      );
    request##onerror <- Dom.handler (fun event ->
        begin match Lwt.state t, idb_error "open" event with
          | Lwt.Fail _, AbortError ->
            () (* Already reported a better exception *)
          | _, ex ->
            Lwt.wakeup_exn set_t ex
        end;
        Js._true
      );
    request##onsuccess <- Dom.handler (fun _event ->
        Lwt.wakeup set_t (request##result);
        Js._true
      );
    t

  let close db =
    db##close ()

  let delete_database db_name =
    let factory = get_factory () in
    let request = factory##deleteDatabase(db_name) in
    let t, set_t = Lwt.wait () in
    request##onerror <- Dom.handler (fun _event ->
        Lwt.wakeup_exn set_t
          (Failure "Error trying to delete IndexedDB database");
        Js._true
      );
    request##onsuccess <- Dom.handler (fun _event ->
        Lwt.wakeup set_t ();
        Js._true
      );
    t

  let store db store_name = { db; store_name; ro_trans = None }

  let create_store db name =
    db##createObjectStore (name) |> ignore

  let rec trans_ro (t:store) setup =
    let r, set_r = Lwt.wait () in
    match t.ro_trans with
    | None ->
      let breakers = ref [Lwt.wakeup_exn set_r] in
      let trans =
        t.db##transaction
          (Js.array [| t.store_name |], Js.string "readonly") in
      t.ro_trans <- Some (trans, breakers);
      trans##onerror <- Dom.handler (fun event ->
          t.ro_trans <- None;
          let ex = idb_error "RO" event in
          if ex = AbortError then
            print_endline
              "IndexedDB transaction failed (Safari bug?): will retry";
          !breakers |> List.iter (fun b -> b ex);
          Js._true
        );
      trans##oncomplete <- Dom.handler (fun _event ->
          t.ro_trans <- None;
          Js._true
        );
      setup (trans##objectStore (t.store_name)) set_r;
      r
    | Some (trans, breakers) ->
      (* Seems we can get here when a transaction is done but
         oncomplete hasn't been called, so retry if we get an
         error. *)
      try
        setup (trans##objectStore (t.store_name)) set_r;
        breakers := Lwt.wakeup_exn set_r :: !breakers;
        r
      with _ex ->
        t.ro_trans <- None;
        trans_ro t setup

  (* On Safari, transactions can fail unpredictably, so wrap
     [trans_ro] with auto-retry. See:

     https://github.com/talex5/cuekeeper/issues/9 *)
  let trans_ro t setup =
    let rec retry delay =
      Lwt.catch
        (fun () -> trans_ro t setup)
        (function
          | AbortError ->
            Lwt_js.sleep (Random.float delay) >>= fun () ->
            retry (delay *. 1.2)
          | ex ->
            Lwt.fail ex)
    in
    retry 1.0

  let trans_rw t setup =
    let r, set_r = Lwt.wait () in
    let trans =
      t.db##transaction
        (Js.array [| t.store_name |], Js.string "readwrite")
    in
    trans##onerror <- Dom.handler (fun event ->
        Lwt.wakeup_exn set_r (idb_error "RW" event);
        Js._true
      );
    trans##oncomplete <- Dom.handler (fun _event ->
        Lwt.wakeup set_r ();
        Js._true
      );
    setup (trans##objectStore (t.store_name));
    r

  let fold f acc t =
    let acc = ref acc in
    trans_ro t @@ fun store set_r ->
    let request = store##openCursor () in
    request##onsuccess <-
      Dom.handler @@ fun _event ->
      Js.Opt.case (request##result)
        (fun () -> Lwt.wakeup set_r !acc)
        (fun cursor ->
           let key = C.to_key cursor##key
           and value = C.to_content cursor##value in
           acc := f !acc key value;
           cursor##continue ());
      Js._true

  let bindings t =
    let acc = []
    and f acc key value = (key, value) :: acc in
    fold f acc t

  let set t key value =
    trans_rw t @@ fun store ->
    ignore (store##put(C.of_content value, C.of_key key))

  let remove t key =
    trans_rw t @@ fun store ->
    ignore (store##delete(C.of_key key))

  let get t key =
    trans_ro t @@ fun store set_r ->
    let request = store##get(C.of_key key) in
    request##onsuccess <-
      Dom.handler @@ fun _event ->
      Js.Optdef.to_option (request##result)
      >|?= C.to_content
      |>   Lwt.wakeup set_r;
      Js._true

  let compare_and_set t key ~test ~new_value =
    let result = ref None
    and key = C.of_key key in
    (trans_rw t @@ fun store ->
     let request = store##get(key) in
     request##onsuccess <-
       Dom.handler (fun _event ->
           if
             request##result
             |>   Js.Optdef.to_option
             >|?= C.to_content
             |>   test
           then (
             begin
               ignore @@ match new_value with
               | None ->
                 store##delete(key)
               | Some new_value ->
                 store##put(C.of_content new_value, key)
             end;
             result := Some true
           ) else (
             result := Some false
           );
           Js._true))
    >|= fun () ->
    match !result with
    | None -> failwith "Transaction completed, but no result!"
    | Some x -> x

end

module String = Make(struct

    type key = string
    let of_key = Js.string
    let to_key = Js.to_string

    type content = string
    let of_content = Js.string
    let to_content = Js.to_string

  end)
