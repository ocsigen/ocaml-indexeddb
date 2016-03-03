(* Copyright (C) 2015, Thomas Leonard. See the LICENSE file for
   details. *)

module type Js_string_conv = sig

  type key
  val of_key : key -> Js.js_string Js.t
  val to_key : Js.js_string Js.t -> key

  type content
  val of_content : content -> Js.js_string Js.t
  val to_content : Js.js_string Js.t -> content

end

module type DB = sig

  type db
  type db_upgrader

  type db_name
  val db_name : string -> db_name

  type store_name
  val store_name : string -> store_name

  (** Connect to database [db_name]. If it doesn't yet exist or is for
      an older version, calls [init] to initialise it first. *)
  val make :
    db_name ->
    version:int ->
    init:(db_upgrader -> unit) ->
    db Lwt.t

  (** Begin closing the connection (returns immediately). *)
  val close : db -> unit

  val delete_database : db_name -> unit Lwt.t

  val create_store : db_upgrader -> store_name -> unit

end

module type STORE = sig

  type db
  type store_name

  type store
  type key
  type content

  val store : db -> store_name -> store

  val set : store -> key -> content -> unit Lwt.t

  val get : store -> key -> content option Lwt.t

  val remove : store -> key -> unit Lwt.t

  (** If [test current_value] for returns true for the current value
      of [key], replace it with [new_value].

      If [new_value] is None, the key is deleted.

      This happens in a single atomic transaction. *)
  val compare_and_set :
    store -> key ->
    test:(content option -> bool) ->
    new_value:content option ->
    bool Lwt.t

  val fold :
    ('acc -> key -> content -> 'acc) ->
    'acc ->
    store ->
    'acc Lwt.t

  val bindings : store -> (key * content) list Lwt.t

end

module type STORE_POLY = sig

  type db
  type store_name

  type 'a store
  type key
  type 'a content

  val store : db -> store_name -> 'a store

  val set : 'a store -> key -> 'a content -> unit Lwt.t

  val get : 'a store -> key -> 'a content option Lwt.t

  val remove : 'a store -> key -> unit Lwt.t

  (** If [test current_value] for returns true for the current value
      of [key], replace it with [new_value].

      If [new_value] is None, the key is deleted.

      This happens in a single atomic transaction. *)
  val compare_and_set :
    'a store -> key ->
    test:('a content option -> bool) ->
    new_value:'a content option ->
    bool Lwt.t

  val fold :
    ('acc -> key -> 'a content -> 'acc) ->
    'acc ->
    'a store ->
    'acc Lwt.t

  val bindings : 'a store -> (key * 'a content) list Lwt.t

end
