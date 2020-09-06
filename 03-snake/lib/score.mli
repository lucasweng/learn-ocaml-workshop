open! Base

type t [@@deriving compare, sexp]

val create : unit -> t

val score : t -> t
