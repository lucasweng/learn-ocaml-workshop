open! Base

type t = Score of int [@@deriving compare, sexp]

let create () = Score 0

let score (Score x) = Score (x + 1)
