open! Base

type t = Score of int [@@deriving compare, sexp]

let create () = Score 0

let score (Score x) = Score (x + 1)

let to_string (Score x) = Int.to_string x
