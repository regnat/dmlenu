(** State of the completion engine *)

type t = {
  separator: string; 
  before_cursor: string;
  after_cursor : string;
  program: Program.t;
  sources : Source.state list; 
  candidates: (Candidate.t * Matching.result) Pagination.t;
  compl_sources : Source.state list;
  compl_candidates: (Candidate.t * Matching.result) Pagination.t;
  entries: (Program.t * Candidate.t) list;
  splitm: (Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list;
  splitc: (Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list;
}

(** The type of state *)

val initial : separator: string -> program: Program.t -> 
  splitm: ((Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list) ->
  splitc: ((Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list) -> t
(** Initial state *)

val on_modify : t -> t
(** Function to be called whenever the input is modified *)

val add_char : string -> t -> t
(** Add a char to the current input. *)

val complete : t -> t * bool
(** Tries to complete the current selected candidate. *)

val left : t -> t
(** Moves left *)

val right : t -> t
(** Moves right *)

val up : t -> t
(** Moves up *)
val down : t -> t
(** Moves down *)

val remove : t -> t * bool
(** Simulate a backspace. The boolean tells you whether the current token has changed *)

val get_list : t -> string list
(** Returns the value of all tokens currently accumulated *)