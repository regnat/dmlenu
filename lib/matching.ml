(* This file deals with matching substring *)
open Core.Std

type result = ((bool * int * int) list)
type t = string -> result option

let handle_case case_sensitive query candidate =
  if case_sensitive then
    query, candidate
  else
    String.lowercase query, String.lowercase candidate

let make_list candidate list =
  let list, old =
    List.fold_left ~f:(fun (list, old) (start', stop') ->
      (true, start', stop') :: (false, old, start') :: list, stop'
    ) ~init:([], 0) list
  in
  let list = List.filter ~f:(fun (_, k, k') -> k <> k')
    (List.rev ((false, old, String.length candidate) :: list)) in
  list

let subset ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  try
    let words = List.filter ~f:((<>) "") (String.split query ~on:' ') in
    let matches = List.map words ~f:(fun word ->
      match String.substr_index candidate ~pattern:word with
      | Some n -> (n, n + String.length word)
      | None -> raise Not_found
      )
    in
    Some
      (make_list candidate
         (List.sort (fun x y -> compare (fst x) (fst y)) matches))
  with Not_found -> None

let partial_match ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
    Option.map (String.substr_index candidate ~pattern:query)
    ~f:(fun k ->
    [
      (false, 0, k) ;
      (true, k, k + String.length query) ;
      (false, k + String.length query, String.length candidate) ;
    ]
    )

let match_prefix ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  if not (String.is_prefix candidate ~prefix:query) then None else
  let qlen = String.length query in
  Some [ (true, 0, qlen) ; (false, qlen, String.length candidate) ]

let fuzzy_match ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  let find_char (lst, offset, rest) c =
    let skipped, rest = String.lsplit2_exn rest ~on:c in
    let offset' = offset + String.length skipped in
    let lst' =
      (true, offset', offset' + 1) ::
      (false, offset, offset') ::
      lst
    in
    (lst', offset' + 1, rest)
  in
  try
    let (lst,offset,_) = String.fold ~f:find_char ~init:([], -1, candidate) query in
    Some (List.rev @@ (false, offset, String.length candidate) :: lst)
  with Not_found ->
    None

let fuzzy_prefix ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  if query <> "" && candidate <> "" && candidate.[0] <> query.[0] then None else
  fuzzy_match ~case ~candidate query

let trivial ~candidate _ = Some [false, 0, String.length candidate]

(* ************************************************************************** *)
let default_match_fun = ref (match_prefix ~case:true)

let set_match_query_fun f = default_match_fun := f

let match_query ~candidate query = !default_match_fun ~candidate query

let on f t = fun s -> t (f s)
