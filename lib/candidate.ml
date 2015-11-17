open Core.Std

type t = {
  display: string;
  real: string;
  doc : string;
  completion: string;
  matching_function: Matching.t;
}

let make ?real ?(doc = "") ?matching_function ?completion display : t = {
  display; doc;
  real = Option.value ~default:display real;
  completion = Option.value ~default:display completion;
  matching_function = 
    Option.value ~default:(Matching.match_query ~candidate: display) matching_function;
}

(* ************************************************************************** *)

let prefixes_first =
  List.partition_tf ~f:(function
    | (_, [(true, _, _); (false, _, _)]) -> true (* TODO: add perfect matching case *)
    |  _                                 -> false)
  |> Fn.compose (fun (l1, l2) -> l1@l2)

let reorder_matched_fun = ref ident
let reorder_matched l = !reorder_matched_fun l
let set_reorder_matched_fun f = reorder_matched_fun := f
