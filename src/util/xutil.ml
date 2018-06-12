open Core

module XList = struct

  let rec zip lst1 lst2 = match lst1, lst2 with
    | [], _ -> []
    | _, [] -> []
    | (x :: xs), (y :: ys) -> (x, y) :: (zip xs ys)

  let unzip lst =
    let f (l, s) (x, y) = (x::l, y::s) in
    List.fold_left ~f:f ~init:([],[]) (List.rev lst)

  let range i j =
    let rec aux n acc =
      if n < i then acc else aux (n - 1) (n :: acc)
    in aux j []

  let print_list f lst =
    let rec print_elements = function
      | [] -> ()
      | h :: t -> f h; print_string ";"; print_elements t
    in
    print_string "["; print_elements lst; print_string "]"

end

module XArray = struct
  let print_array f arr =
    let rec print_elements = function
      | [] -> ()
      | h :: t -> f h; print_string ";"; print_elements t
    in
    print_string "[|"; print_elements (Array.to_list arr); print_string "|]"
end

module XString = struct

  let string_to_list str =
    let rec loop i limit =
      if i = limit then []
      else (String.get str i) :: (loop (i + 1) limit)
    in
    loop 0 (String.length str)

  let string_after s n =
    String.sub s n (String.length s - n)

  let after_of str chr =
    let index = ref 0 in
    for i = 0 to (String.length str - 1) do
      if (String.get str i) = chr then index := i;
    done;
    string_after str (!index + 1)

  let contains str1 str2 =
    let re = Str.regexp_string str2 in
    try
      ignore (Str.search_forward re str1 0); true
    with Not_found ->
      false

  let before_of str chr =
    match String.index str chr with
    | Some i -> Str.string_before str i
    | None -> failwith (Printf.sprintf "before_of %s is failed." str)

end


module String = struct
  include String
  include XString
end

module List = struct
  include List
  include XList
end

module Array = struct
  include Array
  include XArray
end
