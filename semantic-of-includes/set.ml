module type Set = sig
  type 'a t

  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val elements : 'a t -> 'a list
end

module ListSetImpl = struct
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s
end

module ListSet : Set = ListSetImpl

module type SetExt = sig
  include Set

  val of_list : 'a list -> 'a t
end

module ListSetExtedImpl = struct
  include ListSetImpl

  let of_list lst = lst
end

module ListSetExted: SetExt = ListSetExtedImpl

module UniqListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add s lst = if mem s lst then lst else s :: lst
  let elements = Fun.id
end

module OfList(S: Set) = struct
  let of_list lst = List.fold_right S.add lst S.empty
end

module UniqListSet' = OfList(UniqListSet)

module SetTester (S: Set) = struct
  let test = [
    fun _ -> if (S.add 1 S.empty) <> S.empty then true else false
  ]
end

let set_kinds = [ (module ListSetImpl : Set ); (module ListSetExtedImpl); ]
let all_tests =
  let test_generator s =
    let module S = (val s : Set) in
    let module T = SetTester (S) in
    T.test in
  set_kinds |> List.map test_generator |> List.flatten
