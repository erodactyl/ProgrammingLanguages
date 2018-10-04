signature LINKEDLIST =
sig
  exception EmptyList
  datatype 'a linkedlist = Empty | Node of ('a * ('a linkedlist))
  val append : 'a * 'a linkedlist -> 'a linkedlist
  val prepend : 'a * 'a linkedlist -> 'a linkedlist
  val head : 'a linkedlist -> 'a
  val tail : 'a linkedlist -> 'a linkedlist
  val concat : 'a linkedlist * 'a linkedlist -> 'a linkedlist
  val toNativeList : 'a linkedlist -> 'a list
  val map : ('a -> 'b) -> 'a linkedlist -> 'b linkedlist
  val fold : ('a * 'b -> 'b) -> 'b -> 'a linkedlist -> 'b
  val length : 'a linkedlist -> int
end

structure LinkedList :> LINKEDLIST =
struct
  exception EmptyList
  datatype 'a linkedlist = Empty | Node of ('a * ('a linkedlist))
  fun append (el, list) =
  case list of
    Empty => Node(el, Empty)
    | Node _ => Node(el, list)

  fun prepend (el, list) =
    case list of
      Empty => Node(el, Empty)
      | Node(head, tail) => append(head, prepend(el, tail))

  fun head list =
    case list of
      Empty => raise EmptyList
      | Node(h, t) => h

  fun tail list =
    case list of
      Empty => raise EmptyList
      | Node(h, t) => t

  fun concat (list1, list2) =
    case list1 of
      Empty => list2
      | Node(head, tail) => Node(head, concat(tail, list2))

  fun map f list =
    case list of
      Empty => Empty
      | Node(head, tail) => append(f head, map f tail)

  fun toNativeList list =
    case list of
      Empty => []
      | Node(head, tail) => head::toNativeList(tail)

  fun fold reducer value list =
    case list of
      Empty => value
      | Node(head, tail) => fold reducer (reducer(head, value)) tail

  fun length list =
    case list of
      Empty => 0
      | Node(_, tail) => 1 + length tail
end
