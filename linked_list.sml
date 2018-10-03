signature LINKEDLIST =
sig
  exception EmptyList
  datatype 'a linkedlist = Empty | Node of ('a * ('a linkedlist))
  val append : 'a linkedlist * 'a -> 'a linkedlist
  val prepend : 'a linkedlist * 'a -> 'a linkedlist
  val head : 'a linkedlist -> 'a
  val tail : 'a linkedlist -> 'a linkedlist
end

structure LinkedList :> LINKEDLIST =
struct
  exception EmptyList
  datatype 'a linkedlist = Empty | Node of ('a * ('a linkedlist))
  fun append (list, el) =
  case list of
    Empty => Node(el, Empty)
    | Node _ => Node(el, list)

  fun prepend (list, el) =
    case list of
      Empty => Node(el, Empty)
      | Node(head, tail) => append(prepend(tail, el), head)

  fun head list =
    case list of
      Empty => raise EmptyList
      | Node(h, t) => h

  fun tail list =
    case list of
      Empty => raise EmptyList
      | Node(h, t) => t
end
