use "linked_list.sml";

signature BINARYTREE =
sig
  datatype tree = Node of int * tree * tree | Leaf
  val insert : tree * int -> tree
  val delete : tree * int -> tree
  val traverse : tree -> int LinkedList.linkedlist
end

structure BinaryTree :> BINARYTREE =
struct
  exception EmptyTree
  datatype tree = Node of int * tree * tree | Leaf
  
  fun rightmost_child tree =
    case tree of
      Leaf => raise EmptyTree
      | Node(curr, _, Leaf) => curr
      | Node(curr, _, right) => rightmost_child(right)

  fun insert (tree, value) =
    case tree of
      Leaf => Node(value, Leaf, Leaf)
      | Node(curr, left, right) =>  if value > curr
                                    then Node(curr, left, insert(right, value))
                                    else Node(curr, insert(left, value), right)
  
  fun delete (tree, value) =
    case tree of
      Leaf => Leaf
      | Node(curr, Leaf, Leaf) => if curr = value 
                                  then Leaf
                                  else tree
      | Node(curr, Node(head, left, right), Leaf) =>  if curr = value
                                                      then Node(head, left, right)
                                                      else tree
      | Node(curr, left, right) =>  if curr = value
                                    then Node(rightmost_child(tree), left, right)
                                    else if value > curr
                                    then Node(curr, left, delete(right, value))
                                    else Node(curr, delete(left, value), right)

  fun traverse tree =
    case tree of
      Leaf => LinkedList.Empty
      | Node(curr, left, right) => LinkedList.concat(traverse left, LinkedList.append(curr, traverse right))
end