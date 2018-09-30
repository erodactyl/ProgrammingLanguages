(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

fun longest_string1 strings =
  #longest (List.foldl
    (fn (s, { size, longest }) =>
      let val len = String.size(s) (* Calculating String.size only once for each string *)
      in if len > size then { size=len, longest=s } else { size=size, longest=longest }
      end) { size=0, longest="" } strings)

fun longest_string2 strings =
  #longest (List.foldl
    (fn (s, { size, longest }) =>
      let val len = String.size(s) (* Calculating String.size only once for each string *)
      in if len >= size then { size=len, longest=s } else { size=size, longest=longest }
      end) { size=0, longest="" } strings)

fun longest_string_helper should_change strings =
  #longest (List.foldl
    (fn (s, { size, longest }) =>
      let val len = String.size(s) (* Calculating String.size only once for each string *)
      in if should_change(len, size) then { size=len, longest=s } else { size=size, longest=longest }
      end) { size=0, longest="" } strings)

val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer predicate list =
  case list of
    [] => raise NoAnswer
    | head::list' =>
      case predicate head of
        SOME res => res
        | NONE => first_answer predicate list'

fun all_answers predicate list =
  case list of
    [] => SOME []
    | head::tail =>
      case predicate head of
        NONE => NONE
        | SOME list' =>
          case all_answers predicate tail of
            NONE => NONE
            | SOME rest' => SOME (list' @ rest')

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn str => String.size(str))

fun count_some_var (name, pattern) =
  g (fn () => 0) (fn str => if str = name then 1 else 0) pattern

fun check_pat pattern =
  let
    fun pattern_to_list p =
      case p of
          Variable str => [str]
          | TupleP ps => List.foldl (fn (curr, acc) => pattern_to_list(curr) @ acc) [] ps
          | ConstructorP (str, p') => pattern_to_list(p')
          | _ => []
    fun has_dups l =
      case l of
        [] => false
        | head::tail => (List.exists (fn s' => s' = head) tail) orelse has_dups tail
  in
    not (has_dups (pattern_to_list(pattern)))
  end

fun match (value, pattern) =
  case (value, pattern) of
    (Unit, UnitP) => SOME []
    | (Const _, ConstP _) => SOME []
    | (_, Wildcard) => SOME []
    | (value, Variable str) => SOME [(str, value)]
    | (Constructor (str_val, val'), ConstructorP (str_pat, pat)) => if str_val = str_pat then match (val', pat) else NONE
    | (Tuple vals, TupleP pats) => all_answers match (ListPair.zip(vals, pats))
    | _ => NONE

fun first_match value patterns =
  SOME (first_answer (fn pat => match(value, pat)) patterns) handle NoAnswer => NONE
