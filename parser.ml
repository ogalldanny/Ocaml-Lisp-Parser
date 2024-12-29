(*

  This file contains definitions for the OCaml type THING, and the OCaml module
  SCANNER, both of which are needed to write the module PARSER for Project 2.

*)

(* THING. Types of the usual Lisp objects. *)

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

(* SCANNER. Lexical scanner for a subset of Lisp, from the lectures. *)

module Scanner =
struct

(* TOKEN. Expressions are sequences of TOKENs. *)

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
   TOKENs from a file whose pathname is PATH. INPUT is a channel connected to
   that file. CH holds the most recently read CHAR from INPUT. *)

  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in

(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)

  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in

(* NEXT END TOKEN. Read an END TOKEN that indicates the end of INPUT. We don't
   skip a CHAR because there are no more CHARs to skip. *)

  let nextEndToken () =
    EndToken
  in

(*  NEXT NUMBER TOKEN. Read a NUMBER TOKEN. If it doesn't denote an INT then we
    read it as a SYMBOL TOKEN instead. *)

  let nextNumberToken () =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             (try
                NumberToken (int_of_string chars)
              with
                Failure _ ->
                  SymbolToken chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering ""
  in

(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)

  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in

(*  NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN. *)

  let nextSymbolToken () =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling ""
  in

(* NEXT TOKEN. Look at CH to decide what TOKEN is coming next. Dispatch to the
   function that reads the TOKEN and returns it. *)

  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' ->
           nextNumberToken () |
         _ ->
           nextSymbolToken ()

(* Lost? This is MAKE SCANNER's body. Initialize CH by reading the NEXT CHAR,
   and return (but do not call!) the dispatcher NEXT TOKEN. *)

  in nextChar () ;
     nextToken ;;
end ;;




module type Parserish =
  sig
   val makeParser : string -> unit -> thing;;
   exception Can'tParse of string;;
end;;

module Parser = 
struct
exception Can'tParse of string;;
let makeParser path =  
  let s = Scanner.makeScanner path
   in let token = ref Scanner.EndToken 

  in let rec nextThing () = 
   match !token with Scanner.CloseParenToken -> raise(Can'tParse "Unable to parse")
    |Scanner.EndToken -> raise(Can'tParse "Unable to parse")
    |Scanner.NumberToken(n) ->  token := s() ; Number(n) 
    |Scanner.OpenParenToken -> token := s()  ; nextThings() 
    |Scanner.SymbolToken("nil") ->token := s() ;  Nil
    |Scanner.SymbolToken(sym) -> token := s() ; Symbol(sym)

  and nextThings () =
    match !token with Scanner.EndToken -> raise(Can'tParse "Unable to parse") 
    | Scanner.CloseParenToken -> token := s() ; Nil
    | _ -> let head = nextThing()
            in let tail = nextThings()
           in Cons(head, tail)
  in   
  token := s () ;
   nextThing 
   
  
  end;;


  (*
   TESTS. Tests for Project 2.

     James Moen
     08 Nov 24

   Unlike the tests for a lab, these are not worth points! Instead, they help
   test if your parser works correctly.
*)

(* Make a parser NEXT THING that reads from the file "things.txt". *)

let nextThing = Parser.makeParser "things.txt" ;;

(* Each call to NEXT THING reads a Lisp expression, constructs an equivalent
   OCaml object, and returns that object. The comment following each call shows
   what OCaml will print if NEXT THING works correctly. These are the same
   Lisp expressions that were used to test your print function from Lab 9. *)

nextThing () ;;  (* nil *)

(* - : thing = Nil *)

nextThing () ;;  (* 7734 *)

(* - : thing = Number 7734 *)

nextThing () ;;  (* lobyms *)

(* - : thing = Symbol "lobyms" *)

nextThing () ;;  (* (a) *)

(* - : thing = Cons (Symbol "a", Nil) *)

nextThing () ;;  (* (a b) *)

(* - : thing = Cons (Symbol "a", Cons (Symbol "b", Nil)) *)

nextThing () ;;  (* (a b c) *)

(* - : thing = Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))) *)

nextThing () ;;  (* ((a) b c) *)

(* - : thing =
   Cons (Cons (Symbol "a", Nil), Cons (Symbol "b", Cons (Symbol "c", Nil))) *)

nextThing () ;;  (* ((a b) c) *)

(* - : thing =
   Cons (Cons (Symbol "a", Cons (Symbol "b", Nil)), Cons (Symbol "c", Nil)) *)

nextThing () ;;  (* a (b c) *)

(* - : thing =
   Cons (Symbol "a", Cons (Cons (Symbol "b", Cons (Symbol "c", Nil)), Nil)) *)

nextThing () ;;  (* ((a b c)) *)

(* - : thing =
   Cons (Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))), Nil) *)

nextThing () ;;  (* (define ! (lambda (n) (if (= n 0) 1 (âˆ— n (! (âˆ’ n 1)))))) *)

(* - : thing =
   Cons (Symbol "define",
    Cons (Symbol "!",
     Cons
      (Cons (Symbol "lambda",
        Cons (Cons (Symbol "n", Nil),
         Cons
          (Cons (Symbol "if",
            Cons (Cons (Symbol "=", Cons (Symbol "n", Cons (Number 0, Nil))),
             Cons (Number 1,
              Cons
               (Cons (Symbol "*",
                 Cons (Symbol "n",
                  Cons
                   (Cons (Symbol "!",
                     Cons
                      (Cons (Symbol "-",
                        Cons (Symbol "n", Cons (Number 1, Nil))),
                      Nil)),
                   Nil))),
               Nil)))),
          Nil))),
      Nil))) *)

(* At this point, we've read all the Lisp expressions from "things.txt", so if
   you call NEXT THING again, it should raise the exception CAN'T PARSE. This
   is what your code is supposed to do! *)

nextThing () ;;

(* Exception: Parser.Can'tParse "Unexpected end of file". *)

