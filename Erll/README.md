Mx to Erlang compiler
=====================

Install with cabal install in the repository's main folder.

Example:

    $ erll -mcr tests/semiadder.mx
    Writing semiadder.erl
    Running erl -compile semiadder
    semiadder.erl:74: Warning: function ignore/1 is unused
    Running erl -pa ./ -run semiadder main -run init stop -noshell
    Enter a bool:true
    You entered true.
    Enter a bool:true
    You entered true.
    false
    true

(The first bool in the output is the least significant bit, and the second the
most significant bit.)

This semi-adder might not look like much, but remember that Mx is a very
verbose language and the source file is almost 100 lines long!

The prelude is defined in prelude.erl and can be extended with new built-in
functions. Right now, Start, Stop, PrintBool and ReadBool are only defined.

The input language
------------------

Apart from all the constructs in the Mx language, it also supports

 * Type holes

    Writing _ in the source code will emit an error notifying all variables
    in scope. Example:

        a : 1 * _|_ |- let b, c = a in _

        $ erll example.mx
        Hole context:
            b(1,20) : 1
            c(1,23) : ⊥

    When doing case and par, it is helpful to be able to say which identifiers
    will be eaten up in a specific hole. Use the _ along-construct:

        p : 1 | _|_, b : _|_ |-
        connect p via
            u -> _ along u, b
            q -> _ along q

        $ erll example.mx
        == Pretty-Printed ==
        p : 1 ⅋ ⊥, b : ⊥ ⊢
        connect p via
            { u : 1 -> {u : 1, b : ⊥}
            ; q : ⊥ -> {q : ⊥}
            }

    No errors are reported, but the program is annotated with the context in
    each hole.

  * Recursive types

     Not yet implemented in the compiler, you can use mu-types to create ADTs
     such as list. This derivation makes a singleton list:

         type List a = mu x . 1 + (a * x) ;

         A, x : A, xs : ~ List A |-
         let uxs = unfold xs in
         let a @ fe = uxs in
         let f, e = fe in
         let f' = alias f in
         let s = demand f in
         let s' = demand f' in
         connect s via
             cell ->
                 let cons = snd cell in
                 connect cons via
                     x' -> x <-> x'
                     e2 -> connect s' via
                         cell2 -> let nil = fst cell2 in nil
                         e2'   -> e2 <-> e2'
             e' -> e <-> e'

  * Named derivations

    There can be multiple derivations in the same file, and later ones can
    refer to earlier.  The last one is the main derivation.
    Example, not can be defined like this:

        not : ~ (Bool -o Bool) |- [ not ]
           let x,r = not in
           case x of
               inl y -> let () = y in let u = snd r in u
               inr z -> let () = z in let u = fst r in u

    Now, later in the file, not and map is refered to in this way:

        xs : BoolList , ys : ~ BoolList |-
           cut
               not' : ! (Bool -o Bool) ->
                   [ map | Bool , Bool ; not' , xs , ys ]
               not_server ->
                   offer nots for not_server in
                   [ not | ; nots ]

    The effect is just to put the derivation's sequent in the place of the
    referral.

