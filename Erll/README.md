Mx to Erlang compiler
=====================

Install with cabal install in the repository's main folder.

Example:

    $ erll --make --compile --run tests/semiadder.mx
    Writing semiadder.erl
    Running erl -compile semiadder
    semiadder.erl:74: Warning: function ignore/1 is unused
    Running erl -pa ./ -run semiadder main -run init stop -noshell
    true
    false
    false
    false

The two first bools are input written by the user, the second are output from
the system.

The prelude is defined in prelude.erl and can be extended with new built-in
functions. Right now, Start, Stop, PrintBool and ReadBool are only defined.
