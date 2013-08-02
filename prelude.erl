% erl -compile prelude && timeout 1 erl -noshell -s prelude tests
-module(prelude).
-export([newChannel/0
        ,tell/2,ask/1
        ,axiom/2
        ,promote/2,ignore/1,alias/1,demand/1
        ,tests/0]).

%%% START %%%

newChannel() ->
    spawn(fun() ->
        receive
            {tell,Msg} ->
                Go = fun(Go) ->
                    receive
                        {ask,Pid} -> Pid ! Msg;
                        {notify,Note,Pid} -> Pid ! Note, Go(Go)
                    end
                end,
                Go(Go);
            {promote,WorkerPid} ->
                Go = fun
                    (_,N) when N =:= 0 -> WorkerPid ! exit;
                    (Go,N) ->
                        % io:format("Promote Go ~p~n",[N]),
                        receive
                            % {notify,Note,Pid} -> Pid ! Note, Go(Go,N);
                            % Cannot notify like this: you cannot ask on these
                            alias -> Go(Go,N+1);
                            ignore -> Go(Go,N-1);
                            {demand,Pid} ->
                                WorkerPid ! {eval,Pid},
                                Go(Go,N-1)
                        end
                end,
                Go(Go,1)
        end
    end).



ask(Ch) ->
    Ch ! {ask,self()},
    receive
        Msg -> Msg
    end.

tell(Ch,X) -> Ch ! {tell,X}.

axiom(ChX,ChY) ->
    ChX ! {notify,x,self()},
    ChY ! {notify,y,self()},
    receive
        x -> tell(ChY,ask(ChX));
        y -> tell(ChX,ask(ChY));
        {x,exp} -> ChY ! {promote,ChX}
    end.

% Does not (yet) handle concurrent requests in parallel
promote(Ch,Fn) ->
    Ch ! {promote,self()},
    Go = fun(Go) ->
        receive
            exit -> exit;
            {eval,Pid} ->
                spawn(fun() ->
                        X = newChannel(),
                        Pid ! {via,X},
                        Fn(X)
                    end),
                Go(Go)
        end
    end,
    Go(Go).

ignore(Ch) -> Ch ! ignore.

alias(Ch) -> Ch ! alias.

demand(Ch) ->
    Ch ! {demand,self()},
    receive
        {via,X} ->
            % io:format("Demand received: ~p~n",[X]),
            X
    end.

readBool(X) ->
    % X is a channel rw -o Bool * rw
    {I,BO} = ask(X),
    real_world = ask(I),
    B = newChannel(),
    O = newChannel(),
    tell(BO,{B,O}),
    Bool = io:get_line(""),
    T = newChannel(),
    case Bool of
        "true\n" -> tell(B,{right,T});
        _        -> tell(B,{left,T})
    end,
    tell(T,{}),
    tell(O,real_world).

printBool(X) ->
    % X is a channel rw -o Bool -o rw
    {I,BO} = ask(X),
    real_world = ask(I),
    {B,O} = ask(BO),
    case ask(B) of
        {left,T} ->
            {} = ask(T),
            io:format("false~n");
        {right,T} ->
            {} = ask(T),
            io:format("true~n")
    end,
    tell(O,real_world).

%%% STOP %%%

setup_Start(Start) ->
%%% INIT Start %%%
    spawn(fun () -> tell(Start,real_world) end),
%%% END %%%
    ok.

setup_ReadBool(ReadBool) ->
%%% INIT ReadBool %%%
    spawn(fun () -> promote(ReadBool,fun(X) -> readBool(X) end) end),
%%% END %%%
    ok.

setup_PrintBool(PrintBool) ->
%%% INIT PrintBool %%%
    spawn(fun () -> promote(PrintBool,fun(X) -> printBool(X) end) end),
%%% END %%%
    ok.

final_Stop(Stop) ->
%%% FINAL Stop %%%
    real_world = ask(Stop),
%%% END %%%
    ok.

% Tests the axiom rule in both ways. Should return {tt1,tt2}.
test_axiom() ->
    Setup = fun() ->
        X = newChannel(),
        Y = newChannel(),
        spawn(fun() -> axiom(X,Y) end),
        {X,Y}
    end,
    {X1,Y1} = Setup(),
    tell(X1,tt1),
    R1 = ask(Y1),
    {X2,Y2} = Setup(),
    tell(Y2,tt2),
    R2 = ask(X2),
    {R1,R2}.

% Tests exponentials
test_exp() ->
    Q = newChannel(),

    alias(Q),
    alias(Q),

    R = newChannel(),

    spawn(fun() ->
        V = demand(Q),
        axiom(R,V)
    end),

    spawn(fun() ->
        promote(Q,fun(X) ->
            tell(X,tt)
        end)
    end),

    alias(Q),
    ignore(Q),
    ignore(Q),

    U = demand(Q),
    {ask(U),ask(R)}.

% Runs the tests.
tests() ->
    io:format("Running!"),
    io:format("~p~n",[test_axiom()]),
    io:format("~p~n",[test_exp()]).

