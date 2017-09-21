-module(a3).
-export([a3/2, worker/3]).

a3(N, T) ->
    Pid = spawn(a3, worker, [self(), T, N]),

    receive {Pid, S} ->
        math:sqrt(S)
    end.

% Worker will sumRange(L, U) and notify the Parent
worker(P, L, U) ->
    P ! {self(), sumRange(L, U)}.

% Parallel Map (pmap/2)
% @see http://www.cs.otago.ac.nz/cosc441/2017-L10.pdf
pmap(F, [X|Xs]) ->
    S = self(),
    P = spawn(fun () -> S ! {self(), F(X)} end),
    Ys = pmap(F, Xs),
    receive {P, Y} ->
        [Y|Ys]
    end;
pmap(_, []) ->
    [].

% Sum of all integers N, where L <= N <= U
sumRange(L, L) ->
    L;
sumRange(L, U) ->
    % Tail-call recursion allows the compiler
    % to optimise recursion as iteration
    U + sumRange(L, U - 1).
