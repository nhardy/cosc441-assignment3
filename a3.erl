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

% Sum of all integers N, where L <= N <= U
sumRange(L, L) ->
    L;
sumRange(L, U) ->
    U + sumRange(L, U - 1).
