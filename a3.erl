-module(a3).
-export([a3/2]).

% a3 will sum all numbers 1 to N with work divided among
% T threads and then compute the square root
a3(N, T) ->
    % A3 is a reference to the current process
    A3 = self(),

    % For I in the range 1 to T, spawn a new process that
    % will compute a Subtotal for a range we calculate
    Pids = lists:map(fun (I) ->
        Lower = N div T * (I - 1) + 1,
        % If we're the final iteration, set Upper to N to
        % cover cases where N is not evenly divisible by T
        Upper = ternary(I == T, N, Lower + N div T - 1),

        % Spawn process to compute Subtotal and message
        % this back to A3
        spawn_link(fun () ->
            A3 ! {self(), sumRange(Lower, Upper)}
        end)
    end, range(1, T)),

    % Fold over each process id (Pid) and receive the
    % Subtotal for each, accumulating the Total
    Total = lists:foldl(fun (Pid, Acc) ->
        receive {Pid, Subtotal} ->
            Acc + Subtotal
        end
    end, 0, Pids),

    % Compute the square root of the Total
    math:sqrt(Total).

% Ternary expression shortcut
ternary(true, IfTrueExpression, _) ->
    IfTrueExpression;
ternary(false, _, IfFalseExpression) ->
    IfFalseExpression.

% range(Lower, Upper) creates a list of integers
% ranging from Lower to Upper inclusive
range(N, N) ->
    [N];
range(Lower, Upper) ->
    [Lower|range(Lower + 1, Upper)].

% Sum of all integers from Lower to Upper
sumRange(N, N) ->
    N;
sumRange(Lower, Upper) ->
    % Tail-call recursion here allows the compiler
    % to optimise recursion as iteration
    Upper + sumRange(Lower, Upper - 1).
