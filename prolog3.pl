% implementing interpreter part 1 in prolog
% for m_states, the function params are (expression, state, newstate)
% m_state: while
% m_state: if

% m_state: statement list
m_state([], S, S).
m_state([H|T], S, S0) :- m_state(H, S, S1), m_state(T, S1, S0).

% m_state: assign
m_state([V, =, E], S, S0) :- m_value(E, S, X), remove_binding(V, S, S1), add_binding(V, X, S1, S0).

% m_boolean(expression, state) -- only returns T/F values, so we don't need to return anything
m_boolean(true, _).
m_boolean([V1, ==, V2], S) :- m_value(V1, S, X), m_value(V2, S, X). % =:= also tests for equality
m_boolean([V1, <, V2], S) :- m_value(V1, S, X1), m_value(V2, S, X2), X1 < X2.
% and so on for the other comparison operators/cases

% m_value
m_value(E, _, E) :- number(E). % number tests if it's a number
m_value(E, S, V) :- lookup(E, S, V). % for a generic expression
m_value([E1, *, E2], S, V) :- m_value(E1, S, V1), m_value(E2, S, V2), V is V1 * V2.
m_value([E1, +, E2], S, V) :- m_value(E1, S, V1), m_value(E2, S, V2), V is V1 + V2.

% lookup
lookup(V, [[V,X]|_], X) :- !. % ! not necessary, but stops once we found the binding
lookup(V, [[A,B]|T], X) :- lookup(V, T, X), V \= A.

% add_binding
add_binding(V, E, S, [[V|E]|S]).

% remaining solutions will be posted
