% prolog on lists [car | cdr]
% (define append
%  (lambda (l1 l2)
    % (if (null? l1)
    %   l2
    %   (cons (car l1) (append (cdr l1) l2)))))
myappend([],L2,L2).
myappend([H|T],L2,[H|S]) :- myappend(T,L2,S).

% contains: evaluate to true if list contains x
% (define contains
%   (lambda (a l)
%     (cond
%       ((null? l) #f)
%       ((eq? a (car l)) #t)
%       (else (contains a (cdr l))))))
% but there's no "return false" in prolog
contains(A, [A|_]).
contains(A, [_|T]) :- contains(A,T).

% insertbefore: places x before the first a in the list
insertbefore(_,_,[],[]).
insertbefore(A, X, [A|T], [X,A|T]).
insertbefore(A, X, [H|T], [H|S]) :- insertbefore(A, X, T, S).

% insertbeforeall: places x before every a in the list
insertbeforeall(_,_,[],[]).
insertbeforeall(A, X, [A|T], [X,A|S]) :- !, insertbeforeall(A, X, T, S).
insertbeforeall(A, X, [H|T], [H|S]) :- insertbeforeall(A, X, T, S).
% myreverse
myreverse([],[]).
myreverse([H|T], R) :- myreverse(T, S), myappend(S,[H], R).

% NOTE: can reverse the parameters of a function to get its inverse

% factorial
factorial(0, 1).
factorial(N, X) :- factorial(M, Y), N is M+1, X is N * Y.
% NOTE: is is how we do math
% NOTE: with is, everything on the right must be resolved to a value
