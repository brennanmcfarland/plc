% Prolog uses propositional functions
% P(x)
% P(x,y)
% in prolog, facts end in .
% uppercase is for variables

% A database of facts
male(harry).
male(ron).
male(george).
male(fred).
male(arthur).
male(percy).
male(bill).
male(albus).
male(albusjr).
male(neville).
male(jamesjr).
male(teddy).
male(james).
male(vernon).
male(dudley).
male(hypotheticalchild1).

female(hermione).
female(ginny).
female(lily).
female(luna).
female(fleur).
female(molly).
female(rose).
female(petunia).
female(hypotheticalparent).
female(hypotheticalchild2).

parentof(harry,albusjr).
parentof(ginny,albusjr).
parentof(harry,jamesjr).
parentof(ginny,jamesjr).
parentof(james,harry).
parentof(lily,harry).
parentof(vernon,dudley).
parentof(petunia,dudley).
parentof(hypotheticalparent,ginny).
parentof(hypotheticalparent,hypotheticalchild1).
parentof(hypotheticalparent,hypotheticalchild2).
parentof(molly,ginny).
parentof(molly,ron).
parentof(molly,george).
parentof(molly,bill).
parentof(molly,fred).
parentof(asher,lily).
parentof(asher,petunia).

married(harry,ginny).
married(lily,james).
married(petunia,vernon).

% can do functions like married(X,Y)

% functions only return the first valid value they find, type ; to go to the next one

% rules are of the form A :- B,C,D.
% this means (B and C and D imply A)

grandparentof(X,Y) :- parentof(X,Z),parentof(Z,Y).

% example: grandparentof(X,albusjr) -> james

sibling(X,Y) :- parentof(Z,X), parentof(Z,Y),X\=Y. % X\=Y means X and Y do not resolve to the same value, necessary to avoid duplicates
% it will still print the same value twice because of symmetric inputs, but we're ok with that for now

% cousin
% NOTE: it goes from left to right
cousin(X,Y) :- sibling(Z,W), parentof(Z,X), parentof(W,Y). % order matters, this is less efficient than it should be because it does the sibling query first

% a list in prolog is [car | cdr]
myappend([],L,L). % first 2 args is input, 3rd arg is output
myappend([H|T],L,[H|S]) :- myappend(T,L,S). % says if the right side of :- is true, then the left side is true, the right part is how to build it
