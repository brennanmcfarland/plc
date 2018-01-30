; Brennan McFarland
; Jan 31, 2018
; Homework 1

; given a number and a list of numbers in order, insert the number in its proper place
(def insert
  )

; given two lists of numbers that in order, return the combination of both lists in order as a list
(def merge
  )

; given a list of atoms, remove any atom that is a repeat of the atom that immediately precedes it
(def removedups
  )

; given a list of atoms, return a list that contains two lists of atoms:
; the first list contains odd-indexed atoms (starting from 1)
; the second list contains the even-indexed atoms (starting from 1)
(def split
  )

; given an element and a (possibly nested) list, place the element in front of the leftmost
; element non-list atom, as deep in the sublist as needed.  If the leftmost atom is the empty list,
; place the element inside the empty list
(def deepcons
  )

; given a list, return the number of pairs of parentheses
(def numparens
  )

; given a list, duplicate all contents, including any sublists
(def dup*
  )

; given a (possibly nested) list, remove any atom that is the repeat of the atom that immediately
; precedes it in the same sublist
(def removedups*
  )

; given a (possibly nested) list, return a list containing two lists:
; the first list contains the odd-indexed elements (starting from 1)
; the second list contains the even-indexed elements (starting from 1)
; However, if any of these elements are also lists, these elements should be split as well
(def split*
  )

; given a (possibly nested) list, remove any element that, once repeated elements have been removed
; from it, is the repeat of any element (also once elements have been removed from it) that
; immediately precedes it in the same sublist
(def removedups**
  )