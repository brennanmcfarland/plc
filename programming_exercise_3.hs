-- 1. removedups takes a list and removes duplicate elements
-- removedups [1,2,2,3,3,3,4,3,4,5,5,5,4,3,3,2,1] => [1,2,3,4,3,4,5,4,3,2,1]
removedups [] = []
removedups [a] = [a]
removedups lis =
  if (head lis) == ((head . tail) lis)
    then
      removedups (tail lis)
    else
      (head lis) : (removedups (tail lis))

-- 2. cps version of removedups
-- removedups_cps [1,2,2,3,3,3,4,3,4,5,5,5,4,3,3,2,1] (\v -> v) => [1,2,3,4,3,4,5,4,3,2,1]
removedups_cps [] return = return []
removedups_cps [a] return = return [a]
removedups_cps lis return =
  if (head lis) == ((head . tail) lis)
    then
      removedups_cps (tail lis) return
    else
      removedups_cps (tail lis) (\v -> return ((head lis) : v))
