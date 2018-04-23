-- replaceall

removeall [] y = []
removeall l@((Element x):xs) y

removeall l@((Sublist x):xs) y
  | x == y = removall xs
  | othewise = x:removall xs
