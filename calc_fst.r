calc_fst <- function(fn1, fn2)
{
  p1 = read.table(file = fn1, header = T)
  p2 = read.table(file = fn2, header = T)

  diff12 = setdiff(names(p1), names(p2)) # in p1 but not p2
  diff21 = setdiff(names(p2), names(p1)) # in p2 but not p1

  p1[,diff21] <- 0
  p2[,diff12] <- 0
  p1 <- p1[,order(names(p1))]
  p2 <- p2[,order(names(p2))]

  a1 = colSums(p1)
  n1 = length(p1[,1])
  a2 = colSums(p2)
  n2 = length(p2[,1])
  nn1 = n1 * (n1-1)
  nn2 = n2 * (n2-1)
  q1 = a1/n1
  q2 = a2/n2
  N = (q1 - q2)^2 -  (( ( a1*(n1-a1) ) / nn1 ) / n1) -  (( (a2*(n2-a2) ) / nn2 ) / n2)
  D = N + ( (a1*(n1-a1)) / nn1  )   +  ( (a2*(n2-a2)) / nn2   )
  fst = sum(N) / sum(D)

  print(fst)
}
