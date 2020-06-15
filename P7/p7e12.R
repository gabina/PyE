library(markovchain)

imatrix <- function(n){
  mat <- matrix(0, n, n)
  diag(mat) <- 1
  return (mat)
}

p <-  matrix(c(0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1/2,1/2,0,0,1,0,0,0,0,0,1/2,1/6,0,0,1/3,0),byrow=TRUE,nrow=6)

mc <- new("markovchain",states = c("A","B","C","D","E","F"),transitionMatrix = p,name="mc")
r <- solve(imatrix(6) - p)

q <- p[-(1:4),-(1:4)]
s <- solve(imatrix(2) - q)
b <- matrix(c(0,0,1,2/3),byrow=TRUE,nrow=2)
g <- s %*% b 

c1 <- p[1:2,1:2]
c2 <- p[3:4,3:4]
mc1 <- new("markovchain",states = c("A","B"),transitionMatrix = c1,name="c1")
mc2 <- new("markovchain",states = c("C","D"),transitionMatrix = c2,name="c1")

#distribución invariante para C1
steadyStates(mc1)

#distribución invariante para C2
steadyStates(mc2)

steadyStates(mc)
