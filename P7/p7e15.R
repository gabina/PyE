q <- matrix(c(0.565,0.085,0,0,0.7875,0.0125,0,0,0.95),byrow=TRUE,nrow=3)

imatrix <- function(n){
  mat <- matrix(0, n, n)
  diag(mat) <- 1
  return (mat)
}

solve(imatrix(3)-q)

############################
  library(markovchain)

#Estados
states <- c("A","B","C","D")
#Distribución inicial
pi <- c(1,0,0,0)
#Matriz de transición
transition <-matrix(c(0.565,0.085,0,0.35,0,0.7875,0.0125,0.2,0,0,0.95,0.05,0,0,0,1),byrow=TRUE,nrow=4)
#Creo la cadena de markov. Chequea automáticamente las propiedades necesarias
mc <- new("markovchain",states = states,transitionMatrix = transition,name="Ejercicio examenes")

#Cálculo promedio de cuánto tarda un estudiante en alcanzar su título
minutes <- function(x){
  if (x=="D")
    return(0)
  else
    return(1)
}

sumaTotal <- 0
for(i in 1:100000){
  sample <- rmarkovchain(mc,n=200,t0="A")
  dim(sample)<-c(1,200)
  if(sample[1,200]=="D"){
    sumaMin <- sum(apply(sample,MARGIN=c(1,2), FUN = minutes)) + 1
    sumaTotal <- sumaTotal + sumaMin  
  }else{
    print("PROBLEMA")
    i <- i + 1
  }
}

print(sumaTotal/100000)
