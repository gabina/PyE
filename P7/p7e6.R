library(markovchain)

#Distribución inicial
#pi <- c(2/5,1/5,2/5)
#Estados
states <- c("A","B","C")
#Matriz de transición
#transition <- matrix(c(0,1/2,1/2,3/4,0,1/4,1,0,0),byrow=TRUE,nrow=3)
transition <- matrix(c(1/2,1/4,1/4,2/3,0,1/3,3/5,2/5,0),byrow=TRUE,nrow=3)
#Creo la cadena de markov. Chequea automáticamente las propiedades necesarias
mc <- new("markovchain",states = states,transitionMatrix = transition,name="Ejercicio6")

is.irreducible(mc)
#Matriz límite
steadyStates(mc)

period(mc)
transientStates(mc)
predict(mc,newdata=c("A","B"))

conditionalDistribution(mc,"A")

after1Time <- pi * mc
after2Time <- after1Time * mc
after3Time <- after2Time * mc

absorbingStates(mc)

after10Times <- pi * (mc ^ 10)
after10Times
print(mc)
plot(mc)

table(rmarkovchain(mc,n=100,pi))
