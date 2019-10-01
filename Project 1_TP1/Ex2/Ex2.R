#Exercicio A
f<-function(x){
  return(x*exp(-x))
}
integrate(f,lower = 0,upper = Inf)
curve(dchisq(x,5),ylim=c(0,1))
curve(f,type='l',add=T)
