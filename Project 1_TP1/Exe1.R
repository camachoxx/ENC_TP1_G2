#Exercicio A
box_muller<-function(n){
  x<-vector()
  y<-vector()
  for (i in 1:n){
  ome=2*pi*runif(1,0,1)
  R=sqrt(-2*log(runif(1,0,1)))
  x=c(x,R*cos(ome))
  y=c(y,R*sin(ome))
  }
  return(list(x,y))
}

l=box_muller(10)  #list with the first element x and the second y

