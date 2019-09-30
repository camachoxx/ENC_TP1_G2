#Exercicio A
sim.norm<-function(n,m,std){
  x<-vector()
  y<-vector()
  for (i in 1:n){
  ome=2*pi*runif(1,0,1)
  R=sqrt(-2*log(runif(1,0,1)))
  x=c(x,R*cos(ome))
  y=c(y,R*sin(ome))
  }
  x=x*std+m
  y=y*std+m
  return(list(x,y))
}

#Exercicio B
set.seed(123)
l=sim.norm(1000,0,4)  #list with the first element x and the second y
x=unlist(l[1])
y=unlist(l[2])

#Exercicio C
#Make sure to have the working directory correct

pdf("Exercicio1_Nx.pdf",width=7,height=5)
hist(x,freq=F,main="Nx(0,4)",ylim=c(0,0.1),xlab="x",col="grey",cex.main=1.5,cex.axis=1.15,
     breaks=50); rug(x)
curve(dnorm(x,0,4),add=T,lwd=3,lty=1,
      from=-14,to=14)
# drawing a box around the plot
box(lwd=2)
dev.off()

pdf("Exercicio1_Ny.pdf",width=7,height=5)
hist(y,freq=F,main="Ny(0,4)",ylim=c(0,0.1),xlab="x",col="grey",cex.main=1.5,cex.axis=1.15,
     breaks=50); rug(x)
curve(dnorm(x,0,4),add=T,lwd=3,lty=1,
      from=-14,to=14)
# drawing a box around the plot
box(lwd=2)
dev.off()

