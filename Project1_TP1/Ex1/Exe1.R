library(rstudioapi) # to automatically set the working directory to this file's path.

#set the working directory to this file's path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Exercicio A
sim.norm<-function(n, mu, std){
  
  "Box-Muller Algorithm. This function generates samples
  from two independent random variables X,Y ~ N(mu,std)."
  
  x<-vector()
  y<-vector()
  
  for (i in 1:n){
    
    theta=2*pi*runif(1,0,1)
    R=sqrt(-2*log(runif(1,0,1)))
    
    # add N(0,1) observations to the samples
    x=c(x,R*cos(theta))
    y=c(y,R*sin(theta))
  }
  
  # convert all observations from N(0,1) to N(mu,std)
  x=x*std+mu
  y=y*std+mu
  return(list(x,y))
}

Nvariable<-function(m,std){
  return(unlist(sim.norm(3,m,std)[1])^2)
}
sum(Nvariable(0,1))

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
curve(dnorm(x,0,4), add=T, lwd=3, lty=1, from=-14, to=14)
# drawing a box around the plot
box(lwd=2)
dev.off()

pdf("Exercicio1_Ny.pdf",width=7,height=5)
hist(y,freq=F,main="Ny(0,4)",ylim=c(0,0.1),xlab="x",col="grey",cex.main=1.5,cex.axis=1.15,
     breaks=50); rug(x)
curve(dnorm(x,0,4), add=T, lwd=3, lty=1, from=-14, to=14)
# drawing a box around the plot
box(lwd=2)
dev.off()

