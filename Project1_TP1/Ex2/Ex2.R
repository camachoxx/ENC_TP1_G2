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

sim.quisquared<-function(n,df,lower=0,upper=1){
  #forms a quisquared distribution from random uniform values x=(0 to 1)
  s=vector()
  for (i in 1:n){
    x=unlist(sim.norm(df,lower,upper)[1])
    s=c(s,sum(x^2))
  }
  return(s)
}


set.seed(123)
# plotting the histogram
hist(sim.quisquared(10000,3),freq=F,main="Quisquared(3)",ylim=c(0,0.3),xlab="x",col="grey",
     cex.main=1.5,cex.lab=1.15)
# adding the p.d.f. on top of the histogram
curve(dchisq(x,3),add=T,lwd=3,lty=1)
# drawing a box around the plot
box(lwd=2)

f<-function(x){
  return(x*exp(-x))
}

M=optimise(h,interval = c(0,5),maximum = T)$objective

gx=sim.quisquared(10000,3)


#testing zone

h<-function(x){
  #function h(x)=f(x)/g(x)
  #this is for testing we can´t use dchisq
  f(x)/dchisq(x,3)
}

curve(h(x))
curve(M*dchisq(x,3),ylim=c(0,0.5),xlim=c(0,10))
curve(f,type='l',add=T)
