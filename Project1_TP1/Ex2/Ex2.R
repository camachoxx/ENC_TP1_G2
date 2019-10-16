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
  return(list("x"=x,"y"=y))
}

sim.quisquared<-function(n,df,lower=0,upper=1){
  #forms a quisquared distribution from random uniform values x=(0 to 1)
  s=vector()
  for (i in 1:n){
    x=sim.norm(df,lower,upper)$x
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
h<-function(x){
  #function h(x)=f(x)/g(x)
  f(x)/dchisq(x,3)
}
#the max of h(x)=M
M=optimise(h,interval = c(0,5),maximum = T)$objective

sim=function(n){
  v=vector()
  count=0
  for (i in 1:n){
    u=1
    alpha=0
    while(u>alpha){
    x<-sim.quisquared(1,3)
    alpha=(1/M)*f(x)/dchisq(x,3)
    u<-runif(1,0,1)
    count=count+1
    }
  v=c(v,x)
  }
rr=count/n
return(list("vector"=v,"rejection_rate"=rr))
}

set.seed(123)
fl=sim(10000)
ff=fl$vector
rejection_rate=fl$rejection_rate
# plotting the histogram
hist(ff,freq=F,main="f(x)",ylim=c(0,0.4),xlab="x",col="grey",
     cex.main=1.5,cex.lab=1.15)
# adding the p.d.f. on top of the histogram
curve(f(x),add=T,lwd=3,lty=1)
# drawing a box around the plot
box(lwd=2)

#testing zone
curve(h(x))
curve(M*dchisq(x,3),ylim=c(0,0.5),xlim=c(0,10), col = "blue")
curve(f,type='l',add=T)
text(9.9, .44, expression("f(x)"))
text(9, .38, expression("M*dchisq(x,3)"), col = "blue")
