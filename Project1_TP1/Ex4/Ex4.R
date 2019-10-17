# Step 1. generate m datasets/samples under H0 

m=1000; n=25; alpha=0.05; mu0=1; sigma=sqrt(3) 
set.seed(789); 

p=numeric(m)

for(i in 1:m){ 
  x=rnorm(n,mu0,sigma)
  Z=(mean(x)-mu0)/(sigma/sqrt(n)) 
  p[i]=1-pnorm(Z,0,1) }
# Step 2. for each dataset determine if each p ???value is lower than the pre-specified ?? 
# and Step 3. estimate the true significance level 
phat=mean(p<alpha)
phat



binom.test(phat*m,m,p=0.05)






