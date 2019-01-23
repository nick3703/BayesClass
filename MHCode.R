
#Necessary functions#############################
likelihood<-function(dat,theta){
  n<-length(dat)
  y<-sum(dat)
  like<-dbinom(y,n,theta,log=TRUE)
  return(like)
}

prior<-function(a,b,theta){
  prior.val<-dbeta(theta,a,b,log=TRUE)
}


#Necessary Values#################################
theta.star<-c()  #Empty Vector to Store Values In
a.prior<-1 #Prior for alpha
b.prior<-1 #Prior for beta
tuning.parameter<-.1  #Standard Deviation for Proposal
experimental.data<-rbinom(1000,1,.3)
theta.cur<-.8  #Guess for p
nreps<-100 #How long do we want to run chain for
#Metroplis Algorithm


for(l in 1:nreps){
  proposed.theta<-rnorm(1,theta.cur,tuning.parameter) #Proposed theta
  if(proposed.theta>0&proposed.theta<1){ #If not in parameter space, immediately reject
    log.numerator<-likelihood(experimental.data,proposed.theta)+prior(a.prior,b.prior,proposed.theta)
    log.denominator<-likelihood(experimental.data,theta.cur)+prior(a.prior,b.prior,theta.cur)
    log.ratio<-log.numerator-log.denominator #Calculate equation on page 158
    p.move<-log(runif(1)) #Generate a random Uniform
    if(p.move<log.ratio){ #If uniform is less than ratio move
      theta.star[l]<-proposed.theta
      theta.cur<-proposed.theta
    }else{ #Or else stay
      theta.star[l]<-theta.cur
    }
  }else{
    theta.star[l]<-theta.cur
  }
  
}
  

#How often did we jump to a new value?

length(unique(theta.star))/length(theta.star)


plot(theta.star) #Does this look like we have explored the posterior?



