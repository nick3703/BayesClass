library(ggplot2)
x.bar<-.4
s<-.1
s.sq<-s^2

alpha<-x.bar*(x.bar*(1-x.bar)/s.sq-1)

beta<-alpha/x.bar-alpha

sim.vals<-data.frame(vals=rbeta(10000,alpha,beta))
#Visualize Prior
ggplot(aes(x=vals),data=sim.vals)+geom_density()


#Roll Die

n<-100
y<-30

alpha.star<-y+alpha
beta.star<-n-y+beta


sim.vals.posterior<-data.frame(vals=rbeta(10000,alpha.star,beta.star))
#Visualize Prior
ggplot(aes(x=vals),data=sim.vals,colour="black")+stat_density(geom="line",lwd=2,colour="black")+
  stat_density(geom="line",aes(x=vals,colour="red"),data=sim.vals.posterior,lwd=2)
