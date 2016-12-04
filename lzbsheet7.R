library(datasets)
attach(rock)
rock

summary(rock)
plot(density(rock$area,na.rm=T),main="Distribution of Area", xlab="Area",ylab="Density")

plot(density(rock$peri,na.rm=T),main="Distribution of Primeter", xlab="Primeter",ylab="Density")

plot(density(rock$shape,na.rm=T),main="Distribution of Shape", xlab="Shape",ylab="Density")

plot(density(rock$perm,na.rm=T),main="Distribution of Permeability", xlab="Permeability",ylab="Density")

error <- qt(0.975,df=length(rock$area)-1)*sd(rock$area)/sqrt(length(rock$area))
error
left <- mean(rock$area)-error
right <- mean(rock$area)+error
left
right

xbar <- mean(rock$area)
mu0 <- 7000
s <- sd(rock$area)
n <- length(rock$area)
t <- (xbar-mu0)/(s/sqrt(n))
pval <-  pt(t, df=n-1, lower.tail=F)
pval




windows_open<-c(202.0,204.5,207.0,215.5,190.8,215.6,208.8,187.8,204.1,185.7)

windows_closed<-c(193.5,192.2,199.4,177.6,205.4,200.6,181.8,169.2,172.2,192.8)

summary(windows_open)

summary(windows_closed)

par(mfrow=c(1,2))
plot(density(windows_open), xlab="sales in £",col="red",main="Windows open")
plot(density(windows_closed), xlab="sales in £",main="Windows closed")


x1bar <- mean(windows_open)
x2bar <- mean(windows_closed)
n1 <- length(windows_open)
n2 <- length(windows_closed)
s1 <- sd(windows_open)
s2 <- sd(windows_closed)
sp <- sqrt((((n1-1)*(s1^2))+((n2-1)*(s2^2)))/(n1+n2-2))
error <- qt(0.975,df=(n1+n2-2))*sp*(sqrt((1/n1)+(1/n2)))
error
left <- (x1bar-x2bar)-error
right <- (x1bar-x2bar)+error
left
right

t.test(windows_open,windows_closed)
