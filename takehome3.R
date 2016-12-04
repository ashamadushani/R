library(datasets)
attach(trees)
trees

summary(trees)
plot(density(trees$Girth,na.rm=T),main="Distribution of Girth", xlab="Girth",ylab="Density")

plot(density(trees$Height,na.rm=T),main="Distribution of Height", xlab="Height",ylab="Density")

plot(density(trees$Volume,na.rm=T),main="Distribution of Volume", xlab="Volume",ylab="Density")


errorg <- qt(0.975,df=length(trees$Girth)-1)*sd(trees$Girth)/sqrt(length(trees$Girth))
errorg
leftg <- mean(trees$Girth)-errorg
rightg <- mean(trees$Girth)+errorg
leftg
rightg

errorh <- qt(0.975,df=length(trees$Height)-1)*sd(trees$Height)/sqrt(length(trees$Height))
errorh
lefth <- mean(trees$Height)-errorh
righth <- mean(trees$Height)+errorh
lefth
righth

xbar <- mean(trees$Height)
mu0 <- 7000
s <- sd(trees$Height)
n <- length(trees$Height)
t <- (xbar-mu0)/(s/sqrt(n))
pval <-  pt(t, df=n-1)
pval



no_odor <- c(
15.9,18.5,15.9,18.5,18.5,21.9,15.9,15.9,15.9,15.9,
15.9,18.5,18.5,18.5,20.5,18.5,18.5,15.9,15.9,15.9,
18.5,18.5,15.9,18.5,15.9,18.5,15.9,25.5,12.9,15.9)

lavender_odor <-c (
21.9,18.5,22.3,21.9,18.5,24.9,18.5,22.5,21.5,21.9,
21.5,18.5,25.5,18.5,18.5,21.9,18.5,18.5,24.9,21.9,
25.9,21.9,18.5,18.5,22.8,18.5,21.9,20.7,21.9,22.5)


summary(no_odor)
summary(lavender_odor)

par(mfrow=c(1,2))
plot(density(no_odor), xlab="sales in euros",col="red",main="No Odor")
plot(density(lavender_odor), xlab="sales in euros",main="With Lavender Odoor")

x1bar <- mean(no_odor)
x2bar <- mean(lavender_odor)
n1 <- length(no_odor)
n2 <- length(lavender_odor)
s1 <- sd(no_odor)
s2 <- sd(lavender_odor)
sp <- sqrt((((n1-1)*(s1^2))+((n2-1)*(s2^2)))/(n1+n2-2))
error <- qt(0.975,df=(n1+n2-2))*sp*(sqrt((1/n1)+(1/n2)))
error
left <- (x1bar-x2bar)-error
right <- (x1bar-x2bar)+error
left
right

t.test(no_odor,lavender_odor)