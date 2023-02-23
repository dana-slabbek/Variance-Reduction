#estimate the integral using importance sampling
#used rnorm, but got NaNs error, so switched to runif

#integral to estimate
x <- runif(10000,0,1)
g <- 1*sqrt(1-(x*x))
c(mean(g),var(g))


#h = g(x)f(x)dx * q(x)/q(x)
#q(x) is a distribution I chose (normal dist on different intervals)

#first distribution (x~N(0.5,1))
h <- function(x) (1*sqrt(1-(x*x))*exp((x-.25)/2))*((x>=0)&(x<1))
Y <- h(x)
c(mean(Y),var(Y))
xg <- seq(-1,1, length.out=200)
plot(xg,h(xg),type="l")
se = sd(Y)/sqrt(length(Y))
CI = 1.96*sd(Y)/sqrt(length(Y))

#second distribution (x~N(0.25,1))
h <- function(x) (1*sqrt(1-(x*x))*exp((0.5*x-.0625)/2))*((x>=0)&(x<1))
Y <- h(x)
c(mean(Y),var(Y))
xg <- seq(-1,1, length.out=200)
plot(xg,h(xg),type="l")
se = sd(Y)/sqrt(length(Y))
CI = 1.96*sd(Y)/sqrt(length(Y))

#third distribution (x~N(0.1,1))
h <- function(x) (1*sqrt(1-(x*x))*exp((0.2*x-.01)/2))*((x>=0)&(x<1))
Y <- h(x)
c(mean(Y),var(Y))
xg <- seq(-1,1, length.out=200)
plot(xg,h(xg),type="l")
se = sd(Y)/sqrt(length(Y))
CI = 1.96*sd(Y)/sqrt(length(Y))

#se = sd(Y)/sqrt(length(Y))
#CI = 1.96*sd(Y)/sqrt(length(Y))