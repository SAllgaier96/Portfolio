library(invgamma)

#Read in Data
ipo <- read.csv(file='~/Desktop/STAT 251/Final Project/IPO.csv',
                  header=TRUE, sep=",")

sp500 <- read.csv(file='~/Desktop/STAT 251/Final Project/S&P500.csv',
                  header=TRUE, sep=",") 

nasdaq <- read.csv(file='~/Desktop/STAT 251/Final Project/NASDAQ.csv',
                   header=TRUE, sep = ",")

dowjones <- read.csv(file='~/Desktop/STAT 251/Final Project/DJI.csv',
                     header=TRUE, sep = ",")

# Clean datasets to remove n/a values
sp500 <- sp500[complete.cases(sp500), ]

#Prior Values
lambda <-0.027
tau2 <- 0.5
gamma <- 2.1
phi <- 0.0297

#Plot of Prior Distributions of Both Mu's
curve(dnorm(x, lambda, sqrt(tau2)), xlim=c(-3,3), 
      xlab="mu", main="Prior of mu")


#Plot of Prior Distributions of Both Sigma^2
curve(dinvgamma(x, gamma, phi), xlim = c(0,0.2), 
      xlab=expression(sigma^2), main="Prior of sigma2" )


#Plot the Prior of Difference of Mu's
curve(dnorm(x, 0, 1), xlim=c(-3,3), 
      xlab="mu(IPO) - mu(S&P500)", 
      main="Prior of Difference of mus")

 
#Plot the Prior of Difference of Sigma^2
mean_sigma2 <- phi/(gamma-1)
var_sigma2 <- (phi^2)/(((gamma-1)^2)*(gamma-2)) 
sd_sigma2 <- sqrt(2*var_sigma2)

curve(dnorm(x, 0, sd_sigma2), xlim=c(-1,1), 
      xlab="sigma^2(IPO) - sigma^2(S&P500)", 
      main= "Prior of Difference of Sigma2s")


#Starting values
n.sp500 <- length(sp500$change3)
mu <- 0.027  
sigma2 <- 0.5


# initializations for the Gibbs Sampling Algorithm
iters <- 10000
mu.save.sp500 <- rep(0, iters)
mu.save.sp500[1] <- mu
sigma2.save.sp500 <- rep(0, iters)
sigma2.save.sp500[1] <- sigma2

#Gibbs Sampling Algorithm
for(i in 2:iters){
  
  # Full conditional of mu (update the value of the parameters)
  lambda.p <- (tau2*sum(sp500$change3) + sigma2*lambda)/(tau2*n.sp500 + sigma2)
  tau2.p <- sigma2*tau2/(tau2*n.sp500 + sigma2)
  
  # sample a new value of mu
  mu <- rnorm(1, lambda.p, sqrt(tau2.p))
  
  # save the value of mu
  mu.save.sp500[i] <- mu
  
  # full conditional of sigma2 (update the value of the parameters)
  gamma.p <- gamma + n.sp500/2
  phi.p <- phi + sum((sp500$change3 - mu)^2 )/2
  
  # sample new value of sigma2
  sigma2 <- 1/rgamma(1, gamma.p, phi.p)
  
  #save the value of sigma2
  sigma2.save.sp500[i] <- sigma2
  
}

# Now for IPO

n.IPO <- length(ipo$change2)

#Starting values 
mu <- 0.027  
sigma2 <- 0.5

# initializations for the Gibbs Sampling Algorithm
iters <- 10000
mu.save.IPO <- rep(0, iters)
mu.save.IPO[1] <- mu
sigma2.save.IPO <- rep(0, iters)
sigma2.save.IPO[1] <- sigma2

#Gibbs Sampling Algorithm
for(i in 2:iters){
  
  # Full conditional of mu (update the value of the parameters)
  lambda.p <- (tau2*sum(ipo$change2) + sigma2*lambda)/(tau2*n.IPO + sigma2)
  tau2.p <- sigma2*tau2/(tau2*n.IPO + sigma2)
  
  #sample a new value of mu
  mu <- rnorm(1, lambda.p, sqrt(tau2.p))
  
  #save the value of mu
  mu.save.IPO[i] <- mu
  
  # full conditional of sigma2 (update the value of the parameters)
  gamma.p <- gamma + n.IPO/2
  phi.p <- phi + sum((ipo$change2 - mu)^2 )/2
  
  #sample new value of sigma2
  sigma2 <- 1/rgamma(1, gamma.p, phi.p)
  
  #save the value of sigma2
  sigma2.save.IPO[i] <- sigma2
  
}


#Trace Plots
plot(mu.save.sp500)


#We decided it would be best to remove the first 10 iterations
mu.keep.sp500 <- mu.save.sp500[11:10000]

#Check again
plot(mu.keep.sp500)

#We dropped the first ten iterations for IPOs so it will match non-IOPs
mu.keep.IPO <- mu.save.IPO[11:10000]
plot(mu.keep.IPO)


#Plot the two posterior distributions
plot(density(mu.keep.sp500), xlim=c(-.01, 0.07),
     main='Average Daily Returns (S&P500 vs. IPO)')
lines(density(mu.keep.IPO), col="red", xlab="mu",
      main="Posterior of mu")
abline(v=mean(mu.keep.sp500))
abline(v=mean(mu.keep.IPO), col='red')
legend(.045, 280, legend=c("S&P 500", "IPO"),
       col=c("black", "red"), lty=1, cex=0.8)
text(.016, 250, 'mean = .001')
text(.045, 100, 'mean = .031', col='red')

mean(mu.keep.IPO)
mean(mu.keep.sp500)

mean(sigma2.keep.IPO)
mean(sigma2.keep.sp500)

quantile(mu.keep.IPO, c(0.025, 0.975))
quantile(mu.keep.sp500, c(0.025, 0.975))
quantile(sigma2.keep.IPO, c(0.025, 0.975))
quantile(sigma2.keep.sp500, c(0.025, 0.975))

#Subtract the non-IPO mus from the IPO mus
mu.diff <- mu.keep.IPO - mu.keep.sp500


#Plot the posterior of difference of mus
curve(dnorm(x, 0, 1), xlim=c(-.5,.5), ylim=c(0,30),
      xlab="mu(IPO) - mu(S&P500)", 
      main="Prior and Posterior of Difference of mus", col="red")
lines(density(mu.diff))
abline(v=mean(mu.diff))

plot(density(mu.diff),
      xlab="mu(IPO) - mu(S&P500)", 
      main="Posterior of Difference of mus")

#95% Credible Interval 
quantile(mu.diff, c(0.025, 0.975))


#Same thing for sigma2
sigma2.keep.sp500 <- sigma2.save.sp500[11:10000]
sigma2.keep.IPO <- sigma2.save.IPO[11:10000]

plot(density(sigma2.keep.sp500), xlim=c(0,0.03))
lines(density(sigma2.keep.IPO), col="red", xlab=expression(sigma^2),
      main="Posterior Distribution of sigma^2")

sigma2.diff <- sigma2.keep.IPO-sigma2.keep.sp500


curve(dnorm(x, 0, sd_sigma2), xlim=c(-1,1), ylim=c(0,6), 
            xlab="sigma^2(IPO) - sigma^2(S&P500)", 
            main= "Prior and Posterior of Difference of Sigma2s",
            col="red")
lines(density(sigma2.diff))

plot(density(sigma2.diff), 
     xlab = "sigma^2(IPO) - sigma^2(S&P500)",
     main= "Posterior of Difference of Sigma2s")


quantile(sigma2.diff, c(0.025, 0.975))

mean(sigma2.diff>0)

#Mean Difference
mean(mu.diff)
mean(sigma2.diff)

# Percentage of values over 0
mean(mu.diff<0)


#Posterior Predictive Distribution
pred.vec <- rnorm(length(mu.diff), mu.diff, 
                  sqrt(sigma2.diff))

plot(density(pred.vec), xlab="Returns(IPO)-Returns(S&P500)",
     main="Posterior Predictive of Difference of Daily Returns")

mean(pred.vec > 0)
