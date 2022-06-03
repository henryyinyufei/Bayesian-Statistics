# student height example 

# observed data
y <- c(70,75,72)
n <- length(y)
ybar <- mean(y)

# prior
mu0 <- 65
tau0sq <- 9
nu0 <- 175
sig0sq <- 16

# number of iterations
N_iter <- 10000

# set up vectors
mu <- rep(NA, N_iter)
sig2 <- rep(NA, N_iter)

# initialize
mu[1] <- 65
sig2[1] <- 16 


# start iteration
for (t in 2:N_iter){
  
  a <- (mu0/tau0sq) + (n*ybar/(sig2[t-1]))
  b <- (1/tau0sq) + (n/(sig2[t-1]))
  
  
  # draw mu from conditional distribution (normal)
  mu[t] <- rnorm(1, mean = (a/b), sd = sqrt((1/b))) 
  
  # draw sigma2 from conditional distribution (inverse chi-square)
  nu <- mean((y - mu[t])^2)
  deg_free <- nu0 + n
  s2 <- (nu0*sig0sq + nu*n)/(nu0 + n)
  X <- rchisq(1, df = deg_free)
  sig2[t] <- deg_free*s2/X
}

# discard the first 500 draws as burn-in.
mu <- mu[-(1:5000)]
sig2 <- sig2[-(1:5000)]



