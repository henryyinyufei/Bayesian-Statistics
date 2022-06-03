library(mvtnorm)

# write the Metropolis algorithm
metrop_alg <- function(num_iter){
  
  # Step 1: set up theta vector and get init. value
  theta <- matrix(rep(NA, 2*num_iter), ncol = 2)
  theta[1,] <- c(0,0)
  
  # variance-covariance matrix
  S <- diag(2)
  S[1, 2] <- 0.8
  S[2, 1] <- 0.8
  
  # jump distribuion, variance-covariance matrix scaled by 0.3
  sp <- 0.3
  
  # Step 2
  for (i in 2:num_iter){
    
    # Step 2(a): sample theta_star from jumping dist.
    theta_star <- rmvnorm(n = 1, mean = theta[i-1,], sigma = sp*S)
    
    # Step 2(b): Calculate ratio of densities
    curr <- dmvnorm(x = theta[i-1,],mean = c(0,0), sigma = S)
    prop <- dmvnorm(x = theta_star, mean = c(0,0), sigma = S)
    r <- prop/curr
    
    # Step 2(c): accept or reject proposed value
    
    # accept
    if (r > runif(1, min = 0, max = 1)){
      theta[i,] <- theta_star
    }
    
    # reject
    else{
      theta[i,] <- theta[i-1]
    }
  }
  
  return(theta)
}

theta_samples <- metrop_alg(5500)
theta_samples <- theta_samples[-(1:500),]


