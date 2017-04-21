#### SPECIFICATIONS ####
#number of people
n_people <- 1000
#number of items
n_items <- 60
#number of items with DIF
# n_DIF <- c(1, 3, 6)
#number of reps
nreps <- 100
#rho is the amount of DIF explained by the second-order factors
rho <- c(0.4, 0.6, 0.8)
#P_REF is the proportion of people in the reference group
P_REF <- c(0.5, 0.9)
#true_dif
true_dif <- c(0.5, 0.1)

expand.grid(rho, P_REF, alpha, mu2)


#####
alpha <- c(0.85, 0.9, 0.95)
mu1 <- 0
mu2 <- c(0.5, 1)
sdev <- .2
test_reps <- 10000

conditions <- expand.grid(alpha, mu2)
test_graphs <- vector("list", nrow(conditions))

for(i in 1:nrow(conditions)){
  mu2 <- conditions[i, "Var2"]
  alpha <- conditions[i, "Var1"]
  
  test <- rep(NA, test_reps)
  
  k <- rbinom(test_reps, 1, alpha)
  
  test_graphs[[i]] <- qplot((rnorm(test_reps, mu1, sdev)^k) * (rnorm(test_reps, mu2, sdev)^(1-k)), main = paste0(
    "mu1 = ", mu1, ", mu2 = ", mu2, ", alpha = ", alpha, ", sd = ", sdev))
}

multiplot(test_graphs[[1]], test_graphs[[2]], test_graphs[[3]], test_graphs[[4]], 
          test_graphs[[5]], test_graphs[[6]], cols = 2)
