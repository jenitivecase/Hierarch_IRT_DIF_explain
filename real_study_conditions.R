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
sd1 <- .01
sd2 <- .01
k <- rbinom(1, 1, alpha)
(rnorm(1, mu1, sd1)^k) * (rnorm(1, mu2, sd2)^(1-k))
