#### COMMAND LINE ARGUMENT SETUP ####
#commment out when not testing
comm_args <- c("rho=0.8", "P_REF=0.5", "mu2=0.5", "alpha=0.95")

#uncomment for real run
# comm_args <- commandArgs(trailingOnly = TRUE)

args <- strsplit(comm_args,"=",fixed=TRUE)

for (arg in 1:length(args)){
  argname <- args[[arg]][1]
  argval <- as.numeric(args[[arg]][2])
  assign(argname,argval)
}

#### SPECIFICATIONS ####
#number of people
n_people <- 1000
#number of items
n_items <- 60
#number of reps
nreps <- 10
#rho is the amount of DIF explained by the second-order factors - to be specified in job script
#P_REF is the proportion of people in the reference group - to be specified in job script
#alpha is mixture parameter - to be specified in job script
#mu1 is mean of distribution 1 - items with negligible DIF
mu1 <- 0
#mu2 is the mean of distribution 2 - items with "true" DIF - to be specified in job script
#sdev is the standard deviation of each distribution. sdev is equal for both distributions
sdev <- .1
#sdev_D is used to calculate beta1 and beta0
sdev_D <- sqrt(alpha*(sdev^2)) + ((1-alpha)*(sdev^2)) + (alpha*(1-alpha)*((mu1-mu2)^2))


true_params <- readRDS("C:/Users/Jen/Dropbox/REMS/11 Comps/Simulation/Cluster_stuff/20170526_simulation-results/true_params_10reps_0-8rho_0-5PREF_0-5mu_0-95alpha_20170526.rds")

est_param_means <- readRDS("C:/Users/Jen/Dropbox/REMS/11 Comps/Simulation/Cluster_stuff/20170526_simulation-results/est_param_means_10reps_0-8rho_0-5PREF_0-5mu_0-95alpha_20170526.rds")

true_params <- true_params[[6]]

est_param_means <- est_param_means[[6]]

for(i in 1:length(true_params)){
  assign(names(true_params)[i], true_params[[i]])
}

for(i in 1:length(est_param_means)){
  assign(names(est_param_means)[i], est_param_means[[i]])
}


beta1_true <- rho*sdev_D/sd(DIFpredict)
beta0_true <- mean(DIFpredict) - (beta1_true*mean(true_item_params[, "dif_param"]))

beta1
beta1_true

beta0
beta0_true
