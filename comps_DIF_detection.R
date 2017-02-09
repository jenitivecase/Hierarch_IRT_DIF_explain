if(Sys.info()["user"] == "jbrussow"){
  setwd("C:/Users/jbrussow/Dropbox/REMS/11 Comps/")
} else if (Sys.info()["user"] == "Jen"){
  setwd("C:/Users/Jen/Dropbox/REMS/11 Comps/")
}

source("functions.R")

options(scipen = 999)
library(R2OpenBUGS)
library(coda)

#number of people
n_people <- 1000
#number of items
n_items <- 100
#number of items with DIF
n_DIF <- 5
#number of reps
nreps <- 1
#rho is the amount of DIF explained by the second-order factors
rho <- c(0.4, 0.6, 0.8)
#P_REF is the proportion of people in the reference group
P_REF <- c(0.5, 0.7, 0.9)

conditions <- expand.grid(n_people, rho, P_REF)

date <- format.Date(Sys.Date(), "%Y%m%d")


#simulate a set of items
item_test <- item_sim(n_items, n_DIF, b_mean = 0, b_sd = 1, a_mean = 1, a_sd = .5, 
         nodif_mean = 0, nodif_sd = 0.1, dif_mean = 1, dif_sd = 0.1)

#simulate a set of people's ability scores
ability_test <- ability_sim(n_people, P_REF = .8, ref_theta_mean = 0, ref_theta_sd = 1,
                            focal_theta_mean = -0.5, focal_theta_sd = 1)

#get the responses for a single item
response_test <- response_sim(ability_test[1,], item_test[1,])

#get responses for a single person to a set of items
responseset_test <- person_sim(ability_test[1,], item_test)

#get responses for a set of people to a set of items
dataset_test <- one_dataset(ability_test, item_test)

#get values for the DIF predictor
DIFpredict <- DIF_predictor(item_test, rho = 0.4)

#set up grouping variable
group <- ability_test[,2]
dataset <- dataset_test

b.dat <- list("n_people", "n_items", "dataset", "group", "DIFpredict")
b.par <- list("a", "theta", "b", "D", "beta0", "beta1", "var", "prec", "R2")

### DONE TO HERE ###

#do the analysis for one set of responses
time1 <- Sys.time()
analysis_test <- one_analysis(x = dataset_test, n_iter = 1000, n_burn = 300,
                              debug = FALSE)
BUGS_time <- Sys.time - time1
print(BUGS_time)

#ANALYSIS####
# 
# b.dat <- list("n_people","n_items","dataset","group","DIFpredict")
# b.par <- list("a", "theta", "b", "D", "beta0", "beta1", "var", "prec", "R2")
# 
# dataset <- dataset_test
# OUT <- bugs(data=b.dat,inits=NULL,param=b.par, 
#             model.file="BUGScode.txt",n.chains=2, 
#             n.iter=1000, n.burn=500, n.thin=1,debug=TRUE)

#### STAN TEST ####

library(rstan)

stancode_2PL <- "
data {
  int<lower=0> n_people;
  int<lower=0> n_items;
  int dataset[n_people, n_items];
}

parameters {
  real<lower=0> a[n_items];
  real b[n_items];
  real theta[n_people];
}
model {
  for (i in 1:n_people) {
  	for (j in 1:n_items) {
      dataset[i,j] ~ bernoulli_logit(a[j]*(theta[i]-b[j]));
    }
  }	
  
  a ~ lognormal(0,1);
  b ~ normal(0,1);
  theta ~ normal(0,1);
}
"

stancode <- "
data {
  int<lower=0> n_people;
  int<lower=0> n_items;
  int dataset[n_people, n_items];
  int<lower=0, upper=1> group[n_people];
  real DIFpredict[n_items];
}

parameters {
  real<lower=0> a[n_items];
  real b[n_items];
  real theta[n_people];
  real D[n_items];
  real beta0[n_items];
  real beta1[n_items];
  real sigma2;
  real prec;
  real R2;
}
model {
  for (i in 1:n_people) {
    for (j in 1:n_items) {
      dataset[i,j] ~ bernoulli_logit(a[j]*(theta[i]-b[j] + D[j]*group[i]));
    }
  }	

  a ~ lognormal(0,1);
  b ~ normal(0,1);
  theta ~ normal(0,1);

  for (j in 1:n_items) {
    D[j] ~ dnorm(mu[j],prec);
    mu[j] <- beta0 + beta1*DIFpredict[j];
    ss.err[j] <- pow((D[j]-mu[j]),2);
    ss.reg[j] <- pow((mu[j]-mean(D[])),2);
  }

  beta0 ~ dnorm(0,1);
  beta1 ~ dnorm(0,1);
  sigma2 ~ dunif(0,100);
  prec <- 1/sigma2;

  SSE <- sum(ss.err[]);
  SSR <- sum(ss.reg[]);
  R2 <- SSR/(SSR+SSE);
}
"

time1 <- Sys.time()
test <- stan(model_code = stancode, model_name = "stan_test", data = b.dat,
             iter = 1000, warmup = 300, chains = 2, verbose = FALSE, cores = 2)
Stan_time <- Sys.time - time1
print(Stan_time)



#turn intercepts into thresholds, save as DIF estimate
est_D_HGLM[k,,iRHO,iPROP] <- -OUT$mean$b[,3]/OUT$mean$b[,2]
est_coef[k,3:4,iRHO,iPROP] <- -OUT$mean$a[1:2]
est_R2[k,2,iRHO,iPROP] <- OUT$mean$R2


saveRDS(results, paste0(date, "_simulation_output.rds"))
