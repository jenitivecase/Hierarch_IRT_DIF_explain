if(Sys.info()["user"] == "jbrussow"){
  setwd("C:/Users/jbrussow/Dropbox/REMS/11 Comps/")
} else if (Sys.info()["user"] == "Jen"){
  setwd("C:/Users/Jen/Dropbox/REMS/11 Comps/")
}

source("functions.R")

options(scipen = 999)
library(R2OpenBUGS)
library(coda)
library(tidyr)
library(dplyr)
library(rstan)

#number of people
n_people <- 1000
#number of items
n_items <- 100
#number of items with DIF
n_DIF <- 5
#number of reps
nreps <- 100
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

# #get the responses for a single item
# response_test <- response_sim(ability_test[1,], item_test[1,])
# 
# #get responses for a single person to a set of items
# responseset_test <- person_sim(ability_test[1,], item_test)

#get responses for a set of people to a set of items
dataset <- one_dataset(ability_test, item_test)

#get values for the DIF predictor
DIFpredict <- DIF_predictor(item_test, rho = 0.4)

#set up grouping variable
group <- ability_test[,2]
#set up data to go into model
b.dat <- list("n_people", "n_items", "dataset", "group", "DIFpredict")

#load stan model scripts
source("stan_scripts.R")

#precompile stan code for repeated use
precomp <- stanc(model_code = stancode)
precomp_model <- stan_model(stanc_ret = precomp)

#do the analysis for one set of responses
analysis_test <- one_analysis(x = precomp_model, n_iter = 1000, n_burn = 300,
                              debug = FALSE, n_cores = 1)






test <- sampling(precomp_model, model_name = "stan_test", data = b.dat,
             iter = 1000, warmup = 300, chains = 2, verbose = FALSE, cores = 2)


#output formatting code from Jake
params_summary <- summary(test, pars = c("a", "b", "D"),
                          probs = c(0.025, 0.25, 0.75, 0.975))$summary

params <- extract(test, pars = c("a", "b", "D"))

alpha <- colMeans(params$a)
delta <- colMeans(params$delta)
mu <- mean(params$mu)
H <- mean(params$H)



saveRDS(results, paste0(date, "_simulation_output.rds"))
