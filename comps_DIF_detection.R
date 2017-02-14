#### SETUP ####
if(Sys.info()["user"] == "jbrussow"){
  setwd("C:/Users/jbrussow/Dropbox/REMS/11 Comps/")
} else if (Sys.info()["user"] == "Jen"){
  setwd("C:/Users/Jen/Dropbox/REMS/11 Comps/")
}

source("functions.R")
date <- format.Date(Sys.Date(), "%Y%m%d")
options(scipen = 999)

needed_packages <- c("coda", "tidyr", "dplyr", "rstan", "rstudioapi")
for(i in 1:length(needed_packages)){
  package <- needed_packages[i]
  if(!require(paste(package), character.only = TRUE)){
    install.packages(package, character.only = TRUE)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

#### SPECIFICATIONS ####
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

#### data save setup ####
true_params <- vector("list", nreps)
output <- vector("list", nreps)

#### STAN SETUP ####
#load stan model scripts
source("stan_scripts.R")

#precompile stan code for repeated use
precomp <- stanc(model_code = stancode)
precomp_model <- stan_model(stanc_ret = precomp)

#set up data to go into model
b.dat <- list("n_people", "n_items", "dataset", "group", "DIFpredict")

for(i in 1:nreps){
  #### SIMULATION ####
  #simulate a set of items
  true_item_params <- item_sim(n_items, n_DIF, b_mean = 0, b_sd = 1, a_mean = 1, a_sd = .5, 
           nodif_mean = 0, nodif_sd = 0.1, dif_mean = 1, dif_sd = 0.1)
  
  true_params[[i]][[1]] <- true_item_params
  
  #simulate a set of people's ability scores
  true_ability <- ability_sim(n_people, P_REF = P_REF, ref_theta_mean = 0, ref_theta_sd = 1,
                              focal_theta_mean = -0.5, focal_theta_sd = 1)
  
  true_params[[i]][[2]] <- true_ability
  
  #get responses for a set of people to a set of items
  dataset <- one_dataset(true_ability, true_item_params)
  
  true_params[[i]][[3]] <- dataset
  
  #get values for the DIF predictor
  DIFpredict <- DIF_predictor(true_item_params, rho = rho)
  
  true_params[[i]][[4]] <- DIFpredict
  
  #set up grouping variable
  group <- true_ability[,2]
  
  #do the analysis for one set of responses
  analysis <- one_analysis(x = precomp_model, n_iter = 1000, n_burn = 300,
                                debug = FALSE, n_cores = 2)
  
  
  #### OUTPUT ####
  #output formatting code from Jake
  params_summary <- summary(analysis, pars = c("a", "b", "D"),
                            probs = c(0.025, 0.25, 0.75, 0.975))$summary
  
  params <- extract(analysis, pars = c("a", "b", "D"))
  
  alpha <- colMeans(params$a)
  delta <- colMeans(params$delta)
  mu <- mean(params$mu)
  H <- mean(params$H)
  
}  
  
saveRDS(results, paste0(date, "_simulation_output.rds"))
