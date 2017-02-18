#### SETUP ####
if(Sys.info()["user"] == "jbrussow"){
  setwd("C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation")
} else if (Sys.info()["user"] == "Jen"){
  setwd("C:/Users/Jen/Dropbox/REMS/11 Comps/Simulation")
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
nreps <- 1
#rho is the amount of DIF explained by the second-order factors
rho <- 0.4
#P_REF is the proportion of people in the reference group
P_REF <- 0.5

#### data save setup ####
true_params <- vector("list", nreps)
result_objs <- vector("list", nreps)
est_param_summary <- vector("list", nreps)
est_param_means <- vector("list", nreps)

#### STAN SETUP ####
#load stan model scripts
source("stan_scripts.R")

#precompile stan code for repeated use
precomp <- stanc(model_code = stancode)
precomp_model <- stan_model(stanc_ret = precomp)

#set up data to go into model
b.dat <- list("n_people", "n_items", "dataset", "group", "DIFpredict", "n_ref", 
              "n_ref_1")
b.par <- list("a", "theta", "b", "D", "beta0", "beta1", "var", "prec", "R2", 
              "foc_mean", "foc_var")

for(i in 1:nreps){
  #### SIMULATION ####
  #simulate a set of items
  true_item_params <- item_sim(n_items, n_DIF, b_mean = 0, b_sd = 1, a_min = 0.5, a_max = 3, 
           nodif_mean = 0, nodif_sd = 0.1, dif_mean = 1, dif_sd = 0.1)
  
  #simulate a set of people's ability scores
  true_ability <- ability_sim(n_people, P_REF = P_REF, ref_theta_mean = 0, ref_theta_sd = 1,
                              focal_theta_mean = -0.5, focal_theta_sd = 1)
  
  #get responses for a set of people to a set of items
  dataset <- one_dataset(true_ability, true_item_params)
  
  #get values for the DIF predictor
  DIFpredict <- DIF_predictor(true_item_params, rho = rho)
  
  #save the true parameters
  true_params[[i]] <- list(true_item_params, true_ability, dataset, DIFpredict)
  names(true_params[[i]]) <- c("true_item_params", "true_ability", "dataset", 
                               "DIFpredict")
  
  #set up grouping variable
  group <- true_ability[,2]
  n_ref <- sum(group)
  n_ref_1 <- sum(group)+1
  
  #do the analysis for one set of responses
  # analysis <- one_analysis(x = precomp_model, n_iter = 2000, n_burn = 1000,
  #                               debug = FALSE, n_cores = 2)
  
  analysis <- one_analysis_BUGS(dataset, n_iter = 5000, n_burn = 1000)

  #save the analysis object
  result_objs[[i]] <- analysis
  
  #### OUTPUT ####
  #output formatting code from Jake
  params_summary <- summary(analysis, pars = c("a", "b", "D", "beta1", "mu", 
                                               "sigma2", "R2", "theta",
                                               "foc_mean"),
                            probs = c(0.025, 0.25, 0.75, 0.975))$summary
  
  #save the "raw" estimated parameters (actually a summary object)
  est_param_summary[[i]] <- params_summary
  
  params <- extract(analysis, pars = c("a", "b", "D", "beta1", "mu", "sigma2", 
                                       "R2", "theta", "foc_mean"))
  
  alphas <- as.matrix(colMeans(params$a))
  betas <- as.matrix(colMeans(params$b))
  DIF_coef <- as.matrix(colMeans(params$D))
  beta1 <- mean(params$beta1)
  mu <- as.matrix(colMeans(params$mu))
  sigma2 <- mean(params$sigma2)
  R2 <- mean(params$R2)
  theta <- as.matrix(rowMeans(params$theta))
  foc_mean <- mean(params$foc_mean)
  
  #save the means of estimated parameters
  est_param_means[[i]] <- list(alphas, betas, DIF_coef, beta1, 
                          mu, sigma2, R2, theta, foc_mean, foc_var)
  names(est_param_means[[i]]) <- c("alphas", "betas", "DIF_coef", 
                              "beta1", "mu", "sigma2", "R2", "theta", 
                              "foc_mean", "foc_var")
  
}

#write all the good stuff out to disk
folder_name <- paste0(date, "_simulation-results")
file_tag <- paste0(nreps, "reps_", 
                     gsub(".", "-", as.character(rho), fixed = TRUE), "rho_", 
                     gsub(".", "-", as.character(P_REF), fixed = TRUE), "PREF")

if(!dir.exists(paste0(getwd(), "/", folder_name))){
  dir.create(paste0(getwd(), "/", folder_name))
}
setwd(paste0(getwd(), "/", folder_name))

saveRDS(true_params, paste0("true_params_", file_tag, ".rds"))
saveRDS(result_objs, paste0("result_objs_", file_tag, ".rds"))
saveRDS(est_param_summary, paste0("est_param_summary_", file_tag, ".rds"))
saveRDS(est_param_means, paste0("est_param_means_", file_tag, ".rds"))
