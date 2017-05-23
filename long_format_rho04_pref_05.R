#### SETUP ####
# work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation"

work_dir <- getwd()

# if(Sys.info()["user"] == "jbrussow"){
#   setwd(work_dir)
# } else if (Sys.info()["user"] == "Jen"){
#   setwd(gsub("jbrussow", "Jen", work_dir))
# }

source("functions.R")
date <- format.Date(Sys.Date(), "%Y%m%d")
options(scipen = 999)

needed_packages <- c("tidyr", "dplyr", "rstan", "rstudioapi", "robustbase")
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
n_items <- 40
#number of items with DIF
n_DIF <- 2
#number of reps
nreps <- 50
#rho is the amount of DIF explained by the second-order factors
rho <- 0.4
#P_REF is the proportion of people in the reference group
P_REF <- 0.5

#### STAN SETUP ####
#load stan model scripts
source("stan_scripts.R")

b.dat_long <- list("n_people", "n_items", "n_observations", "respondentid", 
                   "itemid", "response", "group", "group_long",
                   "DIFpredict")

#analysis setup
precomp <- stanc(model_code = stancode_long)
precomp_model <- stan_model(stanc_ret = precomp)

#### DATA SAVE SETUP ####
true_params <- vector("list", nreps)
# result_objs <- vector("list", nreps)
est_param_summary <- vector("list", nreps)
est_param_means <- vector("list", nreps)
est_param_medians <- vector("list", nreps)
correlations <- vector("list", nreps)
median_correlations <- vector("list", nreps)
params_extraction <- vector("list", nreps)

#setup output folder for use later
folder_name <- paste0(date, "_simulation-results")
file_tag <- paste0(nreps, "reps_", 
                   gsub(".", "-", as.character(rho), fixed = TRUE), "rho_", 
                   gsub(".", "-", as.character(P_REF), fixed = TRUE), "PREF")

if(!dir.exists(paste0(work_dir, "/", folder_name))){
  dir.create(paste0(work_dir, "/", folder_name))
}
setwd(paste0(work_dir, "/", folder_name))

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

  #restructuring the data to long format
  dataset <- long_format(dataset, group)
  
  #pulling the individual parts back out
  respondentid <- dataset$respondentid
  itemid <- as.numeric(dataset$itemid)
  response <- dataset$response
  group_long <- dataset$group
  
  n_observations <- nrow(dataset)
  
  #conducting the analysis
  analysis <- sampling(precomp_model, data = b.dat_long,
                       iter = 10000, warmup = 5000, chains = 2, verbose = FALSE, cores = 2)
  
  #save the analysis object
  # result_objs[[i]] <- analysis
  
  #### OUTPUT ####
  #pull out the summary of the estimated parameters
  params_summary <- summary(analysis, pars = c("a", "b", "D", "beta1", "mu", 
                                               "sigma2", "R2", "theta",
                                               "foc_mean"),
                            probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary
  
  #save the summary of the estimated parameters
  est_param_summary[[i]] <- params_summary
  
  #calculate the means of the estimated parameters
  params <- extract(analysis, pars = c("a", "b", "D", "beta1", "mu", "sigma2", 
                                       "R2", "theta", "foc_mean"))
  
  params_extraction[[i]] <- params
  
  a_params <- as.matrix(colMeans(params$a))
  b_params <- as.matrix(colMeans(params$b))
  D_params <- as.matrix(colMeans(params$D))
  beta1 <- mean(params$beta1)
  mu <- as.matrix(colMeans(params$mu))
  sigma2 <- mean(params$sigma2)
  R2 <- mean(params$R2)
  theta <- as.matrix(colMeans(params$theta))
  foc_mean <- mean(params$foc_mean)
  
  #save the means of estimated parameters
  est_param_means[[i]] <- list(a_params, b_params, D_params, beta1, 
                               mu, sigma2, R2, theta, foc_mean)
  names(est_param_means[[i]]) <- c("a_params", "b_params", "D_params", 
                                   "beta1", "mu", "sigma2", "R2", "theta", 
                                   "foc_mean")
  
  #save the mean correlations & differences from the expected values
  a_corr <- cor(a_params, true_item_params[,"a_param"])
  b_corr <- cor(b_params, true_item_params[,"b_param"])
  D_corr <- cor(D_params, true_item_params[,"dif_param"])
  theta_corr <- cor(theta, true_ability[, 1])
  foc_mean_diff <- foc_mean-(-.5)
  ref_mean_diff <- mean(theta[1:n_ref])-0
  R2_diff <- R2-(rho^2)
  
  correlations[[i]] <- list(a_corr, b_corr, D_corr, theta_corr, 
                            foc_mean_diff, ref_mean_diff, R2_diff)
  names(correlations[[i]]) <- c("a_corr", "b_corr", "D_corr", "theta_corr",
                                "foc_mean_diff", "ref_mean_diff", "R2_diff")
  
  #calculate the medians of the estimated parameters
  a_params <- as.matrix(colMedians(params$a))
  b_params <- as.matrix(colMedians(params$b))
  D_params <- as.matrix(colMedians(params$D))
  beta1 <- median(params$beta1)
  mu <- as.matrix(colMedians(params$mu))
  sigma2 <- median(params$sigma2)
  R2 <- median(params$R2)
  theta <- as.matrix(colMedians(params$theta))
  foc_mean <- median(params$foc_mean)
  
  #save the medians of estimated parameters
  est_param_medians[[i]] <- list(a_params, b_params, D_params, beta1, 
                                 mu, sigma2, R2, theta, foc_mean)
  names(est_param_medians[[i]]) <- c("a_params", "b_params", "D_params", 
                                     "beta1", "mu", "sigma2", "R2", "theta", 
                                     "foc_mean")
  
  #save the median correlations & differences from the expected values
  a_corr <- cor(a_params, true_item_params[,"a_param"])
  b_corr <- cor(b_params, true_item_params[,"b_param"])
  D_corr <- cor(D_params, true_item_params[,"dif_param"])
  theta_corr <- cor(theta, true_ability[, 1])
  foc_mean_diff <- -.5-foc_mean
  ref_mean_diff <- 0-mean(theta[1:n_ref])
  R2_diff <- (rho^2)-R2
  
  median_correlations[[i]] <- list(a_corr, b_corr, D_corr, theta_corr, 
                                   foc_mean_diff, ref_mean_diff, R2_diff)
  names(median_correlations[[i]]) <- c("a_corr", "b_corr", "D_corr", "theta_corr",
                                       "foc_mean_diff", "ref_mean_diff", "R2_diff")
  
  #write all the good stuff out to disk
  saveRDS(true_params, paste0("true_params_", file_tag, ".rds"))
  # saveRDS(result_objs, paste0("result_objs_", file_tag, ".rds"))
  saveRDS(est_param_summary, paste0("est_param_summary_", file_tag, ".rds"))
  saveRDS(params_extraction, paste0("params_extraction_", file_tag, ".rds"))
  saveRDS(est_param_means, paste0("est_param_means_", file_tag, ".rds"))
  saveRDS(est_param_medians, paste0("est_param_medians_", file_tag, ".rds"))
  saveRDS(correlations, paste0("correlations_", file_tag, ".rds"))
  saveRDS(median_correlations, paste0("median_correlations_", file_tag, ".rds"))
   
  print(paste0("Replication ", i, " complete"))
  print(paste0("Max R-hat value: ", max(params_summary[, "Rhat"])))
  print(paste0("Mean R-hat value: ", mean(params_summary[, "Rhat"])))
}

