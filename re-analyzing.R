##combining results files from multiple runs
#### SETUP ####
# setwd("E:/Comps simulation results 20170302/20170224_simulation-results")
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

# work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/20170224_simulation-results"
work_dir <- "S:/Projects/DLM Secure/Psychometrician Asst Projects/Jennifer Projects/TEST/All_simulation_results_20170224-0306"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  setwd(gsub("jbrussow", "Jen", work_dir))
}

library(tidyr)
library(dplyr)
library(ggplot2)
library(rstan)
library(robustbase)

files <- as.data.frame(list.files(getwd()))
names(files) <- "filename"
files$filename <- as.character(files$filename)
files <- filter(files, filename != "combined")
  
for(i in 1:nrow(files)){
  info <-unlist(strsplit(as.character(
    files[i, "filename"]), "_"))
  len <- length(info)
  condition <- paste0(c(info[(len-1):len]), collapse = "", sep = "_")
  condition <- gsub("\\.rds\\_", "", condition)
  type <- paste0(c(info[1:(len-3)]), collapse = "", sep = "_")
  type <- gsub("\\_$", "", type)
  files[i, "condition"] <- condition
  files[i, "type"] <- type
  file_tag <- paste0(c(info[(len-2):len]), collapse = "", sep = "_")
  file_tag <- gsub("\\.rds\\_", "", file_tag)
  files[i, "file_tag"] <- file_tag
}

conditions <- unique(files$condition)
types <- unique(files$type)

types_conditions <- expand.grid(conditions, types)
names(types_conditions) <- c("conditions", "types")
types_conditions <- apply(types_conditions, 2, as.character)

results_files <- files[which(grepl("result", files$filename)),]
true_param_files <- files[which(grepl("true", files$filename)),]

for(i in 1:nrow(results_files)){
  output <- readRDS(results_files[i, "filename"])
  output <- output[!sapply(output, is.null)] 
  
  n_ref <- gsub("PREF", "", unlist(strsplit(results_files[1,"condition"], "_"))[2])
  n_ref <- as.numeric(gsub("-", "\\.", n_ref))*1000
  
  rho <- gsub("rho", "", unlist(strsplit(results_files[1,"condition"], "_"))[1])
  rho <- as.numeric(gsub("-", "\\.", rho))
  
  params_extraction <- vector("list", length(output))
  # est_param_summary <- vector("list", length(output))
  # est_param_medians <- vector("list", length(output))
  median_correlations <- vector("list", length(output))
  
  true_params <- readRDS(true_param_files[which(true_param_files$file_tag == results_files[i, "file_tag"]),"filename"])
    
  for(j in 1:length(output)){
    params_summary <- summary(output[[j]], pars = c("a", "b", "D", "beta1", "mu",
                                                 "sigma2", "R2", "theta",
                                                 "foc_mean"),
                              probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary

    est_param_summary[[j]] <- params_summary

    params <- extract(output[[j]], pars = c("a", "b", "D", "beta1", "mu", "sigma2",
                                         "R2", "theta", "foc_mean"))
    
    params_extraction[[j]] <- params

    a_params <- as.matrix(colMedians(params$a))
    b_params <- as.matrix(colMedians(params$b))
    D_params <- as.matrix(colMedians(params$D))
    beta1 <- median(params$beta1)
    mu <- as.matrix(colMedians(params$mu))
    sigma2 <- median(params$sigma2)
    R2 <- median(params$R2)
    theta <- as.matrix(colMedians(params$theta))
    foc_mean <- median(params$foc_mean)

    # #save the medians of estimated parameters
    # est_param_medians[[j]] <- list(a_params, b_params, D_params, beta1,
    #                              mu, sigma2, R2, theta, foc_mean)
    # names(est_param_medians[[j]]) <- c("a_params", "b_params", "D_params",
    #                                  "beta1", "mu", "sigma2", "R2", "theta",
    #                                  "foc_mean")
    
    #save the median correlations & differences from the expected values
    a_corr <- cor(a_params, true_params[[j]][["true_item_params"]][,"a_param"])
    b_corr <- cor(b_params, true_params[[j]][["true_item_params"]][,"b_param"])
    D_corr <- cor(D_params, true_params[[j]][["true_item_params"]][,"dif_param"])
    theta_corr <- cor(theta, true_params[[j]][["true_ability"]][,"theta"])
    foc_mean_diff <- -.5-foc_mean
    ref_mean_diff <- 0-mean(true_params[[j]][["true_ability"]][1:n_ref,"theta"])
    R2_diff <- (rho^2)-R2
    
    median_correlations[[j]] <- list(a_corr, b_corr, D_corr, theta_corr, 
                                     foc_mean_diff, ref_mean_diff, R2_diff)
    names(median_correlations[[j]]) <- c("a_corr", "b_corr", "D_corr", "theta_corr",
                                         "foc_mean_diff", "ref_mean_diff", "R2_diff")
    
  }
  
  # saveRDS(est_param_summary, paste0("newest_param_summary_", 
  #                                   results_files[i, "file_tag"], ".rds"))
  # saveRDS(est_param_medians, paste0("est_param_medians_",
  #                                   results_files[i, "file_tag"], ".rds"))
  saveRDS(params_extraction, paste0("params_extraction_",
                                    results_files[i, "file_tag"], ".rds"))
  saveRDS(median_correlations, paste0("median_correlations_",
                                    results_files[i, "file_tag"], ".rds"))
  
  rm(output)
  gc()
}
