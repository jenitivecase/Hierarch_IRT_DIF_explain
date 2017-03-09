#### SETUP ####
# setwd("E:/Comps simulation results 20170302/20170224_simulation-results")
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/20170224_simulation-results"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  setwd(grepl("jbrussow", "Jen", work_dir))
}

library(tidyr)
library(dplyr)
library(ggplot2)

files <- list.files(getwd())

correlation_files <- files[which(grepl("correlations", files))]
true_param_files <- files[which(grepl("true_params", files))]
est_param_files <- files[which(grepl("est_param_summary", files))]

conditions <- c("0-4rho_0-5PREF", "0-4rho_0-9PREF", "0-8rho_0-5PREF", "0-8rho_0-9PREF")

#### FUNCTIONS ####
correlation_get <- function(condition, file_list){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  output <- lapply(output, unlist, recursive = FALSE)
  output <- do.call(rbind, output)
  return(output)
}

param_get <- function(condition, file_list, param_name){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  
  param <- lapply(output, function(x){
    x <- x[which(grepl("param", names(x)))]
  })
  param <- lapply(param, unlist, recursive = FALSE)
  param <- unlist(param, recursive = FALSE)
  
  return(param)
}

est_param_get <- function(condition, file_list, param_name){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  
  param <- lapply(output, as.data.frame)
  param <- bind_rows(param, .id = names(output))
  return(param)
}

#### DATA RETRIEVAL ####

#correlations
correlations_conditions <- vector("list", length(conditions))
names(correlations_conditions) <- conditions

#true params
true_item_params_conditions <- vector("list", length(conditions))
names(true_item_params_conditions) <- conditions

true_ability_params_conditions <- vector("list", length(conditions))
names(true_ability_params_conditions) <- conditions

#est params
est_item_params_conditions <- vector("list", length(conditions))
names(est_item_params_conditions) <- conditions

est_ability_params_conditions <- vector("list", length(conditions))
names(est_ability_params_conditions) <- conditions



for(i in 1:length(conditions)){
  correlations_conditions[[i]] <- correlation_get(conditions[i], correlation_files)
  true_item_params_conditions[[i]] <- param_get(conditions[i], true_param_files, "true_item_params")
  true_ability_params_conditions[[i]] <- param_get(conditions[i], true_param_files, "true_ability")
  est_item_params_conditions[[i]] <- est_param_get(conditions[i], est_param_files, "est_item_params")
  est_ability_params_conditions[[i]] <- est_param_get(conditions[i], est_param_files, "est_ability")
}

#getting it into long format
conditions_ability <- NULL
for(i in 1:length(true_ability_params_conditions)){
  conditions_ability <- c(conditions_ability, rep(names(true_ability_params_conditions[i]), 
                       length(true_ability_params_conditions[[i]])))
}
conditions_item <- NULL
for(i in 1:length(true_item_params_conditions)){
  conditions_item <- c(conditions_item, rep(names(true_item_params_conditions[i]), 
                                                  length(true_item_params_conditions[[i]])))
}

true_ability_params <- matrix(data = c(unlist(true_ability_params_conditions), 
                                    conditions_ability),
                           ncol = 2)

true_item_params <- matrix(data = c(unlist(true_item_params_conditions), 
                                    conditions_item),
                                ncol = 2)

#estimated params




#### FORMATTING ####
recovery <- data.frame(matrix(NA, nrow = length(conditions), ncol = ncol(correlations_conditions[[1]])))

names(recovery) <- colnames(correlations_conditions[[1]])
for(i in 1:nrow(recovery)){
  recovery[i,] <- colMeans(correlations_conditions[[i]])
}

recovery$condition <- conditions
recovery$rho <- grep("rho", unlist(strsplit(recovery$condition, "_")), value = TRUE)
recovery$rho <- gsub("rho", "", recovery$rho)
recovery$rho <- gsub("-", ".", recovery$rho)

recovery$PREF <- grep("PREF", unlist(strsplit(recovery$condition, "_")), value = TRUE)
recovery$PREF <- gsub("PREF", "", recovery$PREF)
recovery$PREF <- gsub("-", ".", recovery$PREF)

recovery <- recovery[, c("rho", "PREF", "a_corr", "b_corr", "D_corr", 
                         "theta_corr", "foc_mean_diff", "ref_mean_diff", "R2_diff")]

#### GRAPHS ####
scale_def <- function(list, column){
  scale <- NA
  for(i in 1:length(list)){
    rounded <- abs(c(round(max(list[[i]][, column]), digits = 1), 
                     round(min(list[[i]][, column]), digits = 1)))
    scale[i] <- rounded[which.max(rounded)]
  }
  scale <- scale[which.max(scale)]
  return(scale)
}

colors <- c("darkblue", "darkred", "darkgreen", "darkorange")


### BIAS
R2_histos <- vector("list", length(conditions))
focmean_histos <- vector("list", length(conditions))
refmean_histos <- vector("list", length(conditions))

for(i in 1:length(conditions)){
  data <- as.data.frame(correlations_conditions[[paste0(conditions[i])]])
  rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- gsub("-", ".", rho)
  
  PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- gsub("-", ".", PREF)
  
  #R2 difference
  xscale <- scale_def(correlations_conditions, "R2_diff")
  increment <- (xscale*2)/25
  
  R2_histos[[i]] <- ggplot(data, aes(x = R2_diff)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = c(-xscale, xscale)) +
    ggtitle(paste0("R-squared recovery bias for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in R-squared", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$R2_diff), color = "black", linetype="dotted")
  
  #focal mean difference
  xscale <- scale_def(correlations_conditions, "foc_mean_diff")
  increment <- (xscale*2)/25
  
  focmean_histos[[i]] <- ggplot(data, aes(x = foc_mean_diff)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = c(-xscale, xscale)) +
    ggtitle(paste0("Focal group mean recovery bias for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in focal group mean", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$foc_mean_diff), color = "black", linetype="dotted")
  
  #reference mean difference
  xscale <- scale_def(correlations_conditions, "ref_mean_diff")
  increment <- (xscale*2)/25
  
  refmean_histos[[i]] <- ggplot(data, aes(x = ref_mean_diff)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = c(-xscale, xscale)) +
    ggtitle(paste0("Reference group mean recovery bias for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in reference group mean", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$ref_mean_diff), color = "black", linetype="dotted")
}

source(paste0(getwd(), "/../multiplot_fun.R"))
multiplot(R2_histos[[1]], R2_histos[[2]], R2_histos[[3]], R2_histos[[4]], cols = 2)

multiplot(focmean_histos[[1]], focmean_histos[[2]], focmean_histos[[3]], focmean_histos[[4]], cols = 2)

multiplot(refmean_histos[[1]], refmean_histos[[2]], refmean_histos[[3]], refmean_histos[[4]], cols = 2)

### CORRELATIONS
{
  
}