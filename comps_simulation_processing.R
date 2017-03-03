#### SETUP ####
setwd("E:/Comps simulation results 20170302/20170224_simulation-results")
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

true_ability_get <- function(condition, file_list){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  
  true_ability <- lapply(output, function(x){
    x <- x[which(grepl("true_ability", names(x)))]
  })
  true_ability <- lapply(true_ability, unlist, recursive = FALSE)
  true_ability <- unlist(true_ability, recursive = FALSE)
  
  return(true_ability)
}

true_item_params_get <- function(condition, file_list){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  true_item_params <- lapply(output, function(x){
    x <- x[which(grepl("true_item_params", names(x)))]
  })
  true_item_params <- lapply(true_item_params, unlist, recursive = FALSE)
  true_item_params <- unlist(true_item_params, recursive = FALSE)
  
  return(true_item_params)
}

#### DATA RETRIEVAL ####
correlations_conditions <- vector("list", length(conditions))
names(correlations_conditions) <- conditions

true_item_params_conditions <- vector("list", length(conditions))
names(true_item_params_conditions) <- conditions

true_ability_params_conditions <- vector("list", length(conditions))
names(true_ability_params_conditions) <- conditions

for(i in 1:length(conditions)){
  correlations_conditions[[i]] <- correlation_get(conditions[i], correlation_files)
  true_item_params_conditions[[i]] <- true_item_params_get(conditions[i], true_param_files)
  true_ability_params_conditions[[i]] <- true_ability_get(conditions[i], true_param_files)
}

#### FORMATTING ####
recovery <- data.frame(matrix(NA, nrow = length(conditions), ncol = ncol(correlations_conditions[[1]])))

names(recovery) <- colnames(correlations_conditions[[1]])
for(i in 1:nrow(recovery)){
  recovery[i,] <- colMeans(correlations_conditions[[i]])
}

recovery$condition <- condition
recovery$rho <- grep("rho", unlist(strsplit(recovery$condition, "_")), value = TRUE)
recovery$rho <- gsub("rho", "", recovery$rho)
recovery$rho <- gsub("-", ".", recovery$rho)

recovery$PREF <- grep("PREF", unlist(strsplit(recovery$condition, "_")), value = TRUE)
recovery$PREF <- gsub("PREF", "", recovery$PREF)
recovery$PREF <- gsub("-", ".", recovery$PREF)

recovery

for(i in 1:length(conditions)){
  data <- as.data.frame(correlations_conditions[[paste0(conditions[i])]])
  rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- gsub("-", ".", rho)
  
  PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- gsub("-", ".", PREF)
  rounded <- c(round(max(data$R2_diff), digits = 1), round(min(data$R2_diff), digits = 1))
  scale <- rounded[which.max(rounded)]
  
  #R2 difference
  ggplot(data, aes(x = R2_diff)) + 
    geom_histogram(binwidth = 0.05, alpha = 0.65, fill = "darkblue") + 
    scale_x_continuous(limits = c(-scale, scale)) +
    ggtitle(paste0("R-squared recovery for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in R-squared", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$R2_diff), color = "black")
  
  #focal mean difference
  ggplot(data, aes(x = foc_mean_diff)) + 
    geom_histogram(binwidth = 0.05, alpha = 0.65, fill = "darkblue") + 
    scale_x_continuous(limits = c(-scale, scale)) +
    ggtitle(paste0("Focal group mean recovery for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in focal group mean", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$foc_mean_diff), color = "black")
  
  #reference mean difference
  ggplot(data, aes(x = ref_mean_diff)) + 
    geom_histogram(binwidth = 0.05, alpha = 0.65, fill = "darkblue") + 
    scale_x_continuous(limits = c(-scale, scale)) +
    ggtitle(paste0("Reference group mean recovery for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in reference group mean", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$ref_mean_diff), color = "black")

  
  true_theta <- true_ability_params_conditions[[i]] 
  est_theta <- sd
}



