#### SETUP ####
# setwd("E:/Comps simulation results 20170302/20170224_simulation-results")
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/20170224_simulation-results/combined"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  setwd(gsub("jbrussow", "Jen", work_dir))
}

library(tidyr)
library(dplyr)
library(ggplot2)

files <- list.files(getwd())
files_df <- as.data.frame(files)
names(files_df) <- "filename"
files_df$filename <- as.character(files_df$filename)
files_df <- filter(files_df, filename != "combined")
files_df <- as.data.frame(files_df[c(grep("pdf", files_df$filename, invert = TRUE)),])
names(files_df) <- "filename"

for(i in 1:nrow(files_df)){
  info <-unlist(strsplit(as.character(
    files_df[i, "filename"]), "_"))
  len <- length(info)
  condition <- paste0(c(info[(len-1):len]), collapse = "", sep = "_")
  condition <- gsub("\\.rds\\_", "", condition)
  type <- paste0(c(info[1:(len-3)]), collapse = "", sep = "_")
  type <- gsub("\\_$", "", type)
  files_df[i, "condition"] <- condition
  files_df[i, "type"] <- type
}

conditions <- unique(files_df$condition)
types <- unique(files_df$type)

correlation_files <- files[which(grepl("correlations", files))]
true_param_files <- files[which(grepl("true_params", files))]
# est_param_files <- files[which(grepl("est_param_summary", files))]
est_param_files <- files[which(grepl("newest_param_summary", files))]
est_param_mean_files <- files[which(grepl("est_param_means", files))]

params_summary_names <- readRDS("../../params_summary_names.rds")
param_means_names <- c("a_params", "b_params", "D_params", "beta1", "mu", 
                       "sigma2", "R2", "theta", "foc_mean")

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

est_param_means_get <- function(condition, file_list, param_name){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  
  param <- lapply(output, function(x) as.data.frame(x[param_name]))
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
# est_item_params_conditions <- vector("list", length(conditions))
# names(est_item_params_conditions) <- conditions
# 
# est_ability_params_conditions <- vector("list", length(conditions))
# names(est_ability_params_conditions) <- conditions

param_means <- vector("list", length(param_means_names))
names(param_means) <- param_means_names

for(i in 1:length(conditions)){
  correlations_conditions[[i]] <- correlation_get(conditions[i], correlation_files)
  true_item_params_conditions[[i]] <- param_get(conditions[i], true_param_files, "true_item_params")
  true_ability_params_conditions[[i]] <- param_get(conditions[i], true_param_files, "true_ability")
  # est_item_params_conditions[[i]] <- est_param_get(conditions[i], est_param_files, "est_item_params")
  # est_ability_params_conditions[[i]] <- est_param_get(conditions[i], est_param_files, "est_ability")
  for(j in 1:length(param_means)){
   param_means[[j]][[i]] <- est_param_means_get(conditions[i], est_param_mean_files, names(param_means[j]))
  }
}

nreps <- length(true_ability_params_conditions[[1]])

#getting it into long format
conditions_ability <- NULL
for(i in 1:length(true_ability_params_conditions)){
  conditions_ability <- c(conditions_ability, rep(names(true_ability_params_conditions[i]), nreps))
}
conditions_item <- NULL
for(i in 1:length(true_item_params_conditions)){
  conditions_item <- c(conditions_item, rep(names(true_item_params_conditions[i]), nreps))
}


true_ability_params <- matrix(data = c(unlist(true_ability_params_conditions), 
                                    conditions_ability),
                           ncol = 2)

true_item_params <- matrix(data = c(unlist(true_item_params_conditions), 
                                    conditions_item),
                                ncol = 2)

#estimated params - need to fix
# est_ability_means <- lapply(est_ability_params_conditions, function(x) x = x$mean)
#   
# est_ability_params <- matrix(data = c(unlist(est_ability_means), 
#                                        conditions_ability),
#                               ncol = 2)
# 
# est_item_means <- lapply(est_item_params_conditions, function(x) x = x$mean)
# 
# est_item_params <- matrix(data = c(unlist(est_item_means), 
#                                       conditions_item),
#                              ncol = 2)

for(i in 1:length(param_means)){
  param_vec_length <- nrow(param_means[[i]][[1]])
  conditions_vec <- NULL
  for(j in 1:length(conditions)){
    conditions_vec <- c(conditions_vec, rep(conditions[j], param_vec_length))
  }
  param_means[[i]] <- matrix(data = c(unlist(param_means[[i]]), conditions_vec), ncol = 2)
}



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

recovery <- recovery[, c("rho", "PREF", "b_corr", "b_corr", "D_corr", 
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

scale_def_corr <- function(list, column){
  scale <- NA
  for(i in 1:length(list)){
    scale[i] <- (floor(((min(list[[i]][, column])) * 10)) / 10)
  }
  scale <- c(scale[which.min(scale)], 1)
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

source(paste0(getwd(), "/../../multiplot_fun.R"))

pdf("Bias_histograms.pdf", width = 10, height = 10)
multiplot(R2_histos[[1]], R2_histos[[2]], R2_histos[[3]], R2_histos[[4]], cols = 2)

multiplot(focmean_histos[[1]], focmean_histos[[2]], focmean_histos[[3]], focmean_histos[[4]], cols = 2)

multiplot(refmean_histos[[1]], refmean_histos[[2]], refmean_histos[[3]], refmean_histos[[4]], cols = 2)
dev.off()

### CORRELATION HISTOGRAMS
a_corr_histo <- vector("list", length(conditions))
b_corr_histo <- vector("list", length(conditions))
D_corr_histo <- vector("list", length(conditions))
theta_corr_histo <- vector("list", length(conditions))

for(i in 1:length(conditions)){
  data <- as.data.frame(correlations_conditions[[paste0(conditions[i])]])
  rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- gsub("-", ".", rho)
  
  PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- gsub("-", ".", PREF)
  
  #a_corr
  xscale <- scale_def_corr(correlations_conditions, "a_corr")
  increment <- (diff(xscale))/25
  
  a_corr_histo[[i]] <- ggplot(data, aes(x = a_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("A-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "A-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$a_corr), color = "black", linetype="dotted")
  
  #b_corr
  xscale <- scale_def_corr(correlations_conditions, "b_corr")
  increment <- (diff(xscale))/25
  
  b_corr_histo[[i]] <- ggplot(data, aes(x = b_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("B-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "B-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$b_corr), color = "black", linetype="dotted")
  
  #D_corr
  xscale <- scale_def_corr(correlations_conditions, "D_corr")
  increment <- (diff(xscale))/25
  
  D_corr_histo[[i]] <- ggplot(data, aes(x = D_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("D-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "D-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$D_corr), color = "black", linetype="dotted")
  
  #theta_corr
  xscale <- scale_def_corr(correlations_conditions, "theta_corr")
  increment <- (diff(xscale))/25
  
  theta_corr_histo[[i]] <- ggplot(data, aes(x = theta_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("Theta correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Theta correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$theta_corr), color = "black", linetype="dotted")
  
}

pdf("corr_histograms.pdf", width = 10, height = 10)
multiplot(a_corr_histo[[1]], a_corr_histo[[2]], a_corr_histo[[3]], a_corr_histo[[4]], cols = 2)

multiplot(b_corr_histo[[1]], b_corr_histo[[2]], b_corr_histo[[3]], b_corr_histo[[4]], cols = 2)

multiplot(D_corr_histo[[1]], D_corr_histo[[2]], D_corr_histo[[3]], D_corr_histo[[4]], cols = 2)

multiplot(theta_corr_histo[[1]], theta_corr_histo[[2]], theta_corr_histo[[3]], theta_corr_histo[[4]], cols = 2)

dev.off()


### CORRELATION SCATTERPLOTS
a_corr_scatter <- vector("list", length(conditions))
b_corr_scatter <- vector("list", length(conditions))
D_corr_scatter <- vector("list", length(conditions))
theta_corr_scatter <- vector("list", length(conditions))

for(i in 1:length(conditions)){
  rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- gsub("-", ".", rho)
  
  PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- gsub("-", ".", PREF)
  
  #a_corr
  data <- as.data.frame(param_means[["a_params"]])
  data <- filter(data, V2 == conditions[i])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data <- as.numeric(data[,1])
  true_a_param <- rep(true_item_params_conditions[[1]][[1]][,"a_param"], nreps)
  data <- cbind(data, true_a_param)
  ##STOPPING HERE FOR NOW
  xscale <- scale_def_corr(correlations_conditions, "a_corr")
  increment <- (diff(xscale))/25
  
  a_corr_scatter[[i]] <- ggplot(data, aes(x = a_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("A-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "A-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$a_corr), color = "black", linetype="dotted")
  
  #b_corr
  xscale <- scale_def_corr(correlations_conditions, "b_corr")
  increment <- (diff(xscale))/25
  
  b_corr_scatter[[i]] <- ggplot(data, aes(x = b_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("B-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "B-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$b_corr), color = "black", linetype="dotted")
  
  #D_corr
  xscale <- scale_def_corr(correlations_conditions, "D_corr")
  increment <- (diff(xscale))/25
  
  D_corr_scatter[[i]] <- ggplot(data, aes(x = D_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("D-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "D-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$D_corr), color = "black", linetype="dotted")
  
  #theta_corr
  xscale <- scale_def_corr(correlations_conditions, "theta_corr")
  increment <- (diff(xscale))/25
  
  theta_corr_scatter[[i]] <- ggplot(data, aes(x = theta_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("Theta correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Theta correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$theta_corr), color = "black", linetype="dotted")
  
}

pdf("corr_scattergrams.pdf", width = 10, height = 10)
multiplot(a_corr_scatter[[1]], a_corr_scatter[[2]], a_corr_scatter[[3]], a_corr_scatter[[4]], cols = 2)

multiplot(b_corr_scatter[[1]], b_corr_scatter[[2]], b_corr_scatter[[3]], b_corr_scatter[[4]], cols = 2)

multiplot(D_corr_scatter[[1]], D_corr_scatter[[2]], D_corr_scatter[[3]], D_corr_scatter[[4]], cols = 2)

multiplot(theta_corr_scatter[[1]], theta_corr_scatter[[2]], theta_corr_scatter[[3]], theta_corr_scatter[[4]], cols = 2)

dev.off()

