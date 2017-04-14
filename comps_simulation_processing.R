#### SETUP ####
# setwd("E:/Comps simulation results 20170302/20170224_simulation-results")
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/all_simulation-results"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  setwd(gsub("jbrussow", "Jen", work_dir))
}

library(tidyr)
library(dplyr)
library(ggplot2)

source("../functions.R")

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

correlation_files <- files[which(grepl("^correlations", files))]
median_correlation_files <- files[which(grepl("^median", files))]
true_param_files <- files[which(grepl("true_params", files))]
est_param_files <- files[which(grepl("est_param_summary", files))]
est_param_mean_files <- files[which(grepl("est_param_means", files))]
est_param_median_files <- files[which(grepl("est_param_medians", files))]


params_summary_names <- readRDS("../params_summary_names.rds")
param_means_names <- c("a_params", "b_params", "D_params", "beta1", "mu", 
                       "sigma2", "R2", "theta", "foc_mean")

#### DATA RETRIEVAL ####

#correlations
correlations_conditions <- vector("list", length(conditions))
names(correlations_conditions) <- conditions

median_correlation_conditions <- vector("list", length(conditions))
names(median_correlation_conditions) <- conditions

#true params
true_item_params <- vector("list", 3)
names(true_item_params) <- c("a_param", "b_param", "dif_param")

est_param_means <- vector("list", length(param_means_names))
names(est_param_means) <- param_means_names

est_param_medians <- vector("list", length(param_means_names))
names(est_param_medians) <- param_means_names

for(i in 1:length(conditions)){
  correlations_conditions[[i]] <- correlation_get(conditions[i], correlation_files)
  
  median_correlation_conditions[[i]] <- correlation_get(conditions[i], median_correlation_files)

  for(j in 1:length(est_param_means)){
   est_param_means[[j]][[i]] <- est_param_means_get(conditions[i], est_param_mean_files, names(est_param_means[j]))
  }
  
  for(j in 1:length(est_param_medians)){
    est_param_medians[[j]][[i]] <- est_param_means_get(conditions[i], est_param_median_files, names(est_param_medians[j]))
  }
  
  for(j in 1:length(true_item_params)){
    true_item_params[[j]][[i]] <- true_param_get(conditions[i], true_param_files, 
                                                 param_type = "true_item_params",
                                                 param_name = names(true_item_params[j]))
  }
}

nreps <- nrow(correlations_conditions[[1]])

#getting it into long format
for(i in 1:length(est_param_means)){
  param_vec_length <- nrow(est_param_means[[i]][[1]])
  conditions_vec <- NULL
  for(j in 1:length(conditions)){
    conditions_vec <- c(conditions_vec, rep(conditions[j], param_vec_length))
  }
  est_param_means[[i]] <- matrix(data = c(unlist(est_param_means[[i]]), conditions_vec), ncol = 2)
}

for(i in 1:length(est_param_medians)){
  param_vec_length <- nrow(est_param_medians[[i]][[1]])
  conditions_vec <- NULL
  for(j in 1:length(conditions)){
    conditions_vec <- c(conditions_vec, rep(conditions[j], param_vec_length))
  }
  est_param_medians[[i]] <- matrix(data = c(unlist(est_param_medians[[i]]), conditions_vec), ncol = 2)
}


for(i in 1:length(true_item_params)){
  param_vec_length <- nrow(true_item_params[[i]][[1]])
  conditions_vec <- NULL
  for(j in 1:length(conditions)){
    conditions_vec <- c(conditions_vec, rep(conditions[j], param_vec_length))
  }
  true_item_params[[i]] <- matrix(data = c(unlist(true_item_params[[i]]), conditions_vec), ncol = 2)
}

#### MEANS RECOVERY DF FORMATTING ####
means_recovery <- data.frame(matrix(NA, nrow = length(conditions), ncol = ncol(correlations_conditions[[1]])))

names(means_recovery) <- colnames(correlations_conditions[[1]])
for(i in 1:nrow(means_recovery)){
  means_recovery[i,] <- colMeans(correlations_conditions[[i]])
}

means_recovery$condition <- conditions
means_recovery$rho <- grep("rho", unlist(strsplit(means_recovery$condition, "_")), value = TRUE)
means_recovery$rho <- gsub("rho", "", means_recovery$rho)
means_recovery$rho <- gsub("-", ".", means_recovery$rho)

means_recovery$PREF <- grep("PREF", unlist(strsplit(means_recovery$condition, "_")), value = TRUE)
means_recovery$PREF <- gsub("PREF", "", means_recovery$PREF)
means_recovery$PREF <- gsub("-", ".", means_recovery$PREF)

means_recovery <- means_recovery[, c("rho", "PREF", "a_corr", "b_corr", "D_corr", 
                         "theta_corr", "foc_mean_diff", "ref_mean_diff", "R2_diff")]


#### MEDIANS RECOVERY DF FORMATTING ####
medians_recovery <- data.frame(matrix(NA, nrow = length(conditions), ncol = ncol(median_correlation_conditions[[1]])))

names(medians_recovery) <- colnames(median_correlation_conditions[[1]])
for(i in 1:nrow(medians_recovery)){
  medians_recovery[i,] <- colMeans(median_correlation_conditions[[i]])
}

medians_recovery$condition <- conditions
medians_recovery$rho <- grep("rho", unlist(strsplit(medians_recovery$condition, "_")), value = TRUE)
medians_recovery$rho <- gsub("rho", "", medians_recovery$rho)
medians_recovery$rho <- gsub("-", ".", medians_recovery$rho)

medians_recovery$PREF <- grep("PREF", unlist(strsplit(medians_recovery$condition, "_")), value = TRUE)
medians_recovery$PREF <- gsub("PREF", "", medians_recovery$PREF)
medians_recovery$PREF <- gsub("-", ".", medians_recovery$PREF)

medians_recovery <- medians_recovery[, c("rho", "PREF", "a_corr", "b_corr", "D_corr", 
                                     "theta_corr", "foc_mean_diff", "ref_mean_diff", "R2_diff")]

####means vs. medians recovery
apply(means_recovery, 2, as.numeric) - apply(medians_recovery, 2, as.numeric)

#### GRAPHS ####
colors <- c("darkblue", "darkred", "darkgreen", "darkorange")
source(paste0(getwd(), "/../multiplot_fun.R"))

### BIAS ####
R2_histos <- vector("list", length(conditions))
focmean_histos <- vector("list", length(conditions))
refmean_histos <- vector("list", length(conditions))

#mean correlations
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

pdf("Mean_bias_histograms.pdf", width = 10, height = 10)
multiplot(R2_histos[[1]], R2_histos[[2]], R2_histos[[3]], R2_histos[[4]], cols = 2)

multiplot(focmean_histos[[1]], focmean_histos[[2]], focmean_histos[[3]], focmean_histos[[4]], cols = 2)

multiplot(refmean_histos[[1]], refmean_histos[[2]], refmean_histos[[3]], refmean_histos[[4]], cols = 2)
dev.off()

#median correlations
for(i in 1:length(conditions)){
  data <- as.data.frame(median_correlation_conditions[[paste0(conditions[i])]])
  rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- gsub("-", ".", rho)
  
  PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- gsub("-", ".", PREF)
  
  #R2 difference
  xscale <- scale_def(median_correlation_conditions, "R2_diff")
  increment <- (xscale*2)/25
  
  R2_histos[[i]] <- ggplot(data, aes(x = R2_diff)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = c(-xscale, xscale)) +
    ggtitle(paste0("R-squared recovery bias for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in R-squared", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$R2_diff), color = "black", linetype="dotted")
  
  #focal mean difference
  xscale <- scale_def(median_correlation_conditions, "foc_mean_diff")
  increment <- (xscale*2)/25
  
  focmean_histos[[i]] <- ggplot(data, aes(x = foc_mean_diff)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = c(-xscale, xscale)) +
    ggtitle(paste0("Focal group mean recovery bias for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in focal group mean", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$foc_mean_diff), color = "black", linetype="dotted")
  
  #reference mean difference
  xscale <- scale_def(median_correlation_conditions, "ref_mean_diff")
  increment <- (xscale*2)/25
  
  refmean_histos[[i]] <- ggplot(data, aes(x = ref_mean_diff)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = c(-xscale, xscale)) +
    ggtitle(paste0("Reference group mean recovery bias for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Difference in reference group mean", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$ref_mean_diff), color = "black", linetype="dotted")
}

pdf("Median_bias_histograms.pdf", width = 10, height = 10)
multiplot(R2_histos[[1]], R2_histos[[2]], R2_histos[[3]], R2_histos[[4]], cols = 2)

multiplot(focmean_histos[[1]], focmean_histos[[2]], focmean_histos[[3]], focmean_histos[[4]], cols = 2)

multiplot(refmean_histos[[1]], refmean_histos[[2]], refmean_histos[[3]], refmean_histos[[4]], cols = 2)
dev.off()


### CORRELATION HISTOGRAMS ####
#mean correlations
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

pdf("mean_corr_histograms.pdf", width = 10, height = 10)
multiplot(a_corr_histo[[1]], a_corr_histo[[2]], a_corr_histo[[3]], a_corr_histo[[4]], cols = 2)

multiplot(b_corr_histo[[1]], b_corr_histo[[2]], b_corr_histo[[3]], b_corr_histo[[4]], cols = 2)

multiplot(D_corr_histo[[1]], D_corr_histo[[2]], D_corr_histo[[3]], D_corr_histo[[4]], cols = 2)

multiplot(theta_corr_histo[[1]], theta_corr_histo[[2]], theta_corr_histo[[3]], theta_corr_histo[[4]], cols = 2)

dev.off()


#median correlations
a_corr_histo <- vector("list", length(conditions))
b_corr_histo <- vector("list", length(conditions))
D_corr_histo <- vector("list", length(conditions))
theta_corr_histo <- vector("list", length(conditions))

for(i in 1:length(conditions)){
  data <- as.data.frame(median_correlation_conditions[[paste0(conditions[i])]])
  rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- gsub("-", ".", rho)
  
  PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- gsub("-", ".", PREF)
  
  #a_corr
  xscale <- scale_def_corr(median_correlation_conditions, "a_corr")
  increment <- (diff(xscale))/25
  
  a_corr_histo[[i]] <- ggplot(data, aes(x = a_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("A-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "A-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$a_corr), color = "black", linetype="dotted")
  
  #b_corr
  xscale <- scale_def_corr(median_correlation_conditions, "b_corr")
  increment <- (diff(xscale))/25
  
  b_corr_histo[[i]] <- ggplot(data, aes(x = b_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("B-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "B-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$b_corr), color = "black", linetype="dotted")
  
  #D_corr
  xscale <- scale_def_corr(median_correlation_conditions, "D_corr")
  increment <- (diff(xscale))/25
  
  D_corr_histo[[i]] <- ggplot(data, aes(x = D_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("D-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "D-parameter correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$D_corr), color = "black", linetype="dotted")
  
  #theta_corr
  xscale <- scale_def_corr(median_correlation_conditions, "theta_corr")
  increment <- (diff(xscale))/25
  
  theta_corr_histo[[i]] <- ggplot(data, aes(x = theta_corr)) + 
    geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
    scale_x_continuous(limits = xscale) +
    ggtitle(paste0("Theta correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "Theta correlation", y = "Count") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_vline(xintercept = mean(data$theta_corr), color = "black", linetype="dotted")
  
}

pdf("median_corr_histograms.pdf", width = 10, height = 10)
multiplot(a_corr_histo[[1]], a_corr_histo[[2]], a_corr_histo[[3]], a_corr_histo[[4]], cols = 2)

multiplot(b_corr_histo[[1]], b_corr_histo[[2]], b_corr_histo[[3]], b_corr_histo[[4]], cols = 2)

multiplot(D_corr_histo[[1]], D_corr_histo[[2]], D_corr_histo[[3]], D_corr_histo[[4]], cols = 2)

multiplot(theta_corr_histo[[1]], theta_corr_histo[[2]], theta_corr_histo[[3]], theta_corr_histo[[4]], cols = 2)

dev.off()



### CORRELATION SCATTERPLOTS ####
#mean correlations
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
  data <- as.data.frame(est_param_means[["a_params"]])
  data <- filter(data, V2 == conditions[i])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data <- as.numeric(data[,1])
  true_param <- as.data.frame(true_item_params[["a_param"]])
  true_param <- filter(true_param, V2 == conditions[i])
  true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
  true_param <- as.numeric(true_param[,1])
  data <- as.data.frame(cbind(data, true_param))
  names(data) <- c("est_param", "true_param")
  
  a_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
    geom_point(alpha = 0.65, color = colors[i]) + 
    ggtitle(paste0("A-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  #b_corr
  data <- as.data.frame(est_param_means[["b_params"]])
  data <- filter(data, V2 == conditions[i])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data <- as.numeric(data[,1])
  true_param <- as.data.frame(true_item_params[["b_param"]])
  true_param <- filter(true_param, V2 == conditions[i])
  true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
  true_param <- as.numeric(true_param[,1])
  data <- as.data.frame(cbind(data, true_param))
  names(data) <- c("est_param", "true_param")
  
  
  b_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
    geom_point(alpha = 0.65, color = colors[i]) + 
    ggtitle(paste0("B-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  #D_corr
  data <- as.data.frame(est_param_means[["D_params"]])
  data <- filter(data, V2 == conditions[i])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data <- as.numeric(data[,1])
  true_param <- as.data.frame(true_item_params[["dif_param"]])
  true_param <- filter(true_param, V2 == conditions[i])
  true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
  true_param <- as.numeric(true_param[,1])
  data <- as.data.frame(cbind(data, true_param))
  names(data) <- c("est_param", "true_param")
  
  D_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
    geom_point(alpha = 0.65, color = colors[i]) + 
    ggtitle(paste0("D-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  
}

pdf("mean_corr_scatterplots.pdf", width = 10, height = 10)
multiplot(a_corr_scatter[[1]], a_corr_scatter[[2]], a_corr_scatter[[3]], a_corr_scatter[[4]], cols = 2)

multiplot(b_corr_scatter[[1]], b_corr_scatter[[2]], b_corr_scatter[[3]], b_corr_scatter[[4]], cols = 2)

multiplot(D_corr_scatter[[1]], D_corr_scatter[[2]], D_corr_scatter[[3]], D_corr_scatter[[4]], cols = 2)

dev.off()


#median correlations
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
  data <- as.data.frame(est_param_medians[["a_params"]])
  data <- filter(data, V2 == conditions[i])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data <- as.numeric(data[,1])
  true_param <- as.data.frame(true_item_params[["a_param"]])
  true_param <- filter(true_param, V2 == conditions[i])
  true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
  true_param <- as.numeric(true_param[,1])
  data <- as.data.frame(cbind(data, true_param))
  names(data) <- c("est_param", "true_param")
  
  a_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
    geom_point(alpha = 0.65, color = colors[i]) + 
    ggtitle(paste0("A-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  #b_corr
  data <- as.data.frame(est_param_medians[["b_params"]])
  data <- filter(data, V2 == conditions[i])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data <- as.numeric(data[,1])
  true_param <- as.data.frame(true_item_params[["b_param"]])
  true_param <- filter(true_param, V2 == conditions[i])
  true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
  true_param <- as.numeric(true_param[,1])
  data <- as.data.frame(cbind(data, true_param))
  names(data) <- c("est_param", "true_param")
  
  b_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
    geom_point(alpha = 0.65, color = colors[i]) + 
    ggtitle(paste0("B-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  #D_corr
  data <- as.data.frame(est_param_medians[["D_params"]])
  data <- filter(data, V2 == conditions[i])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data <- as.numeric(data[,1])
  true_param <- as.data.frame(true_item_params[["dif_param"]])
  true_param <- filter(true_param, V2 == conditions[i])
  true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
  true_param <- as.numeric(true_param[,1])
  data <- as.data.frame(cbind(data, true_param))
  names(data) <- c("est_param", "true_param")
  
  D_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
    geom_point(alpha = 0.65, color = colors[i]) + 
    ggtitle(paste0("D-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  
}

pdf("median_corr_scatterplots.pdf", width = 10, height = 10)
multiplot(a_corr_scatter[[1]], a_corr_scatter[[2]], a_corr_scatter[[3]], a_corr_scatter[[4]], cols = 2)

multiplot(b_corr_scatter[[1]], b_corr_scatter[[2]], b_corr_scatter[[3]], b_corr_scatter[[4]], cols = 2)

multiplot(D_corr_scatter[[1]], D_corr_scatter[[2]], D_corr_scatter[[3]], D_corr_scatter[[4]], cols = 2)

dev.off()
