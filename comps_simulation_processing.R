#### SETUP ####
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/combined_dissertation_results"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  work_dir <- gsub("jbrussow", "Jen", work_dir)
  setwd(work_dir)
} else if(Sys.info()["user"] == "jennifer.brussow"){
  work_dir <- gsub("jbrussow", "jennifer.brussow", work_dir)
  setwd(work_dir)
}

if(!dir.exists("analysis")) dir.create("analysis")

library(tidyr)
library(dplyr)
library(openxlsx)
library(ggplot2)

source("../functions.R")

files <- list.files(work_dir, pattern = ".rds")
files <- as.data.frame(files)
names(files) <- "filename"
files$filename <- as.character(files$filename)


for(i in 1:nrow(files)){
  filename_temp <- as.character(files[i, "filename"])
  filename_temp <- gsub("^[^\\/]*\\/", "", filename_temp)
  info <- unlist(strsplit(filename_temp, "_"))
  len <- length(info)
  condition <- paste0(c(info[(len-3):(len)]), collapse = "_")
  condition <- gsub("\\.rds", "", condition)
  type <- "temp"
  if(grepl("^CIs_analysis", filename_temp)){
    type <- "CIs_analysis"
  } else if(grepl("^CIs_proportion", filename_temp)){
    type <- "CIs_proportion"
  } else if(grepl("^correlation", filename_temp)){
    type <- "correlations"
  } else if(grepl("^est_param_means", filename_temp)){
    type <- "est_param_means"
  } else if(grepl("^est_param_summary", filename_temp)){
    type <- "est_param_summary" 
  } else if(grepl("^true_params", filename_temp)){
    type <- "true_params"
  } else {
    type <- NA
  }
  
  files[i, "condition"] <- condition
  files[i, "type"] <- type
}

conditions <- unique(files$condition)
types <- unique(files$type)

correlation_files <- files[which(grepl("correlations", files$filename)), "filename"]
true_param_files <- files[which(grepl("true_params", files$filename)), "filename"]
est_param_files <- files[which(grepl("est_param_summary", files$filename)), "filename"]
est_param_mean_files <- files[which(grepl("est_param_means", files$filename)), "filename"]
CIs_proportion_files <- files[which(grepl("CIs_proportion", files$filename)), "filename"]
CIs_analysis_files <- files[which(grepl("CIs_analysis", files$filename)), "filename"]

params_summary_names <- readRDS("../params_summary_names.rds")
param_means_names <- c("a_params", "b_params", "D_params", "beta0", "beta1", "mu", 
                       "sigma2", "R2", "theta", "foc_mean")
ability_means_names <- c("theta")

#### DATA RETRIEVAL ####

#correlations
correlations_conditions <- vector("list", length(conditions))
names(correlations_conditions) <- conditions

#true params
true_item_params <- vector("list", 3)
names(true_item_params) <- c("a_param", "b_param", "dif_param")

true_ability_params <- vector("list", 1)
names(true_ability_params) <- c("theta")

est_param_means <- vector("list", length(param_means_names))
names(est_param_means) <- param_means_names

est_ability_means <- vector("list", length(ability_means_names))
names(est_ability_means) <- ability_means_names

for(i in 1:length(conditions)){
  correlations_conditions[[i]] <- correlation_get(conditions[i], correlation_files, work_dir)
  

  for(j in 1:length(est_param_means)){
   est_param_means[[j]][[i]] <- est_param_means_get(conditions[i], est_param_mean_files, 
                                                    names(est_param_means[j]), work_dir)
  }
  
  for(j in 1:length(true_item_params)){
    true_item_params[[j]][[i]] <- true_param_get(conditions[i], true_param_files, 
                                                 param_type = "true_item_params",
                                                 param_name = names(true_item_params[j]), work_dir)
  }
  
  for(j in 1:length(true_ability_params)){
    true_ability_params[[j]][[i]] <- true_param_get(conditions[i], true_param_files, 
                                                 param_type = "true_ability",
                                                 param_name = names(true_ability_params[j]), work_dir)
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


for(i in 1:length(true_item_params)){
  param_vec_length <- nrow(true_item_params[[i]][[1]])
  conditions_vec <- NULL
  for(j in 1:length(conditions)){
    conditions_vec <- c(conditions_vec, rep(conditions[j], param_vec_length))
  }
  true_item_params[[i]] <- matrix(data = c(unlist(true_item_params[[i]]), conditions_vec), ncol = 2)
}

true_ability_params <- unlist(true_ability_params, recursive = FALSE)
for(i in 1:length(true_ability_params)){
  param_vec_length <- nrow(true_ability_params[[i]])
  conditions_vec <- NULL
  conditions_vec <- rep(conditions[i], param_vec_length)
  true_ability_params[[i]] <- cbind(true_ability_params[[i]], conditions_vec)
}

true_ability_params <- do.call(rbind, true_ability_params)

dif_params <- as.data.frame(as.numeric(true_item_params$dif_param[,1]))
names(dif_params) <- "D_param"

ggplot(data = dif_params, aes(x = D_param)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "D-parameter value", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Distribution of D-parameters")

ggsave("./analysis/d-param_distribution.png", width = 8, height = 8)


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

means_recovery$mu <- grep("mu", unlist(strsplit(means_recovery$condition, "_")), value = TRUE)
means_recovery$mu <- gsub("mu", "", means_recovery$mu)
means_recovery$mu <- gsub("-", ".", means_recovery$mu)

means_recovery$alpha <- grep("alpha", unlist(strsplit(means_recovery$condition, "_")), value = TRUE)
means_recovery$alpha <- gsub("alpha", "", means_recovery$alpha)
means_recovery$alpha <- gsub("-", ".", means_recovery$alpha)

means_recovery <- means_recovery[, c("rho", "PREF", "mu", "alpha", "a_corr", "b_corr", "D_corr", 
                         "theta_corr", "foc_mean_diff", "ref_mean_diff", "R2_diff")]

write.csv(means_recovery, "./analysis/means_recovery.csv")

# 
# #### GRAPHS ####
# colors <- c(rep("darkblue", 6), rep("darkred", 6), rep("darkgreen", 6), 
#             rep("darkorange", 6), rep("darkorchid", 6), rep("darkseagreen", 6))
# source("../multiplot_fun.R")
# 
# ### BIAS HISTOGRAMS####
# R2_histos <- vector("list", length(conditions))
# focmean_histos <- vector("list", length(conditions))
# refmean_histos <- vector("list", length(conditions))
# 
# #mean correlations
# for(i in 1:length(conditions)){
#   data <- as.data.frame(correlations_conditions[[paste0(conditions[i])]])
#   rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   rho <- gsub("rho", "", rho)
#   rho <- sprintf("%.1f", as.numeric(gsub("-", ".", rho)))
#   
#   PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   PREF <- gsub("PREF", "", PREF)
#   PREF <- sprintf("%.1f", as.numeric(gsub("-", ".", PREF)))
#   
#   mu <- grep("mu", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   mu <- gsub("mu", "", mu)
#   mu <- sprintf("%.1f", as.numeric(gsub("-", ".", mu)))
#   
#   alpha <- grep("alpha", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   alpha <- gsub("alpha", "", alpha)
#   alpha <- sprintf("%.2f", as.numeric(gsub("-", ".", alpha)))
#   
#   #R2 difference
#   xscale <- scale_def(correlations_conditions, "R2_diff")
#   increment <- (xscale*2)/25
#   
#   R2_histos[[i]] <- ggplot(data, aes(x = R2_diff)) + 
#     geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
#     scale_x_continuous(limits = c(-xscale, xscale)) +
#     labs(x = "Difference in R-squared", y = "Count",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha)) + 
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) + 
#     geom_vline(xintercept = mean(data$R2_diff), color = "black", linetype="dotted")
#   
#   #focal mean difference
#   xscale <- scale_def(correlations_conditions, "foc_mean_diff")
#   increment <- (xscale*2)/25
#   
#   focmean_histos[[i]] <- ggplot(data, aes(x = foc_mean_diff)) + 
#     geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
#     scale_x_continuous(limits = c(-xscale, xscale)) +
#     labs(x = "Difference in focal group mean", y = "Count",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha)) +  
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#     geom_vline(xintercept = mean(data$foc_mean_diff), color = "black", linetype="dotted")
#   
#   #reference mean difference
#   xscale <- scale_def(correlations_conditions, "ref_mean_diff")
#   increment <- (xscale*2)/25
#   
#   refmean_histos[[i]] <- ggplot(data, aes(x = ref_mean_diff)) + 
#     geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
#     scale_x_continuous(limits = c(-xscale, xscale)) + 
#     labs(x = "Difference in reference group mean", y = "Count",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha)) +  
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#     geom_vline(xintercept = mean(data$ref_mean_diff), color = "black", linetype="dotted")
# }
# 
# pdf("./analysis/R2_bias_histograms.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(R2_histos[[i]], R2_histos[[i+1]], R2_histos[[i+2]], 
#             R2_histos[[i+3]], R2_histos[[i+4]], R2_histos[[i+5]], cols = 3, 
#             plot_title = "R-squared recovery bias")
# }
# dev.off()
# 
# pdf("./analysis/focmean_bias_histograms.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(focmean_histos[[i]], focmean_histos[[i+1]], focmean_histos[[i+2]], 
#             focmean_histos[[i+3]], focmean_histos[[i+4]], focmean_histos[[i+5]], cols = 3, 
#             plot_title = "Focal mean recovery bias")
# }
# dev.off()
# 
# 
# pdf("./analysis/refmean_bias_histograms.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(refmean_histos[[i]], refmean_histos[[i+1]], refmean_histos[[i+2]], 
#             refmean_histos[[i+3]], refmean_histos[[i+4]], refmean_histos[[i+5]], cols = 3, 
#             plot_title = "Reference mean recovery bias")
# }
# dev.off()
# 
# rm(R2_histos, focmean_histos, refmean_histos)
# 
# 
# ### CORRELATION HISTOGRAMS ####
# #mean correlations
# a_corr_histo <- vector("list", length(conditions))
# b_corr_histo <- vector("list", length(conditions))
# D_corr_histo <- vector("list", length(conditions))
# theta_corr_histo <- vector("list", length(conditions))
# 
# for(i in 1:length(conditions)){
#   data <- as.data.frame(correlations_conditions[[paste0(conditions[i])]])
#   rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   rho <- gsub("rho", "", rho)
#   rho <- sprintf("%.1f", as.numeric(gsub("-", ".", rho)))
#   
#   PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   PREF <- gsub("PREF", "", PREF)
#   PREF <- sprintf("%.1f", as.numeric(gsub("-", ".", PREF)))
#   
#   mu <- grep("mu", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   mu <- gsub("mu", "", mu)
#   mu <- sprintf("%.1f", as.numeric(gsub("-", ".", mu)))
#   
#   alpha <- grep("alpha", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   alpha <- gsub("alpha", "", alpha)
#   alpha <- sprintf("%.2f", as.numeric(gsub("-", ".", alpha)))
#   
#   #a_corr
#   xscale <- scale_def_corr(correlations_conditions, "a_corr")
#   increment <- (diff(xscale))/25
#   
#   a_corr_histo[[i]] <- ggplot(data, aes(x = a_corr)) + 
#     geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
#     scale_x_continuous(limits = xscale) +
#     labs(x = "A-parameter correlation", y = "Count",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha)) +  
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#     geom_vline(xintercept = mean(data$a_corr), color = "black", linetype="dotted")
#   
#   #b_corr
#   xscale <- scale_def_corr(correlations_conditions, "b_corr")
#   increment <- (diff(xscale))/25
#   
#   b_corr_histo[[i]] <- ggplot(data, aes(x = b_corr)) + 
#     geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
#     scale_x_continuous(limits = xscale) +
#     labs(x = "B-parameter correlation", y = "Count",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha)) +  
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#     geom_vline(xintercept = mean(data$b_corr), color = "black", linetype="dotted")
#   
#   #D_corr
#   xscale <- scale_def_corr(correlations_conditions, "D_corr")
#   increment <- (diff(xscale))/25
#   
#   D_corr_histo[[i]] <- ggplot(data, aes(x = D_corr)) + 
#     geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
#     scale_x_continuous(limits = xscale) +
#     labs(x = "D-parameter correlation", y = "Count",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha)) +  
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#     geom_vline(xintercept = mean(data$D_corr), color = "black", linetype="dotted")
#   
#   #theta_corr
#   xscale <- scale_def_corr(correlations_conditions, "theta_corr")
#   increment <- (diff(xscale))/25
#   
#   theta_corr_histo[[i]] <- ggplot(data, aes(x = theta_corr)) + 
#     geom_histogram(binwidth = increment, alpha = 0.65, fill = colors[i]) + 
#     scale_x_continuous(limits = xscale) +
#     labs(x = "Theta correlation", y = "Count",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha)) +  
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#     geom_vline(xintercept = mean(data$theta_corr), color = "black", linetype="dotted")
#   
# }
# 
# pdf("./analysis/a_corr_histograms.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
# multiplot(a_corr_histo[[i]], a_corr_histo[[i+1]], a_corr_histo[[i+2]], 
#             a_corr_histo[[i+3]], a_corr_histo[[i+4]], a_corr_histo[[i+5]], cols = 3, 
#             plot_title = "a-parameter mean recovery bias")
# }
# dev.off()
# 
# pdf("./analysis/b_corr_histograms.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(b_corr_histo[[i]], b_corr_histo[[i+1]], b_corr_histo[[i+2]], 
#             b_corr_histo[[i+3]], b_corr_histo[[i+4]], b_corr_histo[[i+5]], cols = 3, 
#             plot_title = "b-parameter mean recovery bias")
# }
# dev.off()
# 
# pdf("./analysis/D_corr_histograms.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(D_corr_histo[[i]], D_corr_histo[[i+1]], D_corr_histo[[i+2]], 
#             D_corr_histo[[i+3]], D_corr_histo[[i+4]], D_corr_histo[[i+5]], cols = 3, 
#             plot_title = "D-parameter mean recovery bias")
# }
# dev.off()
# 
# pdf("./analysis/theta_corr_histograms.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(theta_corr_histo[[i]], theta_corr_histo[[i+1]], theta_corr_histo[[i+2]], 
#             theta_corr_histo[[i+3]], theta_corr_histo[[i+4]], theta_corr_histo[[i+5]], cols = 3, 
#             plot_title = "theta-parameter mean recovery bias")
# }
# dev.off()
# 
# rm(a_corr_histo, b_corr_histo, D_corr_histo, theta_corr_histo)
# 
# ### CORRELATION SCATTERPLOTS ####
# #mean correlations
# a_corr_scatter <- vector("list", length(conditions))
# b_corr_scatter <- vector("list", length(conditions))
# D_corr_scatter <- vector("list", length(conditions))
# theta_corr_scatter <- vector("list", length(conditions))
# 
# for(i in 1:length(conditions)){
#   rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   rho <- gsub("rho", "", rho)
#   rho <- sprintf("%.1f", as.numeric(gsub("-", ".", rho)))
#   
#   PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   PREF <- gsub("PREF", "", PREF)
#   PREF <- sprintf("%.1f", as.numeric(gsub("-", ".", PREF)))
#   
#   mu <- grep("mu", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   mu <- gsub("mu", "", mu)
#   mu <- sprintf("%.1f", as.numeric(gsub("-", ".", mu)))
#   
#   alpha <- grep("alpha", unlist(strsplit(conditions[i], "_")), value = TRUE)
#   alpha <- gsub("alpha", "", alpha)
#   alpha <- sprintf("%.2f", as.numeric(gsub("-", ".", alpha)))
#   
#   #a_corr
#   data <- as.data.frame(est_param_means[["a_params"]])
#   data <- filter(data, V2 == conditions[i])
#   data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
#   data <- as.numeric(data[,1])
#   true_param <- as.data.frame(true_item_params[["a_param"]])
#   true_param <- filter(true_param, V2 == conditions[i])
#   true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
#   true_param <- as.numeric(true_param[,1])
#   data <- as.data.frame(cbind(data, true_param))
#   names(data) <- c("est_param", "true_param")
#   
#   a_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
#     geom_point(alpha = 0.65, color = colors[i]) + 
#     labs(x = "True Parameter Value", y = "Estimated Parameter Value",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha),
#          caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
#     scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5)) +
#     scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5)) + 
#     theme(plot.title = element_text(hjust = 0.5, size = 12))
#   
#   #b_corr
#   data <- as.data.frame(est_param_means[["b_params"]])
#   data <- filter(data, V2 == conditions[i])
#   data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
#   data <- as.numeric(data[,1])
#   true_param <- as.data.frame(true_item_params[["b_param"]])
#   true_param <- filter(true_param, V2 == conditions[i])
#   true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
#   true_param <- as.numeric(true_param[,1])
#   data <- as.data.frame(cbind(data, true_param))
#   names(data) <- c("est_param", "true_param")
#   
#   
#   b_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
#     geom_point(alpha = 0.65, color = colors[i]) + 
#     labs(x = "True Parameter Value", y = "Estimated Parameter Value",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha),
#          caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
#     scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
#     scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) 
#   
#   #D_corr
#   data <- as.data.frame(est_param_means[["D_params"]])
#   data <- filter(data, V2 == conditions[i])
#   data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
#   data <- as.numeric(data[,1])
#   true_param <- as.data.frame(true_item_params[["dif_param"]])
#   true_param <- filter(true_param, V2 == conditions[i])
#   true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
#   true_param <- as.numeric(true_param[,1])
#   data <- as.data.frame(cbind(data, true_param))
#   names(data) <- c("est_param", "true_param")
#   
#   D_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
#     geom_point(alpha = 0.65, color = colors[i]) + 
#     labs(x = "True Parameter Value", y = "Estimated Parameter Value",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha),
#          caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
#     scale_x_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 0.5)) +
#     scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 0.5)) +
#     theme(plot.title = element_text(hjust = 0.5, size = 12)) 
#   
#   #theta_corr
#   data <- as.data.frame(est_param_means[["theta"]])
#   data <- filter(data, V2 == conditions[i])
#   data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
#   data <- as.numeric(data[,1])
#   true_param <- as.data.frame(true_ability_params[which(true_ability_params$conditions_vec == conditions[i]), 
#                                                   "true_ability.theta"])
#   true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
#   true_param <- as.numeric(true_param[,1])
#   data <- as.data.frame(cbind(data, true_param))
#   names(data) <- c("est_param", "true_param")
#   
#   theta_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
#     geom_point(alpha = 0.65, color = colors[i]) + 
#     labs(x = "True Parameter Value", y = "Estimated Parameter Value",
#          title = paste0("rho = ", rho, ", reference proportion = ", PREF, 
#                         "\nmu = ", mu, ", alpha = ", alpha),
#          caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
#     scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
#     scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
#     theme(plot.title = element_text(hjust = 0.5, size = 12))
#   
#   
# }
# 
# 
# pdf("./analysis/a_corr_scatterplots.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(a_corr_scatter[[i]], a_corr_scatter[[i+1]], a_corr_scatter[[i+2]], 
#             a_corr_scatter[[i+3]], a_corr_scatter[[i+4]], a_corr_scatter[[i+5]], cols = 3, 
#             plot_title = "a-parameter scatterplots")
# }
# dev.off()
# 
# pdf("./analysis/b_corr_scatterplots.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(b_corr_scatter[[i]], b_corr_scatter[[i+1]], b_corr_scatter[[i+2]], 
#             b_corr_scatter[[i+3]], b_corr_scatter[[i+4]], b_corr_scatter[[i+5]], cols = 3, 
#             plot_title = "b-parameter mean recovery bias")
# }
# dev.off()
# 
# pdf("./analysis/D_corr_scatterplots.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(D_corr_scatter[[i]], D_corr_scatter[[i+1]], D_corr_scatter[[i+2]], 
#             D_corr_scatter[[i+3]], D_corr_scatter[[i+4]], D_corr_scatter[[i+5]], cols = 3, 
#             plot_title = "D-parameter mean recovery bias")
# }
# dev.off()
# 
# pdf("./analysis/theta_corr_scatterplots.pdf", width = 10, height = 10)
# for(i in seq(1, 36, 6)){
#   multiplot(theta_corr_scatter[[i]], theta_corr_scatter[[i+1]], theta_corr_scatter[[i+2]], 
#             theta_corr_scatter[[i+3]], theta_corr_scatter[[i+4]], theta_corr_scatter[[i+5]], cols = 3, 
#             plot_title = "theta-parameter mean recovery bias")
# }
# dev.off()
# 
# rm(a_corr_scatter, b_corr_scatter, D_corr_scatter, theta_corr_scatter)
# gc()


### DECISION CONSISTENCY SCATTERPLOTS ####
#mean correlations
flag_thresholds <- c(.5, .75, 1)

for(j in 1:length(flag_thresholds)){
  flag_amt <- flag_thresholds[j]
  D_decision_scatter <- vector("list", length(conditions))
  dec_consist_out <- createWorkbook()
  
  for(i in 1:length(conditions)){
    rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
    rho <- gsub("rho", "", rho)
    rho <- gsub("-", ".", rho)
    
    PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
    PREF <- gsub("PREF", "", PREF)
    PREF <- gsub("-", ".", PREF)
    
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
    
    
    trueneg <- nrow(data[which(data$true_param < flag_amt & data$est_param < flag_amt),])
    falsepos <- nrow(data[which(data$true_param < flag_amt & data$est_param > flag_amt),])
    falseneg <- nrow(data[which(data$true_param > flag_amt & data$est_param < flag_amt),])
    truepos <- nrow(data[which(data$true_param > flag_amt & data$est_param > flag_amt),])
    
    dec_consist <- matrix(c(truepos, falsepos, falseneg, trueneg), nrow = 2, byrow = TRUE)
    colnames(dec_consist) <- c("True_DIF", "True_NoDIF")
    rownames(dec_consist) <- c("Est_DIF", "Est_NoDIF")
    
    addWorksheet(dec_consist_out, conditions[[i]])
    writeData(dec_consist_out, sheet = i, dec_consist, rowNames = TRUE)
    
    correct_ratio <- round((trueneg+truepos)/(trueneg+truepos+falsepos+falseneg), 3)
    
    D_decision_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
      geom_point(alpha = 0.65, color = colors[i]) + 
      geom_rect(data = data.frame(xmin = flag_amt, xmax = Inf, ymin = -Inf, ymax = flag_amt), 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "red", alpha = 0.2, show.legend = FALSE, inherit.aes = FALSE) +
      geom_rect(data = data.frame(xmin = -Inf, xmax = flag_amt, ymin = flag_amt, ymax = Inf), 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "red", alpha = 0.2, show.legend = FALSE, inherit.aes = FALSE) +
      ggtitle(paste0("DIF flag decision consistency for\nrho = ", rho, ", reference proportion = ", PREF)) + 
      labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
           caption = paste0("Correct ratio = ", correct_ratio)) + 
      scale_x_continuous(breaks = seq(-100, 100, .2)) +
      scale_y_continuous(breaks = seq(-100, 100, .2)) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_hline(yintercept = flag_amt) + 
      geom_vline(xintercept = flag_amt)

  }
  pdf(paste0(flag_amt, "_threshold_mean_decision_scatterplots.pdf"), width = 10, height = 10)
  multiplot(D_decision_scatter[[1]], D_decision_scatter[[2]], D_decision_scatter[[3]], D_decision_scatter[[4]], cols = 2)
  dev.off()
  
  saveWorkbook(dec_consist_out, paste0(flag_amt, "_decision_consistency.xlsx"), overwrite = TRUE)
}



