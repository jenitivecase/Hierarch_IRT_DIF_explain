#### SETUP ####
file_loc <- "E:/Dissertation Simulation results/combined"
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  setwd(gsub("jbrussow", "Jen", work_dir))
} else if (Sys.info()["user"] == "jennifer.brussow"){
  setwd(gsub("jbrussow", "jennifer.brussow", work_dir))
}

library(tidyr)
library(dplyr)
library(openxlsx)
library(ggplot2)

source("functions.R")

files <- list.files(file_loc)
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

params_summary_names <- readRDS("params_summary_names.rds")
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
  correlations_conditions[[i]] <- correlation_get(conditions[i], correlation_files, file_loc)
  

  for(j in 1:length(est_param_means)){
   est_param_means[[j]][[i]] <- est_param_means_get(conditions[i], est_param_mean_files, 
                                                    names(est_param_means[j]), file_loc)
  }
  
  for(j in 1:length(true_item_params)){
    true_item_params[[j]][[i]] <- true_param_get(conditions[i], true_param_files, 
                                                 param_type = "true_item_params",
                                                 param_name = names(true_item_params[j]), file_loc)
  }
  
  for(j in 1:length(true_ability_params)){
    true_ability_params[[j]][[i]] <- true_param_get(conditions[i], true_param_files, 
                                                 param_type = "true_ability",
                                                 param_name = names(true_ability_params[j]), file_loc)
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
  geom_histogram(binwidth = 0.01) +
  labs(x = "D-parameter value", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Distribution of D-parameters")


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

write.csv(means_recovery, "means_recovery.csv")


#### GRAPHS ####
colors <- c("darkblue", "darkred", "darkgreen", "darkorange")
source(paste0(getwd(), "/../multiplot_fun.R"))

### BIAS HISTOGRAMS####
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
  
  #theta_corr
  data <- as.data.frame(est_param_means[["theta"]])
  data <- filter(data, V2 == conditions[i])
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  data <- as.numeric(data[,1])
  true_param <- as.data.frame(true_ability_params[which(true_ability_params$conditions_vec == conditions[i]), 
                                                  "true_ability.theta"])
  true_param <- as.data.frame(lapply(true_param, as.character), stringsAsFactors=FALSE)
  true_param <- as.numeric(true_param[,1])
  data <- as.data.frame(cbind(data, true_param))
  names(data) <- c("est_param", "true_param")
  
  theta_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) + 
    geom_point(alpha = 0.65, color = colors[i]) + 
    ggtitle(paste0("Theta-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
    labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  
}

pdf("mean_corr_scatterplots.pdf", width = 10, height = 10)
multiplot(a_corr_scatter[[1]], a_corr_scatter[[2]], a_corr_scatter[[3]], a_corr_scatter[[4]], cols = 2)

multiplot(b_corr_scatter[[1]], b_corr_scatter[[2]], b_corr_scatter[[3]], b_corr_scatter[[4]], cols = 2)

multiplot(D_corr_scatter[[1]], D_corr_scatter[[2]], D_corr_scatter[[3]], D_corr_scatter[[4]], cols = 2)

multiplot(theta_corr_scatter[[1]], theta_corr_scatter[[2]], theta_corr_scatter[[3]], theta_corr_scatter[[4]], cols = 2)

dev.off()


### DECISION CONSISTENCY SCATTERPLOTS ####
#mean correlations
flag_thresholds <- c(.3, .4, .5, .6)

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



