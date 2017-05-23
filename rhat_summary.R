setwd("F:/Comps simulation results 20170302/All_simulation_results_20170404/combined")

files <- grep("summary", list.files(getwd()), value = TRUE)


library(tidyr)
library(dplyr)
library(openxlsx)
library(ggplot2)

source("C:/Users/Jen/Dropbox/REMS/11 Comps/Simulation/functions.R")
source("C:/Users/Jen/Dropbox/REMS/11 Comps/Simulation/multiplot_fun.R")

colors <- c("darkblue", "darkred", "darkgreen", "darkorange")

Rhat_max_histo <- vector("list", length(conditions))
Rhat_mean_histo <- vector("list", length(conditions))

for(file in 1:length(files)){
  info <- unlist(strsplit(as.character(files[[file]]), "_"))
  len <- length(info)
  condition <- paste0(c(info[(len-1):len]), collapse = "", sep = "_")
  condition <- gsub("\\.rds\\_", "", condition)
  type <- paste0(c(info[1:(len-3)]), collapse = "", sep = "_")
  type <- gsub("\\_$", "", type)
  
  rho <- grep("rho", unlist(strsplit(condition, "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- gsub("-", ".", rho)
  
  PREF <- grep("PREF", unlist(strsplit(condition, "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- gsub("-", ".", PREF)
  
  data <- readRDS(files[[file]])
  
  Rhat_values <- data.frame(matrix(c(rep(NA, length(data)), rep(NA, length(data))), ncol = 2))
  names(Rhat_values) <- c("Rhat_max", "Rhat_mean")
  
  for(j in 1:length(data)){
    params_summary <- as.data.frame(data[[j]])
    Rhat_values[j, "Rhat_max"] <- max(params_summary[, "Rhat"])
    Rhat_values[j, "Rhat_mean"] <- mean(params_summary[, "Rhat"])
  }
  
    Rhat_max_histo[[file]] <- ggplot(Rhat_values, aes(x = Rhat_max)) + 
      geom_histogram(binwidth = 0.02, alpha = 0.65, fill = colors[file]) + 
      scale_x_continuous(limits = c(0.5, 2)) +
      ggtitle(paste0("R-hat max values for\nrho = ", rho, ", reference proportion = ", PREF)) + 
      labs(x = "R-hat  values", y = "Count") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_vline(xintercept = mean(Rhat_values$Rhat_max), color = "black", linetype="dotted")
    
    Rhat_mean_histo[[file]] <- ggplot(Rhat_values, aes(x = Rhat_mean)) + 
      geom_histogram(binwidth = 0.02, alpha = 0.65, fill = colors[file]) + 
      scale_x_continuous(limits = c(0.5, 2)) +
      ggtitle(paste0("R-hat mean values for\nrho = ", rho, ", reference proportion = ", PREF)) + 
      labs(x = "R-hat  values", y = "Count") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_vline(xintercept = mean(Rhat_values$Rhat_mean), color = "black", linetype="dotted")
    
  print(paste0(condition, " number of mean R-hat values < 1.2 ", nrow(Rhat_values[which(Rhat_values$Rhat_mean <= 1.2), ])/nrow(Rhat_values)))
  
  print(paste0(condition, " number of max R-hat values < 1.2 ", nrow(Rhat_values[which(Rhat_values$Rhat_max <= 1.2), ])/nrow(Rhat_values)))
}

pdf("C:/Users/Jen/Dropbox/REMS/11 Comps/Simulation/all_simulation-results/Rhat_histograms.pdf", width = 10, height = 10)
multiplot(Rhat_max_histo[[1]], Rhat_max_histo[[2]], Rhat_max_histo[[3]], Rhat_max_histo[[4]], cols = 2)
multiplot(Rhat_mean_histo[[1]], Rhat_mean_histo[[2]], Rhat_mean_histo[[3]], Rhat_mean_histo[[4]], cols = 2)
dev.off()
