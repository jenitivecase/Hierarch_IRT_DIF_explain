work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/combined_dissertation_results"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  work_dir <- gsub("jbrussow", "Jen", work_dir)
  setwd(work_dir)
} else if(Sys.info()["user"] %in% c("Jennifer.Brussow", "jennifer.brussow")){
  work_dir <- gsub("jbrussow", "jennifer.brussow", work_dir)
  setwd(work_dir)
}

if(!dir.exists("analysis")) dir.create("analysis")


files <- grep("summary", list.files(getwd()), value = TRUE)

library(tidyr)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(rlang)

source("./../functions.R")
source("./../multiplot_fun.R")

colors <- c(rep("darkblue", 6), rep("darkred", 6), rep("darkgreen", 6),
            rep("darkorange", 6), rep("darkorchid", 6), rep("darkseagreen", 6))

Rhat_max_histo <- vector("list", length(conditions))
Rhat_mean_histo <- vector("list", length(conditions))

for(file in 1:length(files)){
  info <- unlist(strsplit(as.character(files[[file]]), "_"))
  len <- length(info)
  condition <- paste0(c(info[(len-1):len]), collapse = "", sep = "_")
  condition <- gsub("\\.rds\\_", "", condition)
  type <- paste0(c(info[1:(len-3)]), collapse = "", sep = "_")
  type <- gsub("\\_$", "", type)
  
  rho <- grep("rho", unlist(strsplit(conditions[i], "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- sprintf("%.1f", as.numeric(gsub("-", ".", rho)))
  
  PREF <- grep("PREF", unlist(strsplit(conditions[i], "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- sprintf("%.1f", as.numeric(gsub("-", ".", PREF)))
  
  mu <- grep("mu", unlist(strsplit(conditions[i], "_")), value = TRUE)
  mu <- gsub("mu", "", mu)
  mu <- sprintf("%.1f", as.numeric(gsub("-", ".", mu)))
  
  alpha <- grep("alpha", unlist(strsplit(conditions[i], "_")), value = TRUE)
  alpha <- gsub("alpha", "", alpha)
  alpha <- sprintf("%.2f", as.numeric(gsub("-", ".", alpha)))
  
  data <- readRDS(files[[file]])
  
  Rhat_values <- data.frame(matrix(c(rep(NA, length(data)), rep(NA, length(data))), ncol = 2))
  names(Rhat_values) <- c("Rhat_max", "Rhat_mean")
  
  #changed to start at 2 due to file weirdness
  for(j in 2:length(data)){
    params_summary <- as.data.frame(data[[j]])
    Rhat_values[j, "Rhat_max"] <- max(params_summary[, "Rhat"])
    Rhat_values[j, "Rhat_mean"] <- mean(params_summary[, "Rhat"])
  }
  
    Rhat_max_histo[[file]] <- ggplot(Rhat_values, aes(x = Rhat_max)) + 
      geom_histogram(binwidth = 0.02, alpha = 0.65, fill = colors[file]) + 
      scale_x_continuous(limits = c(0.5, 2)) + 
      labs(x = "R-hat  values", y = "Count",
           title = paste0("rho = ", UQ(rho), ", reference proportion = ", UQ(PREF),
                          "\nmu = ", UQ(mu), ", alpha = ", UQ(alpha))) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_vline(xintercept = mean(Rhat_values$Rhat_max), color = "black", linetype="dotted")
    
    Rhat_mean_histo[[file]] <- ggplot(Rhat_values, aes(x = Rhat_mean)) + 
      geom_histogram(binwidth = 0.02, alpha = 0.65, fill = colors[file]) + 
      scale_x_continuous(limits = c(0.5, 2)) +
      labs(x = "R-hat  values", y = "Count",
           title = paste0("rho = ", UQ(rho), ", reference proportion = ", UQ(PREF),
                          "\nmu = ", UQ(mu), ", alpha = ", UQ(alpha))) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_vline(xintercept = mean(Rhat_values$Rhat_mean), color = "black", linetype="dotted")
    
  print(paste0(condition, " number of mean R-hat values < 1.2 ", nrow(Rhat_values[which(Rhat_values$Rhat_mean <= 1.2), ])/nrow(Rhat_values)))
  
  print(paste0(condition, " number of max R-hat values < 1.2 ", nrow(Rhat_values[which(Rhat_values$Rhat_max <= 1.2), ])/nrow(Rhat_values)))
}

pdf("./analysis/Rhat_max_histograms.pdf", width = 10, height = 10)
for(i in seq(1, 36, 6)){
  multiplot(Rhat_max_histo[[i]], Rhat_max_histo[[i+1]], Rhat_max_histo[[i+2]],
            Rhat_max_histo[[i+3]], Rhat_max_histo[[i+4]], Rhat_max_histo[[i+5]], cols = 3,
            plot_title = "Histogram of Maximum R-hat values")
}
dev.off()

pdf("./analysis/Rhat_mean_histograms.pdf", width = 10, height = 10)
for(i in seq(1, 36, 6)){
  multiplot(Rhat_mean_histo[[i]], Rhat_mean_histo[[i+1]], Rhat_mean_histo[[i+2]],
            Rhat_mean_histo[[i+3]], Rhat_mean_histo[[i+4]], Rhat_mean_histo[[i+5]], cols = 3,
            plot_title = "Histogram of meanimum R-hat values")
}
dev.off()
