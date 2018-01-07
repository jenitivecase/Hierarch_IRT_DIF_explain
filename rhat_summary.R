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

files <- list.files(work_dir, pattern = ".rds")
files <- as.data.frame(files)
names(files) <- "filename"
files$filename <- as.character(files$filename)
files <- files[which(apply(X = files, MARGIN = 1, FUN = function(x){length(grep("_", unlist(strsplit(x, split = ""))))}) > 3), , drop = FALSE]


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

files <- est_param_files
Rhat_max_histo <- vector("list", length(conditions))
Rhat_mean_histo <- vector("list", length(conditions))

rhat_props <- data.frame(matrix(NA, nrow = length(conditions), ncol = 6))
names(rhat_props) <- c("rho", "PREF", "mu", "alpha", "max_rhat_prop", "mean_rhat_prop")

for(file in 1:length(files)){
  rho <- grep("rho", unlist(strsplit(conditions[file], "_")), value = TRUE)
  rho <- gsub("rho", "", rho)
  rho <- sprintf("%.1f", as.numeric(gsub("-", ".", rho)))
  
  PREF <- grep("PREF", unlist(strsplit(conditions[file], "_")), value = TRUE)
  PREF <- gsub("PREF", "", PREF)
  PREF <- sprintf("%.1f", as.numeric(gsub("-", ".", PREF)))
  
  mu <- grep("mu", unlist(strsplit(conditions[file], "_")), value = TRUE)
  mu <- gsub("mu", "", mu)
  mu <- sprintf("%.1f", as.numeric(gsub("-", ".", mu)))
  
  alpha <- grep("alpha", unlist(strsplit(conditions[file], "_")), value = TRUE)
  alpha <- gsub("alpha", "", alpha)
  alpha <- sprintf("%.2f", as.numeric(gsub("-", ".", alpha)))
  
  data <- readRDS(files[[file]])
  
  Rhat_values <- data.frame(matrix(c(rep(NA, (length(data)-1)), rep(NA, (length(data)-1))), ncol = 2))
  names(Rhat_values) <- c("Rhat_max", "Rhat_mean")
  
  #changed to start at 2 due to file weirdness
  for(j in 2:length(data)){
    params_summary <- as.data.frame(data[[j]])
    Rhat_values[(j-1), "Rhat_max"] <- max(params_summary[, "Rhat"])
    Rhat_values[(j-1), "Rhat_mean"] <- mean(params_summary[, "Rhat"])
  }
  
  Rhat_max_histo[[file]] <- ggplot(Rhat_values, aes(x = Rhat_max)) + 
    geom_histogram(binwidth = 0.02, alpha = 0.65, fill = colors[file]) + 
    scale_x_continuous(limits = c(0.5, 2)) + 
    labs(x = "R-hat values", y = "Count",
       title = paste0("rho = ", rho, ", reference proportion = ", PREF,
                        "\nmu = ", mu, ", alpha = ", alpha)) + 
    theme(plot.title = element_text(hjust = 0.5, size = 12)) + 
    geom_vline(xintercept = mean(Rhat_values$Rhat_max), color = "black", linetype="dotted")+
    geom_vline(xintercept = 1.2, color = "black")
  
  Rhat_mean_histo[[file]] <- ggplot(Rhat_values, aes(x = Rhat_mean)) + 
    geom_histogram(binwidth = 0.02, alpha = 0.65, fill = colors[file]) + 
    scale_x_continuous(limits = c(0.5, 2)) +
    labs(x = "R-hat values", y = "Count",
         title = paste0("rho = ", rho, ", reference proportion = ", PREF,
                        "\nmu = ", mu, ", alpha = ", alpha)) + 
    theme(plot.title = element_text(hjust = 0.5, size = 12)) + 
    geom_vline(xintercept = mean(Rhat_values$Rhat_mean), color = "black", linetype="dotted") +
    geom_vline(xintercept = 1.2, color = "black")
  
  rhat_props[file, c("rho", "PREF", "mu", "alpha")] <- c(rho, PREF, mu, alpha)
  rhat_props[file, "max_rhat_prop"] <- nrow(Rhat_values[which(Rhat_values$Rhat_max <= 1.2), ])/nrow(Rhat_values)
  rhat_props[file, "mean_rhat_prop"] <- nrow(Rhat_values[which(Rhat_values$Rhat_mean <= 1.2), ])/nrow(Rhat_values)
  
  print(paste0(condition, " number of mean R-hat values < 1.2 ", nrow(Rhat_values[which(Rhat_values$Rhat_mean <= 1.2), ])/nrow(Rhat_values)))
  
  print(paste0(condition, " number of max R-hat values < 1.2 ", nrow(Rhat_values[which(Rhat_values$Rhat_max <= 1.2), ])/nrow(Rhat_values)))
}

write.xlsx(rhat_props, "./analysis/Rhat_proportions.xlsx")

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
