#### SETUP ####
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

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

library(tidyr)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(rlang)

source("./../functions.R")

# files <- list.files(work_dir, pattern = ".rds")
# files <- as.data.frame(files)
# names(files) <- "filename"
# files$filename <- as.character(files$filename)
# files <- files[which(apply(X = files, MARGIN = 1, FUN = function(x){length(grep("_", unlist(strsplit(x, split = ""))))}) > 3), , drop = FALSE]
# 
# 
# for(i in 1:nrow(files)){
#   filename_temp <- as.character(files[i, "filename"])
#   filename_temp <- gsub("^[^\\/]*\\/", "", filename_temp)
#   info <- unlist(strsplit(filename_temp, "_"))
#   len <- length(info)
#   condition <- paste0(c(info[(len-3):(len)]), collapse = "_")
#   condition <- gsub("\\.rds", "", condition)
#   type <- "temp"
#   if(grepl("^CIs_analysis", filename_temp)){
#     type <- "CIs_analysis"
#   } else if(grepl("^CIs_proportion", filename_temp)){
#     type <- "CIs_proportion"
#   } else if(grepl("^correlation", filename_temp)){
#     type <- "correlations"
#   } else if(grepl("^est_param_means", filename_temp)){
#     type <- "est_param_means"
#   } else if(grepl("^est_param_summary", filename_temp)){
#     type <- "est_param_summary"
#   } else if(grepl("^true_params", filename_temp)){
#     type <- "true_params"
#   } else {
#     type <- NA
#   }
# 
#   files[i, "condition"] <- condition
#   files[i, "type"] <- type
# }
# 
# conditions <- unique(files$condition)
# types <- unique(files$type)
# 
# correlation_files <- files[which(grepl("correlations", files$filename)), "filename"]
# true_param_files <- files[which(grepl("true_params", files$filename)), "filename"]
# est_param_files <- files[which(grepl("est_param_summary", files$filename)), "filename"]
# est_param_mean_files <- files[which(grepl("est_param_means", files$filename)), "filename"]
# CIs_proportion_files <- files[which(grepl("CIs_proportion", files$filename)), "filename"]
# CIs_analysis_files <- files[which(grepl("CIs_analysis", files$filename)), "filename"]
# 
# params_summary_names <- readRDS("../params_summary_names.rds")
# param_means_names <- c("a_params", "b_params", "D_params", "beta0", "beta1", "mu",
#                        "sigma2", "R2", "theta", "foc_mean")
# ability_means_names <- c("theta")
# 
# #### DATA RETRIEVAL ####
# 
# #correlations
# correlations_conditions <- vector("list", length(conditions))
# names(correlations_conditions) <- conditions
# 
# #true params
# true_item_params <- vector("list", 3)
# names(true_item_params) <- c("a_param", "b_param", "dif_param")
# 
# true_ability_params <- vector("list", 1)
# names(true_ability_params) <- c("theta")
# 
# est_param_means <- vector("list", length(param_means_names))
# names(est_param_means) <- param_means_names
# 
# est_ability_means <- vector("list", length(ability_means_names))
# names(est_ability_means) <- ability_means_names
# 
# #confidence interval files
# CI_values <- vector("list", 7)
# names(CI_values) <- c("a_param_CIs", "b_param_CIs", "D_param_CIs", "theta_param_CIs",
#                       "beta0_CIs", "beta1_CIs", "R2_CIs")
# 
# #actually getting the data
# for(i in 1:length(conditions)){
#   correlations_conditions[[i]] <- correlation_get(conditions[i], correlation_files, work_dir)
#   
# 
#   for(j in 1:length(est_param_means)){
#    est_param_means[[j]][[i]] <- est_param_means_get(conditions[i], est_param_mean_files, 
#                                                     names(est_param_means[j]), work_dir)
#   }
#   
#   for(j in 1:length(true_item_params)){
#     true_item_params[[j]][[i]] <- true_param_get(conditions[i], true_param_files, 
#                                                  param_type = "true_item_params",
#                                                  param_name = names(true_item_params[j]), work_dir)
#   }
#   
#   for(j in 1:length(true_ability_params)){
#     true_ability_params[[j]][[i]] <- true_param_get(conditions[i], true_param_files, 
#                                                  param_type = "true_ability",
#                                                  param_name = names(true_ability_params[j]), work_dir)
#   }
#   
#   
#   for(j in 1:length(CI_values)){
#     CI_values[[j]][[i]] <- est_param_means_get(conditions[i], CIs_analysis_files,
#                                         param_name = names(CI_values[j]), work_dir)
#   }  
#   
#   
# }
# 
# nreps <- nrow(correlations_conditions[[1]])
# 
# #getting it into long format
# for(i in 1:length(est_param_means)){
#   param_vec_length <- nrow(est_param_means[[i]][[1]])
#   conditions_vec <- NULL
#   for(j in 1:length(conditions)){
#     conditions_vec <- c(conditions_vec, rep(conditions[j], param_vec_length))
#   }
#   est_param_means[[i]] <- matrix(data = c(unlist(est_param_means[[i]]), conditions_vec), ncol = 2)
# }
# 
# for(i in 1:length(CI_values)){
#   param_vec_length <- nrow(CI_values[[i]][[1]])
#   conditions_vec <- NULL
#   for(j in 1:length(conditions)){
#     conditions_vec <- c(conditions_vec, rep(conditions[j], param_vec_length))
#   }
#   CI_values[[i]] <- matrix(data = c(unlist(CI_values[[i]]), conditions_vec), ncol = 2)
# }
# 
# for(i in 1:length(true_item_params)){
#   param_vec_length <- nrow(true_item_params[[i]][[1]])
#   conditions_vec <- NULL
#   for(j in 1:length(conditions)){
#     conditions_vec <- c(conditions_vec, rep(conditions[j], param_vec_length))
#   }
#   true_item_params[[i]] <- matrix(data = c(unlist(true_item_params[[i]]), conditions_vec), ncol = 2)
# }
# 
# true_ability_params <- unlist(true_ability_params, recursive = FALSE)
# for(i in 1:length(true_ability_params)){
#   param_vec_length <- nrow(true_ability_params[[i]])
#   conditions_vec <- NULL
#   conditions_vec <- rep(conditions[i], param_vec_length)
#   true_ability_params[[i]] <- cbind(true_ability_params[[i]], conditions_vec)
# }
# 
# true_ability_params <- do.call(rbind, true_ability_params)
# 
# dif_params <- as.data.frame(true_item_params$dif_param)
# names(dif_params) <- c("D_param", "condition")
# dif_params$D_param <- as.numeric(as.character(dif_params$D_param))
# dif_params$condition <- as.character(dif_params$condition)
# 
# #maybe we should save this stuff so we don't have to rerun the above every time!
# saveRDS(correlations_conditions, "correlations_conditions.rds")
# saveRDS(true_item_params, "true_item_params.rds")
# saveRDS(true_ability_params, "true_ability_params.rds")
# saveRDS(est_param_means, "est_param_means.rds")
# saveRDS(dif_params, "dif_params.rds")
# saveRDS(CI_values, "CI_values.rds")
# saveRDS(conditions, "conditions.rds")
# saveRDS(types, "types.rds")


#### READING IN PROCESSED DATA ####
correlations_conditions <- readRDS("correlations_conditions.rds")
true_item_params <- readRDS("true_item_params.rds")
true_ability_params <- readRDS("true_ability_params.rds")
est_param_means <- readRDS("est_param_means.rds")
dif_params <- readRDS("dif_params.rds")
CI_values <- readRDS("CI_values.rds")
conditions <- readRDS("conditions.rds")
types <- readRDS("types.rds")

#### D-PARAMETER DISTRIBUTION CHARTS ####
pdf("./analysis/d-param_distributions.pdf")
for(i in 1:length(conditions)){
  temp <- dif_params[which(dif_params$condition == conditions[i]),]
  
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
  
  plot <- ggplot(data = temp, aes(x = D_param)) +
    #geom_histogram(binwidth = 0.05) +
    geom_density(fill = "darkgray", alpha = 0.8) +
    labs(x = "D-parameter value", y = "Density",
         subtitle = paste0("rho = ", rho, ", reference proportion = ", PREF,
                           "\nmu = ", mu, ", alpha = ", alpha),
         title = "Distribution of D-parameters") + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(limits = c(-1.0, 1.5), breaks = seq(-1, 1.5, .5))
  
  print(plot)
}
dev.off()




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

#### CONFIDENCE INTERVAL PROCESSING ####
CI_recovery <- data.frame(matrix(NA, nrow = length(conditions), ncol = length(CI_values)))

names(CI_recovery) <- names(CI_values)
for(condition in 1:length(conditions)){
  for(param in 1:length(CI_values)){
    CI_recovery[condition,param] <- mean(as.logical(CI_values[[param]]
                                                    [which(CI_values[[param]][,2] == conditions[[condition]]), 1]))
  }
}
write.csv(CI_recovery, "./analysis/CI_recovery.csv")

#### R2 DENSITY PLOTS ####
colors <- c(rep("darkblue", 6), rep("darkred", 6), rep("darkgreen", 6),
            rep("darkorange", 6), rep("darkorchid", 6), rep("darkseagreen", 6))
source("./../multiplot_fun.R")


as.data.frame(est_param_means[["R2"]]) %>%
  mutate("estR2" = as.numeric(as.character(V1))) %>%
  mutate("condition" = as.character(V2)) %>%
  mutate("simR2" = as.numeric(gsub("-", ".", gsub("rho", "", grep("rho", unlist(strsplit(condition, "_")), value = TRUE))))^2) %>%
  select(simR2, estR2, condition) %>%
  ggplot(aes(x = estR2, fill = simR2, color = simR2)) +
  facet_grid(simR2 ~ ., scales = "free", space = "free", switch = "y") +
  geom_density(alpha = 0.8) +
  geom_vline(aes(xintercept = simR2)) + 
  scale_y_continuous(limits = c(0, 4.2), breaks = c(seq(0, 4, 1))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") + 
  labs(y = bquote("Simulated "*~R^2*" value"), x = bquote("Estimated "*~R^2*" value"),
       title = bquote("" *~R^2*" Recovery"))

ggsave("./analysis/R2_recovery_density.png", width = 10, height = 6)



### CORRELATION SCATTERPLOTS ####
#mean correlations
a_corr_scatter <- vector("list", length(conditions))
b_corr_scatter <- vector("list", length(conditions))
D_corr_scatter <- vector("list", length(conditions))
theta_corr_scatter <- vector("list", length(conditions))

for(i in 1:length(conditions)){
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
    labs(x = "True Parameter Value", y = "Estimated Posterior Mean",
         title = paste0("rho = ", rho, ", reference proportion = ", PREF,
                        "\nmu = ", mu, ", alpha = ", alpha),
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
    scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5)) +
    scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    geom_abline(intercept = 0, slope = 1)

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
    labs(x = "True Parameter Value", y = "Estimated Posterior Mean",
         title = paste0("rho = ", rho, ", reference proportion = ", PREF,
                        "\nmu = ", mu, ", alpha = ", alpha),
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
    scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
    scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    geom_abline(intercept = 0, slope = 1)

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
  
  mu <- unlist(strsplit(conditions[i], "_"))[3]
  mu <- gsub("mu", "", mu)
  mu <- as.numeric(gsub("-", ".", mu))

  D_corr_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param)) +
    geom_point(alpha = 0.65, color = colors[i]) +
    labs(x = "True Parameter Value", y = "Estimated Posterior Mean",
         title = paste0("rho = ", rho, ", reference proportion = ", PREF,
                        "\nmu = ", mu, ", alpha = ", alpha),
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
    scale_x_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 0.5)) +
    scale_y_continuous(limits = c(-1, 2), breaks = seq(-1, 2, 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_vline(xintercept = UQ(mu), linetype = "dashed")

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
    labs(x = "True Parameter Value", y = "Estimated Posterior Mean",
         title = paste0("rho = ", rho, ", reference proportion = ", PREF,
                        "\nmu = ", mu, ", alpha = ", alpha),
         caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) +
    scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
    scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    geom_abline(intercept = 0, slope = 1)
  
  print(i)
}


pdf("./analysis/a_corr_scatterplots.pdf", width = 14, height = 12)
for(i in seq(1, 36, 6)){
  multiplot(a_corr_scatter[[i]], a_corr_scatter[[i+1]], a_corr_scatter[[i+2]],
            a_corr_scatter[[i+3]], a_corr_scatter[[i+4]], a_corr_scatter[[i+5]], cols = 3,
            plot_title = "a-parameter scatterplots")
}
dev.off()

pdf("./analysis/b_corr_scatterplots.pdf", width = 14, height = 12)
for(i in seq(1, 36, 6)){
  multiplot(b_corr_scatter[[i]], b_corr_scatter[[i+1]], b_corr_scatter[[i+2]],
            b_corr_scatter[[i+3]], b_corr_scatter[[i+4]], b_corr_scatter[[i+5]], cols = 3,
            plot_title = "b-parameter scatterplots")
}
dev.off()

pdf("./analysis/D_corr_scatterplots.pdf", width = 14, height = 12)
for(i in seq(1, 36, 6)){
  multiplot(D_corr_scatter[[i]], D_corr_scatter[[i+1]], D_corr_scatter[[i+2]],
            D_corr_scatter[[i+3]], D_corr_scatter[[i+4]], D_corr_scatter[[i+5]], cols = 3,
            plot_title = "D-parameter scatterplots")
}
dev.off()

pdf("./analysis/theta_corr_scatterplots.pdf", width = 14, height = 12)
for(i in seq(1, 36, 6)){
  multiplot(theta_corr_scatter[[i]], theta_corr_scatter[[i+1]], theta_corr_scatter[[i+2]],
            theta_corr_scatter[[i+3]], theta_corr_scatter[[i+4]], theta_corr_scatter[[i+5]], cols = 3,
            plot_title = "theta-parameter scatterplots")
}
dev.off()

rm(a_corr_scatter, b_corr_scatter, D_corr_scatter, theta_corr_scatter)
gc()


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
    
    
    data <- data %>%
      mutate(Decision = case_when((abs(true_param) >= flag_amt & abs(est_param) >= flag_amt) ~ "Correct Classification",
                                  (abs(true_param) >= flag_amt & abs(est_param) < flag_amt) ~ "Incorrect Classification",
                                  (abs(true_param) < flag_amt & abs(est_param) < flag_amt) ~ "Correct Classification",
                                  (abs(true_param) < flag_amt & abs(est_param) >= flag_amt) ~ "Incorrect Classification",
                                  TRUE ~ "NA")) %>%
      mutate(Decision_spec = case_when((abs(true_param) >= flag_amt & abs(est_param) >= flag_amt) ~ "True Positive", 
                                       (abs(true_param) >= flag_amt & abs(est_param) < flag_amt) ~ "False Negative",
                                       (abs(true_param) < flag_amt & abs(est_param) < flag_amt) ~ "True Negative", 
                                       (abs(true_param) < flag_amt & abs(est_param) >= flag_amt) ~ "False Positive",
                                       TRUE ~ "NA"))
    
    N <- nrow(data)
    TP_N <- nrow(filter(data, Decision_spec == "True Positive"))
    FP_N <- nrow(filter(data, Decision_spec == "False Positive"))
      
    # dec_consist <- matrix(c(truepos, falsepos, falseneg, trueneg), nrow = 2, byrow = TRUE)
    # colnames(dec_consist) <- c("True_DIF", "True_NoDIF")
    # rownames(dec_consist) <- c("Est_DIF", "Est_NoDIF")
    # 
    # addWorksheet(dec_consist_out, conditions[[i]])
    # writeData(dec_consist_out, sheet = i, dec_consist, rowNames = TRUE)
    # 
    # correct_ratio <- round((trueneg+truepos)/(trueneg+truepos+falsepos+falseneg), 3)
    
    D_decision_scatter[[i]] <- ggplot(data, aes(x = true_param, y = est_param, color = Decision)) +
      geom_point() +
      scale_x_continuous(limits = c(-1.2, 2), breaks = seq(-1, 2, 0.5)) +
      scale_y_continuous(limits = c(-1.2, 2), breaks = seq(-1, 2, 0.5)) +
      labs(x = "True Parameter Value", y = "Estimated Posterior Mean",
           title = paste0("rho = ", rho, ", reference proportion = ", PREF,
                          "\nmu = ", mu, ", alpha = ", alpha),
           caption = paste0("False Pos Rate = ", sprintf("%.3f", FP_N/N),
                            "; Power = ", sprintf("%.3f", TP_N/N),
                            "; Precision = ", sprintf("%.3f", TP_N/(TP_N + FP_N)))) +
      theme_bw() + 
      scale_color_manual(values = c("forestgreen", "darkred")) + 
      geom_hline(aes(yintercept = flag_amt), color = "darkgray") +
      geom_vline(aes(xintercept = flag_amt), color = "darkgray") +
      geom_hline(aes(yintercept = -flag_amt), color = "darkgray") +
      geom_vline(aes(xintercept = -flag_amt), color = "darkgray") +
      theme(legend.position = "bottom") +
      theme(plot.title = element_text(hjust = 0.5, size = 12))
    
    print(i)
  }
  
  pdf(paste0("./analysis/decision-consistency", gsub("\\.", "-", flag_amt), "_scatterplots.pdf"), width = 14, height = 12)
  for(i in seq(1, 36, 6)){
    multiplot(D_decision_scatter[[i]], D_decision_scatter[[i+1]], D_decision_scatter[[i+2]],
              D_decision_scatter[[i+3]], D_decision_scatter[[i+4]], D_decision_scatter[[i+5]], cols = 3,
              plot_title = paste0("Decision Consistency for flag threshold ", flag_amt))
  }
  dev.off()
  
  # saveWorkbook(dec_consist_out, paste0(flag_amt, "_decision_consistency.xlsx"), overwrite = TRUE)
}



