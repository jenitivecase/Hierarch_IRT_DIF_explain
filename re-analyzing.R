##combining results files from multiple runs
#### SETUP ####
# setwd("E:/Comps simulation results 20170302/20170224_simulation-results")
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

# work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/20170224_simulation-results"
work_dir <- "F:/Comps simulation results 20170302/All_simulation_results_20170224-0306"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  setwd(gsub("jbrussow", "Jen", work_dir))
}

library(tidyr)
library(dplyr)
library(ggplot2)
library(rstan)

files <- as.data.frame(list.files(getwd()))
names(files) <- "filename"
files$filename <- as.character(files$filename)
files <- filter(files, filename != "combined")

for(i in 1:nrow(files)){
  info <-unlist(strsplit(as.character(
    files[i, "filename"]), "_"))
  len <- length(info)
  condition <- paste0(c(info[(len-1):len]), collapse = "", sep = "_")
  condition <- gsub("\\.rds\\_", "", condition)
  type <- paste0(c(info[1:(len-3)]), collapse = "", sep = "_")
  type <- gsub("\\_$", "", type)
  files[i, "condition"] <- condition
  files[i, "type"] <- type
}

conditions <- unique(files$condition)
types <- unique(files$type)

types_conditions <- expand.grid(conditions, types)
names(types_conditions) <- c("conditions", "types")
types_conditions <- apply(types_conditions, 2, as.character)

results_files <- files[which(grepl("result", files$filename)),]

for(i in 1:nrow(results_files)){
  output <- readRDS(results_files[i, "filename"])
  output <- output[!sapply(output, is.null)] 
  
  for(j in 1:length(output)){
    params_summary <- summary(output[[j]], pars = c("a", "b", "D", "beta1", "mu", 
                                                 "sigma2", "R2", "theta",
                                                 "foc_mean"),
                              probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary  
  }
  
}