##combining results files from multiple runs
#### SETUP ####
# setwd("E:/Comps simulation results 20170302/20170224_simulation-results")
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

# work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/20170224_simulation-results"
work_dir <- "D:/Comps simulation results 20170302/All_simulation_results_20170404"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  setwd(gsub("jbrussow", "Jen", work_dir))
}

library(tidyr)
library(dplyr)
library(ggplot2)

files <- as.data.frame(list.files(getwd()))
names(files) <- "filename"
files$filename <- as.character(files$filename)
files <- filter(files, filename != "combined")
files <- filter(files, !grepl("result", filename))

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

if(!dir.exists("combined")){dir.create("combined")}

for(i in 1:nrow(types_conditions)){
  condition_matches <- grep(types_conditions[i, "conditions"], files$filename)
  type_matches <- grep(paste0("^", types_conditions[i, "types"]), files$filename)
  indices <- condition_matches[which(condition_matches %in% type_matches)]
  set_files <- files[indices, "filename"]
  
  out <- NA
  
  for(j in 1:length(set_files)){
    output <- readRDS(set_files[j])
    output <- output[!sapply(output, is.null)] 
    out <- append(out, output)
  }
  
  #removes the initial NA
  out <- out[c(2:length(out))]
  
  fname <- paste0(types_conditions[i, "types"], "_", length(out), "reps_", types_conditions[i, "conditions"], ".rds")
  saveRDS(out, paste0("combined/", fname))
  
}
