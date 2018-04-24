#### SETUP ####

work_dir <- paste0(getwd(), "/real_data_application/")

source("mixture_functions.R")
date <- format.Date(Sys.Date(), "%Y%m%d")
options(scipen = 999)

needed_packages <- c("tidyr", "dplyr", "rstan", "rstudioapi", "robustbase", "portableParallelSeeds")
for(i in 1:length(needed_packages)){
  library(needed_packages[i], character.only = TRUE)
}


#### DATA SETUP ####
responses <- read.csv(paste0(work_dir, "data.csv"))
n_people <- nrow(responses)
n_items <- ncol(responses) - 2

items <- read.csv(paste0(work_dir, "items.csv"))

group <- responses$esl

responses <- responses %>%
  select(-esl) %>%
  gather(key = "question", value = "response", -fid, na.rm = TRUE)

#### STAN SETUP ####
#load stan model scripts
source("stan_scripts.R")

b.dat_long <- list("n_people", "n_items", "n_observations", "respondentid", 
                   "itemid", "response", "group", "group_long",
                   "DIFpredict")


#analysis setup
precomp <- stanc(model_code = stancode_long)
precomp_model <- stan_model(stanc_ret = precomp)

#### DATA SAVE SETUP ####



analysis <- sampling(precomp_model, data = b.dat_long,
                     iter = 12000, warmup = 5000, chains = 2, verbose = FALSE, cores = 2)