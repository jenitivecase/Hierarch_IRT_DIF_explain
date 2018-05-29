#### SETUP ####

source("mixture_functions.R")
date <- format.Date(Sys.Date(), "%Y%m%d")
options(scipen = 999)

needed_packages <- c("tidyr", "dplyr", "rstan", "rstudioapi", "robustbase", "portableParallelSeeds")

load_packages <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x)
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}

sapply(needed_packages, load_packages)

#### DATA SETUP ####
responses <- read.csv("data.csv")
n_people <- nrow(responses)
n_items <- ncol(responses) - 2

items <- read.csv("items.csv")
items <- as.data.frame(apply(items, 2, FUN = as.numeric))

group <- responses$esl

responses <- responses %>%
  select(-fid) %>%
  mutate(id = row_number()) %>%
  gather(key = "question", value = "response", -id, -esl, na.rm = FALSE)


n_observations <- nrow(responses)
respondentid <- responses$id
itemid <- as.numeric(gsub("scr", "", responses$question))
response <- responses$response
group_long <- responses$esl

words <- items$words
sentences <- items$sentences
ave_word_sent <- items$ave_word_sent
bigword <- items$bigword
nonmath_word <- items$NonmathWord
prep <- items$prep
passive <- items$passive
confuse <- items$confuse
schematic <- items$schematic
confuseXschem <- items$confuseXschem


#### STAN SETUP ####
#load stan model scripts
source("real_stan_scripts.R")

b.dat_long <- list("n_people", "n_items", "n_observations", "respondentid", 
                   "itemid", "response", "group", "group_long",
                   "words", "sentences", "ave_word_sent", "bigword", 
                   "nonmath_word", "prep", "passive", "confuse",
                   "schematic", "confuseXschem")


#analysis setup
precomp <- stanc(model_code = stancode_long)
precomp_model <- stan_model(stanc_ret = precomp)

#### DATA SAVE SETUP ####

analysis <- sampling(precomp_model, data = b.dat_long,
                     iter = 15000, warmup = 5000, chains = 2, verbose = FALSE, cores = 2)


saveRDS(analysis, "realdata_analysis_object_15000.rds")
