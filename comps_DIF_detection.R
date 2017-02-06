if(Sys.info()["user"] == "jbrussow"){
  setwd("C:/Users/jbrussow/Dropbox/REMS/11 Comps/")
} else if (Sys.info()["user"] == "Jen"){
  setwd("C:/Users/Jen/Dropbox/REMS/11 Comps/")
}

source("functions.R")

options(scipen = 999)
library(R2OpenBUGS)
library(coda)
library(mirt)

#number of people
N_people <- c(1000, 2000, 5000)
#number of items
n_items <- 100
#number of items with DIF
n_DIF <- 5
#number of reps
nreps <- 1
#rho is the amount of DIF explained by the second-order factors
rho <- c(0.2, 0.3, 0.4, 0.5, 0.6)
#P_REF is the proportion of people in the reference group
P_REF <- c(0.5, 0.6, 0.7, 0.8, 0.9)

conditions <- expand.grid(N_people, rho, P_REF)

date <- format.Date(Sys.Date(), "%Y%m%d")


#simulate a set of items
item_test <- item_sim(n_items, n_DIF, b_mean = 0, b_sd = 1, a_mean = 1, a_sd = .5, 
         nodif_mean = 0, nodif_sd = 0.1, dif_mean = 1, dif_sd = 0.1)

#simulate a set of people's ability scores
ability_test <- ability_sim(1000, P_REF = .8, ref_theta_mean = 0, ref_theta_sd = 1,
                            focal_theta_mean = -0.5, focal_theta_sd = 1)

#get the responses for a single item
response_test <- response_sim(ability_test[1,], item_test[1,])

#get responses for a single person to a set of items
responseset_test <- person_sim(ability_test[1,], item_test)

#get responses for a set of people to a set of items
dataset_test <- one_dataset(ability_test, item_test)

### DONE TO HERE ###

#do the analysis for one set of responses
analysis_test <- one_analysis(dataset = dataset_test[,1], groups = ability_test[,2])




#ANALYSIS####
for(i in 1:num_sim){
  mod <- one_analysis(one_dataset(person_param, item_param), specs) 
  results[[i]] <- vector("list", 7)
  results[[i]][[1]] <- extract.mirt(mod, "converged")
  if(extract.mirt(mod, "converged") == TRUE){
    results[[i]][[2]] <- extract.mirt(mod, "logLik")
    results[[i]][[3]] <- extract.mirt(mod, "AIC")
    results[[i]][[4]] <- extract.mirt(mod, "BIC")
    results[[i]][[5]] <- coef(mod, printSE = TRUE, simplify = TRUE)$items
    person_scores_SE <- fscores(mod, QMC = TRUE, full.scores.SE = TRUE)
    results[[i]][[6]] <- empirical_rxx(person_scores_SE)
    results[[i]][[7]] <- M2(mod, QMC = TRUE)
  }
}

saveRDS(results, paste0(date, "_simulation_output.rds"))