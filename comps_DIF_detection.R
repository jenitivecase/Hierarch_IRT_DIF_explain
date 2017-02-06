if(Sys.info()["user"] == "jbrussow"){
  setwd("C:/Users/jbrussow/Dropbox/REMS/11 Comps/")
} else if (Sys.info()["user"] == "Jen"){
  setwd("C:/Users/Jen/Dropbox/REMS/11 Comps/")
}

options(scipen = 999)
library("R2OpenBUGS")
library("coda")

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
item_sim <- function(n_items, n_DIF, b_mean, b_sd, a_mean, a_sd, 
                     nodif_mean, nodif_sd, dif_mean, dif_sd){
  item_param <- matrix(NA, nrow = n_items, ncol = 3)
  colnames(item_param) <- c("b_param", "a_param", "dif_param")
  for(i in 1:(nrow(item_param)-n_DIF)){
    item_param[i, "b_param"] <- rnorm(1, b_mean, b_sd)
    item_param[i, "a_param"] <- rnorm(1, a_mean, a_sd)
    item_param[i, "dif_param"] <- rnorm(1, nodif_mean, nodif_sd)
  }
  for(i in (nrow(item_param)-n_DIF+1):nrow(item_param)){
    item_param[i, "b_param"] <- rnorm(1, b_mean, b_sd)
    item_param[i, "a_param"] <- rnorm(1, a_mean, a_sd)
    item_param[i, "dif_param"] <- rnorm(1, dif_mean, dif_sd)
  }
  return(item_param)
}

item_test <- item_sim(n_items, n_DIF, b_mean = 0, b_sd = 1, a_mean = 1, a_sd = .5, 
         nodif_mean = 0, nodif_sd = 0.1, dif_mean = 1, dif_sd = 0.1)

#simulate a set of people's ability scores
ability_sim <- function(N_people, P_REF, ref_theta_mean, ref_theta_sd, 
                        focal_theta_mean, focal_theta_sd){
  ability_scores <- matrix(NA, nrow = N_people, ncol = 2)
  colnames(ability_scores) <- c("theta", "group")
  ref_cutoff <- nrow(ability_scores)*P_REF
  for(i in 1:ref_cutoff){
    ability_scores[i, 1] <- rnorm(1, ref_theta_mean, ref_theta_sd)
  }
  for(i in (ref_cutoff+1):nrow(ability_scores)){
    ability_scores[i, 1] <- rnorm(1, focal_theta_mean, focal_theta_sd)
  }
  ability_scores[1:ref_cutoff, 2] <- 0
  ability_scores[(ref_cutoff+1):nrow(ability_scores), 2] <- 1
  return(ability_scores)
}

ability_test <- ability_sim(1000, P_REF = .8, ref_theta_mean = 0, ref_theta_sd = 1,
                            focal_theta_mean = -0.5, focal_theta_sd = 1)

#get the responses for a single item
response_sim <- function(person_vec, item_vec){
  guts <- item_vec["a_param"]*(person_vec["theta"]-
                                  (item_vec["b_param"]+item_vec["dif_param"]*person_vec["group"]))
  prob <- exp(guts)/(1+exp(guts))
  ifelse(runif(1, 0, 1) <= prob, return(1), return(0)) 
}

response_test <- response_sim(ability_test[1,], item_test[1,])

#get responses for a single person to a set of items
person_sim <- function(person_vec, item_param = item_param){
  responses_vec <- matrix(NA, nrow=nrow(item_param))
  for(i in 1:nrow(item_param)){
    responses_vec[i] <- response_sim(person_vec, item_param[i,])
  }
  return(responses_vec)
}

responseset_test <- person_sim(ability_test[1,], item_test)

#get responses for a set of people to a set of items
one_dataset <- function(person_param, item_param){
  responses <- matrix(NA, nrow = nrow(person_param), ncol = nrow(item_param))
  for(i in 1:nrow(person_param)){
    responses[i,] <- person_sim(person_param[i,], item_param)
  }
  return(responses)
}

dataset_test <- one_dataset(ability_test, item_test)

#do the analysis for one item's set of responses
one_analysis <- function(dataset, groups){
  sumscores <- rowSums(responses)
  sumZscores <- (sumscores-mean(sumscores))/sd(sumscores)
  mod <- glm(dataset ~ sumscores + groups,family="binomial")
  return(mod)
}

analysis_test <- one_analysis(dataset = dataset_test[,1], groups = ability_test[,2])


### DONE TO HERE ###


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