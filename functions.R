#### DATA GENERATION ####

#simulate a set of items
item_sim <- function(n_items, n_DIF, b_mean, b_sd, a_mean, a_sd, 
                     nodif_mean, nodif_sd, dif_mean, dif_sd){
  item_param <- matrix(NA, nrow = n_items, ncol = 3)
  colnames(item_param) <- c("b_param", "a_param", "dif_param")
  noDIF_rows <- c(1:(nrow(item_param)-n_DIF))
  DIF_rows <- c((nrow(item_param)-n_DIF+1):nrow(item_param))
  
  item_param[, "b_param"] <- rnorm(nrow(item_param), b_mean, b_sd)
  item_param[, "a_param"] <- rnorm(nrow(item_param), a_mean, a_sd)
  item_param[noDIF_rows, "dif_param"] <- rnorm(length(noDIF_rows), 
                                               nodif_mean, nodif_sd)
  item_param[DIF_rows, "dif_param"] <- rnorm(length(DIF_rows), dif_mean, dif_sd)
  
  return(item_param)
}

#simulate a set of people's ability scores
ability_sim <- function(N_people, P_REF, ref_theta_mean, ref_theta_sd, 
                        focal_theta_mean, focal_theta_sd){
  ability_scores <- matrix(NA, nrow = N_people, ncol = 2)
  colnames(ability_scores) <- c("theta", "group")
  ref_cutoff <- nrow(ability_scores)*P_REF
  ref_rows <- c(1:ref_cutoff)
  focal_rows <- c((ref_cutoff+1):nrow(ability_scores))
  
  ability_scores[ref_rows, "theta"] <- rnorm(length(ref_rows), 
                                             ref_theta_mean, ref_theta_sd)
  ability_scores[ref_rows, "group"] <- 0
  
  ability_scores[focal_rows, "theta"] <- rnorm(length(focal_rows), 
                                               focal_theta_mean, focal_theta_sd)
  ability_scores[focal_rows, "group"] <- 1
  
  return(ability_scores)
}

#get the responses for a single item
response_sim <- function(person_vec, item_vec){
  guts <- item_vec["a_param"]*(person_vec["theta"]-
                                 (item_vec["b_param"]+item_vec["dif_param"]*person_vec["group"]))
  prob <- exp(guts)/(1+exp(guts))
  ifelse(runif(1, 0, 1) <= prob, return(1), return(0)) 
}

#get responses for a single person to a set of items
person_sim <- function(person_vec, item_param = item_param){
  responses_vec <- matrix(NA, nrow=nrow(item_param))
  for(i in 1:nrow(item_param)){
    responses_vec[i] <- response_sim(person_vec, item_param[i,])
  }
  return(responses_vec)
}

#get responses for a set of people to a set of items
one_dataset <- function(person_param, item_param){
  responses <- matrix(NA, nrow = nrow(person_param), ncol = nrow(item_param))
  for(i in 1:nrow(person_param)){
    responses[i,] <- person_sim(person_param[i,], item_param)
  }
  #colnames(responses) <- paste0("V", 1:nrow(item_param))
  return(responses)
}

#### PREPARATION ####
#get DIF predictor
DIF_predictor <- function(item_param, rho){
  mean_DIF <- mean(item_param[,"dif_param"])
  sd_DIF <- sd(item_param[,"dif_param"])
  zscores <- (item_param[,"dif_param"] - mean_DIF)/sd_DIF
  
  e1 <- rnorm(nrow(item_param),0,sqrt(1-rho^2))
  
  DIF_predict <- sqrt(rho^2)*zscores + e1
  return(DIF_predict)
}

#### ANALYSIS ####

#do the analysis for one set of responses
one_analysis <- function(x, n_iter = 2000, n_burn = 1000, b_dat = b.dat, 
                         b_par = b.par, model_file = "BUGScode.txt", 
                         debug = FALSE){
  vars <- c(unlist(b_dat))
  mget(vars, envir = globalenv())
  OUT <- bugs(data = b_dat, inits = NULL, parameters.to.save = b_par, 
              model.file = model_file, n.chains = 2, 
              n.iter = n_iter, n.burn = n_burn, n.thin = 1, debug = debug)
  
  return(OUT)
}


##IGNORE THIS!
one_DIF <- function(analysis_results, params){
  DIF_results <- mirt::DIF(analysis_results, which.par = params, 
                           technical = list(NCYCLES = 1500))
  return(DIF_results)
}
