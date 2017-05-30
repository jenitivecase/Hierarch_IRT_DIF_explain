#### DATA GENERATION ####

#simulate a set of items
item_sim <- function(n_items, b_mean, b_sd, a_min, a_max, 
                     mix_alpha, mix_mu1, mix_mu2, mix_sdev){
  item_param <- matrix(NA, nrow = n_items, ncol = 3)
  colnames(item_param) <- c("b_param", "a_param", "dif_param")

  item_param[, "b_param"] <- rnorm(nrow(item_param), b_mean, b_sd)
  item_param[, "a_param"] <- runif(nrow(item_param), a_min, a_max)
  k <- rbinom(nrow(item_param), 1, alpha)
  item_param[, "dif_param"] <- (rnorm(nrow(item_param), mix_mu1, mix_sdev)^k) * 
    (rnorm(nrow(item_param), mix_mu2, mix_sdev)^(1-k))
  
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
  
  DIF_predict <- rho*zscores + e1
  
  # DIF_predict <- sqrt(rho^2)*zscores + e1
  return(DIF_predict)
}

#### LONG FORMAT RESTRUCTURING ####
long_format <- function(data = dataset, group_data = group){
  #prep for reformatting
  data <- as.data.frame(data)
  names(data) <- paste0("Item", 1:ncol(data))
  data$respondentid <- c(1:nrow(data))
  
  #move to long format
  dataset_long <- gather(data, key = respondentid, value = response)
  names(dataset_long)[2] <- "itemid"
  
  #joining group
  group_data <- as.data.frame(group_data)
  group_data$respondentid <- c(1:nrow(group_data))
  dataset_long <- left_join(dataset_long, group_data, by = "respondentid")
  
  dataset_long$itemid <- gsub("Item", "", dataset_long$itemid)
  
  names(dataset_long) <- c("respondentid", "itemid", "response", "group")
  
  return(dataset_long)
}

#### ANALYSIS ####

#do the analysis for one set of responses
one_analysis <- function(x, n_iter = 2000, n_burn = 1000, n_chains = 2, 
                         modelname = "stan_model", b_dat = b.dat, 
                         n_cores = 2, debug = FALSE){
  if(class(x) == "stanmodel"){
    OUT <- sampling(x, data = b.dat,
                    iter = n_iter, warmup = n_burn, chains = n_chains, 
                    verbose = debug, cores = n_cores)
  } else if(class(x) == "character"){
    OUT <- stan(x, model_name = modelname, data = b.dat,
                    iter = n_iter, warmup = n_burn, chains = n_chains, 
                    verbose = debug, cores = n_cores)
  } else {
    stop("Please specify a pre-compiled Stan model or a character variable 
         containing a model specification in the Stan modeling language")
  }
  return(OUT)
}


one_analysis_BUGS <- function(x, n_iter = 1000, n_burn = 300, b_dat = b.dat, 
                         b_par = b.par, model_file = "BUGScode.txt"){
  vars <- c(unlist(b_dat))
  mget(vars, envir = globalenv())
  OUT <- R2OpenBUGS::bugs(data = b_dat, inits = NULL, parameters.to.save = b_par,
              model.file = model_file, n.chains = 2,
              n.iter = n_iter, n.burn = n_burn, n.thin = 1, debug = TRUE)
  return(OUT)
}

#### PROCESSING - DATA RETRIEVAL ####
correlation_get <- function(condition, file_list){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  output <- lapply(output, unlist, recursive = FALSE)
  output <- do.call(rbind, output)
  return(output)
}

# param_get <- function(condition, file_list, param_name){
#   output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
#   
#   param <- lapply(output, function(x){
#     x <- x[which(grepl("param", names(x)))]
#   })
#   param <- lapply(param, unlist, recursive = FALSE)
#   param <- unlist(param, recursive = FALSE)
#   
#   return(param)
# }

true_param_get <- function(condition, file_list, param_type, param_name){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  
  param <- vector("list", length(output))
  
  for(i in 1:length(output)){
    param[[i]] <- as.data.frame(output[[i]][param_type])
  }
  
  for(i in 1:length(param)){
    param[[i]] <- as.data.frame(param[[i]][grep(param_name, names(param[[i]]))])
  }
  
  param <- bind_rows(param, .id = names(output))
  return(param)
}

est_param_get <- function(condition, file_list, param_name){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  
  param <- lapply(output, as.data.frame)
  param <- bind_rows(param, .id = names(output))
  return(param)
}

est_param_means_get <- function(condition, file_list, param_name){
  output <- readRDS(paste0(file_list[grepl(condition, file_list)]))
  
  param <- lapply(output, function(x) as.data.frame(x[param_name]))
  param <- bind_rows(param, .id = names(output))
  return(param)
}

#### CI ANALYSIS ####
CI_retrieval <- function(true_param_vec, est_param_mat){
  if(NROW(true_param_vec) != NROW(est_param_mat)){
    stop("Unequal param count!")
  }
  
  CI_out <- c(rep(NA, NROW(true_param_vec)))
  
  for(i in 1:NROW(true_param_vec)){
    CI_out[i] <- ifelse(true_param_vec[i] > est_param_mat[i, "2.5%"] & 
             true_param_vec[i] < est_param_mat[i, "97.5%"], TRUE, FALSE)
  }
  
  return(CI_out)
}

#### GRAPHING ####
scale_def <- function(list, column){
  scale <- NA
  for(i in 1:length(list)){
    rounded <- abs(c(round(max(list[[i]][, column]), digits = 1), 
                     round(min(list[[i]][, column]), digits = 1)))
    scale[i] <- rounded[which.max(rounded)]
  }
  scale <- scale[which.max(scale)]
  return(scale)
}

scale_def_corr <- function(list, column){
  scale <- NA
  for(i in 1:length(list)){
    scale[i] <- (floor(((min(list[[i]][, column])) * 10)) / 10)
  }
  scale <- c(scale[which.min(scale)], 1)
  return(scale)
}

