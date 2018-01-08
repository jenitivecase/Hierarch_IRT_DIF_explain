true_item_params <- readRDS("true_item_params.rds")
true_ability_params <- readRDS("true_ability_params.rds")
est_param_means <- readRDS("est_param_means.rds")

MSE_out <- data.frame(matrix(NA, nrow = length(conditions), ncol = 8))
names(MSE_out) <- c("rho", "PREF", "mu", "alpha", "a-params", "b-params", "D-params", "thetas")

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
  
  MSE_out[i, c("rho", "PREF", "mu", "alpha")] <- c(rho, PREF, mu, alpha)
  
  #a-params
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
  
  data$bias <- data$true_param - data$est_param
  MSE_out[i, "a-params"] <- sum(purrr::map_dbl(pull(data, bias), function(x) x^2))
  
  #b-params
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
  
  data$bias <- data$true_param - data$est_param
  MSE_out[i, "b-params"] <- sum(purrr::map_dbl(pull(data, bias), function(x) x^2))
  
  #D-params
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
  
  data$bias <- data$true_param - data$est_param
  MSE_out[i, "D-params"] <- sum(purrr::map_dbl(pull(data, bias), function(x) x^2))
  
  #theta
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
  
  data$bias <- data$true_param - data$est_param
  MSE_out[i, "thetas"] <- sum(purrr::map_dbl(pull(data, bias), function(x) x^2))
}


write.xlsx(MSE_out, "./analysis/parameter_estimate_MSEs.xlsx")
