true_item_params <- readRDS("true_item_params.rds")
true_ability_params <- readRDS("true_ability_params.rds")
est_param_means <- readRDS("est_param_means.rds")

big_MSE_out <- data.frame(matrix(NA, nrow = (length(conditions)*100), ncol = 8))
names(big_MSE_out) <- c("rho", "PREF", "mu", "alpha", "a-params", "b-params", "D-params", "thetas")

big_bias_out <- data.frame(matrix(NA, nrow = (length(conditions)*100), ncol = 8))
names(big_bias_out) <- c("rho", "PREF", "mu", "alpha", "a-params", "b-params", "D-params", "thetas")

MSE_out <- data.frame(matrix(NA, nrow = length(conditions), ncol = 8))
names(MSE_out) <- c("rho", "PREF", "mu", "alpha", "a-params", "b-params", "D-params", "thetas")

bias_out <- data.frame(matrix(NA, nrow = length(conditions), ncol = 8))
names(bias_out) <- c("rho", "PREF", "mu", "alpha", "a-params", "b-params", "D-params", "thetas")

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
  
  index_1 <- as.numeric(paste0(10*(i-1), 1))
  index_2 <- as.numeric(paste0(10*(i), 0))
  indices <- seq(index_1, index_2)
  
  big_MSE_out[indices, 
              c("rho", "PREF", "mu", "alpha")] <- c(rep(rho, 100), 
                                                    rep(PREF, 100), 
                                                    rep(mu, 100), 
                                                    rep(alpha, 100))
    
  big_bias_out[indices, 
              c("rho", "PREF", "mu", "alpha")] <- c(rep(rho, 100), 
                                                    rep(PREF, 100), 
                                                    rep(mu, 100), 
                                                    rep(alpha, 100))
  
  MSE_out[i, c("rho", "PREF", "mu", "alpha")] <- c(rho, PREF, mu, alpha)
  bias_out[i, c("rho", "PREF", "mu", "alpha")] <- c(rho, PREF, mu, alpha)
  
  item_seq <- seq(0, 6000, 60)
  theta_seq <- seq(0, 10000, 100)
  
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

  for(repl in 1:100){
    big_bias_out[indices[repl], "a-params"] <- mean(data[c((item_seq[repl]+1):(item_seq[(repl+1)])),
                                                         "bias"])
    big_MSE_out[indices[repl], "a-params"] <- mean(data[c((item_seq[repl]+1):(item_seq[(repl+1)])),
                                                        "bias"]^2)
  }
  
  MSE_out[i, "a-params"] <- mean(big_MSE_out[indices, "a-params"])
  bias_out[i, "a-params"] <- mean(big_bias_out[indices, "a-params"])
  
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
  
  for(repl in 1:100){
    big_bias_out[indices[repl], "b-params"] <- mean(data[c((item_seq[repl]+1):(item_seq[(repl+1)])),
                                                         "bias"])
    big_MSE_out[indices[repl], "b-params"] <- mean(data[c((item_seq[repl]+1):(item_seq[(repl+1)])),
                                                         "bias"]^2)
  }
  
  MSE_out[i, "b-params"] <- mean(big_MSE_out[indices, "b-params"])
  bias_out[i, "b-params"] <- mean(big_bias_out[indices, "b-params"])
  
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

  for(repl in 1:100){
    big_bias_out[indices[repl], "D-params"] <- mean(data[c((item_seq[repl]+1):(item_seq[(repl+1)])),
                                                         "bias"])
    big_MSE_out[indices[repl], "D-params"] <- mean(data[c((item_seq[repl]+1):(item_seq[(repl+1)])),
                                                        "bias"]^2)
  }
  
  MSE_out[i, "D-params"] <- mean(big_MSE_out[indices, "D-params"])
  bias_out[i, "D-params"] <- mean(big_bias_out[indices, "D-params"])
  
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

  for(repl in 1:100){
    big_bias_out[indices[repl], "thetas"] <- mean(data[c((theta_seq[repl]+1):(theta_seq[(repl+1)])),
                                                         "bias"])
    big_MSE_out[indices[repl], "thetas"] <- mean(data[c((theta_seq[repl]+1):(theta_seq[(repl+1)])),
                                                        "bias"]^2)
  }
  
  MSE_out[i, "thetas"] <- mean(big_MSE_out[indices, "thetas"])
  bias_out[i, "thetas"] <- mean(big_bias_out[indices, "thetas"])
}


write.xlsx(MSE_out, "./analysis/mean_parameter_estimate_MSEs.xlsx")
write.xlsx(bias_out, "./analysis/mean_parameter_estimate_bias.xlsx")

write.xlsx(big_MSE_out, "./analysis/replication_parameter_estimate_MSEs.xlsx")
write.xlsx(big_bias_out, "./analysis/replication_parameter_estimate_bias.xlsx")


#anovas
library(car)
library(lsr)


big_MSE_out <- read.xlsx( "./analysis/replication_parameter_estimate_MSEs.xlsx")
big_bias_out <- read.xlsx("./analysis/replication_parameter_estimate_bias.xlsx")


a_bias_anova <- lm(`a-params` ~ rho + PREF + mu + alpha +
                    rho*PREF + rho*mu + rho*alpha + 
                    PREF*mu + PREF*alpha + 
                    mu*alpha +
                    rho*PREF*mu + rho*PREF*alpha + rho*mu*alpha + 
                    PREF*mu*alpha +
                    rho*PREF*mu*alpha, data = big_bias_out)
a_bias_anova_results <- as.data.frame(anova(a_bias_anova)) %>% 
  tibble::rownames_to_column("Predictor")
a_bias_anova_effects <- as.data.frame(lsr::etaSquared(a_bias_anova))

a_bias_anova_out <- cbind(a_bias_anova_results, 
                          rbind(a_bias_anova_effects, rep(NA, ncol(a_bias_anova_effects))))

b_bias_anova <- lm(`b-params` ~ rho + PREF + mu + alpha +
                    rho*PREF + rho*mu + rho*alpha + 
                    PREF*mu + PREF*alpha + 
                    mu*alpha +
                    rho*PREF*mu + rho*PREF*alpha + rho*mu*alpha + 
                    PREF*mu*alpha +
                    rho*PREF*mu*alpha, data = big_bias_out)
b_bias_anova_results <- as.data.frame(anova(b_bias_anova)) %>% 
  tibble::rownames_to_column("Predictor")
b_bias_anova_effects <- as.data.frame(lsr::etaSquared(b_bias_anova))

b_bias_anova_out <- cbind(b_bias_anova_results, 
                          rbind(b_bias_anova_effects, rep(NA, ncol(b_bias_anova_effects))))

D_bias_anova <- lm(`D-params` ~ rho + PREF + mu + alpha +
                    rho*PREF + rho*mu + rho*alpha + 
                    PREF*mu + PREF*alpha + 
                    mu*alpha +
                    rho*PREF*mu + rho*PREF*alpha + rho*mu*alpha + 
                    PREF*mu*alpha +
                    rho*PREF*mu*alpha, data = big_bias_out)
D_bias_anova_results <- as.data.frame(anova(D_bias_anova)) %>% 
  tibble::rownames_to_column("Predictor")
D_bias_anova_effects <- as.data.frame(lsr::etaSquared(D_bias_anova))

D_bias_anova_out <- cbind(D_bias_anova_results, 
                          rbind(D_bias_anova_effects, rep(NA, ncol(D_bias_anova_effects))))

theta_bias_anova <- lm(`thetas` ~ rho + PREF + mu + alpha +
                        rho*PREF + rho*mu + rho*alpha + 
                        PREF*mu + PREF*alpha + 
                        mu*alpha +
                        rho*PREF*mu + rho*PREF*alpha + rho*mu*alpha + 
                        PREF*mu*alpha +
                        rho*PREF*mu*alpha, data = big_bias_out)
theta_bias_anova_results <- as.data.frame(anova(theta_bias_anova)) %>% 
  tibble::rownames_to_column("Predictor")
theta_bias_anova_effects <- as.data.frame(lsr::etaSquared(theta_bias_anova))

theta_bias_anova_out <- cbind(theta_bias_anova_results, 
                          rbind(theta_bias_anova_effects, rep(NA, ncol(theta_bias_anova_effects))))

wb <- createWorkbook()
addWorksheet(wb, "a_bias_anova")
writeData(wb, sheet = "a_bias_anova", a_bias_anova_out)
addWorksheet(wb, "b_bias_anova")
writeData(wb, sheet = "b_bias_anova", b_bias_anova_out)
addWorksheet(wb, "D_bias_anova")
writeData(wb, sheet = "D_bias_anova", D_bias_anova_out)
addWorksheet(wb, "theta_bias_anova")
writeData(wb, sheet = "theta_bias_anova", theta_bias_anova_out)
saveWorkbook(wb, "./analysis/bias_anova_results.xlsx", overwrite = TRUE)




a_MSE_anova <- lm(`a-params` ~ rho + PREF + mu + alpha +
                     rho*PREF + rho*mu + rho*alpha + 
                     PREF*mu + PREF*alpha + 
                     mu*alpha +
                     rho*PREF*mu + rho*PREF*alpha + rho*mu*alpha + 
                     PREF*mu*alpha +
                     rho*PREF*mu*alpha, data = big_MSE_out)
a_MSE_anova_results <- as.data.frame(anova(a_MSE_anova)) %>% 
  tibble::rownames_to_column("Predictor")
a_MSE_anova_effects <- as.data.frame(lsr::etaSquared(a_MSE_anova))

a_MSE_anova_out <- cbind(a_MSE_anova_results, 
                          rbind(a_MSE_anova_effects, rep(NA, ncol(a_MSE_anova_effects))))

b_MSE_anova <- lm(`b-params` ~ rho + PREF + mu + alpha +
                     rho*PREF + rho*mu + rho*alpha + 
                     PREF*mu + PREF*alpha + 
                     mu*alpha +
                     rho*PREF*mu + rho*PREF*alpha + rho*mu*alpha + 
                     PREF*mu*alpha +
                     rho*PREF*mu*alpha, data = big_MSE_out)
b_MSE_anova_results <- as.data.frame(anova(b_MSE_anova)) %>% 
  tibble::rownames_to_column("Predictor")
b_MSE_anova_effects <- as.data.frame(lsr::etaSquared(b_MSE_anova))

b_MSE_anova_out <- cbind(b_MSE_anova_results, 
                          rbind(b_MSE_anova_effects, rep(NA, ncol(b_MSE_anova_effects))))

D_MSE_anova <- lm(`D-params` ~ rho + PREF + mu + alpha +
                     rho*PREF + rho*mu + rho*alpha + 
                     PREF*mu + PREF*alpha + 
                     mu*alpha +
                     rho*PREF*mu + rho*PREF*alpha + rho*mu*alpha + 
                     PREF*mu*alpha +
                     rho*PREF*mu*alpha, data = big_MSE_out)
D_MSE_anova_results <- as.data.frame(anova(D_MSE_anova)) %>% 
  tibble::rownames_to_column("Predictor")
D_MSE_anova_effects <- as.data.frame(lsr::etaSquared(D_MSE_anova))

D_MSE_anova_out <- cbind(D_MSE_anova_results, 
                          rbind(D_MSE_anova_effects, rep(NA, ncol(D_MSE_anova_effects))))

theta_MSE_anova <- lm(`thetas` ~ rho + PREF + mu + alpha +
                         rho*PREF + rho*mu + rho*alpha + 
                         PREF*mu + PREF*alpha + 
                         mu*alpha +
                         rho*PREF*mu + rho*PREF*alpha + rho*mu*alpha + 
                         PREF*mu*alpha +
                         rho*PREF*mu*alpha, data = big_MSE_out)
theta_MSE_anova_results <- as.data.frame(anova(theta_MSE_anova)) %>% 
  tibble::rownames_to_column("Predictor")
theta_MSE_anova_effects <- as.data.frame(lsr::etaSquared(theta_MSE_anova))

theta_MSE_anova_out <- cbind(theta_MSE_anova_results, 
                              rbind(theta_MSE_anova_effects, rep(NA, ncol(theta_MSE_anova_effects))))

wb <- createWorkbook()
addWorksheet(wb, "a_MSE_anova")
writeData(wb, sheet = "a_MSE_anova", a_MSE_anova_out)
addWorksheet(wb, "b_MSE_anova")
writeData(wb, sheet = "b_MSE_anova", b_MSE_anova_out)
addWorksheet(wb, "D_MSE_anova")
writeData(wb, sheet = "D_MSE_anova", D_MSE_anova_out)
addWorksheet(wb, "theta_MSE_anova")
writeData(wb, sheet = "theta_MSE_anova", theta_MSE_anova_out)
saveWorkbook(wb, "./analysis/MSE_anova_results.xlsx", overwrite = TRUE)
