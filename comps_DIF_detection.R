if(Sys.info()["user"] == "jbrussow"){
  setwd("C:/Users/jbrussow/Dropbox/REMS/11 Comps/")
} else if (Sys.info()["user"] == "Jen"){
  setwd("C:/Users/Jen/Dropbox/REMS/11 Comps/")
}

source("functions.R")

options(scipen = 999)
library(R2OpenBUGS)
library(coda)
library(tidyr)
library(dplyr)
library(rstan)

#number of people
n_people <- 1000
#number of items
n_items <- 100
#number of items with DIF
n_DIF <- 5
#number of reps
nreps <- 100
#rho is the amount of DIF explained by the second-order factors
rho <- c(0.4, 0.6, 0.8)
#P_REF is the proportion of people in the reference group
P_REF <- c(0.5, 0.7, 0.9)

conditions <- expand.grid(n_people, rho, P_REF)

date <- format.Date(Sys.Date(), "%Y%m%d")


#simulate a set of items
item_test <- item_sim(n_items, n_DIF, b_mean = 0, b_sd = 1, a_mean = 1, a_sd = .5, 
         nodif_mean = 0, nodif_sd = 0.1, dif_mean = 1, dif_sd = 0.1)

#simulate a set of people's ability scores
ability_test <- ability_sim(n_people, P_REF = .8, ref_theta_mean = 0, ref_theta_sd = 1,
                            focal_theta_mean = -0.5, focal_theta_sd = 1)

# #get the responses for a single item
# response_test <- response_sim(ability_test[1,], item_test[1,])
# 
# #get responses for a single person to a set of items
# responseset_test <- person_sim(ability_test[1,], item_test)

#get responses for a set of people to a set of items
dataset <- one_dataset(ability_test, item_test)

#get values for the DIF predictor
DIFpredict <- DIF_predictor(item_test, rho = 0.4)

#set up grouping variable
group <- ability_test[,2]


b.dat <- list("n_people", "n_items", "dataset", "group", "DIFpredict")
b.par <- list("a", "theta", "b", "D", "beta0", "beta1", "var", "prec", "R2")

### DONE TO HERE ###

#do the analysis for one set of responses
time1 <- Sys.time()
analysis_test <- one_analysis(x = dataset_test, n_iter = 1000, n_burn = 300,
                              debug = FALSE)
BUGS_time <- Sys.time() - time1
print(BUGS_time)

#### ANALYSIS ####
# 
# b.dat <- list("n_people","n_items","dataset","group","DIFpredict")
# b.par <- list("a", "theta", "b", "D", "beta0", "beta1", "var", "prec", "R2")
# 
# dataset <- dataset_test
# OUT <- bugs(data=b.dat,inits=NULL,param=b.par, 
#             model.file="BUGScode.txt",n.chains=2, 
#             n.iter=1000, n.burn=500, n.thin=1,debug=TRUE)

#### STAN TEST ####
source("stan_scripts.R")

#test of full model
time1 <- Sys.time()
test <- stan(model_code = stancode, model_name = "stan_test", data = b.dat,
             iter = 1000, warmup = 300, chains = 2, verbose = FALSE, cores = 2)
Stan_time <- Sys.time() - time1
print(Stan_time)

params_summary <- summary(test, pars = c("a", "b", "D"),
                          probs = c(0.025, 0.25, 0.75, 0.975))$summary

params <- extract(test, pars = c("a", "b", "D"))

alpha <- colMeans(params$a)
delta <- colMeans(params$delta)
mu <- mean(params$mu)
H <- mean(params$H)


#testing stan with long-format data

#prep for reformatting
dataset <- as.data.frame(dataset)
names(dataset) <- paste0("Item", 1:ncol(dataset))
dataset$respondentid <- c(1:nrow(dataset))

#move to long format
dataset_long <- gather(dataset, key = respondentid, value = response)
names(dataset_long)[2] <- "itemid"

#joining group & DIFpredict
group <- as.data.frame(group)
group$respondentid <- c(1:nrow(group))

dataset_long <- left_join(dataset_long, group, by = "respondentid")

# DIFpredict <- as.data.frame(DIFpredict)
# DIFpredict$itemid <- c(paste0("Item", 1:nrow(DIFpredict)))

# dataset_long <- left_join(dataset_long, DIFpredict, by = "itemid")
# dataset_long$itemid <- as.numeric(gsub("Item", "", dataset_long$itemid))

respondentid <- dataset_long$respondentid
itemid <- dataset_long$itemid
response <- dataset_long$response
group <- dataset_long$group
# DIFpredict <- dataset_long$DIFpredict

n_observations <- nrow(dataset_long)

b.dat_long <- list("n_people", "n_items", "n_observations", "respondentid", 
                   "itemid", "response", "group", "DIFpredict")

#precompile the model script so it doesn't have to recompile each time
precomp <- stanc(model_code = stancode_long)
precomp_model <- stan_model(stanc_ret = precomp)

time1 <- Sys.time()
test <- sampling(precomp_model, data = b.dat_long,
             iter = 1000, warmup = 300, chains = 2, verbose = FALSE, cores = 2)
Stan_time_long <- Sys.time() - time1
print(Stan_time_long)








#turn intercepts into thresholds, save as DIF estimate
est_D_HGLM[k,,iRHO,iPROP] <- -OUT$mean$b[,3]/OUT$mean$b[,2]
est_coef[k,3:4,iRHO,iPROP] <- -OUT$mean$a[1:2]
est_R2[k,2,iRHO,iPROP] <- OUT$mean$R2


saveRDS(results, paste0(date, "_simulation_output.rds"))
