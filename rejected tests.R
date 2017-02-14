#### FUNCTION TESTS ####
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



#### BUGS CODE ####
#analysis function with BUGS
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


#### STAN TEST ####
source("stan_scripts.R")

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
