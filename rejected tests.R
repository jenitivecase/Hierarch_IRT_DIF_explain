#### BUGS CODE ####



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
