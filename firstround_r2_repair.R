setwd("F:/Comps simulation results 20170302/All_simulation_results_20170224-0306/combined")

files <- grep("correlations", list.files(getwd()), value = TRUE)

for(file in 1:length(files)){
  data <- readRDS(files[[file]])
  
  for(i in 1:length(data)){
    data[[i]]$R2_diff <- -(data[[i]]$R2_diff)
  }
  
  saveRDS(data, paste0("new", files[[file]]))
}

