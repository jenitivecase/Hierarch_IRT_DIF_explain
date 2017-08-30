#### SETUP ####
options(scipen = 999)
date <- format.Date(Sys.Date(), "%Y%m%d")

work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/combined_dissertation_results"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  work_dir <- gsub("jbrussow", "Jen", work_dir)
  setwd(work_dir)
} else if(Sys.info()["user"] == "jennifer.brussow"){
  work_dir <- gsub("jbrussow", "jennifer.brussow", work_dir)
  setwd(work_dir)
}


files <- list.files(work_dir)

files <- grep("0-9alpha", files, value = TRUE)

for(i in 1:length(files)){
  file.rename(files[i], gsub("0-9alpha", "0-90alpha", files[i]))
}
