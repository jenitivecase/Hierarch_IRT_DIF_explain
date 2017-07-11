work_dir <- "C:/Users/jbrussow/Dropbox/REMS/11 Comps/Simulation/Cluster_stuff"

if(Sys.info()["user"] == "jbrussow"){
  setwd(work_dir)
} else if (Sys.info()["user"] == "Jen"){
  setwd(gsub("jbrussow", "Jen", work_dir))
}

conditions <- read.csv("conditions.csv")

# library(portableParallelSeeds)
# seeds <- seedCreator(nrow(conditions), (ncol(conditions)-1), seed = "72714", file = "clusterseeds.rds")

master_script <- NULL

for(i in 1:nrow(conditions)){
  rho <- conditions[i, "rho"]
  P_REF <- conditions[i, "P_REF"]
  mu2 <- conditions[i, "mu2"]
  alpha <- conditions[i, "alpha"]
  
  name <- paste0("IRT_DIF_rho", gsub("\\.", "", rho), 
                 "_pref", gsub("\\.", "", P_REF), 
                 "_mu", gsub("\\.", "", mu2), 
                 "_alpha", gsub("\\.", "", alpha))
  
  args <- paste0("rho=", rho, 
                 " P_REF=", P_REF, " mu2=", mu2,
                 " alpha=", alpha)
  cat("#MSUB -N ",  name, "
#MSUB -l nodes=1:ppn=3,walltime=60:00:00:00
#MSUB -l pmem=12gb
#MSUB -M jbrussow@ku.edu
#MSUB -m abe 
#MSUB -j oe 
#MSUB -q crmda
#MSUB -o ", name, ".log


cd $PBS_O_WORKDIR

R --vanilla -f cluster_base_script.R --args ", args,
      sep = "", file = paste0(name, "_job.sh"))
  
  submission <- paste0(name, "_job.sh")
  master_script <- rbind(master_script, submission)
}

cat("submissions=\'",master_script,"\'
for submission in $submissions
do
  msub $submission
done", sep = " ",
    file="master_script.sh")
