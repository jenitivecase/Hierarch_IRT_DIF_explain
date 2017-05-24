library(portableParallelSeeds)


conditions <- read.csv("conditions.csv")

for(i in 1:nrow(conditions)){
  rho <- conditions[i, "rho"]
  P_REF <- conditions[i, "P_REF"]
  mu2 <- conditions[i, "mu2"]
  alpha <- conditions[i, "alpha"]
  
  filename <- paste0("seeds_", gsub(".", "-", as.character(rho), fixed = TRUE), "rho_", 
                     gsub(".", "-", as.character(P_REF), fixed = TRUE), "PREF_", 
                     gsub(".", "-", as.character(mu2), fixed = TRUE), "mu_",
                     gsub(".", "-", as.character(alpha), fixed = TRUE), "alpha.rds")
  
  seedCreator(nReps = 100, streamsPerRep = 4, file = filename)
}
