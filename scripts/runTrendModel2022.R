###final script for running model for 1996-2022 trend analysis
library(shenAquatics) # previously mapShen
library(plotHacks)
library(tidyr)
library(reshape2)
library(jagsUI)
library(here) # Added by T. Lambert, 14 Mar 2024

here::i_am("scripts/runTrendModel2022.R") # declare file location and find project root

# Q: Are minNumYears and firstYear used anywhere? (Don't see them below, so no unless global variables are used within functions...)
minNumYears<-2
firstYear<-1996

for(age in 0:1) {
  
  ## Read in the data and MCMC parameters
  # source("scripts/prepDataTrend2022.R") # Version 1: Prepare the data directly from Microsoft Access database (currently not functional)
  source(here("scripts", "prepDataTrend2022_preProcessed.R")) # Version 2: from Evan's pre-processed data files
  
  ## USER INPUT: Define the model
  model.name <- c("RanWatershedCovGeoCat2", # TAFS -- full version
                  "NoCov")[2] # user defined integer
  # model<-"scripts/trendModelRanWatershedCovGeoCat2.R" # TAFS -- full version, explicitly defined
  model <- paste0("scripts/trendModel", model.name, ".R")
  
  out <- jagsUI(data = jagsData,
                inits = init,
                parameters.to.save = pars.to.save,
                model.file = model,
                n.iter = n.iter,
                n.thin = thin,
                n.chains = n.chains,
                n.adapt = NULL,
                n.burnin = n.burnin,
                parallel = T)
  
  # saveRDS(out, paste0("results/realTrendOutCovGeoCatTwo",age,".rds"))
  saveRDS(out, paste0("results/realTrendOut",model.name, ".", age,".rds"))
}
