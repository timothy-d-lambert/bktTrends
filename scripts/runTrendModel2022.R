###final script for running model for 1996-2022 trend analysis
library(mapShen)
library(plotHacks)
library(tidyr)
library(reshape2)
library(jagsUI)

minNumYears<-2
firstYear<-1996

for(age in 0:1){

source("scripts/prepDataTrend2022.R")

model<-"scripts/trendModelRanWatershedCovGeoCat2.R"

out<-jagsUI(data=jagsData,inits=init,parameters.to.save=pars.to.save,model.file=model,
             n.iter=n.iter,n.thin=thin,n.chains=n.chains,n.adapt=NULL,n.burnin=n.burnin,
             parallel=T)

saveRDS(out,paste0("results/realTrendOutCovGeoCatTwo",age,".rds"))
}
