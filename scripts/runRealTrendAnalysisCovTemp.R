#############Look at inidivual brook trout info##########################################
library(mapShen)
library(plotHacks)
library(tidyr)
library(reshape2)
library(jagsUI)
library(lme4)

minNumYears<-2
firstYear<-1996

for(age in 0:1){

  source("scripts/prepDataTrend2022.R")
  
  jagsData$elev<-as.numeric(scale(trendOut$siteTemp))
  
  model<-"scripts/trendModelRanWatershedNoCov.R"
    
  out<-jagsUI(data=jagsData,inits=init,parameters.to.save=pars.to.save,model.file=model,
               n.iter=n.iter,n.thin=thin,n.chains=n.chains,n.adapt=NULL,n.burnin=n.burnin,
               parallel=T)
  
  saveRDS(out,paste0("results/realTrendOutNoCov",age,".rds"))
}




out<-readRDS("results/realTrendOutNoCov1.rds")

trendOut[,":="(trendMean=out$mean$trend,
               trend2.5=out$q2.5$trend,
               trend97.5=out$q97.5$trend,
               trendF=out$f$trend,
               siteRan=out$mean$siteEffect,
               siteRan2.5=out$q2.5$siteEffect,
               siteRan97.5=out$q97.5$siteEffect,
               nYears=apply(nArray[,,1,1],c(1),function(x){sum(!is.na(x))}))]

# # watershedOut<- data.table(watershedNum=1:length(out$mean$trendWs))
# # watershedOut[,":="(trendMean=out$mean$trendWs,
# #                     trend2.5=out$q2.5$trendWs,
# #                     trend97.5=out$q97.5$trendWs,
# #                     trendF=out$f$trendWs)]
# # watershedOut<-unique(n[,.(watershedNum,catchment)]) %>%
# #   setkey(watershedNum) %>%
# #   .[watershedOut]
# 
# 
# setkey(trendOut,SiteID)
# trendOut<-width[,unique(meanWidth),SiteID] %>%
#   setkey(SiteID) %>%
#   .[trendOut]
# 
# trendOut[,siteMean:=siteRan+pctSili*out$mean$mu[1]+
#            pctBasa*out$mean$mu[2]+pctGran*out$mean$mu[3]]
# 
# saveRDS(trendOut,"results/realTrendResultsCov.rds")

# png.par("figures/ThorntonAndMeadow.png",res=400,
#          mfrow=c(1,2),mar=c(2,3.5,2,0.5),
#          height=4,col="gray80",fg="gray80",col.main="gray80")
# 
# plot(out$mean$N[19,which(!is.na(nArray[19,,1,2]))]~
#        I((1994:2022)[which(!is.na(nArray[19,,1,2]))]),
#      type='o',pch=19,xlab="",
#      ylab="",main="N.F. Thornton River")
# title(ylab="Adult Brook Trout (#/100 m)",line=2)
# 
# plot(out$mean$N[87,which(!is.na(nArray[87,,1,2]))]~
#        I((1994:2022)[which(!is.na(nArray[87,,1,2]))]),
#      type='o',pch=19,xlab="",
#      ylab="Adult Brook Trout (#/100 m)",
#      main="Meadow Run")
# 
# dev.off()
