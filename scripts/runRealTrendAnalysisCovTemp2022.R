#############Look at inidivual brook trout info##########################################
library(mapShen)
library(plotHacks)
library(tidyr)
library(reshape2)
library(jagsUI)

minNumYears<-2
firstYear<-1996
for(age in 0:1){

sites<-aqData("sites") %>%
  setkey("SiteID") %>%
  .[InPark==1]

wsAttr<-fread("C:/Users/echildress/Documents/mapShen/data/wsArea.csv") %>%
  .[SiteID=="1F210",pctBasa:=pctBasa+pctCarb] %>%
  .[SiteID!="1F210"&pctCarb>0,pctSili:=pctSili+pctCarb] %>%
  setkey(SiteID)


sites<-wsAttr[sites]

siteVisits<-aqData("siteVisits") %>%
  setkey(SiteVisit_ID) %>%
  .[SiteID %in% sites$SiteID & FishSampleType %in% c("QUANT","QUAL")] %>%
  .[sDate>as.POSIXct(paste0(firstYear,"-01-01"))] %>%
  .[!SiteID %in% c("ROSE","Rapidan")] %>%
  .[SiteID!="2F035"|sDate!=as.POSIXct("2002-09-23")] %>%
  .[SiteID!="3F262"] %>%
  .[,.(SiteVisit_ID,sDate,SiteID,FishSampleType)]

width<-aqData("habitat") %>%
  .[,.(width=mean(Width_m,na.rm=T),
       siteLength=as.numeric(max(CX))),.(SiteID,sDate)] %>%
  setkey(SiteID,sDate)


game<-aqData("game") %>%
  .[SiteVisit_ID %in% siteVisits$SiteVisit_ID] %>%
  .[TL>=131,AGE:=1] %>%
  .[,.(SiteID,sDate,AGE,RUN,SPECIES,TL)] %>%
  .[SiteID=="2F244"&sDate==as.POSIXct("2004-09-27"),AGE:=0] %>%
  setkey(SiteID,sDate,AGE,RUN)

nongame<-aqData("nongame") %>%
  .[SiteVisit_ID %in% siteVisits$SiteVisit_ID] %>%
  .[,.(SiteID,sDate,RUN)] %>%
  unique() %>%
  setkey(SiteID,sDate,RUN)

runs<-unique(rbind(nongame[,.(SiteID,sDate,RUN)],game[,.(SiteID,sDate,RUN)])) %>%
  .[RUN %in% 1:3] %>%
  crossing(AGE=0:1) %>%
  data.table() %>%
  setkey(SiteID,sDate,AGE,RUN)

game<-game[SPECIES=="BKT"] %>%
  .[,SPECIES:=NULL]

#temporarily assign ages that were not assigned in 2006
sites2006<-game[year(sDate)==2006,unique(SiteID)]
ageAssign2006<-data.table(SiteID=sites2006,
                          TL_Age0=c(80,94,90,90,76,85,85,79,72,77,65,85,89,84,79,91,85,90,
                                    90,93,95,90,83,75,72,88,89,85,86,82,74,76,96,71,81,87,
                                    91,63,87,63,80,80,56,71,87,76,76,90,81,69,95,96,68,54,
                                    75,63,71,71,51,80,73,71,69,61,77,85,48,76))
ageAssign2006<-ageAssign2006[SiteID %in% c("1F219","2F238","2F240","2F245","2F250","2F251","3F259","3F261")]

for(s in ageAssign2006$SiteID){
  game[!AGE %in% c(0,1) & year(sDate)==2006 & SiteID==s,
       AGE:=ifelse(TL<=ageAssign2006[SiteID==s,TL_Age0],0,1)]
}


runs<-runs[SiteID %in% game$SiteID] %>%
  .[,nYears:=length(unique(year(sDate))),SiteID] %>%
  .[nYears>=minNumYears] %>%
  .[,nYears:=NULL]

n<-game[,.N,.(SiteID,sDate,RUN,AGE)] %>%
  setkey(SiteID,sDate,AGE,RUN) %>%
  .[runs] %>%
  .[is.na(N),N:=0] %>%
  .[,year:=year(sDate)] %>%
  .[!duplicated(.[,.(SiteID,year,RUN,AGE)])]#one is duplicated in data, moormans was sampled twice in 1995, this drops the second one (post-flood)

# n<-n[!SiteID %in% flood]

n<-n[,siteNum:=as.numeric(as.factor(SiteID))] %>%
  .[,yearNum:=year-min(year)+1]


width<-width[n[,.(SiteID,sDate,siteNum,yearNum)] %>%
              unique() %>%
              setkey(SiteID,sDate)] %>%
  .[siteLength==0,siteLength:=NA] %>%
  .[,":="(meanWidth=mean(width,na.rm=T),meanLength=mean(siteLength,na.rm=T)),SiteID] %>%
  .[is.na(width),width:=meanWidth] %>%
  .[is.na(siteLength),siteLength:=meanLength] %>%
  .[!duplicated(cbind(siteNum,yearNum))]

missingYears<-(1:max(n$yearNum))[!1:max(n$yearNum) %in% n$yearNum]
n<-rbind(n,data.table(SiteID=n$SiteID[1],sDate=as.POSIXct(NA),RUN=1,AGE=1,N=NA,
                      year=NA,siteNum=1,yearNum=missingYears))
n<-addCatchment(n)
n[,watershedNum:=as.numeric(as.factor(catchment))]

trendOut<-unique(n[,.(SiteID,siteNum,watershedNum,catchment)]) %>%
  setkey(SiteID)

trendOut<-sites[,.(SiteID,Elev_m,majGeol,Aspect_deg,STREAM,Lon_n83,Lat_n83,pctSili,pctBasa,pctGran,wsArea)] %>%
  .[trendOut] %>%
  setkey(siteNum)

totalCatch<-n[,.(catch=sum(N)),.(SiteID,sDate,AGE,siteNum,yearNum)] %>%
  setkey(siteNum,yearNum,AGE)

nArray<-melt(n,id.vars=c("siteNum","yearNum","AGE","RUN"),measure.vars="N") %>%
  acast(siteNum~yearNum~RUN~AGE)

catch<-apply(nArray,c(1,2),sum,na.rm=T)
noSample<-apply(nArray,c(1,2),function(x){all(is.na(x))})
catch[noSample]<-NA
endsIn0<-apply(catch,1,function(x){x[max(which(!is.na(x)))]==0})
last0<-apply(catch,1,function(x){max(which(!is.na(x)))})[endsIn0]
first0<-apply(catch[endsIn0,],1,
              function(x){
                lastCatch<-max(which(x!=0&!is.na(x)))
                min(which(x==0&(1:ncol(catch))>lastCatch))})
first0<-first0[first0 != last0]
for(i in 1:length(first0)){
  nArray[as.numeric(names(first0)[i]),(first0[i]+1):ncol(catch),,]<-NA
}

rm(list=c("catch","noSample","endsIn0","last0","first0"))

siteYear<-unique(n[,.(siteNum,yearNum)])


pCov<-melt(rbind(width[,.(width,siteNum,yearNum)],
                 data.table(width=NA,siteNum=1,yearNum=missingYears)),
           id.vars=c("siteNum","yearNum")) %>%
  acast(siteNum~yearNum)
for(r in 1:nrow(pCov)){
  pCov[r,is.na(pCov[r,])]<-mean(pCov[r,],na.rm=T)
}

siteLength<-melt(rbind(width[,.(siteLength,siteNum,yearNum)],
                       data.table(siteLength=NA,siteNum=1,yearNum=missingYears)),
                 id.vars=c("siteNum","yearNum")) %>%
  acast(siteNum~yearNum)
siteLength[is.na(siteLength)]<-100

nInit<-apply(nArray[,,,age+1],c(1,2),sum,na.rm=T)+1

init=function() list(N=nInit)

#Structures for draws from posterior for Bayesian P to avoid reps that were not sampled
sampleYears<-apply(nArray[,,1,age+1],1,function(x){a<-which(!is.na(x)); c(a,rep(NA,ncol(nArray)-length(a)))}) %>% t()
nSampleYears<-apply(nArray[,,1,age+1],1,function(x){sum(!is.na(x))})

flood<-c("2F072","2F074","2F093","3F044","3F045","3F084","3F079")

jagsData<-list(y=nArray[,,,age+1],
               # nRow=dim(nArray)[1],
               nYears=max(siteYear$yearNum),
               nSites=max(siteYear$siteNum),
               # site=siteYear$siteNum,
               year=(1:max(siteYear$yearNum)-mean(1:max(siteYear$yearNum)))/sd(1:max(siteYear$yearNum)),
               pCov=(pCov-mean(pCov))/sd(pCov),
               sampleYears=sampleYears,
               nSampleYears=nSampleYears,
               watershed=trendOut$watershedNum,
               nWatersheds=max(trendOut$watershedNum),
               siteLength=siteLength,
               pctSili=trendOut$pctSili,
               pctBasa=trendOut$pctBasa,
               pctGran=trendOut$pctGran,
               elev=as.numeric(scale(trendOut$Elev_m)),
               wsArea=as.numeric(scale(log(trendOut$wsArea))),
               flood=as.numeric(trendOut$SiteID %in% flood))

n.chains = 3  		# number of chains
n.adapt = 2000		# number of sampler tuning iterations
n.burnin = 20000 # number of iterations to discard
n.iter = 220000	  # total iterations
thin = 200	  # number to thin by

model <- "scripts/trendModelRanWatershedCov3.r"

pars.to.save <- c("mu","site","sd.year","sigma","sd.site",
                  "p.mu","p.b","trendMu","trendSd","trendWsSd","sd.p.site",
                  "betaElev","betaWsArea","betaElevArea","betaFlood","betaTrendFlood",
                  "betaTrendElev","betaTrendWsArea","betaTrendElevArea",
                  "trendWs","trend","year.ran","site.ran","eps",
                  "N","yRep","siteEffect","sd.ws","wsRan")

out<-jagsUI(data=jagsData,inits=init,parameters.to.save=pars.to.save,model.file=model,
             n.iter=n.iter,n.thin=thin,n.chains=n.chains,n.adapt=NULL,n.burnin=n.burnin,
             parallel=T)

saveRDS(out,paste0("results/realTrendOutCov",minNumYears,"Year",firstYear,"Start",age,".rds"))
}
out<-readRDS("results/realTrendOutCov3Year1.rds")

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
