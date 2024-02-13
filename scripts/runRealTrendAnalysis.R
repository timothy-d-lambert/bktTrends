#############Look at inidivual brook trout info##########################################
library(mapShen)
library(plotHacks)
library(tidyr)
library(reshape2)
library(jagsUI)

sites<-aqData("sites") %>%
  setkey("SiteID") %>%
  .[InPark==1]

geolAdd<-data.table(SiteID=c("1F219","1F234","1F231","1F233","1F236","2F221","2F222","2F242",
              "2F244","2F247","2F255","3F206","3F259","3F261"),
              MAJ_GEOL=c("Basaltic","Basaltic","Basaltic","Basaltic","Basaltic","Granitic","Granitic","Granitic","Granitic",
                "Basaltic","Granitic","Basaltic","Siliciclastic","Granitic"))
for(s in geolAdd$SiteID){
  sites[SiteID==s,MAJ_GEOL:=geolAdd[SiteID==s,MAJ_GEOL]]
}              

siteVisits<-aqData("siteVisits") %>%
  setkey(SiteVisit_ID) %>%
  .[SiteID %in% sites$SiteID & FishSampleType %in% c("QUANT","QUAL")] %>%
  .[sDate>as.POSIXct("1960-01-01")]

width<-aqData("habitat") %>%
  .[,.(width=mean(Width_m,na.rm=T),
       siteLength=as.numeric(max(CX))),.(SiteID,sDate)] %>%
  setkey(SiteID,sDate)


game<-aqData("game") %>%
  .[SiteVisit_ID %in% siteVisits$SiteVisit_ID] %>%
  .[TL>=131,AGE:=1] %>%
  setkey(SiteID,sDate,AGE,RUN)

nongame<-aqData("nongame") %>%
  .[SiteVisit_ID %in% siteVisits$SiteVisit_ID] %>%
  setkey(SiteID,sDate,RUN)

runs<-unique(rbind(nongame[,.(SiteID,sDate,RUN)],game[,.(SiteID,sDate,RUN)])) %>%
  .[RUN %in% 1:3] %>%
  crossing(AGE=0:1) %>%
  data.table() %>%
  setkey(SiteID,sDate,AGE,RUN)

game<-game[SPECIES=="BKT"] %>%
  .[,SPECIES:=NULL]

runs<-runs[SiteID %in% game$SiteID] %>%
  .[,nYears:=length(unique(year(sDate))),SiteID] %>%
  .[nYears>=4] %>%
  .[,nYears:=NULL]

# runs<-runs[SiteID %in% game$SiteID] %>%
#   .[,nYears:=length(unique(sDate)),SiteID] %>%
#   .[nYears>=2] %>%
#   .[,nYears:=NULL]

n<-game[,.N,.(SiteID,sDate,RUN,AGE)] %>%
  setkey(SiteID,sDate,AGE,RUN) %>%
  .[runs] %>%
  .[is.na(N),N:=0] %>%
  .[,year:=year(sDate)] %>%
  .[!duplicated(.[,.(SiteID,year,RUN,AGE)])]#one is duplicated in data, moormans was sampled twice in 1995, this drops the second one (post-flood)

# n[,meanWidth:=mean(width,na.rm=T),SiteID]
# n[is.na(width),width:=meanWidth]
# n<-n[!is.na(width)]

# n<-n[!SiteID %in% flood]

n<-n[,siteNum:=as.numeric(as.factor(SiteID))] %>%
  .[,yearNum:=year-min(year)+1]


width<-width[n[,.(SiteID,sDate,siteNum,yearNum)] %>%
              unique() %>%
              setkey(SiteID,sDate)] %>%
  .[siteLength==0,siteLength:=NA] %>%
  .[,":="(meanWidth=mean(width,na.rm=T),meanLength=mean(siteLength,na.rm=T)),SiteID] %>%
  .[is.na(width),width:=meanWidth] %>%
  .[is.na(siteLength),siteLength:=meanLength]

missingYears<-(1:max(n$yearNum))[!1:max(n$yearNum) %in% n$yearNum]
n<-rbind(n,data.table(SiteID="1F001",sDate=as.POSIXct(NA),RUN=1,AGE=1,N=NA,
                      year=NA,siteNum=1,yearNum=missingYears))
n<-addCatchment(n)
n[,watershedNum:=as.numeric(as.factor(catchment))]

trendOut<-unique(n[,.(SiteID,siteNum,watershedNum)]) %>%
  setkey(SiteID)

trendOut<-sites[,.(SiteID,Elev_m,MAJ_GEOL,Aspect_deg,STREAM,Lon_n83,Lat_n83,PCT_SILI,PCT_BASA,PCT_GRAN,WS_AREA_ha)] %>%
  .[trendOut] %>%
  setkey(siteNum)

totalCatch<-n[,.(catch=sum(N)),.(SiteID,sDate,AGE,siteNum,yearNum)] %>%
  setkey(siteNum,yearNum,AGE)

nArray<-melt(n,id.vars=c("siteNum","yearNum","AGE","RUN"),measure.vars="N") %>%
  acast(siteNum~yearNum~RUN~AGE)

# outSum<-n[,.(catch=sum(N)),.(SiteID,sDate,AGE,siteNum,yearNum)] %>%
#   melt(id.vars=c("SiteID","sDate","AGE","siteNum","yearNum")) %>%
#   dcast.data.table(SiteID+sDate+siteNum+yearNum~AGE) %>%
#   setnames(c("0","1"),c("catch0","catch1"))

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

nInit<-apply(nArray[,,,2],c(1,2),sum,na.rm=T)+1

init=function() list(N=nInit)

#Structures for draws from posterior for Bayesian P to avoid reps that were not sampled
sampleYears<-apply(nArray[,,1,2],1,function(x){a<-which(!is.na(x)); c(a,rep(NA,ncol(nArray)-length(a)))}) %>% t()
nSampleYears<-apply(nArray[,,1,2],1,function(x){sum(!is.na(x))})

jagsData<-list(y=nArray[,,,2],
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
               siteLength=siteLength)

n.chains = 3  		# number of chains
n.adapt = 2000		# number of sampler tuning iterations
n.burnin = 50000 # number of iterations to discard
n.iter = 200000	  # total iterations
thin = 150				  # number to thin by

model <- "scripts/trendModelRanWatershed.r"

pars.to.save <- c("mu","site","sd.year","sigma","sd.site",
                  "p.mu","p.b","trendMu","trendSd","trendWsSd","sd.p.site",
                  "trendWs","trend","year.ran","site.ran","eps",
                  "N","yRep")

out<-jagsUI(data=jagsData,inits=init,parameters.to.save=pars.to.save,model.file=model,
             n.iter=n.iter,n.thin=thin,n.chains=n.chains,n.adapt=NULL,n.burnin=n.burnin,
             parallel=T)

saveRDS(out,"results/realTrendOutRanWatershed.rds")

# out<-readRDS("results/realTrendOutRanWatershed.rds")

trendOut[,":="(trendMean=out$mean$trend,
               trend2.5=out$q2.5$trend,
               trend97.5=out$q97.5$trend,
               trendF=out$f$trend,
               siteMean=out$mean$site.ran,
               site2.5=out$q2.5$site.ran,
               site97.5=out$q97.5$site.ran,
               nYears=apply(nArray[,,1,1],c(1),function(x){sum(!is.na(x))}))]

watershedOut<- data.table(watershedNum=1:length(out$mean$trendWs))
watershedOut[,":="(trendMean=out$mean$trendWs,
                    trend2.5=out$q2.5$trendWs,
                    trend97.5=out$q97.5$trendWs,
                    trendF=out$f$trendWs)]
watershedOut<-unique(n[,.(watershedNum,catchment)]) %>%
  setkey(watershedNum) %>%
  .[watershedOut]

flood<-c("2F072","2F074","2F093","3F044","3F045","3F084","3F079")


setkey(trendOut,SiteID)
trendOut<-width[,unique(meanWidth),SiteID] %>%
  setkey(SiteID) %>%
  .[trendOut]

saveRDS(trendOut,"results/realTrendResults.rds")

tiff.par("figures/bktTrendMap.tif")
mapShenStreams()
mapAqSites(siteId=trendOut$SiteID,bg=ifelse(trendOut$trendMean>0,"blue","red"),cex=abs(trendOut$trendMean)*5,pch=21,
           lwd=2,col=ifelse(trendOut$trendF>=0.975,"yellow",
                            ifelse(trendOut$trendF>=0.9,"orange","gray")))
dev.off()
# 
# plot(out$mean$yRep[,,1]~jagsData$y[,,1])
# abline(0,1)
# mean(out$mean$yRep[,,1]>jagsData$y[,,1],na.rm=T)
# 
# plot(density(out$mean$yRep[,,1][!is.na(out$mean$yRep[,,1])]),col='red')
# for(i in sample(1:dim(out$sims.list$yRep)[1],500)){
#   points(density(out$sims.list$yRep[i,,,1],na.rm=T),type='l',col=gray(0.3,0.3))
# }
# points(density(out$mean$yRep[,,1],na.rm=T),type='l',col='red',lwd=2)
# points(density(jagsData$y[,,1],na.rm=T),type='l',col='blue',lwd=2)
# 
# 
# plot(out$mean$yRep[,,2]~jagsData$y[,,2])
# abline(0,1)
# mean(out$mean$yRep[,,2]>jagsData$y[,,2],na.rm=T)
# 
# plot(density(out$mean$yRep[,,2][!is.na(out$mean$yRep[,,2])]),col='red')
# for(i in sample(1:dim(out$sims.list$yRep)[1],500)){
#   points(density(out$sims.list$yRep[i,,,2],na.rm=T),type='l',col=gray(0.3,0.3))
# }
# points(density(out$mean$yRep[,,2],na.rm=T),type='l',col='red',lwd=2)
# points(density(jagsData$y[,,2],na.rm=T),type='l',col='blue',lwd=2)
# 
# 
# 
# plot(out$mean$yRep[,,3]~jagsData$y[,,3])
# abline(0,1)
# mean(out$mean$yRep[,,3]>jagsData$y[,,3],na.rm=T)
# 
# plot(density(out$mean$yRep[,,3][!is.na(out$mean$yRep[,,3])]),col='red')
# for(i in sample(1:dim(out$sims.list$yRep)[1],500)){
#   points(density(out$sims.list$yRep[i,,,3],na.rm=T),type='l',col=gray(0.3,0.3))
# }
# points(density(out$mean$yRep[,,3],na.rm=T),type='l',col='red',lwd=2)
# points(density(out$q50$yRep[,,3],na.rm=T),type='l',col='red',lwd=2,lty=2)
# points(density(jagsData$y[,,3],na.rm=T),type='l',col='blue',lwd=2)

# plot(NA,ylim=c(-1,100),xlim=c(min(year(totalCatch$sDate),na.rm=T),max(year(totalCatch$sDate),na.rm=T)))
# boof<-NULL
# for(s in bla){
#   nPred<-out$mean$mu+out$mean$site.ran[s]+jagsData$year*out$mean$trend[s]
#   nPred<-exp(nPred)
#   opac<-0.2+(out$f$trend[s]>0.9)*0.4+(out$f$trend[s]>0.95)*0.4
#   points(nPred~I(min(year(totalCatch$sDate),na.rm=T):max(year(totalCatch$sDate),na.rm=T)),type='l',lwd=2,
#          col=ifelse(out$mean$trend[s]>0,rgb(0,0,1,opac),rgb(1,0,0,opac)))
#   boof<-c(boof,min(nPred)/max(nPred))
# }

trendOut[,":="(percChange=exp(trendMean/sd(1:max(siteYear$yearNum)))-1,
               percChange2.5=exp(trend2.5/sd(1:max(siteYear$yearNum)))-1,
               percChange97.5=exp(trend97.5/sd(1:max(siteYear$yearNum)))-1)]

setkey(trendOut,percChange)
plot(I(1:nrow(trendOut))~trendOut$percChange,
     pch=ifelse(trendOut$trendF>0.975,19,1),
     xlim=c(-0.15,0.12),yaxt='n',ylab="")
axis(2,at=1:nrow(trendOut),trendOut$SiteID,las=1)
with(trendOut,error.bar(percChange,1:nrow(trendOut),upper.y=percChange97.5,
                        upper.x=percChange97.5,lower.x=percChange2.5,x.bar = T,y.bar=F,
                        interval.type = "abs",lwd=ifelse(trendOut$trendF>0.975,2,1),
                        col=ifelse(trendOut$SiteID %in% flood,"blue","black")))
floodRow<-match(flood,trendOut$SiteID)
points(floodRow~trendOut$percChange[floodRow],col='blue',pch=ifelse(trendOut$trendF>0.975,19,1))
abline(v=0,lty=2)

trendOutLong<-trendOut[nYears<10]
setkey(trendOutLong,percChange)

plot(I(1:nrow(trendOutLong))~trendOutLong$percChange,
     pch=ifelse(trendOutLong$trendF>0.975,19,1),
     xlim=c(-0.15,0.12),yaxt='n',ylab="")
axis(2,at=1:nrow(trendOutLong),trendOutLong$SiteID,las=1)
with(trendOutLong,error.bar(percChange,1:nrow(trendOutLong),upper.y=percChange97.5,
                        upper.x=percChange97.5,lower.x=percChange2.5,x.bar = T,y.bar=F,
                        interval.type = "abs",lwd=ifelse(trendOutLong$trendF>0.975,2,1),
                        col=ifelse(trendOutLong$SiteID %in% flood,"blue","black")))
floodRow<-match(flood,trendOutLong$SiteID)
points(floodRow~trendOutLong$percChange[floodRow],col='blue',pch=ifelse(trendOutLong$trendF>0.975,19,1))
abline(v=0,lty=2)



s<-91
predYearSF<-(out$mean$mu+out$mean$site.ran[s]+out$mean$eps[s,]+out$mean$trend[s]*jagsData$year+out$mean$year.ran)
predSF<-(out$mean$mu+out$mean$site.ran[s]+out$mean$eps[s,]+out$mean$trend[s]*jagsData$year)
plot(predYearSF[siteYear[siteNum==s,yearNum]]~siteYear[siteNum==s,yearNum],pch=19,type='o',col='blue')
plot(predSF[siteYear[siteNum==s,yearNum]]~siteYear[siteNum==s,yearNum],pch=19,type='o',col='blue')
