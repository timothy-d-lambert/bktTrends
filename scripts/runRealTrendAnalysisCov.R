#############Look at inidivual brook trout info##########################################
library(mapShen)
library(plotHacks)
library(tidyr)
library(reshape2)
library(jagsUI)

sites<-aqData("sites") %>%
  setkey("SiteID") %>%
  .[InPark==1]

wsAttr<-fread("C:/Users/echildress/Documents/mapShen/data/wsArea.csv") %>%
  setkey(SiteID)

sites<-wsAttr[sites]

siteVisits<-aqData("siteVisits") %>%
  setkey(SiteVisit_ID) %>%
  .[SiteID %in% sites$SiteID & FishSampleType %in% c("QUANT","QUAL")] %>%
  .[sDate>as.POSIXct("1994-01-01")]

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
  .[nYears>=2] %>%
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

trendOut<-sites[,.(SiteID,Elev_m,majGeol,Aspect_deg,STREAM,Lon_n83,Lat_n83,pctSili,pctBasa,pctGran,wsArea)] %>%
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

flood<-c("2F072","2F074","2F093","3F044","3F045","3F084","3F079")

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
               siteLength=siteLength,
               pctSili=trendOut$pctSili,
               pctBasa=trendOut$pctBasa,
               pctGran=trendOut$pctGran,
               elev=as.numeric(scale(trendOut$Elev_m)),
               wsArea=as.numeric(scale(log(trendOut$wsArea))),
               flood=as.numeric(trendOut$SiteID %in% flood))

n.chains = 3  		# number of chains
n.adapt = 2000		# number of sampler tuning iterations
n.burnin = 50000 # number of iterations to discard
n.iter = 200000	  # total iterations
thin = 150				  # number to thin by

model <- "scripts/trendModelRanWatershedCov2.r"

pars.to.save <- c("mu","site","sd.year","sigma","sd.site",
                  "p.mu","p.b","trendMu","trendSd","trendWsSd","sd.p.site",
                  "betaElev","betaWsArea","betaElevArea","betaFlood","betaTrendFlood",
                  "betaTrendElev","betaTrendWsArea","betaTrendElevArea",
                  "trendWs","trend","year.ran","site.ran","eps",
                  "N","yRep")

out<-jagsUI(data=jagsData,inits=init,parameters.to.save=pars.to.save,model.file=model,
             n.iter=n.iter,n.thin=thin,n.chains=n.chains,n.adapt=NULL,n.burnin=n.burnin,
             parallel=T)

saveRDS(out,"results/realTrendOutCov.rds")

# out<-readRDS("results/realTrendOutCov.rds")

trendOut[,":="(trendMean=out$mean$trend,
               trend2.5=out$q2.5$trend,
               trend97.5=out$q97.5$trend,
               trendF=out$f$trend,
               siteRan=out$mean$site.ran,
               siteRan2.5=out$q2.5$site.ran,
               siteRan97.5=out$q97.5$site.ran,
               nYears=apply(nArray[,,1,1],c(1),function(x){sum(!is.na(x))}))]

# watershedOut<- data.table(watershedNum=1:length(out$mean$trendWs))
# watershedOut[,":="(trendMean=out$mean$trendWs,
#                     trend2.5=out$q2.5$trendWs,
#                     trend97.5=out$q97.5$trendWs,
#                     trendF=out$f$trendWs)]
# watershedOut<-unique(n[,.(watershedNum,catchment)]) %>%
#   setkey(watershedNum) %>%
#   .[watershedOut]


setkey(trendOut,SiteID)
trendOut<-width[,unique(meanWidth),SiteID] %>%
  setkey(SiteID) %>%
  .[trendOut]

trendOut[,siteMean:=siteRan+pctSili*out$mean$mu[1]+
           pctBasa*out$mean$mu[2]+pctGran*out$mean$mu[3]]

saveRDS(trendOut,"results/realTrendResultsCov.rds")



for(el in c(-1,1,4)){
  for(wa in c(-2,0,2)){
    if(wa==2&el>-1) next
    if(wa==0&el>1) next
    pred<-array(NA,dim=c(27,3))
    plot(NA,xlim=c(0,27),ylim=c(0,150),
         main=paste0("el = ",el,", wa = ",wa))
    legend(20,80,c("siliciclastic","basaltic","granitic"),lty=1,lwd=2,
           col=palette()[1:3],bty='n')
    for(g in 1:3){
      tr<-out$mean$trendMu[g]+out$mean$betaTrendElev*el+
        out$mean$betaTrendWsArea*wa+out$mean$betaTrendElevArea*el*wa
      pred[,g]<- exp(out$mean$mu[g]+out$mean$betaElev*el+
        out$mean$betaWsArea*wa+out$mean$betaElevArea*el*wa+
        tr*(1:27-mean(1:27))/sd(1:27))
      points(pred[,g]~I(1:27),type='l',col=palette()[g],lwd=2)
    }
  }
}

sList<-which(jagsData$elev>=1&jagsData$pctGran>0.75)

sNum<-81
pred<- exp(out$mean$site.ran[sNum]+
             out$mean$mu[1]*jagsData$pctSili[sNum]+
             out$mean$mu[2]*jagsData$pctBasa[sNum]+
             out$mean$mu[3]*jagsData$pctGran[sNum]+
             out$mean$trend[sNum]*(1:27-mean(1:27))/sd(1:27))

predYear<-exp(log(pred)+out$mean$year.ran)
predAll<-exp(log(predYear)+out$mean$eps[sNum,])

plot(catch~yearNum,data=totalCatch[siteNum==sNum&AGE==1])
points(pred~I(1:27),type='l')
points(predYear~I(1:27),pch=19)

plot(NA,xlim=range(jagsData$year),ylim=c(0,40))
for(q in c(0.25,0.5,0.75)){
pred<-out$mean$mu+
  (out$mean$betaTrendElev*qnorm(q,0,1)+
     out$mean$betaTrendWsArea*qnorm(q,0,1)+
     out$mean$betaTrendElevArea
     out$mean$trendMu[1])*jagsData$year
              
points(exp(pred)~jagsData$year,type='l',lwd=2,lty=c(1,2,3)[which(q==c(0.25,0.5,0.75))])
}


mapShenStreams()
mapAqSites(siteId=trendOut$SiteID,bg=ifelse(trendOut$trendMean>0,"blue","red"),cex=abs(trendOut$trendMean)*5,pch=21,
           lwd=2,col=ifelse(trendOut$trendF>=0.9,"yellow","gray"))


plot(out$mean$yRep[,,1]~jagsData$y[,,1])
abline(0,1)
mean(out$mean$yRep[,,1]>jagsData$y[,,1],na.rm=T)

plot(density(out$mean$yRep[,,1][!is.na(out$mean$yRep[,,1])]),col='red')
for(i in sample(1:dim(out$sims.list$yRep)[1],500)){
  points(density(out$sims.list$yRep[i,,,1],na.rm=T),type='l',col=gray(0.3,0.3))
}
points(density(out$mean$yRep[,,1],na.rm=T),type='l',col='red',lwd=2)
points(density(jagsData$y[,,1],na.rm=T),type='l',col='blue',lwd=2)


plot(out$mean$yRep[,,2]~jagsData$y[,,2])
abline(0,1)
mean(out$mean$yRep[,,2]>jagsData$y[,,2],na.rm=T)

plot(density(out$mean$yRep[,,2][!is.na(out$mean$yRep[,,2])]),col='red')
for(i in sample(1:dim(out$sims.list$yRep)[1],500)){
  points(density(out$sims.list$yRep[i,,,2],na.rm=T),type='l',col=gray(0.3,0.3))
}
points(density(out$mean$yRep[,,2],na.rm=T),type='l',col='red',lwd=2)
points(density(jagsData$y[,,2],na.rm=T),type='l',col='blue',lwd=2)



plot(out$mean$yRep[,,3]~jagsData$y[,,3])
abline(0,1)
mean(out$mean$yRep[,,3]>jagsData$y[,,3],na.rm=T)

plot(density(out$mean$yRep[,,3][!is.na(out$mean$yRep[,,3])]),col='red')
for(i in sample(1:dim(out$sims.list$yRep)[1],500)){
  points(density(out$sims.list$yRep[i,,,3],na.rm=T),type='l',col=gray(0.3,0.3))
}
points(density(out$mean$yRep[,,3],na.rm=T),type='l',col='red',lwd=2)
points(density(out$q50$yRep[,,3],na.rm=T),type='l',col='red',lwd=2,lty=2)
points(density(jagsData$y[,,3],na.rm=T),type='l',col='blue',lwd=2)

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

s<-91
predYearSF<-(out$mean$mu+out$mean$site.ran[s]+out$mean$eps[s,]+out$mean$trend[s]*jagsData$year+out$mean$year.ran)
predSF<-(out$mean$mu+out$mean$site.ran[s]+out$mean$eps[s,]+out$mean$trend[s]*jagsData$year)
plot(predYearSF[siteYear[siteNum==s,yearNum]]~siteYear[siteNum==s,yearNum],pch=19,type='o',col='blue')
plot(predSF[siteYear[siteNum==s,yearNum]]~siteYear[siteNum==s,yearNum],pch=19,type='o',col='blue')
