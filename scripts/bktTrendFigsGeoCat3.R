library(mcr)

source("scripts/prepDataTrend2022.R")

outFiles<-paste0("results/realTrendOutCovGeoCatTwo",c(0,1),".rds")

#marginal effects plots for bkt trend analysis with immutable site characteristics as predictors

years<-min(n$year,na.rm=T):max(n$year,na.rm=T)

alph<-0.2
geolCols<-c(rgb(197,108,57,alph*255,maxColorValue = 255),
            rgb(111,73,48,alph*255,maxColorValue = 255),
            gray(0.7,alph))
geolCols2<-adjustcolor(geolCols,alpha.f=255)

for(a in 0:1){
  out<-readRDS(outFiles[a+1])
  trendOut[,paste0("trend",a):=out$mean$trend]
}

############Trend vs Temp##################################################
trendOut[,tempStd:=as.numeric(scale(siteTemp))]

png.par("figures/trendVsTemp.png",mfrow=c(3,1),bg="white",
        height=8,width=3.75,mar=c(2.5,3,0.5,0))
palette(geolCols2)

plot(tempStd~Elev_m,data=trendOut,col=as.factor(majGeol),pch=19,cex=1.5,
     ylab="Relative Stream Temperature",xlab="Elevation (m)")

for(a in 0:1){
  tempMod<-lm(get(paste0("trend",a))~majGeol*tempStd,data=trendOut[!SiteID %in% flood])
  predTemp<-seq(min(trendOut$tempStd),max(trendOut$tempStd),length.out=20)
  pred<-array(NA,dim=c(length(predTemp),3))
  for(g in 1:3){
    pred[,g]<-predict(tempMod,newdata=data.table(tempStd=predTemp,
                                           majGeol=c("basaltic","granitic","siliciclastic")[g]))
  }

  plot(get(paste0("trend",a))~tempStd,data=trendOut[!SiteID %in% flood],
      pch=19,cex=1.5,col=as.factor(majGeol),xlab="Relative Stream Temperature",
      ylab="")
  title(ylab=paste0("Trend in ",ifelse(a==0,"YOY","Adult")," Brook Trout Abundance"),line=2)
  for(g in 1:3){
    points(pred[,g]~predTemp,type='l',lwd=2,col=geolCols2[g])
  }
}
dev.off()





###############################################################
nftr<-trendOut[STREAM=="Thornton River, N. Fork",siteNum]
png.par("figures/northForkMeadowN.png",mfrow=c(1,2),
        col="white",cex=1.5,fg="white",mgp=c(2,0.5,0),
        mar=c(1.5,3,0,0),
        bg=rgb(168,145,101,maxColorValue = 255))
        # bg="yellow")
plot(NA,xlim=c(1996,2022),ylim=c(0,175),ylab="Adult Brook Trout per 100 m",
     xlab="")
for(s in nftr){
  points(out$mean$N[s,!is.na(nArray[s,,1,2])]~I((1996:2022)[!is.na(nArray[s,,1,2])]),
         type='l',lwd=3,col=geolCols2[which(s==nftr)])  
}

legend(2000,170,c("Lower","Middle","Upper"),lwd=3,col=geolCols2,bty='n')

mead<-trendOut[STREAM=="Meadow Run",siteNum]
plot(NA,xlim=c(1996,2022),ylim=c(0,175),ylab="",
     xlab="")
for(s in mead){
  points(out$mean$N[s,!is.na(nArray[s,,1,2])]~I((1996:2022)[!is.na(nArray[s,,1,2])]),
         type='l',lwd=3,col=geolCols2[c(1,3)][which(s==mead)])  
}
dev.off()
###############################################################


############Temporal trend by geology type#######################
out<-readRDS(outFiles[2])

plot(NA,xlim=range(n$year,na.rm=T),ylim=c(0,125),
     xlab="",ylab=bquote(Predicted~Adult~Brook~Trout~Abundance~(per~100~m)))

# for(r in 1:200){
#   s<-sample(1:nrow(out$sims.list$mu),1)
#   for(i in 1:3){
#     pred<-exp(out$sims.list$mu[s,i]+jagsData$year*out$sims.list$trendMu[s,i])
#     points(pred~years,
#            type='l',col=geolCols[i],lwd=1)
#   }
# }
for(i in 1:3){
  pred<-out$sims.list$mu[,i]+jagsData$year%o%out$sims.list$trendMu[,i]
  
  predHigh<-exp(apply(pred,1,function(x){quantile(x,probs=0.975)}))
  predLow<-exp(apply(pred,1,function(x){quantile(x,probs=0.025)}))
  predMean<-exp(apply(pred,1,mean))
  error.band(years,predMean,upper.y=predHigh,lower.y=predLow,interval.type = "abs",
             col=geolCols[i])
}

for(i in 1:3){
  pred<-out$sims.list$mu[,i]+jagsData$year%o%out$sims.list$trendMu[,i]
  predMean<-exp(apply(pred,1,mean))
  points(predMean~years,
         type='l',col=geolCols2[i],lwd=3)
}

legend(2009,110,c("Siliciclastic","Basaltic","Granitic"),
       lwd=3,col=geolCols2,bty='n')


#########Temporal trend by elevation by wsArea by geology type###############
#NOT UPDATED FOR TREND X ELEV X GEOL STRUCTURE

# elevArea<-list(siliciclastic=data.table(elev=c(350,350,350,500,500,650),
#                                         wsArea=c(3,15,30,3,15,3)),
#                basaltic=data.table(elev=c(350,350,350,500,500,650,650,900),
#                                    wsArea=c(3,15,30,3,15,3,15,3)),
#                granitic=data.table(elev=c(350,350,350,500,500,650,650,900),
#                                    wsArea=c(3,15,30,3,15,3,15,3)))
# elevFull<-c(350,500,650,900)
# wsAreaFull<-c(3,15,30)
# 
# for(g in 1:3){
#   par(mfrow=c(4,3))
#   for(el in elevFull){
#     for(wa in wsAreaFull){
#       if(!wa %in% elevArea[[g]][elev==el,wsArea]){
#         plot(1:10,pch=NA,axes=F,xlab="",ylab="")
#         next
#       }
#       
#       e<-(el-attributes(scale(trendOut$Elev_m))$'scaled:center')/
#         attributes(scale(trendOut$Elev_m))$'scaled:scale'
#       a<-(log(wa)-attributes(scale(log(trendOut$wsArea)))$'scaled:center')/
#         attributes(scale(log(trendOut$wsArea)))$'scaled:scale'
#       
#       pred<-out$sims.list$mu[,g]+out$sims.list$betaElev*e+out$sims.list$betaWsArea*a+
#         a*e*out$sims.list$betaElevArea+
#         t(jagsData$year %o% (out$sims.list$trendMu[,g]+e*out$sims.list$betaTrendElev+
#                                a*out$sims.list$betaTrendWsArea+a*e*out$sims.list$betaTrendElevArea))
#       
#       predHigh<-exp(apply(pred,2,function(x){quantile(x,probs=0.975)}))
#       predLow<-exp(apply(pred,2,function(x){quantile(x,probs=0.025)}))
#       predMean<-exp(apply(pred,2,mean))
#       
#       plot(NA,xlim=range(years),ylim=c(0,max(predHigh)),
#            xlab="",ylab=bquote(Predicted~Adult~Brook~Trout~Abundance~(per~100~m)),
#            main=paste0("Geol = ",c("sili","basa","gran")[g],
#                        "; Elev = ",el,
#                        "; wsArea = ",wa))
#       error.band(years,predMean,upper.y=predHigh,lower.y=predLow,interval.type = "abs",
#                  col=geolCols[g])
#       points(predMean~years,type='l',lwd=3,col=geolCols2[g])
#     }
#   }
# }

#########Trend est across elevation split by wsArea by geology type###############
#NOT UPDATED FOR TREND X ELEV X GEOL STRUCTURE

# getElevLimits<-function(g,wa){
#   
#   mg<-c("siliciclastic","basaltic","granitic")[g]
#   
#   hull<-trendOut[majGeol==mg][
#     chull(trendOut[majGeol==mg,Elev_m],
#           trendOut[majGeol==mg,log(wsArea)])]
#   hull<-rbind(hull,hull[1,])
#   hull[,":="(wsAreaShift=data.table::shift(wsArea),elevShift=data.table::shift(Elev_m))]
#   hull<-hull[wsArea<wa&wsAreaShift>wa|wsAreaShift<wa&wsArea>wa]
#   
#   elev<-hull[,.(Elev_m+(elevShift-Elev_m)/(log(wsAreaShift)-log(wsArea))*(log(wa)-log(wsArea)))]
#   c(min(elev),max(elev))
# }
# 
# overallElLim<-range(trendOut$Elev_m)
# 
# wsAreas<-c(3,20)
# png.par("figures/bktTrendElevSlicesGeoCat.png",mfcol=c(3,2),lend=1,
#         width=6.5,height=8,bg="white",mar=c(3,3,1,0.5))
# for(age in 0:1){
#   out<-readRDS(outFiles[age+1])
#   for(g in 1:3){
#     plot(NA,xlim=overallElLim,ylim= c(-15,10),
#          xlab="Elevation (m)",ylab=bquote(Predicted~Trend~("%"~Change~Year^-1)),
#          main=c("Siliciclastic","Basaltic","Granitic")[g])
#     for(wa in wsAreas){
#       elLim<-getElevLimits(g,wa)
#       el<-seq(elLim[1],elLim[2],length.out=100)
#       
#       e<-(el-attributes(scale(trendOut$Elev_m))$'scaled:center')/
#         attributes(scale(trendOut$Elev_m))$'scaled:scale'
#       a<-(log(wa)-attributes(scale(log(trendOut$wsArea)))$'scaled:center')/
#         attributes(scale(log(trendOut$wsArea)))$'scaled:scale'
#       
#       pred<-out$sims.list$trendMu[,g]+t(e%o%out$sims.list$betaTrendElev)+
#         a*out$sims.list$betaTrendWsArea
#       pred<-(exp(diff(jagsData$year)[1]*pred)-1)*100
#       
#       predHigh<-apply(pred,2,function(x){quantile(x,probs=0.975)})
#       predLow<-apply(pred,2,function(x){quantile(x,probs=0.025)})
#       predMean<-apply(pred,2,mean)
#       
#       
#       error.band(el,predMean,upper.y=predHigh,lower.y=predLow,interval.type = "abs",
#                  col=geolCols[which(wa==wsAreas)])
#       points(predMean~el,type='l',lwd=3,col=geolCols2[which(wa==wsAreas)])
#     }
#     abline(h=0,lty=2)
#     if(g==1&age==0){
#       legend(700,-5,c(as.expression(bquote(.(wsAreas[1])~km^2)),
#                       as.expression(bquote(.(wsAreas[2])~km^2))),
#              lwd=10,col=geolCols[1:2],bty='n',
#              title="Watershed Area")
#       legend(700,-5,c(as.expression(bquote(.(wsAreas[1])~km^2)),
#                       as.expression(bquote(.(wsAreas[2])~km^2))),
#              lwd=2,col=geolCols2[1:2],bty='n',
#              title="Watershed Area")
#     }
#   }
# }
# dev.off()

##########Contour plots of trends across elev/ws area parameter space#############
grains<-100
# trendCols<-colorRampPalette(c(colorRampPalette(c("red","red","white"))(4),"blue"))(grains)


trendScale<-seq(-1.4,1.19,length.out=grains)
trendScale<-seq(-0.8363,0.7565,length.out=grains)
trendCols<-colorRampPalette(c("red","white"))(which.min(abs(trendScale)))
# trendCols<-c(trendCols,
#              colorRampPalette(c(trendCols[length(trendCols)],
#                                 "blue"))(length(trendScale)-length(trendCols)))
trendCols<-c(trendCols,
             colorRampPalette(c(trendCols[length(trendCols)],
                                "blue"))(length(trendCols)))[1:grains] 



png.par("figures/bktTrendContoursGeoCat.png",mfcol=c(3,2),
        height=9,width=7.5,mar=c(3.5,4.5,1,0.5),bg='white',
        res=600)
for(ag in 0:1){
  out<-readRDS(outFiles[ag+1])
  for(g in 1:3){ 
    
    mg<-c("siliciclastic","basaltic","granitic")[g]
    
    el<- seq(243,951,length.out=grains)
    wa<- seq(0.26,3.59,length.out=grains)
    
    e<-(el-attributes(scale(trendOut$Elev_m))$'scaled:center')/
      attributes(scale(trendOut$Elev_m))$'scaled:scale'
    a<-(wa-attributes(scale(log(trendOut$wsArea)))$'scaled:center')/
      attributes(scale(log(trendOut$wsArea)))$'scaled:scale'
    
    predTrend<-array(NA,dim=c(grains,grains,nrow(out$sims.list$mu)))
    for(i in 1:grains){
      for(j in 1:grains){
        predTrend[i,j,]<-out$sims.list$trendMu[,g]+
          e[j]*(out$sims.list$betaTrendElev+
                  (g==2)*out$sims.list$betaTrendElevBasa+
                  (g==3)*out$sims.list$betaTrendElevGran)+
          a[i]*(out$sims.list$betaTrendWsArea+
            (g==2)*out$sims.list$betaTrendWsAreaBasa+
            (g==3)*out$sims.list$betaTrendWsAreaGran)
      }
    }
    
    trendMean<-apply(predTrend,c(1,2),mean)
    
    #create image of the trend across the parameter space
    image(wa,el,trendMean,col=trendCols,zlim=range(trendScale),main=mg,
          xaxt="n",ylab="",xlab="Watershed Area (ha)")
    title(ylab="Elevation (m)",line=2.5)
    axis(1,at=log(c(1,2,5,10,20)),labels=c(1,2,5,10,20))
    
    #define and cross hatch the non-significant trend area
    trendSig95<-apply(predTrend,c(1,2),
                      function(x){as.numeric(quantile(x,probs=0.025)<0) == 
                          as.numeric(quantile(x,probs=0.975)<0)})
    tp1<-tp2<-NULL
    for(i in 1:nrow(trendSig95)){
      if(all(trendSig95[i,])) next
      if(trendSig95[i,1]==F){
        tp1<-rbind(tp1,c(i,1))
      }
      if(trendSig95[i,ncol(trendSig95)]==F){
        tp2<-rbind(tp2,c(i,ncol(trendSig95)))
      }
      if(trendSig95[i,1]==T&any(!trendSig95[i,])){
        tp1<-rbind(tp1,c(i,min(which(!trendSig95[i,]))))
      }
      if(trendSig95[i,ncol(trendSig95)]==T&any(!trendSig95[i,])){
        tp2<-rbind(tp2,c(i,max(which(!trendSig95[i,]))))
      }
    }
    
    polygon(wa[c(tp1[,1],tp2[nrow(tp2):1,1])],el[c(tp1[,2],tp2[nrow(tp2):1,2])],
            density=8,border=NA,col='gray70',angle=105)
    polygon(wa[c(tp1[,1],tp2[nrow(tp2):1,1])],el[c(tp1[,2],tp2[nrow(tp2):1,2])],
            density=8,border=NA,col='gray70',angle=15)
    
    #add contour lines over crosshatching
    # contour(wa,el,trendMean,add=T,drawlabels = F,
    #         levels=contLocs)
    
    #define the unsampled area and make it gray
    
    hull<-trendOut[majGeol==mg][
      chull(trendOut[majGeol==mg,Elev_m],
            trendOut[majGeol==mg,log(wsArea)])]
    hullElev<-hullArea<-NULL
    for(h in 1:nrow(hull)){
      hullElev[h]<-which.min(abs(hull$Elev_m[h]-el))
      hullArea[h]<-which.min(abs(log(hull$wsArea[h])-wa))
    }
    
    polygon(c(par("usr")[2],log(hull$wsArea),log(hull$wsArea[1]),par("usr")[c(2,2,1,1,2)]),
            c(par("usr")[4],hull$Elev_m,hull$Elev_m[1],par("usr")[c(4,3,3,4,4)]),
            border=NA,col='gray')
    points(Elev_m~log(wsArea),data=trendOut[majGeol==mg],pch=ifelse(endsIn0,1,19),cex=1,col=gray(0.3))
    
    if(ag==0&g==1){
      y.leg<-seq(800,900,length.out=grains)
      for(i in 1:grains){
        points(c(2.8,3),rep(y.leg[i],2),type='l',
               col=trendCols[i],lend='butt',lwd=0.8)
      }
      
      
      text(3.2,800+which.min(abs(trendScale-log(0.9)/diff(jagsData$year)[1])),"-10%")
      # text(3.2,800+which.min(abs(trendScale-log(0.95)/diff(jagsData$year)[1])),"-5%")
      text(3.2,800+which.min(abs(trendScale))/grains*100,"0%")
      text(3.2,800+which.min(abs(trendScale-log(1.1)/diff(jagsData$year)[1])),"10%")
    }
    
    #text(contLabs[[ag+1]][[g]]$x,contLabs[[ag+1]][[g]]$y,contLabs[[ag+1]][[g]]$lab)
    
  }
}
dev.off()

######Site Level N Figs for YOY##############################


svg.par("figures/bktNEstBySite.svg",height=4*nrow(out$mean$N),width=10,
        mfcol=c(nrow(out$mean$N),2))
for(a in 0:1){
  out<-readRDS(outFiles[a+1])
  for(i in 1:nrow(out$mean$N)){
    plot(out$mean$N[i,][!is.na(jagsData$y[i,,1])]~I((2023-length(out$mean$N[1,])):2022)[!is.na(jagsData$y[i,,1])],
         ylim=c(0,max(out$q97.5$N[i,][!is.na(jagsData$y[i,,1])])),
         xlim=c(1994,2022))
    error.bar(c((2023-length(out$mean$N[1,])):2022)[!is.na(jagsData$y[i,,1])],
              out$mean$N[i,][!is.na(jagsData$y[i,,1])],
              upper.y=out$q97.5$N[i,][!is.na(jagsData$y[i,,1])],
              lower.y=out$q2.5$N[i,][!is.na(jagsData$y[i,,1])],interval.type = "abs")
    text(2008,max(out$q97.5$N[i,][!is.na(jagsData$y[i,,1])])*0.8,
         paste0(trendOut[siteNum==i,SiteID],"\n",
                trendOut[siteNum==i,STREAM],"\n",
                "Elev = ",trendOut[siteNum==i,Elev_m]," m\n",
                "wsArea = ",trendOut[siteNum==i,wsArea],"\n",
                "pctSili = ",trendOut[siteNum==i,pctSili],"\n",
                "pctBasa = ",trendOut[siteNum==i,pctBasa],"\n",
                "pctGran = ",trendOut[siteNum==i,pctGran],"\n"))
    pred<-exp(out$mean$siteEffect[i]+
                out$mean$trend[i]*jagsData$year)
    points(pred~I((2023-length(out$mean$N[1,])):2022),type='l',lty=ifelse(out$overlap0$trend[i],2,1))
  }
}
dev.off()

########Site Level Trend Lines on One Plot ############################
png.par("figures/bktTrendLineBySiteGeoCat.png",height=9,width=6.5,
        mfcol=c(3,2),bg="white")

for(a in 0:1){
  out<-readRDS(outFiles[a+1])
  
  for(g in c("siliciclastic","basaltic","granitic")){
    plot(NA,ylim=c(0,250),xlim=c(1994,2022)) 
    for(i in which(!trendOut$SiteID %in% flood & trendOut$majGeol==g)){
      pred<-exp(out$mean$siteEffect[i]+
                  out$mean$trend[i]*jagsData$year)
      points(pred~I((2023-length(out$mean$N[1,])):2022),type='l',lty=ifelse(out$overlap0$trend[i],2,1),lwd=2,
             col=geolCols2[which(c("siliciclastic","basaltic","granitic")==trendOut$majGeol[i])])
    }
  }
}
dev.off()

######YOY vs adult trend #########################################
out<-readRDS(outFiles[1])
yoyTrend<-out$mean$trend
yoySig<-!out$overlap0$trend
out<-readRDS(outFiles[2])
adultTrend<-out$mean$trend
adultSig<-!out$overlap0$trend

trendLm<-mcreg(adultTrend,yoyTrend,method.reg="Deming")@para

grayC<-gray(0.6,1)

png.par("figures/yoyVsAdultTrendGeoCat.png",res=400,bg='white',
        mar=c(2.5,3,0.5,0.5),width=3.5,height=3.5)
plot(yoyTrend~adultTrend,pch=21,col=ifelse(adultSig,"black",grayC),
     bg=ifelse(yoySig,"black",grayC),cex=1,lwd=2,
     ylab="",xlab="Adult Trend",xlim=c(-1.3,0.85),ylim=c(-1.3,0.85))
title(ylab="Young-Of-Year Trend",line=2)
abline(0,1,lty=2)
# legend(-1.3,0.8,c("Signicant","Non-signicant","YOY","Adult"),
#        pch=c(16,16,16,1),col=c("black",grayC,"black","black"),
#        bty='n',pt.cex=c(0.8,0.8,0.8,1),pt.lwd=c(1,1,1,2))
dev.off()


###########Detection Figure######################################

for(a in 0:1){
  out<-readRDS(outFiles[a+1])
  
  pred<-out$sims.list$p.mu+t(seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100)%o%
                               out$sims.list$p.b)
  
  assign(paste0("predHigh",a),1/(1+exp(-apply(pred,2,function(x){quantile(x,probs=0.975)}))))
  assign(paste0("predLow",a),1/(1+exp(-apply(pred,2,function(x){quantile(x,probs=0.025)}))))
  assign(paste0("predMean",a),1/(1+exp(-apply(pred,2,mean))))
  
  assign(paste0("predHigh3",a),1-(1-get(paste0("predHigh",a)))^3)
  assign(paste0("predLow3",a),1-(1-get(paste0("predLow",a)))^3)
  assign(paste0("predMean3",a),1-(1-get(paste0("predMean",a)))^3)
  
}
tiff.par("figures/bktDetectionGeoCat.tif")
plot(predMean0~seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
     type='l',lwd=2,ylim=c(0,1),xlab="Standardized Mean Stream Width",
     ylab="Detection Probability")
error.band(seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
           predMean0,upper.y=predHigh0,lower.y=predLow0,interval.type = "abs",
           col='gray90')
points(predMean0~seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
       type='l',lwd=2,col='gray60',lty=2)
error.band(seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
           predMean1,upper.y=predHigh1,lower.y=predLow1,interval.type = "abs",
           col='gray90')
points(predMean1~seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
       type='l',lwd=2,lty=2)

error.band(seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
           predMean30,upper.y=predHigh30,lower.y=predLow30,interval.type = "abs",
           col='gray90')
points(predMean30~seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
       type='l',lwd=2,col='gray60')
error.band(seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
           predMean31,upper.y=predHigh31,lower.y=predLow31,interval.type = "abs",
           col='gray90')
points(predMean31~seq(min(jagsData$pCov),max(jagsData$pCov),length.out=100),
       type='l',lwd=2)

legend(-1.5,0.4,c("Adult","YOY","Single Pass","Three Pass"),
       lty=c(1,1,2,1),lwd=2,col=c("black","gray60","black","black"),
       bty='n')

dev.off()

###############Parameter Value Figure##########################33
tiff.par("figures/parameterValuesGeoCat.tif",mar=c(2.5,5,1,1),width=3.75,height=6)

pars<-rev(c("mu","betaElev","betaWsArea","betaElevArea","betaFlood","trendMu","betaTrendFlood",
            "betaTrendElev","betaTrendWsArea","sd.site","sd.ws","sd.year","sigma","trendSd","trendWsSd",
            "p.mu","p.b","sd.p.site"))

plot(NA,xlim=c(-0.9,4),ylim=c(0,length(pars)+12),
     xlab="Parameter Value",ylab="",yaxt='n')

axis(2,at=rev(1:(length(pars)+12)),
     c(as.expression(bquote(mu[gran])),
       as.expression(bquote(mu[basa])),
       as.expression(bquote(mu[sili])),
       as.expression(bquote(beta[elev-gran])),
       as.expression(bquote(beta[elev-basa])),
       as.expression(bquote(beta[elev-sili])),
       as.expression(bquote(beta[wsArea-gran])),
       as.expression(bquote(beta[wsArea-basa])),
       as.expression(bquote(beta[wsArea-sili])),
       as.expression(bquote(beta[elev-wsArea])),
       as.expression(bquote(beta[flood])),
       as.expression(bquote(gamma[gran])),
       as.expression(bquote(gamma[basa])),
       as.expression(bquote(gamma[sili])),
       as.expression(bquote(gamma[flood])),
       as.expression(bquote(gamma[elev-gran])),
       as.expression(bquote(gamma[elev-basa])),
       as.expression(bquote(gamma[elev-sili])),
       as.expression(bquote(gamma[wsArea-gran])),
       as.expression(bquote(gamma[wsArea-basa])),
       as.expression(bquote(gamma[wsArea-sili])),
       as.expression(bquote(sigma[site])),
       as.expression(bquote(sigma[ws])),
       as.expression(bquote(sigma[year])),
       as.expression(bquote(sigma[disp])),
       as.expression(bquote(sigma[site~trend])),
       as.expression(bquote(sigma[ws~trend])),
       as.expression(bquote(p)),
       as.expression(bquote(p[width])),
       as.expression(bquote(sigma[p~site]))),
     las=1)

cols<-c("gray","black")
for(a in 0:1){
  out<-readRDS(outFiles[a+1])
  ind<-0
  for(p in pars){
    whichPar<-which(names(out$mean)==p)
    ind<-ind+1
    if(p %in% c("mu","trendMu")){
      for(g in 1:3){
        points(I(ind+a/4-0.125)~out$mean[[whichPar]][g],pch=19,col=cols[a+1])
        error.bar(out$mean[[whichPar]][g],ind+a/4-0.125,
                  upper.x=out$q97.5[[whichPar]][g],
                  lower.x=out$q2.5[[whichPar]][g],
                  interval.type="abs",x.bar=T,col=cols[a+1])
        if(g<3) {ind<-ind+1}
      }
    } else{if(p %in% c("betaElev","betaWsArea","betaTrendElev","betaTrendWsArea")){
        for(g in 1:3){
        sims<-out$sims.list[[whichPar]]+
          out$sims.list[[paste0(p,"Basa")]]*(g==2)+
          out$sims.list[[paste0(p,"Gran")]]*(g==3)
       
        points(I(ind+a/4-0.125)~mean(sims),pch=19,col=cols[a+1])
        error.bar(mean(sims),ind+a/4-0.125,
                  upper.x=quantile(sims,0.975),
                  lower.x=quantile(sims,0.025),
                  interval.type="abs",x.bar=T,col=cols[a+1])
        if(g<3){ind<-ind+1}
        }
      } else{
          points(I(ind+a/4-0.125)~out$mean[[whichPar]],pch=19,col=cols[a+1])
          error.bar(out$mean[[whichPar]],ind+a/4-0.125,
                    upper.x=out$q97.5[[whichPar]],
                    lower.x=out$q2.5[[whichPar]],
                    interval.type="abs",x.bar=T,col=cols[a+1])
      }
    }
  }
}
legend(2,5,c("Adult","YOY"),pch=19,col=c("black","gray"),bty='n')
abline(v=0,lty=2)
dev.off()

#######Change Arrows##############################
arrowL<-0.05

pred<-array(NA,dim=c(jagsData$nSites,2,2))

for(a in 0:1){
  out<-readRDS(outFiles[a+1])
  for(i in 1:nrow(out$mean$N)){
    pred[i,,a+1]<-exp(out$mean$siteEffect[i]+
                        out$mean$trend[i]*jagsData$year[c(1,jagsData$nYears)])
  }
}

png.par("figures/bktChangeArrowsGeoCat.png",height=6,width=7,
        mfcol=c(1,2),bg="white",mar=c(2.8,1.5,0.5,0.5),bty='n')

for(a in 0:1){
  plot(NA,xlim=range(pred),ylim=c(0,dim(pred)[1]),
       xlab=paste0(c("YOY","Adult")[a+1]," Brook Trout Abundance (#/100 m)"),
       ylab="",yaxt="n")
  predSort<-order(pred[,2,a+1]-pred[,1,a+1])
  
  for(s in 1:length(predSort)){
    arrows(pred[predSort[s],1,a+1],s,pred[predSort[s],2,a+1],
           col=geolCols2[which(trendOut$majGeol[predSort[s]]==
                                 c("siliciclastic","basaltic","granitic"))],
           length=arrowL,lwd=ifelse(trendOut$SiteID[predSort[s]] %in% flood,3,1),
           lty=ifelse(out$overlap0$trend[predSort[s]],2,1))
    arrows(pred[predSort[s],2,a+1]-1*sign(diff(pred[predSort[s],,a+1])),s,pred[predSort[s],2,a+1],
           col=geolCols2[which(trendOut$majGeol[predSort[s]]==
                                 c("siliciclastic","basaltic","granitic"))],
           length=arrowL,lwd=ifelse(trendOut$SiteID[predSort[s]] %in% flood,2,1),
           lty=1)
  }
  if(a==0){
    legend(130,80,c("Siliciclastic","Basaltic","Granitic","","Flood","No Flood","","Significant","Non-significant"),
           col=c(geolCols2,NA,geolCols2[c(2,2)],NA,geolCols2[c(2,2)]),
           lwd=c(1,1,1,0,3,1,0,1,1),lty=c(1,1,1,1,1,1,1,1,2),
           bty='n')
  }
}

dev.off()


png.par("figures/bktChangeArrowsElevGeoCat.png",height=9.5,width=6,
        mfrow=c(3,2),bg="white",mar=c(2.8,4,0.5,0.5))


for(g in 1:3){
  for(a in 0:1){
    plot(NA,xlim=range(pred),ylim=range(trendOut$Elev_m),
         xlab=paste0(c("YOY","Adult")[a+1]," Brook Trout Abundance (#/100 m)"),
         ylab="")
    title(ylab="Elevation (m)",line=2.7)
    
    rows<-which(trendOut$majGeol==c("siliciclastic","basaltic","granitic")[g])
    
    arrows(pred[rows,1,a+1],trendOut$Elev_m[rows],pred[rows,2,a+1],
           col=geolCols2[g],
           length=arrowL,lwd=ifelse(trendOut$SiteID[rows] %in% flood,3,1),
           lty=ifelse(out$overlap0$trend[rows],2,1))
    arrows(pred[rows,2,a+1]-1*sign(apply(pred[rows,,a+1],1,diff)),trendOut$Elev_m[rows],pred[rows,2,a+1],
           col=geolCols2[g],
           length=arrowL,lwd=ifelse(trendOut$SiteID[s] %in% flood,2,1),
           lty=1)
  }
}
# if(a==0){
#   legend(130,80,c("Siliciclastic","Basaltic","Granitic","","Flood","No Flood","","Significant","Non-significant"),
#          col=c(geolCols2,NA,geolCols2[c(2,2)],NA,geolCols2[c(2,2)]),
#          lwd=c(1,1,1,0,3,1,0,1,1),lty=c(1,1,1,1,1,1,1,1,2),
#          bty='n')
# }
dev.off()

png.par("figures/bktChangeArrowsElev2GeoCat.png",height=9.5,width=6,
        mfrow=c(3,2),bg="white",mar=c(2.8,4,0.5,0.5))


for(g in 1:3){
  for(a in 0:1){
    plot(NA,ylim=range(pred),xlim=range(trendOut$Elev_m),
         ylab="",
         xlab="Elevation (m)")
    title(ylab=paste0(c("YOY","Adult")[a+1]," Brook Trout Abundance (#/100 m)"),line=2.7)
    
    rows<-which(trendOut$majGeol==c("siliciclastic","basaltic","granitic")[g])
    
    arrows(trendOut$Elev_m[rows],pred[rows,1,a+1],
           trendOut$Elev_m[rows],pred[rows,2,a+1],
           col=geolCols2[g],
           length=arrowL,lwd=ifelse(trendOut$SiteID[rows] %in% flood,3,1),
           lty=ifelse(out$overlap0$trend[rows],2,1))
    arrows(trendOut$Elev_m[rows],pred[rows,2,a+1]-1*sign(apply(pred[rows,,a+1],1,diff)),
           trendOut$Elev_m[rows],pred[rows,2,a+1],
           col=geolCols2[g],
           length=arrowL,lwd=ifelse(trendOut$SiteID[s] %in% flood,2,1),
           lty=1)
  }
}
# if(a==0){
#   legend(130,80,c("Siliciclastic","Basaltic","Granitic","","Flood","No Flood","","Significant","Non-significant"),
#          col=c(geolCols2,NA,geolCols2[c(2,2)],NA,geolCols2[c(2,2)]),
#          lwd=c(1,1,1,0,3,1,0,1,1),lty=c(1,1,1,1,1,1,1,1,2),
#          bty='n')
# }
dev.off()

png.par("figures/bktChangeArrowsWsAreaGeoCat.png",height=9.5,width=6,
        mfrow=c(3,2),bg="white",mar=c(2.8,4,0.5,0.5))


for(g in 1:3){
  for(a in 0:1){
    plot(NA,xlim=range(pred),ylim=c(0,max(trendOut$wsArea)),
         xlab=paste0(c("YOY","Adult")[a+1]," Brook Trout Abundance (#/100 m)"),
         ylab="")
    title(ylab=bquote(Watershed~Area~(km^2)),line=2.7)
    
    rows<-which(trendOut$majGeol==c("siliciclastic","basaltic","granitic")[g])
    
    arrows(pred[rows,1,a+1],trendOut$wsArea[rows],pred[rows,2,a+1],
           col=geolCols2[g],
           length=arrowL,lwd=ifelse(trendOut$SiteID[rows] %in% flood,3,1),
           lty=ifelse(out$overlap0$trend[rows],2,1))
    arrows(pred[rows,2,a+1]-1*sign(apply(pred[rows,,a+1],1,diff)),trendOut$wsArea[rows],pred[rows,2,a+1],
           col=geolCols2[g],
           length=arrowL,lwd=ifelse(trendOut$SiteID[s] %in% flood,2,1),
           lty=1)
  }
}
# if(a==0){
#   legend(130,80,c("Siliciclastic","Basaltic","Granitic","","Flood","No Flood","","Significant","Non-significant"),
#          col=c(geolCols2,NA,geolCols2[c(2,2)],NA,geolCols2[c(2,2)]),
#          lwd=c(1,1,1,0,3,1,0,1,1),lty=c(1,1,1,1,1,1,1,1,2),
#          bty='n')
# }
dev.off()

png.par("figures/bktChangeArrowsWsArea2GeoCat.png",height=9.5,width=6,
        mfrow=c(3,2),bg="white",mar=c(2.8,4,0.5,0.5))


for(g in 1:3){
  for(a in 0:1){
    plot(NA,ylim=c(0,200),xlim=range(log(trendOut$wsArea)),
         ylab="",
         xlab=bquote(Watershed~Area~(km^2)))
    title(ylab=paste0(c("YOY","Adult")[a+1]," Brook Trout Abundance (#/100 m)"),line=2.7)
    
    rows<-which(trendOut$majGeol==c("siliciclastic","basaltic","granitic")[g])
    
    arrows(log(trendOut$wsArea[rows]),pred[rows,1,a+1],log(trendOut$wsArea[rows]),pred[rows,2,a+1],
           col=geolCols2[g],
           length=arrowL,lwd=ifelse(trendOut$SiteID[rows] %in% flood,3,1),
           lty=ifelse(out$overlap0$trend[rows],2,1))
    arrows(log(trendOut$wsArea[rows]),pred[rows,2,a+1]-1*sign(apply(pred[rows,,a+1],1,diff)),
           log(trendOut$wsArea[rows]),pred[rows,2,a+1],
           col=geolCols2[g],
           length=arrowL,lwd=ifelse(trendOut$SiteID[s] %in% flood,2,1),
           lty=1)
  }
}
# if(a==0){
#   legend(130,80,c("Siliciclastic","Basaltic","Granitic","","Flood","No Flood","","Significant","Non-significant"),
#          col=c(geolCols2,NA,geolCols2[c(2,2)],NA,geolCols2[c(2,2)]),
#          lwd=c(1,1,1,0,3,1,0,1,1),lty=c(1,1,1,1,1,1,1,1,2),
#          bty='n')
# }
dev.off()


################Flood Figures#################################################
pred<-array(NA,dim=c(jagsData$nSites,jagsData$nYears,2,2))

floodSites<-which(trendOut$SiteID %in% flood)

for(a in 0:1){
  out<-readRDS(outFiles[a+1])
  for(i in floodSites){
    pred[i,,a+1,1]<-exp(out$mean$siteEffect[i]+
                          out$mean$trend[i]*jagsData$year)
    pred[i,,a+1,2]<-exp(out$mean$siteEffect[i]+
                          (out$mean$trend[i]-out$mean$betaTrendFlood)*jagsData$year)
  }
}

#########Naive predictions compared to informed###############################
setkey(trendOut,siteNum)
for(age in 0:1){
  out<-readRDS(outFiles[age+1])
  trendOut[,paste0("trend",age):=out$mean$trend]
  trendOut[,paste0("n2022_",age):=out$mean$N[,ncol(out$mean$N)]]
  
  elStd<-(trendOut$Elev_m-attributes(scale(trendOut$Elev_m))$'scaled:center')/
    attributes(scale(trendOut$Elev_m))$'scaled:scale'
  waStd<-(log(trendOut$wsArea)-attributes(scale(log(trendOut$wsArea)))$'scaled:center')/
    attributes(scale(log(trendOut$wsArea)))$'scaled:scale'
  
  trend<-out$mean$trendMu[match(trendOut$majGeol,c("siliciclastic","basaltic","granitic"))]+
    out$mean$betaTrendElev*elStd+out$mean$betaTrendWsArea*waStd+
    out$mean$trendWs[trendOut$watershedNum]
  
  
  pred22<-out$mean$mu[match(trendOut$majGeol,c("siliciclastic","basaltic","granitic"))]+
    out$mean$betaElev*elStd+out$mean$betaWsArea*waStd+
    out$mean$betaElevArea*elStd*waStd+trend*max(jagsData$year)+
    out$mean$wsRan[trendOut$watershedNum]
  
  # trend<-(exp(diff(jagsData$year)[1]*trend)-1)*100#convert to % per year
  
  trendOut[,c(paste0("predTrend",age),paste0("predN2022_",age)):=list(trend,pred22)]
}


#####################Harvest Evident in Random Effects?#########################################################
harvest<-c("Piney River","Thornton River, S. Fork",
           "Hughes River","Brokenback Run","Naked Creek, E. Branch", "Naked Creek, W. Branch",
           "Cedar Run","Whiteoak Canyon Run","Rose River","Conway River",
           "South River","Ivy Creek","Doyles River","Jeremy's Run","East Hawksbill Creek",
           "Little Hawksbill Creek","Big Run")
trendOut[,harvest:=SiteID %in% sites[STREAM %in% c(harvest,"Thornton River, N. Fork","Pass Run"),SiteID]]
trendOut[,siteRan:=out$mean$siteEffect-(out$mean$mu[match(majGeol,c("siliciclastic","basaltic","granitic"))]+
           out$mean$betaElev*elStd+out$mean$betaWsArea*waStd+
           out$mean$betaElevArea*elStd*waStd+
           out$mean$wsRan[watershedNum])]
trendOut[,siteTrendRan:=out$mean$trend-(out$mean$trendMu[match(majGeol,c("siliciclastic","basaltic","granitic"))]+
                                               out$mean$betaTrendElev*elStd+out$mean$betaTrendWsArea*waStd+
                                               out$mean$trendWs[watershedNum])]
trendOut[,siteEffect:=out$mean$siteEffect]

boxplot(siteRan~harvest,data=trendOut[!SiteID %in% flood])
boxplot(siteTrendRan~harvest,data=trendOut[!SiteID %in% flood])
##no is the answer for site level
trendOut[,wsRan:=out$mean$wsRan[watershedNum]]
trendOut[,wsTrendRan:=out$mean$trendWs[watershedNum]]

ranWs<-trendOut[,.(harvest=any(harvest),wsRan=mean(wsRan),wsTrendRan=mean(wsTrendRan)),watershedNum]
t.test(wsTrendRan~harvest,data=ranWs)

##########Posterior Predictive Check###################################
tiff.par("figures/bktPostPredict.tif",width=6.5,height=7,
         mar=c(2.5,3,1,1),mfcol=c(3,2))
pLab<-1
for(age in 0:1){
  out<-readRDS(outFiles[age+1])
  for(r in 1:3){
    lim<-max(c(out$mean$yRep[,,r],nArray[,,r,age+1]),na.rm=T)
    plot(out$mean$yRep[,,r]~nArray[,,r,age+1],pch=19,
         ylab="",xlab="Observed Count",col=gray(0.3,0.2),
         xlim=c(0,lim),
         ylim=c(0,lim))
    title(ylab="Predicted Count",line=2)
    abline(0,1)
    text(lim*0.8,lim*0.1,
         bquote(italic(p)==.(round(sum(out$mean$yRep[,,r]<=nArray[,,r,age+1],na.rm=T)/sum(!is.na(nArray[,,r,age+1])),2))))
    panelLabel(bquote(bold(.(letters[pLab]))))
    pLab<-pLab+1
  }
}
dev.off()




