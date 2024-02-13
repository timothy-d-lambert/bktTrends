library(sf)
library(tmap)
library(data.table)
library(mapShen)

age<-1
source("scripts/prepDataTrend2022.R")

outFiles<-paste0("results/realTrendOutCovGeoCatPostCheck",c(0,1),".rds")

# stream<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/vectorStreams.shp")
# ws<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/bktPredSiteWsInit.shp")
# st_crs(stream)<-st_crs(boundary)

streamSegs<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/bktPredSegsInit.shp")
# streamSegs<-streamSegs[streamSegs$SiteID %in% ws$SiteID,]


boundary<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/PkBndryPly_ESA_assesment_2021_dissolve.shp")
boundBuffer<-st_buffer(boundary,dist=500)

pd<-fread("data/predSiteAttributes.csv") %>%
  setkey(SiteID)
pdSp<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/bktPredSitesInit.shp")

huc<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/CVIWatershed.shp") %>%
  st_transform(st_crs(pdSp)) %>%
  st_crop(st_bbox(boundBuffer))

sites<-aqData("sites")

for(i in 1:nrow(pdSp)){
  pdSp$huc[i]<-huc[which(st_intersects(pdSp[i,], huc, sparse = FALSE)), ]$HUC_CVI
}

pd$watershedNum<-trendOut$watershedNum[match(pdSp$huc,trendOut$catchment)]


whichSites<-pd[which=T,wsArea>1.2&majGeol!="carbonate"]
pd<-pd[whichSites]
streamSegs<-streamSegs[whichSites,]

for(age in 0:1){
  out<-readRDS(outFiles[age+1])
  elStd<-(pd$elev-attributes(scale(trendOut$Elev_m))$'scaled:center')/
    attributes(scale(trendOut$Elev_m))$'scaled:scale'
  waStd<-(log(pd$wsArea)-attributes(scale(log(trendOut$wsArea)))$'scaled:center')/
    attributes(scale(log(trendOut$wsArea)))$'scaled:scale'
  
  trend<-out$mean$trendMu[match(pd$majGeol,c("siliciclastic","basaltic","granitic"))]+
    # out$mean$trendMu[1]*pd$pctSili+out$mean$trendMu[2]*pd$pctBasa+out$mean$trendMu[3]*pd$pctGran+
    out$mean$betaTrendElev*elStd+out$mean$betaTrendWsArea*waStd+
    ifelse(!is.na(pd$watershedNum),out$mean$trendWs[pd$watershedNum],0)
    
  
  pred22<-out$mean$mu[match(pd$majGeol,c("siliciclastic","basaltic","granitic"))]+
    # out$mean$mu[1]*pd$pctSili+out$mean$mu[2]*pd$pctBasa+out$mean$mu[3]*pd$pctGran+
    out$mean$betaElev*elStd+out$mean$betaWsArea*waStd+
    out$mean$betaElevArea*elStd*waStd+trend*max(jagsData$year)+
    ifelse(!is.na(pd$watershedNum),out$mean$wsRan[pd$watershedNum],0)
    
  trend<-(exp(diff(jagsData$year)[1]*trend)-1)*100#convert to % per year
  
  pd[,c(paste0("trend",age),paste0("nPres",age)):=list(trend,pred22)]
  streamSegs[,paste0("trend",age)]<-trend
  streamSegs[,paste0("nPres",age)]<-pred22
  
}
streamSegs$nPres1Disp<-(ifelse(streamSegs$nPres1<0,0,streamSegs$nPres1)+1)
streamSegs$nPres1Disp[streamSegs$nPres1>log(100)]<-log(100)+1


# streamSegs$nPres1Disp<-log(streamSegs$nPres1)


#brokenback covered by Hughes, cedar and white oak canyon covered by Robinson
harvest<-c("Piney River","Thornton River, S. Fork",
           "Hughes River","Brokenback Run","Naked Creek, E. Branch", "Naked Creek, W. Branch",
           "Cedar Run","Whiteoak Canyon Run","Rose River","Conway River",
           "South River","Ivy Creek","Doyles River","Jeremy's Run","East Hawksbill Creek",
           "Little Hawksbill Creek","Big Run")
harvSites<-sites[STREAM %in% harvest] %>%
  .[!SiteID %in% c("BIGRUN","BROKENBACK","HUGHES")] %>%
  .[,minElev:=min(Elev_m,na.rm=T),STREAM] %>%
  .[Elev_m==minElev,SiteID]
harvSites[harvSites=="2FVA10"]<-"2FVA9"
harvWs<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/wsPly.shp") %>%
  .[.$SiteID %in% harvSites,]
harvWs<-rbind(harvWs,st_read("C:/Users/echildress/Documents/mapShen/data/gis/bktPredSiteWsInit.shp")%>%
        .[.$SiteID %in% c(306,637),])

# harvWs<-rbind(harvWs[harvWs$SiteID %in% c("2F113_1","2F029","2FVA9"),],
#   st_intersection(harvWs[!harvWs$SiteID %in% c("2F113_1","2F029","2FVA9"),],st_buffer(boundary,dist=20),sparse=F)[,"SiteID"])
harvWs<-st_intersection(harvWs,st_buffer(boundary,dist=20),sparse=F)[,"SiteID"]
harvCombo<-harvWs %>%
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>%
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))

# cols<-colorRampPalette(c(colorRampPalette(c("red","red","white"))(4),"blue"))(100)

bbox1<-st_bbox(streamSegs[streamSegs$SiteID %in% 1:round(max(streamSegs$SiteID)/2),])
bbox2<-st_bbox(streamSegs[streamSegs$SiteID %in% round(max(streamSegs$SiteID)/2):max(streamSegs$SiteID),])


# m1<-  tm_shape(st_buffer(boundary,dist=20),bbox=bbox1)+tm_borders()+
#   tm_shape(streamSegs[!is.na(streamSegs$trend1),])+
#   tm_lines(col="trend1",lwd="nPres1Disp",
#           palette=cols,
#           scale=6,style="cont",
#           lwd.legend=c(1,log(20)+1,log(100)+1,log(200)+1),
#           lwd.legend.labels = c(1,20,100,200),
#           labels=paste0(seq(-15,5,5),"%"),
#           title.col="Trend",title.lwd="2022 Abundance")+
#   tm_shape(harvCombo)+tm_borders(col='yellow')+
#   tm_layout(bg="gray",frame=F,
#             legend.position=c(0.25,0.1),legend.stack="horizontal",
#             legend.width=0.7)+
#   tm_scale_bar(position=c(0.45,0.1),width=0.2)
# 
# m2<-  tm_shape(st_buffer(boundary,dist=20),bbox=bbox2)+tm_borders()+
#   tm_shape(streamSegs[!is.na(streamSegs$trend1),])+
#   tm_lines(col="trend1",lwd="nPres1Disp",
#            palette=cols,
#            scale=6,style="cont",
#            legend.col.show=F,legend.lwd.show=F,
#            title.col="Trend",title.lwd="2022 Abundance")+
#   tm_shape(harvCombo)+tm_borders(col='yellow')+
#   tm_layout(bg="gray",frame=F)
# 
# tmap_save(tmap_arrange(m1,m2,ncol=2),"figures/bktPredParkwide.png",
#           dpi=400,width=6.5,units="in")
# 
# m3<- tm_shape(st_buffer(boundary,dist=20),bbox=st_bbox(streamSegs[!is.na(streamSegs$trend1),]))+tm_borders()+
#   tm_shape(streamSegs[!is.na(streamSegs$trend1),])+
#   tm_lines(col="trend1",lwd="nPres1Disp",
#            palette=cols,
#            scale=6,style="cont",
#            lwd.legend=c(1,log(20)+1,log(100)+1,log(200)+1),
#            lwd.legend.labels = c(1,20,100,200),
#            labels=paste0(seq(-15,5,5),"%"),
#            title.col="Trend",title.lwd="2022 Abundance")+
#   tm_shape(harvCombo)+tm_borders(col='yellow')+
#   tm_layout(bg="gray",frame=F, legend.position=c(0.45,0.1),
#             legend.stack="horizontal",legend.width=0.7)+
#   tm_scale_bar(position=c(0.47,0.05))
# 
# tmap_save(m3,"figures/bktPredParkwide2.png",
#           dpi=400,height=8.5,units="in")



pd[,trendCat:=as.numeric(trend1>= -2.5) + as.numeric(trend>=2.5)+1]
pd[,abunCat:=as.numeric(nPres1>log(20))+as.numeric(nPres1>log(50))+1]


# palette(c("blue",rgb(0,0,1,0.3),"red"))
# png.par("figures/mgmtCat.png",mfrow=c(3,1),res=400,bg="white",width=3.25,height=6.5)
# plot(elev~log(wsArea),data=pd[pd$majGeol=="siliciclastic"],col=factor(trendCat,levels=c(3,2,1)),pch=19,cex=ifelse(abunCat,1.5,0.8))
# plot(elev~log(wsArea),data=pd[pd$majGeol=="basaltic"],col=factor(trendCat,levels=c(3,2,1)),pch=19,cex=ifelse(abunCat,1.5,0.8))
# plot(elev~log(wsArea),data=pd[pd$majGeol=="granitic"],col=factor(trendCat,levels=c(3,2,1)),pch=19,cex=ifelse(abunCat,1.5,0.8))
# dev.off()


inPark<-which(st_intersects(streamSegs,boundary,sparse=F))
trendScale<-seq(min(pd[wsArea>1.3&SiteID %in% inPark,trend1]),
                max(pd[wsArea>1.3&SiteID %in% inPark,trend1]),length.out=100)
ptCols<-colorRampPalette(c("red","red","gray"))(which.min(abs(trendScale)))
# ptCols<-c(trendCols,
#               colorRampPalette(c(trendCols[length(trendCols)],
#                                  "blue"))(length(trendScale)-length(trendCols))) 
ptCols<-c(ptCols,
          colorRampPalette(c(ptCols[length(ptCols)],
                             "blue","blue"))(length(ptCols)))[1:100] 

# ptCols<-colorRampPalette(c(colorRampPalette(c("red","red","gray"))(4),"blue"))(100)
pd$col<-ptCols[apply(outer(pd$trend1,seq(min(pd$trend1),max(pd$trend1),length.out=length(ptCols)),"-"),1,function(x){which.min(abs(x))})]



png.par("figures/mgmtCat.png",mfrow=c(3,1),res=400,bg="white",width=3.25,height=6.5,
        mar=c(3,3.5,2,0.5),mgp=c(1.8,0.5,0))
for(g in 1:3){
  mg<-c("siliciclastic","basaltic","granitic")[g]
  plot(elev~log(wsArea),data=pd[wsArea>1.3&SiteID %in% inPark],pch=NA,
       ylab="",xlab=bquote(Watershed~Area~(km^2)),
       xaxt='n',main=paste0(toupper(substr(mg,1,1)),substr(mg,2,nchar(mg))))
  points(elev~log(wsArea),data=pd[majGeol==mg&wsArea>=1.3&SiteID %in% inPark],col=col,
       pch=19,cex=abunCat*0.5)
  # panelLabel(c("a","b","c")[g])
  axis(1,at=log(c(2,5,10,20)),c(2,5,10,20))
  title(ylab="Elevation (m)",line=2.3)
  hull<-trendOut[majGeol==mg][chull(trendOut[majGeol==mg,.(Elev_m,log(wsArea))])]
  polygon(log(hull$wsArea),hull$Elev_m,border="black")
  
  if(g==1){
    y.leg<-seq(700,900,length.out=length(ptCols))
    for(i in 1:length(ptCols)){
      points(c(3,3.2),rep(y.leg[i],2),type='l',
             col=ptCols[i],lend='butt',lwd=0.8)
    }
    
    
    text(3.4,700+which.min(abs(trendScale+10))*2,"-10%")
    text(3.4,700+which.min(abs(trendScale+5))*2,"-5%")
    text(3.4,700+which.min(abs(trendScale))*2,"0%")
    text(3.4,700+which.min(abs(trendScale-5))*2,"5%")
    text(3.2,950,"Trend")
    
    legend(2.1,1000,c("< 20","20-50","> 50"),pch=19,col=ptCols[50],
           pt.cex=c(0.5,1,1.5),bty='n',title="Abundance")
  }
}
dev.off()
palette(c("red","gray","blue"))

pd[,mgmtCat:=as.numeric(trendCat>1&abunCat>1)*3+as.numeric(abunCat>1&trendCat==1)*2+as.numeric(trendCat==1&abunCat==1)*1]

png.par("figures/mgmtCat2.png",mfrow=c(3,1),res=400,bg="white",width=3.25,height=6)
for(g in 1:3){
  plot(elev~log(wsArea),data=pd,pch=NA)
  mg<-c("siliciclastic","basaltic","granitic")[g]
  for(tc in 1:3){
    bloop<-chull(pd[pd$majGeol==mg&trendCat==tc,.(log(wsArea),elev)])
    bloop<-pd[pd$majGeol==mg&trendCat==tc][bloop]
    polygon(log(bloop$wsArea),bloop$elev,col=palette()[tc],border=NA)
    
    # bloop<-chull(pd[pd$majGeol==mg&abunCat==F,.(log(wsArea),elev)])
    # bloop<-pd[pd$majGeol==mg&abunCat==F][bloop]
    # polygon(log(bloop$wsArea),bloop$elev,density=15,col="white",border=NA,lwd=0.5)
    # polygon(log(bloop$wsArea),bloop$elev,density=15,angle=135,col="white",border=NA,lwd=0.5)
  }
}
dev.off()

streamSegs$trendCat<-as.numeric(pd$trend1>= -2.5) + as.numeric(pd$trend1>=2.5)+1
streamSegs$abunCat<-(as.numeric(pd$nPres1>log(20))+as.numeric(pd$nPres1>log(50)))*2+1
# 
# m1<- tm_shape(st_buffer(boundary,dist=20),bbox=bbox1)+tm_borders()+
#   tm_shape(streamSegs[!is.na(streamSegs$trend1),])+
#   tm_lines(col="trend1",lwd="abunCat",
#            palette=cols,
#            scale=7,style="cont",
#            lwd.legend=c(1,4,7),
#            lwd.legend.labels = c("1-20","20-50","50+"),
#            labels=paste0(seq(-15,5,5),"%"),
#            title.col="Trend",title.lwd="2022 Abundance")+
#   tm_shape(harvCombo)+tm_borders(col='yellow')+
#   tm_layout(bg="gray",frame=F, legend.position=c(0.45,0.1),
#             legend.stack="horizontal",legend.width=0.7)+
#   tm_scale_bar(position=c(0.47,0.05))
# 
# m2<- tm_shape(st_buffer(boundary,dist=20),bbox=bbox2)+tm_borders()+
#   tm_shape(streamSegs[!is.na(streamSegs$trend1),])+
#   tm_lines(col="trend1",lwd="abunCat",
#          palette=cols,
#          scale=7,style="cont",
#          legend.col.show=F,legend.lwd.show=F,
#          title.col="Trend",title.lwd="2022 Abundance")+
#   tm_shape(harvCombo)+tm_borders(col='yellow')+
#   tm_layout(bg="gray",frame=F)
# 
# tmap_save(tmap_arrange(m1,m2,ncol=2),"figures/bktPredParkwideCat.png",
#           dpi=400,width=6.5,units="in")


cols<-colorRampPalette(c("red","white"))(3)
m3<-tm_shape(harvCombo,bbox=st_bbox(streamSegs[!is.na(streamSegs$trend1),]))+tm_fill(col="gray50")+
  tm_shape(st_buffer(boundary,dist=20))+tm_borders(col="gray20")+
  tm_shape(streamSegs[!is.na(streamSegs$trend1)&pd$wsArea>=1.2,])+
  tm_lines(col="trend1",lwd="abunCat",
           palette=c("red","red","white","blue","blue"),
           # palette=c(cols,"blue","blue"),
           scale=7,style="cont",
           lwd.legend=c(1,4,7),
           lwd.legend.labels = c("< 20","20-50","> 50"),
           midpoint=0,
           breaks=seq(-10,5,5),
           labels=paste0(seq(-10,5,5),"%"),
           legend.col.reverse=T,
           title.col="Trend",title.lwd="2022 Abundance")+
  tm_layout(bg="gray65",frame=F, legend.position=c(0.45,0.1),
            legend.stack="horizontal",legend.width=0.7)+
  tm_scale_bar(position=c(0.47,0.05))

tmap_save(m3,"figures/bktPredParkwideCat2.png",
          dpi=1000,height=8,units="in")

m3<-tm_shape(harvCombo,bbox=st_bbox(streamSegs[!is.na(streamSegs$trend1),]))+tm_fill(col="gray50")+
  tm_shape(st_buffer(boundary,dist=20))+tm_borders(col="gray20")+
  tm_shape(streamSegs[!is.na(streamSegs$trend1)&pd$wsArea>=1.2,])+
  tm_lines(col="trend1",lwd="abunCat",
           palette=c("red","red","white","blue","blue"),
           # palette=c(cols,"blue","blue"),
           scale=7,style="cont",
           lwd.legend=c(1,4,7),
           lwd.legend.labels = c("< 20","20-50","> 50"),
           midpoint=0,
           breaks=seq(-10,5,5),
           labels=paste0(seq(-10,5,5),"%"),
           legend.col.reverse=T,
           title.col="Trend",title.lwd="2022 Abundance")+
  tm_layout(frame=F, legend.position=c(0.45,0.1),scale = 1.5,
            legend.stack="horizontal",legend.width=0.7,bg.color="transparent")+
  tm_scale_bar(position=c(0.47,0.05))

tmap_save(m3,"figures/bktPredParkwideCatTransparent.png",
          dpi=1000,height=8,units="in",bg=NA)

