#install.packages("whitebox", repos="http://R-Forge.R-project.org")

# library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(stars)
library(rayshader)
library(rgl)
library(mapShen)

whitebox::wbt_init()

# knitr::knit_hooks$set(webgl = hook_webgl)

# theme_set(theme_classic())

# tmap_mode("view")

dem <- raster("data/gis/dem/w001001.adf")
writeRaster(dem,"data/gis/dem/wbtDem.tif",overwrite=T)

dem<- raster("data/gis/dem/wbtDem.tif")
# tm_shape(dem)+
#   tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE)+
#   tm_scale_bar()

wbt_hillshade(dem = "data/gis/dem/wbtDem.tif",
              output = "data/gis/dem/hillshade.tif",
              azimuth = 115)
hillshade <- raster("data/gis/dem/hillshade.tif")
#
# tm_shape(hillshade)+
#   tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
#   tm_scale_bar()

wbt_fill_single_cell_pits(
                   dem = "data/gis/dem/wbtDem.tif",
                  output = "data/gis/dem/filled.tif")

wbt_breach_depressions_least_cost(
  dem = "data/gis/dem/filled.tif",
  output = "data/gis/dem/breached.tif",
  dist = 5,
  fill = TRUE)

wbt_fill_depressions_wang_and_liu(
  dem = "data/gis/dem/breached.tif",
  output = "data/gis/dem/breachedFilled.tif"
)

wbt_d8_flow_accumulation(input = "data/gis/dem/breachedFilled.tif",
                         output = "data/gis/dem/d8flow.tif")

wbt_d8_pointer(dem = "data/gis/dem/breachedFilled.tif",
               output = "data/gis/dem/d8Pointer.tif")

wbt_extract_streams(flow_accum = "data/gis/dem/d8flow.tif",
                    output = "data/gis/rasterStreams.tif",
                    threshold = 7000)

# wbt_downslope_flowpath_length("data/gis/dem/d8Pointer.tif",
#                               "data/gis/distToOut.tif")

wbt_raster_streams_to_vector("data/gis/rasterStreams.tif",
                             "data/gis/dem/d8Pointer.tif",
                             "data/gis/vectorStreams.shp")

boundary<-st_read("data/gis/PkBndryPly_ESA_assesment_2021_dissolve.shp")
boundBuffer<-st_buffer(boundary,dist=500)

# streamDist<-raster("data/gis/distToOut.tif")*streams
# values(streamDist)[!is.na(values(streamDist))&round(values(streamDist),-1)%%500!=0]<-NA
# predSites<-rasterToPoints(streamDist,spatial=T) %>%
#   st_as_sf() %>%
#   st_intersection(boundBuffer)
# predSites$ID<-1:nrow(predSites)
# predSites<-predSites[,c("ID")]
# st_write(predSites, "data/gis/bktPredSitesInit.shp",append=F)

streamsShp<-st_read("data/gis/vectorStreams.shp")
st_crs(streamsShp)<-st_crs(boundary)
streamsShp<-st_intersection(streamsShp,st_buffer(boundary,dist=150)) 
streamsShp<-do.call(rbind,lapply(1:nrow(streamsShp),function(i){st_cast(streamsShp[i,],"LINESTRING")}))
streamsShp<-streamsShp[which(st_intersects(streamsShp,boundary,sparse=F)),]

streamSegs<-NULL
for(i in 1:nrow(streamsShp)){
  sl<-as.numeric(st_length(streamsShp[i,]))
  nodes<-seq(10/sl,1-10/sl,length.out=2+floor(sl/500))
  
  subSegs<-list()
    for(n in 2:length(nodes)){
      subSegs[[n-1]]<-st_linesubstring(streamsShp[i,],nodes[n-1],nodes[n])
    }
  subSegs<-do.call(rbind,subSegs) %>%
    st_as_sf()
  streamSegs<-rbind(streamSegs,subSegs)  
}
  

streamPts<-st_boundary(streamSegs[1,]) %>% st_cast("POINT") %>% .[2,]

for(i in 1:nrow(streamSegs)){
 streamPts[i,]<-st_boundary(streamSegs[i,]) %>% st_cast("POINT") %>% .[2,]
}
streamPts$SiteID<-1:nrow(streamPts)
streamSegs$SiteID<-1:nrow(streamSegs)

st_write(streamPts[,"SiteID"],"data/gis/bktPredSitesInit.shp",append=F)
st_write(streamSegs[,"SiteID"],"data/gis/bktPredSegsInit.shp",append=F)

# i<-1243
# bloop<-st_bbox(rbind(streamSegs[i,],streamPts[i,]))
# bloop[c(1,2)]<-bloop[c(1,2)]-500
# bloop[c(3,4)]<-bloop[c(3,4)]+500
# tm_shape(streamSegs,bbox=bloop)+tm_lines(col='blue')+
#   tm_shape(streamPts[i,])+tm_symbols()+
#   tm_shape(streamSegs[i,])+tm_lines(col='orange')+
#   tm_shape(boundary)+tm_borders()

makeWs<-function(s){
  tmp<-tempfile()
  ppFile<-paste0(tmp,".shp")
  wsFile<-paste0(tmp,".tif")
  ppS<-streamPts[streamPts$SiteID==s,]
  st_write(ppS,ppFile,layer="ws")
  wbt_watershed(d8_pntr = "data/gis/dem/d8Pointer.tif",
                pour_pts = ppFile,
                output = wsFile)
  w<-raster(wsFile) %>%
    st_as_stars() %>%
    st_as_sf(merge=T)
  names(w)[1]<-"SiteID"
  w$SiteID<-s

  #clean up temporary files (they will fill disk space quickly otherwise)
  tmpName<-length(tstrsplit(tmp,"[\\]"))
  tmpName<-substr(tstrsplit(tmp,"[\\]")[tmpName],1,7)
  toRemove<-list.files(tempdir())
  toRemove<-paste0(tempdir(),"/",toRemove[grepl(tmpName,toRemove)])
  file.remove(toRemove)

  return(w)
}
ws<-NULL
i<-0
while(Sys.time()<as.POSIXct("2023-11-03 15:30:00",tz= "America/New_York")&
      i<nrow(streamPts)){
  i<-i+1
  ws<-rbind(ws,makeWs(i))
  print(paste(max(i),Sys.time()))
  if(i==1){
    st_write(ws,"data/gis/bktPredSiteWsInit.shp",append=F)
    ws<-NULL
  }
  if(i %in% seq(100,nrow(streamPts),100)){
    st_write(ws,"data/gis/bktPredSiteWsInit.shp",append=T)
    ws<-NULL
  }
}
st_write(ws,"data/gis/bktPredSiteWsInit.shp",append=T)

ws<-st_read("data/gis/bktPredSiteWsInit.shp")
# ws<-do.call(rbind,lapply(sites$SiteID,makeWs)) #this takes an hour or two for 300 sites
# st_write(ws,"data/gis/wsPly.shp",append=F)



mapWs<-function(s,expansion=0.2,boundary=F,...){
  sBox<-st_bbox(ws[ws$SiteID==s,])
  if(expansion<5){
    xAdd<-(sBox[3]-sBox[1])*expansion
    yAdd<-(sBox[4]-sBox[2])*expansion
  } else{
    xAdd<-yAdd<-expansion
  }
  sBox<-sBox+c(-xAdd,-yAdd,xAdd,yAdd)

  sHillshade<-raster::crop(hillshade,extent(sBox))
  sStreams<-st_crop(streamsShp,sBox)

  m<-tm_shape(sHillshade,bbox=sBox)+
    tm_raster(style = "cont", palette = "Greys",legend.show = F)+
    tm_shape(ws[ws$SiteID==s,])+tm_borders(col="cornflowerblue",lwd=2,lty=1)+
    tm_shape(streamPts[streamPts$SiteID==s,])+tm_symbols(size=1,col='red') +
    tm_scale_bar()+
    tm_layout(main.title=s,main.title.position = "center")
  if(nrow(sStreams)>0){
    m<-m + tm_shape(sStreams,bbox=sBox)+
            tm_lines(col='blue')
    m<-m+tm_shape(streamSegs[streamSegs$SiteID==s,])+tm_lines(col="yellow")
  }

  if(boundary){
    m<-m+
      tm_shape(get("boundary",env=globalenv()))+
      tm_borders(col="forestgreen",lwd=1,lty=1)
  }
  return(m)
}
# #############MAKE MAPS###########################################
# 
# for(s in ws$SiteID){
#   m<-mapWs(s)
#   tmap_save(m,paste0("figures/wsDelineation/",s,".png"))
# }
# 
