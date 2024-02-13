library(sf)
library(raster)
library(mapShen)
ws<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/bktPredSiteWsInit.shp")
dem <- raster("C:/Users/echildress/Documents/mapShen/data/gis/dem/w001001.adf")
predSites<-st_read("C:/Users/echildress/Documents/mapShen/data/gis/bktPredSitesInit.shp")

ws$wsArea<-as.numeric(st_area(ws))/1000000#km2
wsArea<-data.table(ws) %>%
  .[,.(wsArea=sum(wsArea)),SiteID] %>%
  setkey(SiteID)

wsArea[,":="(pctSili=0,pctBasa=0,pctGran=0,pctCarb=0)]

geolTypes<-data.table(name=c("Cca","Cch","Cchs","Ccw","Ce","Ct","Cwa","Jd","Ob","OCc","Ybm","Ycg","Ylg","Yll","Yod",
                             "Yog","Yom","Yon","Yon","Yoq","Yor","Yos","Yml","Zcm","Zcp","Zcs","Zmd","Zrqt","Zsr"),
                      class=c("s","s","s","s","c","c","c","b","c","c",
                              "g","g","g","g","g","g","g","g","g","g","g","g","g",
                              "b","s","b","b","b","g"))

bedrock<-st_read(dsn="C:/Users/echildress/Documents/mapShen/data/gis/Bdrck_2009.shp")
bedrock$geol<-geolTypes$class[match(bedrock$MapUnit,geolTypes$name)]

wsArea[,":="(pctBasa=0,pctGran=0,pctSili=0,pctCarb=0)]

for(s in wsArea$SiteID){
  clippedBedrock<-suppressWarnings(st_intersection(bedrock,ws[ws$SiteID==s,]))
  bedrockProps<-data.table(geo=clippedBedrock$geol,area=as.numeric(st_area(clippedBedrock))) %>%
    .[,.(pct=area/sum(area),geo)] %>%
    .[,.(pct=sum(pct)),geo]
  
  for(b in bedrockProps$geo[bedrockProps$geo %in% c("s","b","g","c")]){
    wsArea[SiteID==s,c("pctSili","pctBasa","pctGran","pctCarb")[which(b==c("s","b","g","c"))]:=bedrockProps[geo==b,pct]]
  }
}
wsArea[,majGeol:=c("basaltic","granitic","siliciclastic","carbonate")[which.max(c(pctBasa,pctGran,pctSili,pctCarb))],SiteID]
wsArea[,elev:=raster::extract(dem,predSites)]

write.csv(wsArea,"data/predSiteAttributes.csv",row.names=F)
st_write(predSites, "data/gis/bktPredSites.shp",append=F)
st_write(ws,"data/gis/bktPredSiteWs.shp",append=F)
