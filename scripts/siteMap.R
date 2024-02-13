library(tigris)
library(sf)
library(tmap)

alph<-0.2
geolCols<-c(rgb(197,108,57,alph*255,maxColorValue = 255),
            rgb(111,73,48,alph*255,maxColorValue = 255),
            gray(0.7,alph),
            "cornflowerblue")
geolCols2<-adjustcolor(geolCols,alpha.f=255)

us_geo<-tigris::states(class="sf")
boundary<-sf::st_read("C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/gis/PkBndryPly_ESA_assesment_2021_dissolve.shp")

par(bg=NA)
m<-tm_shape(us_geo[us_geo$STUSPS=="VA",])+tm_fill(col="gray90",lwd=2,lty=1,bg=NA)+
  tm_shape(boundary)+tm_fill(col="black")+
  tm_layout(frame=F,bg.color="transparent")
tmap_save(m,"figures/shenLocation.png",bg=NA)

geolTypes<-data.table(name=c("Cca","Cch","Cchs","Ccw","Ce","Ct","Cwa","Jd","Ob","OCc","Ybm","Ycg","Ylg","Yll","Yod",
                             "Yog","Yom","Yon","Yon","Yoq","Yor","Yos","Yml","Zcm","Zcp","Zcs","Zmd","Zrqt","Zsr"),
                      class=c("s","s","s","s","c","c","c","b","c","c",
                              "g","g","g","g","g","g","g","g","g","g","g","g","g",
                              "b","s","b","g","b","g"))

bedrock<-st_read(dsn="C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/gis/Bdrck_2009.shp")
bedrock$geol<-geolTypes$class[match(bedrock$MapUnit,geolTypes$name)]
bedrock<-suppressWarnings(st_intersection(bedrock,boundary))

bedrock$geol[bedrock$geol=="s"]<-"siliciclastic"
bedrock$geol[bedrock$geol=="b"]<-"basaltic"
bedrock$geol[bedrock$geol=="g"]<-"granitic"
bedrock$geol[bedrock$geol=="c"]<-"carbonatic"
bedrock$geol<-factor(bedrock$geol,levels=c("siliciclastic","basaltic","granitic","carbonatic"))

sites<-sp::SpatialPointsDataFrame(cbind(trendOut$Lon_n83,trendOut$Lat_n83),trendOut,
                                  proj4string=CRS("+proj=longlat +datum=NAD83")) %>%
  sp::spTransform(CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
                             +no_defs +ellps=GRS80 +towgs84=0,0,0")) %>%
  st_as_sfc()

m2<-tm_shape(boundary)+tm_borders(col="black")+
  tm_shape(bedrock)+
  tm_fill(col="geol",title="",palette=geolCols2)+
  tm_shape(sites)+tm_symbols(col='black',shape=19,size=0.1)+
  tm_logo("figures/shenLocation.png",height=4,
          position=c(-0.02,0.8))+
  tm_legend(position=c(0.52,0.18))+
  tm_scale_bar(position=c(0.52,0.13))+
  tm_layout(frame=F)+
  tm_compass(type="arrow",position=c(0.77,0.195))

tmap_save(m2,filename="figures/shenGeoSites.png",
          width=2.75,units="in",dpi=400)

m2<-tm_shape(boundary)+tm_borders(col="black")+
  tm_shape(bedrock)+
  tm_fill(col="geol",title="",palette=geolCols2)+
  tm_shape(sites)+tm_symbols(col='black',shape=19,size=0.1)+
  tm_legend(position=c(0.5,0.18))+
  tm_scale_bar(position=c(0.42,.13))+
  tm_layout(frame=F,bg.color="transparent")
tmap_save(m2,filename="figures/shenGeoSites2.png",
          width=3.5,units="in",dpi=400,bg=NA)
