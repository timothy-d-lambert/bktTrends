library(mapShen)
library(waterData)
library(readxl)
library(zoo)

getStationInfo<-function(sid){
  url<-paste0("https://waterdata.usgs.gov/va/nwis/inventory?",
              paste0("site_no=",sid,"&",collapse=""),
              "group_key=NONE&format=sitefile_output&sitefile_output_format=rdb",
              "&column_name=site_no&column_name=station_nm",
              "&column_name=dec_lat_va&column_name=dec_long_va",
              "&column_name=alt_va&column_name=drain_area_va")
  
  file<-tempfile()
  download.file(url, destfile = file)
  cl<-"character"
  names(cl)<-"15s"
  d<-fread(file, skip = "15s", colClasses = cl)
  setnames(d,c("sid", "stationName", "lat", "long", 
               "a", "b", "elevation", "c", "d", "drainArea"))
  d<-d[,.(sid, stationName, lat, long, elevation, drainArea)]
  file.remove(file)
  return(d)
}

stations<-data.table(stationId=c("01632900","01634500","01665500","02028500"),
                     wsArea=c(245,264,296.6,245.5),
                     name=c("smithCreek","cedarCreek","rapidanRiver","rockfishRiver"))

stations[,c("lat","long","wsArea"):=getStationInfo(stationId)[,c("lat","long","drainArea")],stationId]

q<-importDVs(stations$stationId[1]) %>%
  data.table()
q<-q %>%
  setnames("val",stations[stationId==q$staid[1],name]) %>%
  .[,.(smithCreek,dates)] %>%
  setkey(dates)

for(s in stations$stationId[2:4]){
  bla<-importDVs(s) %>%
    data.table() %>%
    .[,.(val,dates)] %>%
    setnames("val",stations[stationId==s,name]) %>%
    setkey(dates)
  q<-merge(q,bla,all=T)
}

swasSites<-c("pain","pine","stan","wor1")
for(s in swasSites){
  bla<-read_excel("C:/Users/echildress/Documents/waterQuality/data/SWAS_Discharge_Hourly_20210322.xlsx",
                  sheet=toupper(s)) %>%
    data.table() %>%
    .[,.(mean(cfs,na.rm=T)),.(date=as.Date(datetime))] %>%
    setnames("V1",s) %>%
    setkey(date)
  q<-merge(q,bla,all=T,by.x="dates",by.y="date")
}

stations<-rbind(stations,
                data.table(stationId=NA,
                           wsArea=c(12.71,12.4,10.68,5.12),
                           name=c(swasSites),
                           lat=c(c(38.1986,38.70176,38.44461,38.25083)),
                           long=c(-78.7934,-78.26776,-78.3708,-78.74853)))

stSpat<-sp::SpatialPointsDataFrame(cbind(stations$long,stations$lat),stations,
                                            proj4string=CRS("+proj=longlat +datum=NAD83"))

stSpat<-sp::spTransform(stSpat,
                       CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
                             +no_defs +ellps=GRS80 +towgs84=0,0,0"))
stations[,UTM_E:=coordinates(stSpat)[,1]]
stations[,UTM_N:=coordinates(stSpat)[,2]]

rm(stSpat)

##Liz's data is here: O:\Working\WATER\SWAS-VTSSS\Hydrolab data for SWAS Sites\Insitu Data from VTSS Sites.xlsx
lizPath<-"O:/Working/WATER/SWAS-VTSSS/Hydrolab data for SWAS Sites/Insitu Data from VTSSS Sites.xlsx"
lizSheets<-excel_sheets(lizPath)
lizDat<-do.call(rbindlist,
                args=list(l=lapply(lizSheets[grepl("VT",lizSheets)],
                                   function(x){read_excel(lizPath,sheet=x,skip=3) %>%
                                       data.table() %>%
                                       .[,site:=x]}),
                          use.names=F))
setnames(lizDat,c("date","hydrolab","temp","do","doSat","cond",
                  "pH","tds","q","notes","site"))
lizDat<-lizDat[!is.na(q),.(site,date,q)] %>%
  .[,SiteID:=substr(site,1,4)] %>%
  setkey(SiteID)
lizDat<-read_excel("C:/Users/echildress/Documents/waterQuality/data/SWAS_VTSSS_TIME_PRIMARY_SITE_DOCUMENTATION_20190912.xlsx") %>%
        data.table() %>%
        setkey(Site_ID) %>%
        setnames(c("Site_ID","Y","X"),c("SiteID","lat","long")) %>%
        .[,.(SiteID,lat,long)] %>%
        .[lizDat]

qPoint<-makeDischarge()

siteInfo<-fread("C:/Users/echildress/Documents/mapShen/data/wsArea.csv") %>%
  .[,.(SiteID,wsArea)] %>%
  setkey(SiteID)
sites<-aqData("sites") %>%
  .[,.(SiteID,UTMX_E,UTMY_N)] %>%
  setkey(SiteID)

siteInfo<-sites[siteInfo]

# sites<-sp::SpatialPointsDataFrame(cbind(siteInfo$UTMX_E,siteInfo$UTMY_N),siteInfo,
#                                   proj4string=CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
#                              +no_defs +ellps=GRS80 +towgs84=0,0,0"))

siteInfo[,":="(nearest=stations$name[which.min(sqrt((UTMX_E-stations$UTM_E)^2+(UTMY_N-stations$UTM_N)^2))],
               nearestDist=min(sqrt((UTMX_E-stations$UTM_E)^2+(UTMY_N-stations$UTM_N)^2))),SiteID]

qPoint<-siteInfo[qPoint]
setkey(qPoint,SiteID,sDate)

qPoint[,qPred:=..q[dates %in% as.Date(sDate),get(nearest)]/stations[name==unique(nearest),wsArea]*wsArea*0.028316847,SiteID]

overallMod<-lm(qPred~q,data=qPoint)

qPoint[!is.na(q)&!is.na(qPred),N:=.N,SiteID]

siteVisits<-aqData("siteVisits") %>%
  setkey(SiteID,sDate)

qPoint<-siteVisits[,.(SiteID,sDate,Project)][qPoint]

rSqBySite<-data.table(SiteID=qPoint[N>=10,unique(SiteID)])
for(s in rSqBySite$SiteID){
  a<-lm(qPred~q,data=qPoint[SiteID==s])
  rSqBySite[SiteID==s,rSq:=summary(a)$r.squared]
  rSqBySite[SiteID==s,n:=nrow(qPoint[SiteID==s&!is.na(q)&!is.na(qPred)])]
}
setkey(rSqBySite,SiteID)

rSqBySite<-qPoint[,.(SiteID,nearestDist)] %>%
  unique() %>%
  setkey(SiteID) %>%
  .[rSqBySite]

contCor<-cor(q[,.(smithCreek,cedarCreek,rapidanRiver,rockfishRiver,pain,pine,stan,wor1)],use="pairwise.complete.obs")^2
contDist<-NULL
for(s in stations$name){
  contDist<-rbind(contDist,stations[,sqrt((UTM_E[which(name==s)]-UTM_E)^2+(UTM_N[which(name==s)]-UTM_N)^2)])
}

winterHigh<-q[month(dates) %in% 12:2,
              lapply(.SD, quantile,0.95,na.rm=T),
              by=year(dates)]
winterNa<-q[month(dates) %in% 12:2,
            lapply(.SD,function(x){any(is.na(x))}),
            year(dates)]
for(n in names(winterHigh)[2:ncol(winterHigh)]){
  if(!any(winterNa[[n]])) next
  winterHigh[winterNa[[n]]][[n]]<-NA
}

springHigh<-q[month(dates) %in% 3:5,
              lapply(.SD, quantile,0.95,na.rm=T),
              by=year(dates)]
springNa<-q[month(dates) %in% 3:5,
            lapply(.SD,function(x){any(is.na(x))}),
            year(dates)]
for(n in names(springHigh)[2:ncol(springHigh)]){
  if(!any(springNa[[n]])) next
  springHigh[springNa[[n]]][[n]]<-NA
}

fallLow<-q[month(dates) %in% 9:11,
              lapply(.SD, quantile,0.05,na.rm=T),
              by=year(dates)]
fallNa<-q[month(dates) %in% 9:11,
            lapply(.SD,function(x){any(is.na(x))}),
            year(dates)]
for(n in names(fallLow)[2:ncol(fallLow)]){
  if(!any(fallNa[[n]])) next
  fallLow[fallNa[[n]]][[n]]<-NA
}

summerLow<-q[month(dates) %in% 6:8,
           lapply(.SD, quantile,0.05,na.rm=T),
           by=year(dates)]
summerNa<-q[month(dates) %in% 6:8,
          lapply(.SD,function(x){any(is.na(x))}),
          year(dates)]
for(n in names(summerLow)[2:ncol(summerLow)]){
  if(!any(summerNa[[n]])) next
  summerLow[summerNa[[n]]][[n]]<-NA
}

annualMin<-q[,lapply(.SD, min),by=year(dates)]
annual7DayMin<-q[,lapply(.SD,function(x){min(rollmean(x,7))}),
                 by=year(dates)]
annualMax<-q[,lapply(.SD, max),by=year(dates)]

cor(winterHigh,use="pairwise.complete.obs")^2
cor(springHigh,use="pairwise.complete.obs")^2
cor(fallLow,use="pairwise.complete.obs")^2
cor(summerLow,use="pairwise.complete.obs")^2
cor(annualMin,use="pairwise.complete.obs")^2
cor(annualMax,use="pairwise.complete.obs")^2
cor(annual7DayMin,use="pairwise.complete.obs")^2

