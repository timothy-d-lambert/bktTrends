#not runable without mapShen package
source("scripts/prepDataTrend2022.R")

outFiles<-paste0("results/realTrendOutCovGeoCatTwo",c(0,1),".rds")

for(a in 0:1){
  out<-readRDS(outFiles[a+1])
  trendOut[,paste0("trend",a):=out$mean$trend]
}

#temperature vs elevation
plot(siteTemp~Elev_m,data=trendOut)
tempElevMod<-lm(siteTemp~Elev_m,data=trendOut)
abline(tempElevMod)

#trend vs temp plus other covs
for(a in 0:1){
  assign(paste0("trendTempMod",a),
  lm(get(paste0("trend",a))~log(wsArea)*majGeol+siteTemp*majGeol,
     data=trendOut[!SiteID %in% flood]))
}
summary(trendTempMod0)
anova(trendTempMod0)

summary(trendTempMod1)
anova(trendTempMod1)

#without interaction between geology and temp, which confuses things and is not significant (and has the same AIC)
for(a in 0:1){
  assign(paste0("trendTempMod",a),
         lm(get(paste0("trend",a))~log(wsArea)*majGeol+siteTemp,
            data=trendOut[!SiteID %in% flood]))
}

summary(trendTempMod0)
anova(trendTempMod0)

summary(trendTempMod1)
anova(trendTempMod1)