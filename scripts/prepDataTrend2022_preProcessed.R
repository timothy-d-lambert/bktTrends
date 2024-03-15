# doesn't run without the mapShen package (renamed shenAquatics on git)
# library(mapShen) # same as the line below, renamed because what started as a mapping package turned into something else
library(shenAquatics) # github.com/evanchildress/shenAquatics
library(plotHacks) # on github.com/evanchildress/plotHacks
library(tidyr)
library(reshape2)
library(jagsUI)
library(lme4)
library(here) # Added T. Lambert 14Mar2024

# declare script location (and find project root)
here::i_am(path = "scripts/prepDataTrend2022_preProcessed.R")
here::here()

# Note: See runTrendModel2022.R for the script that processes data directly from Microsoft Access. This script uses those pre-processed data outputs (jagsData.rds, n.csv, trendOut.csv) to define the objects used by runTrendModel2022.R, which are:
# jagsData
# init
# pars.to.save
# model
# n.iter
# thin
# n.chains
# n.burnin
# In addition, the objects n and trendOut are used in the subsequent plotting scripts.

# Define jagsData
jagsData <- readRDS(file = here("Data", "jagsData.rds"))

# Define init
# # Original def'n:
# nInit<-apply(nArray[,,,age+1],c(1,2),sum,na.rm=T)+1
# init=function() list(N=nInit)
# 
# New def'n:
# (substitute jagsData$y = nArray[,,,age+1])
nInit <- apply(jagsData$y,c(1,2),sum,na.rm=T)+1
init = function() list(N=nInit)


n.chains = 3  		# number of chains
n.adapt = 2000		# number of sampler tuning iterations --> WHERE IS THIS USED?
n.burnin = 20000 # number of iterations to discard
n.iter = 220000	  # total iterations
thin = 200	  # number to thin by

model <- "scripts/trendModelRanWatershedCovGeoCat.r"

pars.to.save <- c("mu","site","sd.year","sigma","sd.site",
                  "p.mu","p.b","trendMu","trendSd","trendWsSd","sd.p.site",
                  "betaTrendElevBasa","betaTrendElevGran",
                  "betaTrendWsAreaBasa","betaTrendWsAreaGran",
                  "betaElev","betaWsArea","betaElevArea",
                  "betaElevBasa","betaElevGran","betaWsAreaBasa","betaWsAreaGran",
                  "betaFlood","betaTrendFlood",
                  "betaTrendElev","betaTrendWsArea","betaTrendElevArea",
                  "trendWs","trend","year.ran","site.ran","eps",
                  "N","yRep","siteEffect","sd.ws","wsRan")


n <- read.csv(here("data/n.csv"))
trendOut <- read.csv(here("data/trendOut.csv"))
