#
# .star variables are adjusted after fitting (mean of random subtracted to make 
#it zero and added to intercept) to improve mixing

# intercepts are weighted by geology type

# flood effect is included (main and interaction with year) for sites with a 
#500-year flood in 1995 (i.e., staunton, moormans, lower rapidan) because
#all had slow rebuilds as habitat and canopy recovered

# current structure only includes random watershed level effects 

model{
  
  # Abundance model  
  
  for(i in 1:nSites){
    for(j in 1:nYears){
      N[i,j] ~ dpois(lambda[i,j])
      log(lambda[i,j]) <- siteEffect[i]+
        trend[i]*year[j] +
        year.ran.star[j] + eps.star[i,j] + 
        log(siteLength[i,j]/100)
    }
  }
  
  ## priors
  # mu.star ~ dnorm(0, 0.01)      # overall intercept

    mu.star ~ dnorm(0, 0.01) #siliciclastic intercept
    trendMu[1] ~ dnorm(0, 0.01)   #average linear trend for siliciclastic
    
    betaBasa<-0# ~ dnorm(0, 0.01) #basaltic diff from sili
    betaGran<-0# ~ dnorm(0, 0.01) #granitic diff from sili
    betaTrendBasa<-0# ~ dnorm(0, 0.01) 
    betaTrendGran<-0# ~ dnorm(0, 0.01) 
    
    # trendMu.star[i] ~ dnorm(0, 0.01)   #average linear trend by geology
    
  trendSd ~ dunif(0,1) #sd of trend across sites
  trendTau <- pow(trendSd,-2)
  
  trendWsSd ~dunif(0,1)
  trendWsTau <- pow(trendWsSd,-2)
  
  
  
  betaElev<-0#~dnorm(0,0.01)
  betaWsArea<-0#~dnorm(0,0.01)
  betaElevArea<-0#~dnorm(0,0.01)
  betaFlood~dnorm(0,0.01)
  
  betaTrendElev<-0#~dnorm(0,0.01)
  betaTrendWsArea<-0#~dnorm(0,0.01)
  betaTrendElevArea<-0#~dnorm(0,0.01)
  betaTrendFlood~dnorm(0,0.01)
  
  for(i in 1:nSites){
    siteEffect[i]~dnorm(mu.star+betaBasa*basa[i]+betaGran*gran[i]+
                          betaElev*elev[i]+betaWsArea*wsArea[i]+
                          betaElevArea*elev[i]*wsArea[i]+wsRan[watershed[i]]+
                          betaFlood*flood[i],
                        tau.site) #random site effect
    
    #trend split out such that trendMu equals the main effect of year and others represent interactions with year
    #these are site level fixed effects
    
    trendSite[i] <-trendMu[1]+betaTrendBasa*basa[i]+betaTrendGran*gran[i]+
      elev[i]*betaTrendElev+wsArea[i]*betaTrendWsArea +
      betaTrendElevArea*wsArea[i]*elev[i]+
      flood[i]*betaTrendFlood + #fixed effects
      trendWs[watershed[i]] #random watershed effect
    
    trend[i] ~ dnorm(trendSite[i],trendTau) # random site effect on trend
    
    for(j in 1:nYears){
      eps.star[i,j] ~ dnorm(0, tau)  # Over-dispersion
    }
    
  }
  for(w in 1:(nWatersheds)){
    trendWs[w]~dnorm(0,trendWsTau)
    wsRan[w]~dnorm(0,tau.ws)
    # trendWs.star[w]~dnorm(0,trendWsTau)
  }
  
  tau.site <- pow(sd.site, -2)
  sd.site ~ dunif(0,2)
  # sd2.site <- pow(sd.site, 2)
  
  tau.ws <- pow(sd.ws,-2)
  sd.ws~dunif(0,2)
  
  for (j in 1:nYears){
    year.ran.star[j] ~ dnorm(0, tau.year)  # Random year effect
  }
  
  tau.year <- pow(sd.year, -2) 
  sd.year ~ dunif(0,3)
  # sd2.year <- pow(sd.year, 2)
  
  
  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 2)
  # sigma2 <- pow(sigma, 2)
  
  
  # Detection model
  
  for(i in 1:nSites){
    for(j in 1:nYears){
      y[i,j,1] ~ dbin(p[i,j], N[i,j])
      y[i,j,2] ~ dbin(p[i,j], N[i,j]-y[i,j,1])
      y[i,j,3] ~ dbin(p[i,j], N[i,j]-y[i,j,1]-y[i,j,2])
      
      p[i,j] <- 1/(1 + exp(-lp.lim[i,j]))
      lp.lim[i,j] <- min(999, max(-999, lp[i,j]))
      lp[i,j] <- p.b*pCov[i,j] + p.site.ran[i]
    }
  }
  
  ## priors  
  p.mu ~ dnorm(0, 0.37) #less wide prior for logit to avoid clumping at the edges
  p.b ~ dnorm(0, 0.37)
  
  for(i in 1:nSites){
    p.site.ran[i] ~ dnorm(p.mu,tau.p.site) 
  }
  tau.p.site <- pow(sd.p.site, -2) 
  sd.p.site ~ dunif(0,1)
  # sd2.p.site <- pow(sd.p.site, 2)
  
  #clean up random effects
  for(j in 1:nYears){
    year.ran[j] <- year.ran.star[j]-mean(year.ran.star)
    for(i in 1:nSites){
      eps[i,j]<-eps.star[i,j]-mean(eps.star)
    }
  }
  # for(i in 1:nSites){
  #   trendSite[i] <- trendSite.star[i]-mean(trendSite.star)
  # }
  # for(w in 1:nWatersheds){
  #   trendWs[w] <- trendWs.star[w]-mean(trendWs.star)
  # }
  
  # 
  # for(i in 1:nSites){
  #   site.ran[i]<-site.ran.star[i]-mean(site.ran.star)
  # }
  #   
  
    mu[1]<-mu.star+mean(year.ran.star)+mean(eps.star)
    mu[2]<-mu[1]+betaBasa
    mu[3]<-mu[1]+betaGran
    
    trendMu[2]<-trendMu[1]+betaTrendBasa
    trendMu[3]<-trendMu[1]+betaTrendGran
    #trendMu[i] <- trendMu.star[i] + mean(trendWs.star)
  
  
  
  #posterior predictive check
  for(i in 1:nSites){
    for(j in 1:nSampleYears[i]){
      nRep[i,sampleYears[i,j]] ~ dpois(lambda[i,sampleYears[i,j]])
      yRep[i,sampleYears[i,j],1] ~ dbin(p[i,sampleYears[i,j]], nRep[i,sampleYears[i,j]])
      yRep[i,sampleYears[i,j],2] ~ dbin(p[i,sampleYears[i,j]], nRep[i,sampleYears[i,j]]-yRep[i,sampleYears[i,j],1])
      yRep[i,sampleYears[i,j],3] ~ dbin(p[i,sampleYears[i,j]], nRep[i,sampleYears[i,j]]-yRep[i,sampleYears[i,j],1]-yRep[i,sampleYears[i,j],2])
    }
  }
  
  
  
}