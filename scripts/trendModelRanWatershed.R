model{
  
  # Abundance model  
  
  for(i in 1:nSites){
    for(j in 1:nYears){
      N[i,j] ~ dpois(lambda[i,j])
      log(lambda[i,j]) <- mu.star+trend[i]*year[j] +
        site.ran.star[i] + year.ran.star[j] + eps[i,j] + log(siteLength[i,j]/100)
    }
  }
  
  ## priors
  mu.star ~ dnorm(0, 0.01)      # overall intercept
  trendMu ~ dnorm(0, 0.01)   #average linear trend
  trendSd ~ dunif(0,1) #variance of trend
  trendTau <- pow(trendSd,-2)
  
  trendWsSd ~dunif(0,1)
  trendWsTau <- pow(trendWsSd,-2)
  
  for(w in 1:nWatersheds){
    trendWs[w]~dnorm(trendMu,trendWsTau)
  }
  
  for(i in 1:nSites){
    site.ran.star[i] ~ dnorm(0,tau.site)     # random site effects
    trend[i] ~ dnorm(trendWs[watershed[i]],trendTau)#trend with random site effect
    # trend[i]<-trendMu #trend without random site effect
  }
  tau.site <- pow(sd.site, -2)
  sd.site ~ dunif(0,2)
  # sd2.site <- pow(sd.site, 2)
  
  for (j in 1:nYears){
    year.ran.star[j] ~ dnorm(0, tau.year)  # Random year effect
  }
  tau.year <- pow(sd.year, -2) 
  sd.year ~ dunif(0,2)
  # sd2.year <- pow(sd.year, 2)
  
  for(i in 1:nSites){
    for(j in 1:nYears){
      eps[i,j] ~ dnorm(0, tau)  # Over-dispersion
    }
  }
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
  p.mu ~ dnorm(0, 0.37)
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
  }
  
  for(i in 1:nSites){
    # trend[i]<-trend.star[i]-mean(trend.star)+trendMu
    site.ran[i]<-site.ran.star[i]-mean(site.ran.star)
  }
  
  mu<-mu.star+mean(year.ran.star)+mean(site.ran.star)
  
  #posterior predictive check
  # for(i in 1:nSites){
  #   for(j in 1:nSampleYears[i]){
  #     nRep[i,sampleYears[i,j]] ~ dpois(lambda[i,sampleYears[i,j]])
  #     yRep[i,sampleYears[i,j],1] ~ dbin(p[i,sampleYears[i,j]], nRep[i,sampleYears[i,j]])
  #     yRep[i,sampleYears[i,j],2] ~ dbin(p[i,sampleYears[i,j]], nRep[i,sampleYears[i,j]]-yRep[i,sampleYears[i,j],1])
  #     yRep[i,sampleYears[i,j],3] ~ dbin(p[i,sampleYears[i,j]], nRep[i,sampleYears[i,j]]-yRep[i,sampleYears[i,j],1]-yRep[i,sampleYears[i,j],2])
  #   }
  # }
  
}