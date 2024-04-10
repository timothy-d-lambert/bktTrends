# Detection model testing
# IN PROGRESS MARCH 25, 2024 -- I've just copied basic NIMBLE depletion model so far... need to rewrite the model for SHEN BKT, read in the data, and start running it...

# This script compares five models for detection:
# 1. A single value of p
# 2. p1 (1st pass), p2 (2nd and 3rd passes)
# 3. Variable catchability
# 4. Interference
# 5. Handling time


#### This script: 2(a) Model with a depletion process ####

#### I set up a basic Poisson/binomial N-mixture model, and then compare two alternative formulations:
#  (1) Standard: y ~ Binomial(N_i,t, p) -- N-mixture_base.R
#  (2) Model with a depletion process -- This script
#      (a) y_i,t,j ~ Binom(N_i,t, p*(1-p))
#      (b) y_i,t,j ~ Binom(N_i,t - y_i,t,j-1, p)

# The take-home message is that model 2(B) improves precision, relative to 2(A).


# Preliminaries
rm(list = ls())

library(here)
here::i_am(path = "NIMBLE/N-mixture_depletion.R")

library(nimble)
library(MCMCvis)



#### 1. Simplest possible standard model without depletion ####

# Generate data
set.seed(24)
M <- 15 # number of sites
J <- 3 # number of passes per site
C <- matrix(NA, nrow = M, ncol = J) # to contain count data

# Parameter values
lambda <- 15 # expected abundance
p <- 0.4 # P(detection)

# Generate local abundance data (the truth)
N <- rpois(n = M, lambda = lambda)

# Generate count data
C[,1] <- rbinom(n = M, size = N, prob = p)
C[,2] <- rbinom(n = M, size = N - C[,1], prob = p)
C[,3] <- rbinom(n = M, size = N - C[,1] - C[,2], prob = p)


## Analysis with NIMBLE
# model building
model <- nimbleCode({
  # priors
  lambda ~ dgamma(shape = 0.001, rate = 0.001)
  p ~ dunif(min = 0, max = 1)
  # likelihood
  for(i in 1:M) {
    N[i] ~ dpois(lambda) # state model
    # observational model:
    C[i,1] ~ dbin(p, N[i])
    C[i,2] ~ dbin(p, N[i] - C[i,1])
    C[i,3] ~ dbin(p, N[i] - C[i,1] - C[i,2])
  }
  # # derived quantity
  # lifespan <- -1/log(theta)
})

## Data -- already in C
# # read in data
# my.data <- list(released = 57, survived = 19)
my.data <- list(C = C, M = 150, J = 2)

# specify parameters to monitor
parameters.to.save <- c("lambda", "p")

# pick initial values
Nst <- apply(C, 1, sum) # SUM of counts at a site
inits <- function()(list(N = Nst))
# initial.values <- function() list(theta = runif(1,0,1))

# specify MCMC details
n.thin <- 20
n.iter <- 10000
n.burnin <- 2000
n.chains <- 3

# run NIMBLE
mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          inits = inits,
                          monitors = parameters.to.save,
                          thin = n.thin,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)

# calculate numerical summaries (requires MCMCvis)
MCMCsummary(object = mcmc.output, round = 2)

# visualize parameter posterior distribution (caterpillar plot)
MCMCplot(object = mcmc.output, 
         params = 'p')

# check convergence
MCMCtrace(object = mcmc.output,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = "p")
