## Practice with shenAquatics and bktTrends
## Author: Tim Lambert
## Started Feb. 25, 2024

# Preliminaries
library(here)
here::i_am(path = "practice-by-Tim.R")


#### Install Evan's packages ####
# only need to do this first time, or if package is updated 
library(devtools)

# Method 1: Install from GitHub
install_github("evanchildress/shenAquatics") # option A: Evan's GitHub
install_github("timothy-d-lambert/shenAquatics") # option B: from my fork/branch

# Method 2: install locally
install_local("/Users/timothylambert/Repos/shenAquatics")


# # Other options:
# #   build the package
# build(pkg = "/Users/timothylambert/Repos/shenAquatics",
#       path = NULL,
#       binary = FALSE,
#       vignettes = TRUE,
#       manual = TRUE,
#       args = NULL,
#       quiet = FALSE)
# 
# #   build the manual
# build_manual(pkg = "/Users/timothylambert/Repos/shenAquatics", path = NULL)
# # (note: saved in Repos, not shenAquatics)


install_github("evanchildress/plotHacks")

# Load the package
library(shenAquatics)
??shenAquatics


#### Revise some functions: ####

# aqConnector() -- ODBC database location has been updated to my older MacBook Pro.
library(here)
source(file = "/Users/timothylambert/Repos/shenAquatics/R/aqConnector.R")
