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
install_local("/Users/timothylambert/Repos/shenAquatics", force = TRUE)


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
# # (note: manual is saved in Repos, not shenAquatics)


install_github("evanchildress/plotHacks")

# Load the package
library(shenAquatics)
??shenAquatics


#### Revise some functions: ####

# aqConnector() -- ODBC database location has been updated to my older MacBook Pro.
library(here)
source(file = "/Users/timothylambert/Repos/shenAquatics/R/aqConnector.R")







# TESTING -- getting ODBC drivers to work:

# Confirm the path is correct by reading in a csv
wsArea <- read.csv(file = "/Users/timothylambert/Repos/shenAquatics/data/wsArea.csv")
X <- read.csv(file = here("data/predSiteAttributes.csv"))


con<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=/Users/timothylambert/Repos/shenAquatics/data/All_Stream_data_2020.accdb")

RShowDoc("RODBC", package="RODBC")



library(RODBC)

sqlTables(con)



con <- odbcConnectAccess2007('your_db_file.accdb')
sqlFetch(con, 'your_table')

odbcClose(con)

