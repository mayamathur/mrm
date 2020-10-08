
# audited 2020-6-19


################################## PRELIMINARIES ##################################

rm(list=ls())

library(dplyr)
library(testthat)
library(data.table)

stitched.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/*Main-text sims/Raw data"

# where to put the merged and prepped results
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/*Main-text sims/Prepped data"

# helper fns, such as truncLogit
setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study")
source("helper_MRM.R")

##### Read in Datasets #####

setwd(stitched.data.dir)
# read in results for each of the three covariate contrasts of interest
s1 = fread("stitched_BC.csv")
s2 = fread("stitched_BCrare.csv")
s3 = fread("stitched_B.csv")

# make non-overlapping scenario names
s2$scen.name = s2$scen.name + max(s1$scen.name)
s3$scen.name = s3$scen.name + max(s2$scen.name)

# name the covariate patterns
s1$contrast = "BC" 
s2$contrast = "BC-rare" 
s3$contrast = "B" 

s = bind_rows(s1, s2, s3)
max(s$scen.name)

nrow(s)/(1600*500*3)  # 1600 rows per scenario (MR vs. DL * 800), 500 reps per scen, 3 covariate contrasts
length(unique(s$scen.name))/1600  # of 1600 total


# time per doParallel with 500 reps/file
# median: 22 min
# max: 137 miin
summary(s$repTime)/60


# make iterate-level stats
s3 = make_s3_data(.s=s)
setwd(prepped.data.dir)
fwrite(s3, "s3_dataset.csv")

agg = make_agg_data(s3)
nrow(agg)  # would be 1600*3 if there were no simulation failures at the scenario level

setwd(prepped.data.dir)
fwrite(agg, "*agg_dataset.csv")


#  MOVE ELSEWHERE:
# ##### Bias correction simulations (on smaller dataset) #####
# setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-29 bias corrections")
# x = fread("stitched.csv") %>% select(-c("V1", "X", "X.1"))
# table(x$calib.method)
# 
# setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-28 bias corrections")
# x2 = fread("stitched_MR_and_params_methods.csv") %>% select(-c("V1", "X"))
# table(x2$calib.method)

# #### Additional Dataset with Bias Corrections #####
# names(x)[ !names(x) %in% names(x2) ]
# names(x2)[ !names(x2) %in% names(x) ]
# nrow(x)
# nrow(x2)
# sBias = bind_rows(x, x2)
# nrow(sBias)
# 
# # should be 1500 reps per scenario because each has 3 methods (MR, MR bt mn both correct, params)
# table(sBias$scen.name.in.main)
# 
# s3Bias = make_s3_data(sBias)
# 
# s3Bias %>% group_by(calib.method) %>%
#   summarise(mean(PhatRelBias, na.rm=TRUE),
#             mean(Phat, na.rm=TRUE))

