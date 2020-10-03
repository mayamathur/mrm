
# audited 2020-6-19

################################## PRELIMINARIES ##################################

rm(list=ls())

library(dplyr)
library(testthat)
library(data.table)

stitched.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/**2020-9-26 main sims (in RSM_1)"

# where to put the merged and prepped results
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/**2020-9-26 main sims (in RSM_1)"

# helper fns, such as truncLogit
setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study")
source("helper_MRM.R")

##### Read in Datasets #####

setwd(stitched.data.dir)
#s = read.csv("stitched_main_sims.csv")
s = fread("stitched_main_sims.csv")

nrow(s)/(1600*500)
length(unique(s$scen.name))/1600  # of 1600 total


# time per doParallel with 500 reps/file
# median: 17 min
# max: 52 miin
summary(s$repTime)/60

# bias correction simulations
setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-29 bias corrections")
x = fread("stitched.csv") %>% select(-c("V1", "X", "X.1"))
table(x$calib.method)

setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-28 bias corrections")
x2 = fread("stitched_MR_and_params_methods.csv") %>% select(-c("V1", "X"))
table(x2$calib.method)


################################## MAKE ITERATE-LEVEL STATS ##################################



##### Save Dataset ####
s3 = make_s3_data(.s=s)
setwd(prepped.data.dir)
fwrite(s3, "s3_dataset_MRM.csv")

#### Additional Dataset with Bias Corrections #####s
names(x)[ !names(x) %in% names(x2) ]
names(x2)[ !names(x2) %in% names(x) ]
nrow(x)
nrow(x2)
sBias = bind_rows(x, x2)
nrow(sBias)

# should be 1500 reps per scenario because each has 3 methods (MR, MR bt mn both correct, params)
table(sBias$scen.name.in.main)

s3Bias = make_s3_data(sBias)

s3Bias %>% group_by(calib.method) %>%
  summarise(mean(PhatRelBias, na.rm=TRUE),
            mean(Phat, na.rm=TRUE))

################################## MAKE NEW VARIABLES AND AGGREGATE ##################################

# read back in to avoid going through the above again
setwd(prepped.data.dir)
s3 = fread("s3_dataset_MRM.csv")

agg = make_agg_data(s3)
nrow(agg)  # would be 1600 if there were no simulation failures at the scenario level


################################## MAKE FINAL ANALYSIS DATASET ##################################

# # simulation reps per scenario
# summary(agg$sim.reps)
# sort(agg$sim.reps)

##### Save Final Dataset #####
setwd(prepped.data.dir)
fwrite(agg, "*agg_dataset_as_analyzed.csv")


