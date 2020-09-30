
# audited 2020-6-19

################################## PRELIMINARIES ##################################

rm(list=ls())

library(dplyr)
library(testthat)
library(data.table)

stitched.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-26"


# where to put the merged and prepped results
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-26"

# helper fns, such as truncLogit
setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study")
source("helper_MRM.R")


################################## MAKE ITERATE-LEVEL STATS ##################################

setwd(stitched.data.dir)
#s = read.csv("stitched_main_sims.csv")
s = fread("stitched_main_sims.csv")

nrow(s)/(1600*500)
length(unique(s$scen.name))/1600  # of 1600 total


# time per doParallel with 500 reps/file
# median: 1/2 hr
# max: 4 hrs
summary(s$repTime)/60


# ##### Clean Up Bootstrapping Errors #####
# # look at bootstrapping errors
# notes = unique(s$Note)
# # siimplify the notes
# s$Note2 = as.character(s$Note)
# s$Note2[ grepl( pattern = "computationally singular", x = s$Note ) == TRUE ] = "system is computationally singular"
# s$Note2[ grepl( pattern = "Lapack", x = s$Note ) == TRUE ] = "Lapack routine dgesv: system is exactly singular" 
# s$Note2[ grepl( pattern = "estimated adjustment 'w' is infinite", x = s$Note ) == TRUE ] = "estimated adjustment 'w' is infinite" 
# 
# # type of failures
# prop.table( table(s$Note2, useNA = "ifany") )
# 
# # proportion failures by calib.method
# s %>% group_by(calib.method) %>%
#   summarise( mean( !is.na(Note2) ) )



# IMPORTANT NOTE: if you add variables here, need to add them to analysis.vars
#  inside make_agg_data
#  vector above so that they are grouped in the dplyr work below
s3 = s %>%
  # iterate-level states:
  mutate( 
    # bias-corrected Phat and Diff (the "2" suffix)
    # using the bootstrap mean
    Phat2 = Phat - (PhatBtMn - Phat),
    LogitPhat = truncLogit(Phat),
    LogitPhat2 = LogitPhat - ( LogitPhatBtMn - LogitPhat ),
    Diff2 = Diff - (DiffBtMn - Diff),

    PhatBias = (Phat - TheoryP),
    Phat2Bias = (Phat2 - TheoryP),
    # not doing LogitPhat on its own because redundant with Phat itself
    LogitPhat2Bias = ( expit(LogitPhat2) - TheoryP ),  # @note this is on the exponentiated scale for consistency with the others
    
    DiffBias = (Diff - TheoryDiff),
    Diff2Bias = (Diff2 - TheoryDiff),
    
    PhatAbsBias = abs(Phat - TheoryP),
    Phat2AbsBias = abs(Phat2 - TheoryP),
    # not doing LogitPhat on its own because redundant with Phat itself
    LogitPhat2AbsBias = abs( expit(LogitPhat2) - TheoryP ),
    
    DiffAbsBias = abs(Diff - TheoryDiff),
    Diff2AbsBias = abs(Diff2 - TheoryDiff),
    
    # @note that these are relative ABSOLUTE bias
    PhatRelBias = PhatAbsBias/TheoryP,
    Phat2RelBias = Phat2AbsBias/TheoryP,
    LogitPhat2RelBias = LogitPhat2AbsBias/TheoryP,
    
    DiffRelBias = DiffAbsBias/TheoryDiff,
    Diff2RelBias = Diff2AbsBias/TheoryDiff,
    
    # diagnostics
    EstMeanAbsBias = abs(EstMean - TrueMean),
    EstMeanRelBias = EstMeanAbsBias / TrueMean,
    
    EstVarAbsBias = abs(EstVar - TrueVar),
    EstVarRelBias = EstVarAbsBias / TrueVar )

# recode calib.method
s3$calib.method.pretty = NA
s3$calib.method.pretty[ s3$calib.method == "DL" ] = "Two-stage"
s3$calib.method.pretty[ s3$calib.method == "MR" ] = "One-stage"
# @add the bias corrections here

# unique scenario variable
#s3$unique.scen = paste(s3$scen.name, s3$calib.method)

# remove dumb columns
s3 = s3 %>% select( -c("V1", "X.1") )

##### Save Dataset ####
# saving now BEFORE we overwrite Phat, etc., with the versions that are aggregated by scenario
setwd(prepped.data.dir)
fwrite(s3, "s3_dataset_MRM.csv")

################################## MAKE NEW VARIABLES AND AGGREGATE ##################################

# read back in to avoid going through the above again
setwd(prepped.data.dir)
s3 = fread("s3_dataset_MRM.csv")

agg = make_agg_data(s3)
dim(agg)  # should be 1600?


# # we never have to remove scenarios now :)
# # boot failures
# mean(agg$PhatBtFail); summary(agg$PhatBtFail)
# mean(agg$DiffBtFail); summary(agg$DiffBtFail)



################################## MAKE FINAL ANALYSIS DATASET ##################################

# # simulation reps per scenario
# summary(agg$sim.reps)
# sort(agg$sim.reps)

##### Save Final Dataset #####
setwd(prepped.data.dir)
fwrite(agg, "*agg_dataset_as_analyzed.csv")


