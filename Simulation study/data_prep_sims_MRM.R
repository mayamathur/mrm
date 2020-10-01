
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


make_s3_data = function(.s){
  
  # IMPORTANT NOTE: if you add variables here, need to add them to analysis.vars
  #  inside make_agg_data
  #  vector above so that they are grouped in the dplyr work below
  s3 = .s %>%
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
  s3$calib.method.pretty[ s3$calib.method == "params" ] = "Parameters (diagnostic)"
  s3$calib.method.pretty[ s3$calib.method == "MR bt mean both correct" ] = "One-stage, bias-corrected"
  
  # remove dumb columns
  if ( "V1" %in% names(s3) ) s3 = s3 %>% select( -c("V1") )
  if ( "X.1" %in% names(s3) ) s3 = s3 %>% select( -c("X.1") )
  
  return(s3)
}




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


