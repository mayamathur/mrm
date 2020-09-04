
# audited 2020-6-19

################################## PRELIMINARIES ##################################

library(dplyr)
library(testthat)

# we first ran all results with calib.method = "DL" (two-stage method), although we didn't
#  yet have that as a parameter
# then we introduced the calib.method parameter so we could manipulate it, and we ran
#  calib.method = "MR" (one-stage) as a new round of simulations

# location for calib.method = DL results
stitched.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-4 bias diagnostics 1"

# where to put the merged and prepped results
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results"


################################## DATA PREP ##################################

setwd(stitched.data.dir)
s = read.csv("stitched.csv")

dim(s)
length(unique(s$scen.name))  # expect 240

# sanity check: simulation reps per level of manipulated scenario parameters
table(s$k)
table(s$V)
table(s$minN)
table(s$true.effect.dist)
table(s$TheoryP)  

# minutes per doParallel
summary(s$rep.time) / 60


##### Clean Up Bootstrapping Errors #####
# look at bootstrapping errors
notes = unique(s$Note)
# siimplify the notes
s$Note2 = as.character(s$Note)
s$Note2[ grepl( pattern = "computationally singular", x = s$Note ) == TRUE ] = "system is computationally singular"
s$Note2[ grepl( pattern = "Lapack", x = s$Note ) == TRUE ] = "Lapack routine dgesv: system is exactly singular" 
s$Note2[ grepl( pattern = "estimated adjustment 'w' is infinite", x = s$Note ) == TRUE ] = "estimated adjustment 'w' is infinite" 

# type of failures
prop.table( table(s$Note2, useNA = "ifany") )

# proportion failures by calib.method
s %>% group_by(calib.method) %>%
  summarise( mean( !is.na(Note2) ) )


##### Outcome and Parameter Variables #####
# "outcome" variables used in analysis
analysis.vars = c( 
  "Phat",
  
  "TheoryP.ref",
  "PhatRef", 
  
  "TheoryDiff",
  "Diff",
  
  "CoverPhat",
  "CoverPhatRef",
  "CoverDiff",
  
  "PhatCIWidth",
  "PhatRefCIWidth",
  "DiffCIWidth",
  
  ##### variables to be created in mutate below:
  "PhatAbsBias",
  "Phat2AbsBias",
  "DiffAbsBias",
  "Diff2AbsBias",
  
  "PhatBias",
  "Phat2Bias",
  "DiffBias",
  "Diff2Bias",
  
  "PhatRelBias",
  "Phat2RelBias",
  "DiffRelBias",
  "Diff2RelBias",
  
  # diagnostics regarding meta-analysis estimates
  "EstMeanRelBias",
  "EstMeanAbsBias",
  "EstVarAbsBias",
  "EstVarRelBias",
  
  # diagnostics regarding bootstraps
  "PhatEmpSD",
  "PhatBtSD",
  
  "DiffEmpSD",
  "DiffBtSD"
  )



# variables that define the scenarios
param.vars = c("scen.name",
               "calib.method",
               "k",
               "V",
               "minN",
               "true.effect.dist",
               "TheoryP")


################################## MAKE NEW VARIABLES AND AGGREGATE ##################################

# bias-corrected Phat and Diff
# using the bootstrap mean
# @note that PhatBtMn is a misnomer; it's actually the bootstrap bias estimate
s$Phat2 = s$Phat - s$PhatBtMn
s$Diff2 = s$Diff - s$DiffBtMn

# IMPORTANT NOTE: if you add variables here, need to add them to analysis.vars
#  vector above so that they are grouped in the dplyr work below
s3 = s %>%
  # iterate-level states:
  mutate( PhatBias = (Phat - TheoryP),
          Phat2Bias = (Phat2 - TheoryP),
          DiffBias = (Diff - TheoryDiff),
          Diff2Bias = (Diff2 - TheoryDiff),
          
          PhatAbsBias = abs(Phat - TheoryP),
          Phat2AbsBias = abs(Phat2 - TheoryP),
          DiffAbsBias = abs(Diff - TheoryDiff),
          Diff2AbsBias = abs(Diff2 - TheoryDiff),
          
          PhatRelBias = Phat/TheoryP,
          Phat2RelBias = Phat2/TheoryP,
          DiffRelBias = Diff/TheoryDiff,
          Diff2RelBias = Diff2/TheoryDiff,
          
          # diagnostics
          EstMeanRelBias = EstMean / TrueMean,
          EstMeanAbsBias = abs(EstMean - TrueMean),
          
          EstVarRelBias = EstVar / TrueVar,
          EstVarAbsBias = abs(EstVar - TrueVar),
          
          PhatEmpSD = sd(Phat),
          PhatBtSD = mean(PhatBtSD),
          DiffEmpSD = sd(Diff),
          DiffBtSD = mean(DiffBtSD)
          )

# recode calib.method
s3$calib.method.pretty = NA
s3$calib.method.pretty[ s3$calib.method == "DL" ] = "Two-stage"
s3$calib.method.pretty[ s3$calib.method == "MR" ] = "One-stage"

# unique scenario variable
s3$unique.scen = paste(s3$scen.name, s3$calib.method)

##### Save Dataset ####
# saving now BEFORE we overwrite Phat, etc., with the versions that are aggregated by scenario
setwd(prepped.data.dir)
write.csv(s3, "s3_dataset_MRM.csv")

# sanity check for one scenario
# mean varies across iterates, as expected
summary(s3$PhatRelBias[s3$scen.name == "134" & s3$calib.method == "MR"])
table( s3$scen.name == "134" & s3$calib.method == "MR" )


##### Overwrite Analysis Variables As Their Within-Scenario Means #####

s4 = s3 %>%
  # scenario-level stats:
  group_by_at(param.vars) %>%
  mutate( sim.reps = n(),
          bca.success = mean( is.na(Note) ) ) %>%
  # more scenario-level stats (mean outcomes)
  group_by_at(param.vars) %>%
  mutate_at( analysis.vars,
             function(x) mean(x, na.rm = TRUE) )


# sanity check: SDs of all analysis variables should be 0 within unique scenarios
t = data.frame( s4 %>% group_by(unique.scen) %>%
              summarise_at( analysis.vars, sd ) )
expect_equal( FALSE, 
              any( !as.matrix( t[, 2:(ncol(t)) ] ) %in% c(0, NA) ) )


# sanity check for one scenario
# same mean as above but no longer varies across scenarios
table(s4$PhatRelBias[s4$scen.name == "134" & s4$calib.method == "MR"])


# make aggregated data by keeping only first row for each 
#  combination of scenario name and calib.method
s4$unique.scen = paste(s4$scen.name, s4$calib.method)
agg = s4[ !duplicated(s4$unique.scen), ]
dim(agg)  # should be 240 * 2 methods = 480


# note some scenarios always have NA for coverage:
table(is.na(s4$CoverDiff)) 

# **proportion of scenarios removed due to frequent BCA failure
# e.g., because Phatdiff was almost 0, and k was so large that every boot iterate had PhatDiff = 0
mean(agg$bca.success < 0.05)

##### Save Intermediate Datasets #####
setwd(prepped.data.dir)
write.csv(agg, "agg_dataset_with_bca_failures_MRM.csv")


################################## MAKE FINAL ANALYSIS DATASET ##################################

# remove scenarios with too many BCa failures
agg = agg %>% filter(bca.success > 0.05)
dim(agg)

# simulation reps per scenario
summary(agg$sim.reps)
sort(agg$sim.reps)

##### Save Final Dataset #####
setwd(prepped.data.dir)
write.csv(agg, "*agg_dataset_as_analyzed.csv")


