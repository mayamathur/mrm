
# audited 2020-6-19

################################## PRELIMINARIES ##################################

library(dplyr)
library(testthat)

# we first ran all results with calib.method = "DL" (two-stage method), although we didn't
#  yet have that as a parameter
# then we introduced the calib.method parameter so we could manipulate it, and we ran
#  calib.method = "MR" (one-stage) as a new round of simulations

# location for calib.method = DL results
stitched.data.dir1 = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-6-15 all with calib.method = DL"

# location for calib.method = MR results
stitched.data.dir2 = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-6-19 all with calib.method = MR"

# where to put the merged and prepped results
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results"


################################## DATA PREP ##################################

setwd(stitched.data.dir1)
s1 = read.csv("stitched.csv")
# add calib.method because we ran these sims before introducing this as a manipulated scenario parameter
s1$calib.method = "DL"

setwd(stitched.data.dir2)
s2 = read.csv("stitched.csv")

# merge them
s = rbind(s1, s2)

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

# @temp
s$Phat[ s$scen.name == "165" ]

##### Outcome and Parameter Variables #####
# "outcome" variables used in analysis
analysis.vars = c( 
  "Phat",
  
  "TheoryP.ref",
  # "PhatRef",  # NOT USING BECAUSE OF ERROR IN DOPARALLEL THAT RECORDS THIS (SEE THAT FILES)
  
  "TheoryDiff",
  "Diff",
  
  "CoverPhat",
  "CoverPhatRef",
  "CoverDiff",
  
  "PhatCIWidth",
  "PhatRefCIWidth",
  "DiffCIWidth",
  
  # to be created in mutate below:
  "PhatAbsBias",
  "DiffAbsBias",
  
  "PhatBias",
  "DiffBias",
  
  "PhatRelBias",
  "DiffRelBias",
  
  "EstMeanRelBias",
  "EstMeanAbsBias",
  "EstVarAbsBias",
  "EstVarRelBias")




param.vars = c("scen.name",
               "calib.method",
               "k",
               "V",
               "minN",
               "true.effect.dist",
               "TheoryP")


################################## MAKE NEW VARIABLES AND AGGREGATE ##################################

# IMPORTANT NOTE: if you add variables here, need to add them to analysis.vars
#  vector above so that they are grouped in the dplyr work below
s3 = s %>%
  # iterate-level states:
  mutate( PhatBias = (Phat - TheoryP),
          DiffBias = (Diff - TheoryDiff),
          
          PhatAbsBias = abs(Phat - TheoryP),
          DiffAbsBias = abs(Diff - TheoryDiff),
          
          PhatRelBias = Phat/TheoryP,
          DiffRelBias = Diff/TheoryDiff,
          
          # diagnostics
          EstMeanRelBias = EstMean / TrueMean,
          EstMeanAbsBias = abs(EstMean - TrueMean),
          
          EstVarRelBias = EstVar / TrueVar,
          EstVarAbsBias = abs(EstVar - TrueVar) )

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
# mean = 1.18 and varies across iterates, as expected
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
# still 1.18 but no longer varies across scenarios
table(s4$PhatRelBias[s4$scen.name == "134" & s4$calib.method == "MR"])


# make aggregated data by keeping only first row for each 
#  combination of scenario name and calib.method
s4$unique.scen = paste(s4$scen.name, s4$calib.method)
agg = s4[ !duplicated(s4$unique.scen), ]
dim(agg)  # should be 240 * 2 methods = 480

# @ temp
# THE SCENARIO DISAPPEARS??
# BM: FIGURE OUT WHY THE SCENARIO I WANT HAS DISAPPEARED UPON AGGREGATION
table( s4$unique.scen[ s4$scen.name == "134" & s4$calib.method == "MR" ] )
agg$PhatRelBias[ agg$unique.scen == "134 MR" ]

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


