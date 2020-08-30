
# audited 2020-6-19

################################## PRELIMINARIES ##################################

library(dplyr)

# we first ran all results with calib.method = "DL" (two-stage method), although we didn't
#  yet have that as a parameter
# then we introduced the calib.method parameter so we could manipulate it, and we ran
#  calib.method = "MR" (one-stage) as a new round of simulations

# location for calib.method = DL results
stitched.data.dir1 = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-6-15 all with calib.method = DL"

# location for calib.method = MR results
stitched.data.dir2 = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-6-19 all with calib.method = MR"

# where to put the merged and prepped results
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/*2020-6-19 merged results in paper"


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
  
  "EstMeanAbsBias")

param.vars = c("scen.name",
               "calib.method",
               "k",
               "V",
               "minN",
               "true.effect.dist",
               "TheoryP")


################################## MAKE NEW VARIABLES AND AGGREGATE ##################################

s3 = s %>%
  # iterate-level state:
  mutate( PhatBias = (Phat - TheoryP),
          DiffBias = (Diff - TheoryDiff),
          
          PhatAbsBias = abs(Phat - TheoryP),
          DiffAbsBias = abs(Diff - TheoryDiff),
          
          PhatRelBias = Phat/TheoryP,
          DiffBias = Diff/TheoryDiff,
          
          # diagnostics
          EstMeanAbsBias = abs(EstMean - TrueMean),
          EstVarBias = abs(EstVar - TrueVar)
          ) %>%
  # scenario-level stats:
  group_by_at(param.vars) %>%
  mutate( sim.reps = n(),
          bca.success = mean( is.na(Note) ) ) %>%
  # more scenario-level stats (mean outcomes)
  group_by_at(param.vars) %>%
  mutate_at( analysis.vars,
             function(x) mean(x, na.rm = TRUE) )
# # sanity check
# # SD should always be 0 because we overwrote the variables after grouping on scen.name
# data.frame( s3 %>% group_by(scen.name) %>%
#   summarise( sd(CoverPhat) ) )

# recode calib.method
s3$calib.method.pretty = NA
s3$calib.method.pretty[ s3$calib.method == "DL" ] = "Two-stage"
s3$calib.method.pretty[ s3$calib.method == "MR" ] = "One-stage"

# make aggregated data by keeping only first row for each 
#  combination of scenario name and calib.method
s3$unique.scen = paste(s3$scen.name, s3$calib.method)
agg = s3[ !duplicated(s3$unique.scen), ]
dim(agg)  # should be 240 * 2 methods = 480

# note some scenarios always have NA for coverage:
table(is.na(s3$CoverDiff)) 

# **proportion of scenarios removed due to frequent BCA failure
# e.g., because Phatdiff was almost 0, and k was so large that every boot iterate had PhatDiff = 0
mean(agg$bca.success < 0.05)

##### Save Intermediate Datasets #####
setwd(prepped.data.dir)
write.csv(s3, "s3_dataset_MRM.csv")
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


