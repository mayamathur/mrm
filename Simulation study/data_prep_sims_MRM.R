
# "**" = result reported in paper

################################## PRELIMINARIES ##################################

library(dplyr)

stitched.data.dir = "~/Desktop"
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results"


################################## DATA PREP ##################################

setwd(stitched.data.dir)
s = read.csv("stitched.csv")

dim(s)
length(unique(s$scen.name))  # of 240 total

# sanity check: simulation reps per level of manipulated scenario parameters
table(s$k)
table(s$V)
table(s$minN)
table(s$true.effect.dist)
table(s$TheoryP)  # P = 0.50 still running

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
prop.table( table(s$Note2, useNA = "ifany") )


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
  
  # to be created in mutate below:
  "PhatAbsBias",
  "PhatRefAbsBias",
  "DiffAbsBias")

param.vars = c("scen.name",
               "k",
               "V",
               "minN",
               "true.effect.dist",
               "TheoryP")


################################## MAKE NEW VARIABLES AND AGGREGATE ##################################

s2 = s %>%
  # iterate-level state:
  mutate( PhatAbsBias = abs(Phat - TheoryP),
          PhatRefAbsBias = abs(PhatRef - TheoryP.ref),
          DiffAbsBias = abs(Diff - TheoryDiff) ) %>%
  # scenario-level stats:
  group_by_at(param.vars) %>%
  mutate( sim.reps = n(),
          bca.success = mean( is.na(Note) ) ) %>%
  # more scenario-level stats (mean outcomes)
  group_by_at(param.vars) %>%
  mutate_at( analysis.vars,
             function(x) mean(x, na.rm = TRUE) )

table(s2$bca.success[ s2$scen.name == 26])  # ~~~ remove
# # sanity check
# # SD should always be 0 because we overwrote the variables after grouping on scen.name
# data.frame( s2 %>% group_by(scen.name) %>%
#   summarise( sd(CoverPhat) ) )

# make aggregated data
agg = s2[ !duplicated(s2$scen.name),]

# **number of scenarios removed due to frequent BCA failure
# e.g., because Phatdiff was almost 0, and k was so large that every boot iterate had PhatDiff = 0
mean(agg$bca.success < 0.05)

##### Save Intermediate Datasets #####
setwd(results.dir)
write.csv(s2, "s2_dataset_MRM.csv")
write.csv(agg, "agg_dataset_with_bca_failures_MRM.csv")


################################## FINAL ANALYSIS DATASET ##################################

agg = agg %>% filter(bca.success > 0.05)
dim(agg)


# simulation reps per scenario
summary(agg$sim.reps)
sort(agg$sim.reps)

##### Save Final Dataset #####
setwd(prepped.data.dir)
write.csv(agg, "*agg_dataset_as_analyzed.csv")


