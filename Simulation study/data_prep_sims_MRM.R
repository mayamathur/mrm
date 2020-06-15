
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





# # try to understand the CoverDiff issue
# mod = lm( CoverDiff ~ k + minN + true.effect.dist + TheoryP + V*TheoryDiff,
#           data = agg )
# summary(mod)
# # ~~ bm: look at linear predictor of this to see when expectation is at least 0.9?
# # **things that IMPROVE coverage: larger k, larger TheoryP
# # **things that WORSEN coverage: larger V
# # I suspect the small TheoryDiff also doesn't help
# 
# View( agg %>%
#         #filter(k>=150) %>%
#         filter(k>=50 & TheoryDiff>0.05) %>%
#         group_by( true.effect.dist, V ) %>%
#         summarise( n(),
#                    
#                    mean(TheoryDiff),
#                    mean(TheoryP),
#                    mean(TheoryP.ref),
#                    
#                    mean(CoverDiff, na.rm = TRUE),
#                    min(CoverDiff, na.rm = TRUE),
#                    
#                    mean(CoverPhat, na.rm = TRUE),
#                    min(CoverPhat, na.rm = TRUE),
#                    
#                    mean(CoverPhatRef, na.rm = TRUE),
#                    min(CoverPhatRef, na.rm = TRUE)) )
# # need a pretty big k for this to work well
# # restricting to large TheoryDiff helps, too
# # k>=50 and TheoryDiff>.1 gives min coverage at least 89%
# # k>= 100 on its own doesn't work for high heterogeneity:
# 
# table(agg$TheoryDiff>.1)
# 
# # look at scenarios with inadequate coverage of Phat or Phat.ref
# View( agg %>% filter( CoverPhat < 0.9 | CoverPhatRef < 0.9 ) )
# # most evil scenarios:
# # k=150 or 50, V = 0.01, minN=800, normal, TheoryP=0.05
# # scenarios 26 and 28
# 
# # the problem is that the CI was almost always missing for this scenario
# temp = s[ s$scen.name == 26,]
# 
# table(is.na(temp$Note))
