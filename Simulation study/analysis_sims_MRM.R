
# "**" = result reported in paper

# compare CI width for the two methods



################################## PRELIMINARIES ##################################

library(dplyr)
library(xtable)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results"
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results"
#overleaf.dir = /Users/mmathur/Dropbox/Apps/Overleaf/Moderators\ in\ meta-regression/From\ R
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"

setwd(prepped.data.dir)
agg = read.csv("*agg_dataset_as_analyzed.csv")

setwd(code.dir)
source("helper_MRM.R")

################################## I^2 PARAMETERIZATION OF HETEROGENEITY ##################################


# for each possible sample size in our sims, convert all the heterogeneity 
#  parameters to I^2
round( I2( t2 = c(.01, .04, .25),
           N = 100), 2 )

round( I2( t2 = c(.01, .04, .25),
           N = 850), 2 )
# so the tau = 0.5 (t2 = 0.25) scenarios are approximately I^2 = 50%
# which is exactly Higgins' (2003) benchmark for "moderate" heterogeneity :)



################################## REGRESS PERFORMANCE METRICS ON SIMULATION PARAMETERS ##################################

##### Which Scen Params Predict Coverage And Bias? #####
# coverage of difference
mod = lm( CoverDiff ~ k + minN + true.effect.dist + V + TheoryP + TheoryDiff,
          data = agg )
summary(mod)
# **things that IMPROVE coverage: larger k, smaller V, larger TheoryP, smaller TheoryDiff
# **things that don't matter much: minN, true effect distribution

# and for bias
mod = lm( PhatAbsBias ~ k + minN + true.effect.dist + V + TheoryP + TheoryDiff,
          data = agg )
summary(mod)




################################## STATS AND TABLES FOR PAPER: ALL SCENARIOS ##################################

##### Summary Stats Reported In-line #####

data.frame( agg %>%
  group_by(TRUE) %>%  # for some reason, summarise doesn't work without grouping...
  summarise( n.scens = n(),
             
             PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
             MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
             MinCoverPhat = min(CoverPhat, na.rm = TRUE),
             
             PhatRefAbsBias = mean(PhatRefAbsBias, na.rm = TRUE),
             MeanCoverPhatRef = mean(CoverPhatRef, na.rm = TRUE),
             MinCoverPhatRef = min(CoverPhatRef, na.rm = TRUE),
             
             DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
             MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
             MinCoverDiff = min(CoverDiff, na.rm = TRUE) ) )

##### Summary Table #####
t1 = data.frame( agg %>%
                   group_by(k, V) %>%  # for some reason, summarise doesn't work without grouping...
                   summarise( n.scens = n(),
                              
                              PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
                              MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
                              MinCoverPhat = min(CoverPhat, na.rm = TRUE),
                              
                              PhatRefAbsBias = mean(PhatRefAbsBias, na.rm = TRUE),
                              MeanCoverPhatRef = mean(CoverPhatRef, na.rm = TRUE),
                              MinCoverPhatRef = min(CoverPhatRef, na.rm = TRUE),
                              
                              DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
                              MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
                              MinCoverDiff = min(CoverDiff, na.rm = TRUE) ) )

t1 = round(t1, 2)
print( xtable(t1), include.rownames = FALSE )


################################## STATS AND TABLES FOR PAPER: K>=100, THEORYP > 0.05 ##################################

##### Summary Stats Reported In-line #####
data.frame( agg %>%
  #filter( k >= 100 & TheoryDiff >.05) %>%
  filter( k >= 100 & TheoryDiff >.05) %>%
  group_by(TRUE) %>%  # for some reason, summarise doesn't work without grouping...
  summarise( n.scens = n(),
             
             PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
             MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
             MinCoverPhat = min(CoverPhat, na.rm = TRUE),

             PhatRefAbsBias = mean(PhatRefAbsBias, na.rm = TRUE),
             MeanCoverPhatRef = mean(CoverPhatRef, na.rm = TRUE),
             MinCoverPhatRef = min(CoverPhatRef, na.rm = TRUE),

             DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
             MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
             MinCoverDiff = min(CoverDiff, na.rm = TRUE) ) )

##### Summary Table #####
t2 = data.frame( agg %>%
              filter( k >= 100 & TheoryDiff >.05) %>%
              group_by(k, V) %>%  # for some reason, summarise doesn't work without grouping...
              summarise( n.scens = n(),
                         
                         PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
                         MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
                         MinCoverPhat = min(CoverPhat, na.rm = TRUE),
                         
                         PhatRefAbsBias = mean(PhatRefAbsBias, na.rm = TRUE),
                         MeanCoverPhatRef = mean(CoverPhatRef, na.rm = TRUE),
                         MinCoverPhatRef = min(CoverPhatRef, na.rm = TRUE),
                         
                         DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
                         MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
                         MinCoverDiff = min(CoverDiff, na.rm = TRUE) ) )
t2 = round(t2, 2)
print( xtable(t2), include.rownames = FALSE )
