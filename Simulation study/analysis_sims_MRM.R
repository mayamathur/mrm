
# "**" = result reported in paper



################################## PRELIMINARIES ##################################

library(dplyr)
library(xtable)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/*2020-6-19 merged results in paper"
results.dir = prepped.data.dir
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"

setwd(prepped.data.dir)
# data aggregated by scenario
agg.all = read.csv("*agg_dataset_as_analyzed.csv")
# data for each simulation iterate
s = read.csv("s3_dataset_MRM.csv")

setwd(code.dir)
source("helper_MRM.R")

# CHOOSE WHICH CALIB.METHOD TO ANALYZE (ONE- OR TWO-STAGE):
to.analyze = "Two-stage"
#to.analyze = "One-stage"
agg = agg.all %>% filter( calib.method.pretty %in% to.analyze )
s = s %>% filter( calib.method.pretty %in% to.analyze )

options(scipen=999)



####### @TEMP ONLY: TRY TO DIAGNOSE BIAS AND COVERAGE

# In existing sims: Is Phat always biased upward toward 0.50? Could the granularity of the CDF be a problem (in the tails or with small k); and might this even cause empirical proportion to disagree with TheoryP? When Phat is biased, does the bootstrap mean reflect this? 

# data from individual simulation iterates
t = s %>% group_by(scen.name)


################################## I^2 PARAMETERIZATION OF HETEROGENEITY ##################################


# for each possible sample size in our sims, convert all the heterogeneity 
#  parameters to I^2
table(agg$muN)
table(agg$V)

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

# for absolute bias
mod = lm( PhatAbsBias ~ k + minN + true.effect.dist + V + TheoryP,
          data = agg )
summary(mod)
# things that WORSEN abs bias: larger TheoryP

# for bias
mod = lm( PhatBias ~ k + minN + true.effect.dist + V + TheoryP,
          data = agg )
summary(mod)
# things that make bias positive: larger V, larger TheoryP, larger minN


################################## STATS AND TABLES FOR PAPER: ALL SCENARIOS ##################################

##### Summary Stats Reported In-line #####
my_summarise(agg)


##### Summary Table #####
( t1 = data.frame( my_summarise( agg %>% group_by(k, V) ) ) )
print( xtable(t1), include.rownames = FALSE )



################################## STATS AND TABLES FOR PAPER: K>=100, THEORYP > 0.05 ##################################

##### Summary Stats Reported In-line #####
my_summarise( agg %>% filter( k >= 100 & TheoryDiff >.05) )


##### Summary Table #####
( t2 = data.frame( my_summarise( agg %>% filter( k >= 100 & TheoryDiff >.05) %>% group_by(k, V) ) ) )
print( xtable(t2), include.rownames = FALSE )
