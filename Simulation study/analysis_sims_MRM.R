
# "**" = result reported in paper



################################## PRELIMINARIES ##################################

library(dplyr)
library(xtable)

options(scipen=999)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results"
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
#to.analyze = "Two-stage"
to.analyze = "One-stage"
agg = agg.all %>% filter( calib.method.pretty %in% to.analyze )
s = s %>% filter( calib.method.pretty %in% to.analyze )



####### @TEMP ONLY: TRY TO DIAGNOSE BIAS AND COVERAGE
# bm
# In existing sims: Is Phat always biased upward toward 0.50? Could the granularity of the CDF be a problem (in the tails or with small k); and might this even cause empirical proportion to disagree with TheoryP? When Phat is biased, does the bootstrap mean reflect this? 

# compare to old versions
agg.old = read.csv("*agg_dataset_as_analyzed.csv")
agg.old$Phat[ agg.old$unique.scen == "134 MR" ]  # agrees

s$Phat[ s$unique.scen == "134 MR" ]
mean(s$Phat[ s$unique.scen == "134 MR" ])
agg$Phat[ agg$unique.scen == "134 MR" ]

# only look at scenarios that remained in agg because they had sufficient
#  successful bootstrap iterates
scens = unique(agg$scen.name)

# data from individual simulation iterates
# look for a single scenario with bad bias
t = s %>% filter( scen.name %in% scens ) %>%
  group_by(scen.name) %>%
  summarise( PhatRelBias = mean(PhatRelBias),
             DiffRelBias = mean(DiffRelBias),
             PhatAbsBias = mean(PhatAbsBias),
             PhatBias = mean(PhatBias),
             
             TheoryP = mean(TheoryP),
             Phat = mean(Phat),
             
             TheoryDiff = mean(TheoryDiff),
             Diff = mean(Diff)
  ) %>%
  arrange( desc(PhatRelBias) )
View(t)
# scenario 164 is interesting because TheoryP = 0.20 (not extreme) but average Phat is 0.31
# and TheoryDiff is biased upward as well
# and k=20 for this one

# maybe also look for a scenario with larger k, like 50-100, but bad PhatDiff

temp = s[ s$unique.scen == "164 MR", ]
View(temp)

# Phat
table(temp$TheoryP)
mean(temp$Phat)

# estimated mean is almost exactly right
table(temp$TrueMean)
mean(temp$EstMean)
mean(temp$EstMean) / temp$TrueMean[1]  # relative bias: 1.00

# estimated heterogeneity is a little high but fairly close
table(temp$TrueVar)
mean(temp$EstVar)
mean(temp$EstVar)/temp$TrueVar[1]  # relative bias in heterogeneity estimation  = 1.24***

##### Compare Relative Bias of Ours to Standard Estimands #####

library(ggplot2)
ggplot( data = agg, aes(x = EstVarRelBias, y = PhatRelBias) ) +
  geom_point()

# **important: point out that heterogeneity estimation doesn't perform much better than Phat and PhatDiff for bias
summary(agg$PhatRelBias)
summary(agg$DiffRelBias)
summary(agg$EstMeanRelBias)
summary(agg$EstVarRelBias)

####### @END OF TEMP

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

# for relative bias
mod = lm( PhatRelBias ~ k + minN + true.effect.dist + V + TheoryP,
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
