
# check on sims are they run

setwd("~/Desktop")
s = read.csv("stitched.csv")
dim(s)  # 471600 on Sherlock
length(unique(s$scen.name))  # 1014 on Sherlock (of 1600)

agg  = s %>% group_by(scen.name, clustered) %>%
  summarise( mean(is.na(PhatLo)),
             mean(is.na(DiffLo)),
             
             .TheoryP = TheoryP[1],
             PhatMn = mean(Phat),
             PhatRelBias = mean(Phat)/.TheoryP,
             CoverPhat = mean(CoverPhat, na.rm = TRUE),
             
             .TheoryDiff = TheoryDiff[1],
             DiffMn = mean(Diff),
             DiffRelBias = mean(Diff)/.TheoryDiff,
             CoverDiff = mean(CoverDiff, na.rm = TRUE),
             
             EstVarRelBias = mean(EstVar)/V
             
  )

data.frame(agg)

mean(agg$CoverDiff); min(agg$CoverDiff)
mean(agg$CoverPhat); min(agg$CoverPhat)
mean(agg$PhatRelBias); min(agg$PhatRelBias); max(agg$PhatRelBias)
mean(agg$EstVarRelBias); min(agg$EstVarRelBias);  max(agg$EstVarRelBias)


# 
# # "**" = result reported in paper
# 
# library(dplyr)
# 
# # bm: go back to data_prep_sims to get the scenario variables
# # can then use this to do relbias2 ~ scen characteristics
# # i.e., what are the scenarios for which the bootstrap correction works?
# 
# # quickly look at simulations
# 
# setwd("~/Desktop")
# s = read.csv("stitched.csv")
# length(unique(s$scen.name))
# 
# agg = s %>%
#   filter( !is.na(PhatBtMn) ) %>%
#   group_by(scen.name) %>%
#   summarise( n = n(),
#              k = k[1],
#              PhatEmpSD = sd( Phat, na.rm = TRUE ),
#              TheoryP = TheoryP[1],
#              Phat = mean(Phat),
#              # subtract off the bias
#              Phat2 = mean(Phat - PhatBtMn, na.rm = TRUE),
#              bias = Phat - TheoryP[1],
#              
#              relbias = Phat/TheoryP[1],
#              bias2 = Phat2 - TheoryP[1],
#              relbias2 = Phat2/TheoryP[1],
#              
#              bias.bt = mean(PhatBtMn, na.rm=TRUE),
#              
#              PhatBtSD = mean(PhatBtSD, na.rm = TRUE),
#              
#              # currently filtering for never failing
#              propBootFail = mean(!is.na(Note)),
#               )
# 
# 
# # how well does boot bias predict actual bias?
# # unit of analysis is scenarios
# # hope for slope of 1
# mod = lm( agg$bias ~ agg$bias.bt )
# summary(mod)
# 
# xmax = max( c(agg$bias.bt, agg$bias ), na.rm = TRUE )
# xmin = min( c(agg$bias.bt, agg$bias ), na.rm = TRUE )
# 
# # bm: try to figure out characteristics of scenarios in which boot mean isn't a good indicator of bias
# 
# 
# 
# 
# library(ggplot2)
# ggplot( data = agg,
#         aes( x = bias,
#              y = bias.bt ) ) + 
#   geom_point(alpha = 0.4) + 
#   # hoped relationship
#   geom_abline(intercept = 0,
#               slope = 1, 
#               color = "gray") + 
#   # actual relationship:
#   geom_abline(intercept = coef(mod)[1],
#               slope = coef(mod)[2]) + 
#   scale_x_continuous( limits = c(xmin, xmax) ) +
#   scale_y_continuous( limits = c(xmin, xmax) ) +
#   theme_classic()
# 
# 
# # also see whether we can predict low bias with observable characteristics
# # and coverage
# 
# # look at bias-corrected Phat vs. ordinary Phat
# # **slope is 2 whether we include or remove scenarios with any bootstrap failures, so that's not the issue
# summary( agg$bias ); summary( agg$bias2 )
# summary(agg$relbias); summary(agg$relbias2)
# 
# 
# 
# # seems a little better with bias-correction?
# 
##### Look at Dist of Phat for Handful of Scenarios #####

# bm
# scenarios with little bias
agg = agg %>% arrange( PhatRelBias )

( badScen = agg$unique.scen[ agg$PhatRelBias > 1.2 ] )
( goodScen = agg$unique.scen[ agg$PhatRelBias < 1.01 ][1:5] )

temp = s %>% filter(unique.scen %in% goodScen)

# Phat mean for scenario
temp = temp %>% group_by(unique.scen) %>%
  mutate( PhatMn = mean(Phat, na.rm = TRUE),
          PhatBtMn.scen = mean(PhatBtMn + Phat, na.rm = TRUE) )

ggplot( ) +
  geom_histogram( data = temp,
                  aes( x = Phat ),
                  alpha = 0.3,
                  color = "red") +

  # distribution of bootstrap means
  # PhatBtMn = bootmean - Phat
  geom_histogram( data = temp,
                  aes( x = PhatBtMn + Phat ),  # actual bootstrap mean
                  alpha = 0.3,
                  color = "blue") +

  geom_vline( data = temp,
              aes(xintercept = TheoryP),
              lty = 2) +

  geom_vline( data = temp,
              aes(xintercept = PhatMn),
              color = "red",
              lty = 2) +

  geom_vline( data = temp,
              aes(xintercept = PhatBtMn.scen ),
              color = "blue",
              lty = 2) +

  theme_classic() +
  facet_wrap( ~ unique.scen,
              scales = "free"  )

# black line: truth
# red: Phat (an overestimate for all of these)
# blue: boot mean also overestimates Phat, but not by as much as Phat overestimates TheoryP
# the bad scenarios are highly right-skewed

# @working on log scale for CI construction to make more pivotal?
# i.e., bootstrap the log-Phats
# this looks better:
# interpolation might help bootstrap as well so that estimator is more continuous
ggplot( ) +
  geom_histogram( data = temp,
                  aes( x = log(Phat) ),
                  alpha = 0.3,
                  color = "red") +
  
  theme_classic() +
  facet_wrap( ~ unique.scen,
              scales = "free"  )


##### Look at Dist of EstVar for Handful of Scenarios #####

# bm
# scenarios with little bias
agg = agg %>% arrange( PhatRelBias )ss

( badScen = agg$unique.scen[ agg$PhatRelBias > 1.2 ] )
( goodScen = agg$unique.scen[ agg$PhatRelBias < 1.01 ][1:5] )

temp = s %>% filter(unique.scen %in% goodScen)


ggplot( ) +
  geom_histogram( data = temp,
                  aes( x = EstVar ),
                  alpha = 0.3,
                  color = "black") +
  
  # distribution of bootstrap means
  # PhatBtMn = bootmean - Phat
  geom_histogram( data = temp,
                  aes( x = EstVar ),  # actual bootstrap mean
                  alpha = 0.3,
                  color = "blue") +
  
  geom_vline( data = temp,
              aes(xintercept = V),
              lty = 2) +

  
  theme_classic() +
  facet_wrap( ~ unique.scen,
              scales = "free"  )




##### Look for a Highly Biased Scenario in Which Using Boot Mean Doesn't Help #####
# 224
View( agg %>% arrange( relbias2 ) )

# bm
agg %>% filter( relbias2 > 1.2 &
                  TheoryP >= 0.20 &
                  k >= 10 )

temp = s %>% filter( scen.name == 14 )

mean(temp$Phat)
mean(temp$Phat2, na.rm = TRUE)
mean(temp$TheoryP)
mean(temp$Phat + temp$PhatBtMn, na.rm = TRUE)  # Phat2

ggplot(  ) +
  geom_histogram( data = temp,
                aes( x = Phat ),
                alpha = 0.3) +

  # distribution of bootstrap means
  # PhatBtMn = bootmean - Phat
  geom_histogram( data = temp,
                aes( x = PhatBtMn + Phat ),  # actual bootstrap mean
                alpha = 0.3,
                color = "blue") +

  # black line: truth
  geom_vline( data = temp,
              aes(xintercept = temp$TheoryP[1]),
              lty = 2) +

  # red line: mean of Phats
  geom_vline( data = temp,
              aes(xintercept = mean(temp$Phat, na.rm = TRUE)),
              color = "red",
              lty = 2) +


  theme_classic()
# 
# # why is the bootstrap mean ever NA?
# # there are some entire iterates in which the whole bootstrap thing is NA
# # should probably use my_boot to retain those samples
# # is the story better for scenarios in which this didn't happen often?
# # bm
# 
# summary( lm( agg$bias ~ agg$bias.bt ) )
# 
# # maybe also look at this: https://stats.stackexchange.com/questions/129478/when-is-the-bootstrap-estimate-of-bias-valid/310042#:~:text=The%20bootstrap%20estimate%20of%20bias%20is%20between%20an%20estimator%20%CB%86,had%20the%20population%20at%20hand.
# 
# ##### Bootstrap SE #####
# # **the bootstrap SEs are almost exactly right
# # **so it's the bias that is the problem
# 
# # boot SD is slightly lower than empirical, but not by much
# # slope = 0.97 vs. expected 1 
# summary( lm( agg$PhatEmpSD ~ agg$PhatBtSD ) )
# 
# # average ratio of the two is almost exactly one
# mean( agg$PhatBtSD / agg$PhatEmpSD )
# 
# xmax = max( c(agg$PhatEmpSD, agg$PhatBtSD ), na.rm = TRUE )
# xmin = min( c(agg$PhatEmpSD, agg$PhatBtSD ), na.rm = TRUE )
# 
# library(ggplot2)
# ggplot( data = agg,
#         aes( x = PhatEmpSD,
#              y = PhatBtSD ) ) + 
#   geom_point(alpha = 0.4) + 
#   geom_abline(intercept = 0,
#               slope = 1) + 
#   scale_x_continuous( limits = c(xmin, xmax) ) +
#   scale_y_continuous( limits = c(xmin, xmax) ) +
#   theme_classic()




##### OLD:


################################## PRELIMINARIES ##################################

rm(list=ls())

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

# restrict s to the analyzed scenarios in agg
s = s %>% filter( unique.scen %in% agg$unique.scen )

# ####### @TEMP ONLY: TRY TO DIAGNOSE BIAS AND COVERAGE
# # bm
# # In existing sims: Is Phat always biased upward toward 0.50? Could the granularity of the CDF be a problem (in the tails or with small k); and might this even cause empirical proportion to disagree with TheoryP? When Phat is biased, does the bootstrap mean reflect this? 
# 
# # compare to old versions
# agg.old = read.csv("*agg_dataset_as_analyzed.csv")
# agg.old$Phat[ agg.old$unique.scen == "134 MR" ]  # agrees
# 
# # sanity check for one scenario
# s$Phat[ s$unique.scen == "134 MR" ]
# mean(s$Phat[ s$unique.scen == "134 MR" ])
# agg$Phat[ agg$unique.scen == "134 MR" ]
# 
# # only look at scenarios that remained in agg because they had sufficient
# #  successful bootstrap iterates
# scens = unique(agg$scen.name)
# 
# ##### Look at Evil Scenario  164 #####
# # data from individual simulation iterates
# # look for a single scenario with bad bias
# t = s %>% filter( scen.name %in% scens ) %>%
#   group_by(scen.name) %>%
#   summarise( PhatRelBias = mean(PhatRelBias),
#              DiffRelBias = mean(DiffRelBias),
#              PhatAbsBias = mean(PhatAbsBias),
#              PhatBias = mean(PhatBias),
#              
#              TheoryP = mean(TheoryP),
#              Phat = mean(Phat),
#              
#              TheoryDiff = mean(TheoryDiff),
#              Diff = mean(Diff)
#   ) %>%
#   arrange( desc(PhatRelBias) )
# # View(t)
# # scenario 164 is interesting because TheoryP = 0.20 (not extreme) but average Phat is 0.31
# # and TheoryDiff is biased upward as well
# # and k=20 for this one
# 
# # maybe also look for a scenario with larger k, like 50-100, but bad PhatDiff
# 
# temp = s[ s$unique.scen == "164 MR", ]
# View(temp)
# 
# # Phat
# table(temp$TheoryP)
# mean(temp$Phat)
# mean(temp$Phat)/temp$TheoryP[1]
# 
# # estimated mean is almost exactly right
# table(temp$TrueMean)
# mean(temp$EstMean)
# mean(temp$EstMean) / temp$TrueMean[1]  # relative bias: 1.00
# 
# # estimated heterogeneity is a little high but fairly close
# table(temp$TrueVar)
# mean(temp$EstVar)
# mean(temp$EstVar)/temp$TrueVar[1]  # relative bias in heterogeneity estimation  = 1.24***
# 
# ##### Compare Relative Bias of Ours to Standard Estimands #####
# 
# library(ggplot2)
# ggplot( data = agg, aes(x = EstVarRelBias, y = PhatRelBias) ) +
#   geom_point()
# 
# # **important: point out that heterogeneity estimation doesn't perform much better than Phat and PhatDiff for bias
# summary(agg$PhatRelBias)
# summary(agg$DiffRelBias)
# summary(agg$EstMeanRelBias)
# summary(agg$EstVarRelBias)
# 
# ####### @END OF TEMP

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

##### Predictors of PhatRelBias #####
# focus on observable variables within scenarios
# i.e., estimates rather than parameters
obsVars = c("k", "minN", "Phat", "Diff", "EstMean", "EstVar")

string = paste( "PhatRelBias ~", paste(obsVars, collapse = "+"), sep="" )
mod = lm( eval(parse(text=string)),
          data = agg )
summary(mod)
# things that IMPROVE relative Phat bias (negative coefficients):
# larger k
# larger minN
# larger Phat
# LARGER EstVar - important because above wanted smaller EstVar 

# @coefficient direction reverses based on whether we aggregate by 
#  scenario or not:
# this happens if we do NOT restrict s to the analyzed scenarios in agg
summary(lm(PhatRelBias ~ Phat, data = agg))
summary(lm(PhatRelBias ~ Phat, data = s))

mean(s$PhatRelBias[s$Phat>0.2])
mean(s$PhatRelBias)


##### Predictors of DiffRelBias #####
string = paste( "DiffRelBias ~", paste(obsVars, collapse = "+"), sep="" )
mod = lm( eval(parse(text=string)),
          data = agg )
summary(mod)
# things that IMPROVE relative diff bias (negative coefficients):
# larger k
# larger minN
# larger diff
# smaller EstVar

# bm1
# coefficient of Diff remains negative in these models:
summary(lm(DiffRelBias ~ Diff, data = agg))
summary(lm(DiffRelBias ~ Diff, data = s))

summary(lm(DiffRelBias ~ (Diff>0.05), data = s))

mean(s$DiffRelBias[s$Diff>0.05])
mean(s$DiffRelBias)

# do coverage as well



################################## STATS AND TABLES FOR PAPER: ALL SCENARIOS ##################################

##### Summary Stats Reported In-line #####
# across all scenarios
my_summarise(agg)
# **important: average relative bias of heterogeneity estimation is 0.35
# so that seems like a reasonable benchmark for our metrics

# compare PhatRelBias to means of iterates rather than of scenarios
mean(s$PhatRelBias, na.rm = TRUE)

# bm

#@temp: look at relationship between relative bias in variance and in Phat estimation
# @interesting and maybe convincing
plot(agg$EstVarRelBias, agg$PhatRelBias)
xmax = max( c(agg$EstVarRelBias, agg$PhatRelBias ), na.rm = TRUE )
xmin = min( c(agg$EstVarRelBias, agg$PhatRelBias ), na.rm = TRUE )

library(ggplot2)
ggplot( data = agg,
        aes( x = EstVarRelBias,
             y = PhatRelBias ) ) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0,
              slope = 1) +
  scale_x_continuous( limits = c(xmin, xmax) ) +
  scale_y_continuous( limits = c(xmin, xmax) ) +
  theme_classic()

summary( lm(agg$EstVarRelBias ~ agg$PhatRelBias) )


##### Summary Table #####
( t1 = data.frame( my_summarise( agg %>% group_by(k, V) ) ) )
print( xtable(t1), include.rownames = FALSE )



################################## STATS AND TABLES FOR PAPER: K>=100, THEORYP > 0.05 ##################################

##### Summary Stats Reported In-line #####

# for Diff
# now working with iterate-level data in order to use only observable variables:
t = s %>% 
  filter( k>= 100, Diff > 0.05 ) %>%
  #group_by(unique.scen) %>%  # THE GROUP BY CHANGES THE DIRECTION OF THE FILTERING EFFECT...
  summarise( mean(DiffRelBias),
             mean(EstVarRelBias) )
colMeans(t)
# **this criterion gets relative bias to be similar to the overall 0.35 seen for the heterogeneity estimate


# for Phat
t = s %>% 
  #filter( EstVar > 0.04 ) %>%
  #filter(true.effect.dist == "normal") %>%
  #group_by(unique.scen) %>%  # THE GROUP BY CHANGES THE DIRECTION OF THE FILTERING EFFECT...
  summarise( mean(PhatRelBias),
             mean(EstVarRelBias) )
colMeans(t)





my_summarise( agg %>% filter( k >= 100 & TheoryDiff >.05) )$PhatRelBias

# old version that used TheoryDiff:
my_summarise( agg %>% filter( k >= 100 & TheoryDiff >.05) )


##### Summary Table #####
( t2 = data.frame( my_summarise( agg %>% filter( k >= 100 & TheoryDiff >.05) %>% group_by(k, V) ) ) )
print( xtable(t2), include.rownames = FALSE )
