
################################## PRELIMINARIES ##################################

rm(list=ls())

library(dplyr)
library(xtable)

options(scipen=999)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-23 for RSM_1"
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
# @TEMP ONLY: EXCLUDE SCENS THAT AREN'T DONE RUNNING
agg = agg %>% filter(sim.reps>300)
s = s %>% filter( calib.method.pretty %in% to.analyze )

# restrict s to the analyzed scenarios in agg
s = s %>% filter( unique.scen %in% agg$unique.scen )



################################## I^2 AND ICC TO REPORT ##################################


##### I^2 Parameterization of Heterogeneity #####

# for each possible sample size in our sims, convert all the heterogeneity 
#  parameters to I^2
table(agg$muN)
table(agg$V)

round( I2( t2 = c(0.0025, .01, .04, .25, .64),
           N = 100), 2 )

round( I2( t2 = c(0.0025, .01, .04, .25, .64),
           N = 850), 2 )
# so the tau = 0.5 (t2 = 0.25) scenarios are approximately I^2 = 50%
# which is exactly Higgins' (2003) benchmark for "moderate" heterogeneity :)

##### ICC in Clustered Scenarios #####
agg.all %>% filter(clustered == TRUE) %>%
  summarise(min(ICCpop),
            mean(ICCpop),
            median(ICCpop),
            max(ICCpop))



################################## REGRESS PERFORMANCE METRICS ON SIMULATION PARAMETERS ##################################

# focus on observable variables within scenarios
# i.e., estimates rather than parameters
obsVars = c("k", "minN", "Phat", "Diff", "EstMean", "EstVar")

outcomes = c("PhatRelBias", "CoverPhat", "DiffRelBias",  "CoverDiff")




# at scenario level rather than individual iterate level
for (i in outcomes){
  string = paste( i, "~", paste(obsVars, collapse = "+"), sep="" )
  mod = lm( eval(parse(text=string)),
            data = s )
  
  library(sandwich)
  SEs = diag( vcovCL(mod, type="HC0", cluster = ~ scen.name) )
  SEs = SEs[ !names(SEs) == "(Intercept)" ]

  coefs = mod$coefficients[ !names(mod$coefficients) == "(Intercept)" ]
  z = coefs/SEs
  pvals = 2 * ( 1 - pnorm( abs(z) ) )
  
  # which vars are good vs. bad for the outcome?
  # flip coeff signs so that positive means it improves the outcome
  if ( grepl(pattern = "Bias", x = i) ) coefs = -coefs
  good = names( coefs[ coefs > 0 & pvals < 0.001 ] )
  bad = names( coefs[ coefs < 0 & pvals < 0.001 ] )
  
  good = paste(good, collapse = ",")
  bad = paste(bad, collapse = ",")
  
  newRow = data.frame( outcome = i,
                       good = good, 
                       bad = bad )
  
  if (i==outcomes[1]) res = newRow else res = rbind(res, newRow)
  
  cat( paste("\n\n*******************", toupper(i), " PREDICTORS*******************\n" ) )
  print(summary(mod))
}

res


# for Phat metrics: want larger minN, larger EstVar
# for Diff metrics: want larger k, minN, Diff

# test some rules of thumb
summary(s$EstVar)
summary(s$EstVar)

s$totalN = s$k * s$muN

sGood = s %>%
  filter(TRUE)
 filter( V > 0.01 )

temp = sGood %>%
  group_by(scen.name) %>%
  summarise( nReps = n(),
             scenCover = mean(CoverPhat, na.rm = TRUE),
             clustered = clustered[1],
             true.effect.dist = true.effect.dist[1]) %>%
  filter(nReps > 300)

# *** exponential definitely hurts a lot
# clustered too (to a lesser extent)
# the expo and clustered scenarios are the worst
data.frame( temp %>% group_by(clustered, true.effect.dist) %>%
              summarise( min(scenCover),
                         mean(scenCover),
                         mean(scenCover < 0.85),
                         mean(scenCover < 0.9) ) )

# look at how skewed they are
d = sim_data2(k=150,m=75,b0=0, bc=0.5, bb=1,V=0.05,Vzeta=0.05*.8,minN=800, muN=850,sd.w=1, true.effect.dist = "expo")

d = sim_data2(k=150,m=150,b0=0, bc=0.5, bb=1,V=0.05,Vzeta=0,minN=800, muN=850,sd.w=1, true.effect.dist = "expo")

# bm: come back to this...
# next step should be to check the simulation code, I guess
hist(d$Mi)

################################## STATS AND TABLES FOR PAPER: ALL SCENARIOS ##################################

##### Summary Stats Reported In-line #####
# across all scenarios
my_summarise(agg)
# **important: average relative bias of heterogeneity estimation is 0.68
# so that seems like a reasonable benchmark for our metrics

# was performance different in clustered vs. unclustered?
data.frame( my_summarise( agg %>% group_by(clustered) ) )



data.frame( my_summarise( agg %>% filter(k>100) %>% group_by(clustered ) ) )

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

# **report this?
summary( lm(agg$EstVarRelBias ~ agg$PhatRelBias) )


##### Summary Table #####
( t1 = data.frame( my_summarise( agg %>% group_by(k, V) ) ) )
print( xtable(t1), include.rownames = FALSE )



################################## STATS AND TABLES FOR PAPER: K>=100, THEORYP > 0.05 ##################################

##### Summary Stats Reported In-line #####

# for Diff
# now working with iterate-level data in order to use only observable variables:
t = s %>% 
  filter( k >= 50 ) %>%
  summarise( mean(DiffRelBias),
             mean(EstVarRelBias) )
colMeans(t)
# **this criterion gets relative bias to be similar to the overall 0.35 seen for the heterogeneity estimate


# for Phat
t = s %>% 
  #filter(true.effect.dist == "normal") %>%
  #group_by(unique.scen) %>%  # THE GROUP BY CHANGES THE DIRECTION OF THE FILTERING EFFECT...
  summarise( mean(PhatRelBias),
             mean(EstVarRelBias) )
colMeans(t)



# for Phat coverage
t = agg %>% 
  #filter( V > 0.05) %>%
  #group_by(unique.scen) %>%  # THE GROUP BY CHANGES THE DIRECTION OF THE FILTERING EFFECT...
  summarise( mn = mean(CoverPhat, na.rm = TRUE),
             min = min(CoverPhat, na.rm = TRUE),
             Pbad = mean(CoverPhat<0.85, na.rm=TRUE))
colMeans(t)


# bm: ~~could the issue be throwing away boot reps that don't converge??
# look at scens with poor coverage
temp = agg %>% 
  filter(CoverPhat < 0.7)
table(temp$clustered)

param.vars = c("scen.name",
               #"Method",
               #"calib.method",
               #"calib.method.pretty",
               "k",
               "m",
               "V",
               "Vzeta",
               "minN",
               "true.effect.dist",
               "TheoryP")

View( temp %>% select(CoverPhat, param.vars, ICCpop) )

# clustered does worse for coverage
my_summarise(agg %>% filter(clustered == FALSE))

mean(temp$PhatBtSD - temp$PhatEmpSD)   # very similar, so not the fault of the SD, I think
mean(temp$PhatRelBias)  # actually less biased than full dataset?
mean(temp$PhatAbsBias) 

my_summarise( agg %>% filter( m>50 & k>50 & EstVar>0.05) )

mean(agg$CoverPhat[agg$clustered == TRUE]<.85)
mean(agg$CoverPhat[agg$clustered == FALSE]<.85)
### end searhcinfg


##### Summary Table #####
( t2 = data.frame( my_summarise( agg %>% filter( k >= 50) %>% group_by(k, V) ) ) )
print( xtable(t2), include.rownames = FALSE )
