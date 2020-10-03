

################################## PRELIMINARIES ##################################

rm(list=ls())

library(dplyr)
library(xtable)
library(data.table)
library(tibble)
library(testthat)

options(scipen=999)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/**2020-9-26 main sims (in RSM_1)"
results.dir = prepped.data.dir
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"


setwd(prepped.data.dir)
# data aggregated by scenario
agg.all = fread("*agg_dataset_as_analyzed.csv")
expect_equal( 1581, nrow(agg.all) ) # 1271 from data prep script
# data for each simulation iterate
s = fread("s3_dataset_MRM.csv")

setwd(code.dir)
source("helper_MRM.R")

# CHOOSE WHICH CALIB.METHOD TO ANALYZE (ONE- OR TWO-STAGE):
#to.analyze = "Two-stage"
to.analyze = "One-stage"

agg = agg.all %>% filter( calib.method.pretty %in% to.analyze )
# @test: more stringent bca.success criterion
#agg = agg %>% filter( bca.success > .10 )

# cut to avoid low-bca-success ones
s = s %>% filter( calib.method.pretty %in% to.analyze & 
                    scen.name %in% agg$scen.name )

# # restrict s to the analyzed scenarios in agg
# s = s %>% filter( unique.scen %in% agg$unique.scen )


summary(s$repTime)/60

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



################################## REGRESS PERFORMANCE METRICS ON OBSERVED STATS ##################################

# focus on observable variables within scenarios
# i.e., estimates rather than parameters
obsVars = c("k", "muN", "Phat", "PhatRef", "EstMean", "EstVar", "PhatBtFail",
            # last two are only somewhat observed:
            "true.effect.dist", "clustered")

outcomes = c("PhatRelBias", "CoverPhat", "DiffRelBias",  "CoverDiff")


# obsVars = c("k", "muN", "Phat", "Diff", "EstMean", "EstVar", "PhatBtFail",
#             # last two are only somewhat observed:
#             "true.effect.dist", "clustered")
# outcomes = c("CoverDiff")

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
  
  
  # best subsets
  # bm
  # ex on page 298 here is good: https://journal.r-project.org/archive/2018/RJ-2018-059/RJ-2018-059.pdf
  library(rFSA)
  string = paste( i, " ~ 1", sep="" )
  keepers = c(obsVars, i)
  mod2 = FSA( formula = eval( parse(text = string) ),
              #data = s[1:1000,] %>% select(keepers),  # for testing
              data = s %>% select(keepers),
              cores = 8,
              m = 2,  # order of interactions to try
              interactions = FALSE,
              criterion = AIC)
  mod2
  
  if ( i == outcomes[1] ) bestMod = list( summary(mod2)[[2]] ) else bestMod[[ length(bestMod) + 1 ]] = summary(mod2)[[2]]
}


# look at results
res

bestMod


selectVars = "Diff"
data.frame( my_summarise(dat = make_agg_data( s %>% filter(Phat>0.10 & PhatRef > 0.10) ),
             description = "DiffRelBias test") )

# for Phat metrics: want larger muN, larger EstVar, lower PhatBtFail
# for Diff metrics: want larger k, muN, Diff, lower PhatBtFail



################################## TEST RULES OF THUMB ##################################

# bm
# overall
selectVars = "all"
data.frame( my_summarise(dat = agg,
                         description = "All reps") )

# reproduce previous findings
temp = agg %>% filter( bca.success>0.05 &
                         clustered == FALSE & 
                         V > 0.0025 & 
                         V < 0.64 &
                         EstVar > 0 )  # we previously discarded these reps

data.frame( my_summarise(dat = temp,
                         description = "Reproduce previous") )

# bm

# look at filtering rules
data.frame( my_summarise(dat = agg %>% filter(bca.success>0.1),
                         description = "All reps") )

##### For Phat #####

# make filtered dfs
agg2 = make_agg_data( s %>% filter(PhatBtFail==0) )
agg3 = make_agg_data( s %>% filter( !(true.effect.dist == "expo" & clustered == TRUE) ) )
agg4 = make_agg_data( s %>% filter(true.effect.dist == "normal") )
agg5 = make_agg_data( s %>% filter(clustered == FALSE) )

selectVars = "Phat"

t = rbind( my_summarise(dat = agg,
                        description = "All reps"),
           
           my_summarise(dat = agg2,
                        description = "No bt fails"),
           
           
           my_summarise(dat = agg3,
                        description = "Not clustered expo"),
           
           # **this one gives 0% chance of coverage<85%
           my_summarise(dat = agg4,
                        description = "Normal"),
           
           my_summarise(dat = agg5,
                        description = "Unclustered") )  

View(t)


##### For Diff #####

agg12 = make_agg_data(s %>% filter(PhatBtFail==0) )

agg8 = agg %>% filter( bca.success > .10 )

agg6 = make_agg_data(s %>% filter(k >= 100) )

agg7 = make_agg_data(s %>% filter( k >= 100 &
                                     !(clustered == TRUE & true.effect.dist == "expo") ) ) 

agg9 = make_agg_data(s %>% filter( k >= 100 &
                                     true.effect.dist == "normal" ) ) 

selectVars = "Diff"
t = rbind( my_summarise(dat = agg,
                        description = "All reps"),
           
           my_summarise(agg8,
                        description = "BCA success > 0.10"),
           
           my_summarise(agg12,
                        description = "No bt fails"),
           
           my_summarise(agg6,
                        description = "k>=100"),
           
           my_summarise( agg7,
                         description = "k>=100/not clustered expo"),
           
           my_summarise( agg9,
                         description = "k>=100/normal")
)  

View(t)


# bm: try getting an approximation of bootstrapping the bootstrap by bias-correcting the bootstrap SDs by the scenario's average bias




################################## BIAS CORRECTIONS ##################################

# these used 1000 boot reps for the bias corrections and CIs


setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/*2020-10-1 correct meta-regression (Supplement)")

s2b = fread("fake.csv")

s3b = make_s3_data(s2b)

# one row per scenario, so 3 rows per scen.name.in.main
aggb = make_agg_data(s3b)

selectVars = "all"

t = data.frame( rbind( my_summarise(dat = aggb %>% filter(calib.method == "MR"), description = "MR"),
                   my_summarise(dat = aggb %>% filter(calib.method == "MR bt both correct"), description = "Bt correct"),
                   my_summarise(dat = aggb %>% filter(calib.method == "params"), description = "params") ) )

t = t %>% select("Scenarios", "PhatBias", "PhatRelBias", "DiffBias", "DiffRelBias")
t

# put this in the paper! 

# yes, the bias corrections make the bias worse...





##### OLD STUFF:

# ################################## STATS AND TABLES FOR PAPER: ALL SCENARIOS ##################################
# 
# ##### Summary Stats Reported In-line #####
# # across all scenarios
# my_summarise(agg)
# # **important: average relative bias of heterogeneity estimation is 0.68
# # so that seems like a reasonable benchmark for our metrics
# 
# 
# # was performance different in clustered vs. unclustered?
# data.frame( my_summarise( agg %>% group_by(clustered) ) )
# 
# 
# 
# data.frame( my_summarise( agg %>% filter(k>100) %>% group_by(clustered ) ) )
# 
# # compare PhatRelBias to means of iterates rather than of scenarios
# mean(s$PhatRelBias, na.rm = TRUE)
# 
# # bm
# 
# #@temp: look at relationship between relative bias in variance and in Phat estimation
# # @interesting and maybe convincing
# plot(agg$EstVarRelBias, agg$PhatRelBias)
# xmax = max( c(agg$EstVarRelBias, agg$PhatRelBias ), na.rm = TRUE )
# xmin = min( c(agg$EstVarRelBias, agg$PhatRelBias ), na.rm = TRUE )
# 
# library(ggplot2)
# ggplot( data = agg,
#         aes( x = EstVarRelBias,
#              y = PhatRelBias ) ) +
#   geom_point(alpha = 0.4) +
#   geom_abline(intercept = 0,
#               slope = 1) +
#   scale_x_continuous( limits = c(xmin, xmax) ) +
#   scale_y_continuous( limits = c(xmin, xmax) ) +
#   theme_classic()
# 
# # **report this?
# summary( lm(agg$EstVarRelBias ~ agg$PhatRelBias) )
# 
# 
# ##### Summary Table #####
# ( t1 = data.frame( my_summarise( agg %>% group_by(k, V) ) ) )
# print( xtable(t1), include.rownames = FALSE )
# 
# 
# 
# ################################## STATS AND TABLES FOR PAPER: K>=100, THEORYP > 0.05 ##################################
# 
# ##### Summary Stats Reported In-line #####
# 
# # for Diff
# # now working with iterate-level data in order to use only observable variables:
# t = s %>% 
#   filter( k >= 50 ) %>%
#   summarise( mean(DiffRelBias),
#              mean(EstVarRelBias) )
# colMeans(t)
# # **this criterion gets relative bias to be similar to the overall 0.35 seen for the heterogeneity estimate
# 
# 
# # for Phat
# t = s %>% 
#   #filter(true.effect.dist == "normal") %>%
#   #group_by(unique.scen) %>%  # THE GROUP BY CHANGES THE DIRECTION OF THE FILTERING EFFECT...
#   summarise( mean(PhatRelBias),
#              mean(EstVarRelBias) )
# colMeans(t)
# 
# 
# 
# # for Phat coverage
# t = agg %>% 
#   #filter( V > 0.05) %>%
#   #group_by(unique.scen) %>%  # THE GROUP BY CHANGES THE DIRECTION OF THE FILTERING EFFECT...
#   summarise( mn = mean(CoverPhat, na.rm = TRUE),
#              min = min(CoverPhat, na.rm = TRUE),
#              Pbad = mean(CoverPhat<0.85, na.rm=TRUE))
# colMeans(t)
# 
# 
# # bm: ~~could the issue be throwing away boot reps that don't converge??
# # look at scens with poor coverage
# temp = agg %>% 
#   filter(CoverPhat < 0.7)
# table(temp$clustered)
# 
# param.vars = c("scen.name",
#                #"Method",
#                #"calib.method",
#                #"calib.method.pretty",
#                "k",
#                "m",
#                "V",
#                "Vzeta",
#                "minN",
#                "true.effect.dist",
#                "TheoryP")
# 
# View( temp %>% select(CoverPhat, param.vars, ICCpop) )
# 
# # clustered does worse for coverage
# my_summarise(agg %>% filter(clustered == FALSE))
# 
# mean(temp$PhatBtSD - temp$PhatEmpSD)   # very similar, so not the fault of the SD, I think
# mean(temp$PhatRelBias)  # actually less biased than full dataset?
# mean(temp$PhatAbsBias) 
# 
# my_summarise( agg %>% filter( m>50 & k>50 & EstVar>0.05) )
# 
# mean(agg$CoverPhat[agg$clustered == TRUE]<.85)
# mean(agg$CoverPhat[agg$clustered == FALSE]<.85)
# ### end searhcinfg
# 
# 
# ##### Summary Table #####
# ( t2 = data.frame( my_summarise( agg %>% filter( k >= 50) %>% group_by(k, V) ) ) )
# print( xtable(t2), include.rownames = FALSE )
