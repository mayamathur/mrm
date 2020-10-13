
################################## PRELIMINARIES ##################################

rm(list=ls())

library(dplyr)
library(xtable)
library(data.table)
library(tibble)
library(testthat)

options(scipen=999)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/*Main-text sims/Prepped data"
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/*Main-text sims/Results from R"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"


setwd(prepped.data.dir)
# data aggregated by scenario
agg = fread("*agg_dataset.csv")
expect_equal( 4679, nrow(agg) ) # compare to data prep script
# data for each simulation iterate
s = fread("s3_dataset.csv")

setwd(code.dir)
source("helper_MRM.R")

# takes about 5 min
regressions.from.scratch = FALSE



################################## GENERAL STATS ON SIMULATIONS ##################################

##### I^2 Parameterization of Heterogeneity #####

# for each possible sample size in our sims, convert all the heterogeneity 
#  parameters to I^2
table(s$muN)
table(s$V)

round( I2( t2 = c(0.0025, .01, .04, .25, .64),
           N = 100), 2 )

round( I2( t2 = c(0.0025, .01, .04, .25, .64),
           N = 850), 2 )
# so the tau = 0.5 (t2 = 0.25) scenarios are approximately I^2 = 50%
# which is exactly Higgins' (2003) benchmark for "moderate" heterogeneity :)

##### ICC in Clustered Scenarios #####
agg %>% filter(clustered == TRUE) %>%
  summarise(min(ICCpop),
            mean(ICCpop),
            median(ICCpop),
            max(ICCpop))

##### Number and Percentage of Successful Reps #####

# total scenarios
# this is 2X the number of data-generation parameter combinations because 
#  each has both MR and DL rows
nrow(agg)
# 2400: number of unique data-generation parameters
nrow(agg) / (2400 * 2)
# proportion of scenarios that didn't run
1 - (nrow(agg) / (2400 * 2))

table(agg$calib.method)

data.gen.params = c( "k",
                     "V",
                     "Vzeta",
                     "minN",
                     "true.effect.dist",
                     "TheoryP",
                     "contrast")

# number of unique data generation parameters actually represented in
#  the results
x = s %>% group_by_at(data.gen.params) %>%
  summarise(meanNA(Phat))
nrow(x)  # 2388 of 2400 possible


################################## ONE-STAGE VS. TWO-STAGE ##################################

param.vars = c("calib.method.pretty",
  "k",
  "V",
  "Vzeta",
  "minN",
  "true.effect.dist",
  "TheoryP",
  "contrast")

outcomes = c("PhatRelBias", "CoverPhat", "DiffRelBias",  "CoverDiff", "PhatCIWidth", "DiffCIWidth")

# sanity check:
# make sure we listed all the param vars
t = s %>% group_by_at(param.vars) %>%
  summarise( n = n(),
             .groups = "keep" )
# 500 such that each scenario is uniquely defined by the param vars
expect_equal( unique(t$n), 500 )


# compare one-stage to two-stage method
t = s %>% group_by_at(param.vars) %>%
  group_by(calib.method.pretty) %>%
  summarise_at( .vars = outcomes,
                .funs = meanNA )
View(t)  
  


################################## REGRESS PERFORMANCE METRICS ON OBSERVED STATS ##################################

# focus on observable variables within scenarios
# i.e., estimates rather than parameters
obsVars = c("k", "muN", "Phat", "PhatRef", "EstMean", "EstVar", "PhatBtFail", "calib.method.pretty",
            # last two are only somewhat observed:
            "true.effect.dist", "clustered")

outcomes = c("PhatRelBias", "CoverPhat", "DiffRelBias",  "CoverDiff")

# takes about 5 min
if ( regressions.from.scratch == TRUE ) {
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
  
  setwd(results.dir)
  write.csv(res, "performance_predictors.csv")
}

s
################################## TABLES FOR PAPER, WITH RULES OF THUMB ##################################


# choose which average to take across scenarios ("median" or "mean")
averagefn = "median"


##### For Phat #####

# make filtered dfs (will form rows of table)
agg2 = make_agg_data( s %>% filter(contrast != "BC-rare") )
agg3 = make_agg_data( s %>% filter(true.effect.dist == "normal") )
# excluding only clustered expo is same as excluding all clustered
agg4 = make_agg_data( s %>% filter( !(clustered == TRUE & true.effect.dist == "expo") ) )

selectVars = "Phat"

t1 = rbind( my_summarise(dat = agg,
                        description = "All reps",
                        averagefn = averagefn),
           
           # **this one gives 0% chance of coverage<85%
           my_summarise(dat = agg3,
                        description = "Normal",
                        averagefn = averagefn),
           
           # **this one gives 1% chance of coverage<85%
           my_summarise(dat = agg4,
                        description = "Not clustered expo",
                        averagefn = averagefn) )  

View(t1)

# save pretty table for paper
keepers = c("Scenarios",
            "n.scens", 
            "PhatBias",
            "PhatAbsBias",
            "PhatRelBias",
            "EstMeanRelBias",
            "EstVarRelBias",
            "CoverPhat",
            "BadPhatCover")
setwd(results.dir)
setwd("Tables to prettify")
write.csv(t1 %>% select(keepers), "Phat_results_table.csv")


##### For Diff #####

agg5 = make_agg_data( s %>% filter(contrast != "BC-rare" &
                                     !(clustered == TRUE & true.effect.dist == "expo") ) )

# tried but not useful:
#agg5 = make_agg_data( s %>% filter(contrast != "BC-rare" & k>=100 ) )

# agg6 = make_agg_data(s %>% filter(k >= 100) )
# 
# agg7 = make_agg_data(s %>% filter( k >= 100 &
#                                      !(clustered == TRUE & true.effect.dist == "expo") ) ) 
# 
# agg9 = make_agg_data(s %>% filter( k >= 100 &
#                                      true.effect.dist == "normal" ) ) 
# 
# agg10 = make_agg_data(s %>% filter( k >= 100 &
#                                       muN==850 &
#                                       true.effect.dist == "normal" &
#                                       clustered == FALSE) ) 

selectVars = "Diff"

t2 = rbind( my_summarise(dat = agg,
                         description = "All reps",
                         averagefn = averagefn),
            
            my_summarise(dat = agg3,
                         description = "Normal",
                         averagefn = averagefn),
            
            
            my_summarise(dat = agg2,
                         description = "Not BC-rare",
                         averagefn = averagefn),
            

            my_summarise(dat = agg5,
                         description = "Not BC-rare nor clustered expo",
                         averagefn = averagefn)
) 
View(t2)

# save pretty table for paper
keepers = c("Scenarios",
            "n.scens", 
            "DiffBias",
            "DiffAbsBias",
            "DiffRelBias",
            "EstMeanRelBias",
            "EstVarRelBias",
            "CoverDiff",
            "BadDiffCover")
setwd(results.dir)
setwd("Tables to prettify")
write.csv(t2 %>% select(keepers), "diff_results_table.csv")



###### All Metrics for All of the Filtered Datasets (Online Dataset) #####

# diff, phat, logitphat, etc.

selectVars = "all"
t3 = rbind( my_summarise(dat = agg,
                         description = "All reps",
                         averagefn = averagefn),
            
            my_summarise(dat = agg2,
                         description = "No BC-rare",
                         averagefn = averagefn),
            
            my_summarise(dat = agg3,
                         description = "Normal",
                         averagefn = averagefn),
            
            my_summarise(dat = agg4,
                         description = "Not clustered expo",
                         averagefn = averagefn),
            
            my_summarise(dat = agg5,
                         description = "No BC-rare/not clustered expo",
                         averagefn = averagefn) ) 


setwd(results.dir)
write.csv(t3, "extended_performance_table.csv")



################################## BIAS-CORRECT META-REGRESSION ESTIMATES IN WORST SCENARIOS ##################################

setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/Supplement - boot-correct meta-regression")

# "b" for "bias correction"
s2b = fread("stitched.csv")
s2b$contrast = "BC-rare"

s3b = make_s3_data(s2b)


# has one row per scenario, so 3 rows per scen.name.in.main
aggb = make_agg_data(s3b)

selectVars = "all"

t = data.frame( rbind( my_summarise(dat = aggb %>% filter(calib.method == "MR"),
                                    description = "MR"),
                       
                       my_summarise(dat = aggb %>% filter(calib.method == "MR bt both correct"),
                                    description = "Bt correct"),
                       
                       my_summarise(dat = aggb %>% filter(calib.method == "params"),
                                    description = "params") ) )

t = t %>% select("Scenarios", "PhatBias", "PhatAbsBias",  "PhatRelBias", "DiffBias", "DiffAbsBias", "DiffRelBias")
t

# yes, the bias corrections make the bias worse...

setwd(results.dir)
setwd("Tables to prettify")
fwrite(t, "supplement_meta_regression_corrections.csv")
