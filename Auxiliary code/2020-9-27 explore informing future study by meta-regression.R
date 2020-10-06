
# Could we inform future study design by meta-regression?
#  - looking for best calibrated estimate *conditional* on high quality
#  - looking for what study design would maximize precision conditional on covariates

################################## AWR DATA ################################## 

rm(list=ls())

library(dplyr)
library(tidyverse)
library(testthat)
library(robumeta)
#library(MetaUtility)
library(boot)
library(metafor)
library(ICC)
library(ggplot2)

# use development version of prop_stronger because clustering addition
#  isn't yet on CRAN
setwd("~/Dropbox/Personal computer/Independent studies/MetaUtility R package/MetaUtility/R")
source("functions.R")


boot.reps = 1000

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied example"
# overleaf.dir = "~/Dropbox/Apps/Overleaf/Moderators in meta-regression (MRM)"
# results.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Results from R"

setwd(code.dir)
source("helper_applied_MRM.R")

# get the boot fns
setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study")
source("helper_MRM.R")
source("bootfuns.R")

setwd(prepped.data.dir)
dm = read.csv("mathur_data_prepped.csv")



################################## BEST-EVIDENCED INTERVENTIONS ##################################

# # also include missingness indicator
# dm$qual.miss2 = dm$qual.missing < 0.2
# table(dm$qual.miss2)

# covars = list( NA,
#                
#                c("qual.y.prox2",
#                  "qual.exch2",
#                  "qual.gen2",
#                  "qual.sdb2") )

# recode so that a score of 1 is ideal (as for the binary ones)
dm$qual.retention = 100 - dm$qual.missing

# if adding more covar sets, need to add more calib things below (search for "if adding more covar sets")
covars = list( NA,
               
               "qual.y.prox2",
               
               c("qual.y.prox2",
                 "qual.exch2",
                 "qual.gen2",
                 "qual.sdb2",
                 "qual.retention") )

# dichotomizes the continuous one
dm = dm %>% rowwise() %>%
  mutate(qualSum = sum( qual.y.prox2, qual.exch2, qual.gen2, low.miss ) ) 

#dm = dm[ !is.na(dm$qual.missing), ]


for ( i in 1:length(covars) ) {
  
  .covars = unlist( covars[i] )
  
  cat( paste("\n\n\n******************** COVAR SET", i, "********************\n") )
  
  # linear predictor string
  if ( any( is.na(.covars) )  ) {
    string = "logRR ~ 1"
  } else {
    string = paste( c( "logRR", paste(.covars, collapse = " + ") ), collapse = " ~ " )
  }
  
  # fit appropriate meta-regression (or marginal meta-analysis)
  m = robu( eval( parse(text = string)),
            data = dm,
            studynum = authoryear,  
            var.eff.size = varlogRR,
            modelweights = "HIER",  # need this to exactly reproduce AWR's RR = 1.22
            small = TRUE)
  
  print(m)
  
  t2 = m$mod_info$tau.sq
  int = m$b.r[1]  # intercept
  
  # marginal model
  if ( any( is.na(.covars) )  ) {
    # marginal calibrated estimates
    calib = c(int) + sqrt( c(t2) / ( c(t2) + dm$varlogRR) ) * ( dm$logRR - c(int) )
    
    # name the new calibrated estimates with a number
    varName = paste("calib", i, sep="")
    dm[,varName] = calib
    
  } else {  # for model with moderators
    # design matrix of only the moderators
    Z = as.matrix( dm %>% select(.covars) )
    head(Z)
    
    # confirm same ordering
    colnames(Z); m
    
    # moderator coefficients
    # exclude intercept
    bhat = as.matrix( m$b.r[ 2:( length(.covars) + 1 ) ], ncol = 1 )
    int = m$b.r[1]  # intercept
    
    dm$linpredZ = Z %*% bhat
    
    # linear predictor for each study but without intercept
    #exp(dm$linpredZ)
    
    
    # shift point estimates to set their moderators all to 0
    dm$yi.shift = dm$logRR - dm$linpredZ  # shifted to have moderators set to 0
    
    # calibrated estimate, shifted to set effect modifiers to 1
    # @ASSUMES WE WANT ALL EFFECT MODIFIERS EQUAL TO 1 FOR HIGH-QUALITY STUDY
    # MRM Eq. 2.3
    # hence sum(bhat)
    calib = c(int) + sum(bhat) + sqrt( c(t2) / ( c(t2) + dm$varlogRR) ) * ( dm$yi.shift - c(int) )
    
    # name the new calibrated estimates with a number
    varName = paste("calib", i, sep="")
    dm[,varName] = calib
  }
  
  cat(varName)
  
  # find winning interventions
  # sorted version of dataset
  #temp = dm %>% arrange( desc( dm[[varName]] ) )
  temp = dm %>% arrange( desc( dm[[varName]] ) )
  
  winVec = temp$unique[1:10]
  winners = paste( winVec, collapse = "\n" )


  cat( paste("\n\nWINNERS WHEN CONDITIONING ON COVAR SET", i, ":\n", winners) )


  if ( i == 1 ){
    winList = list(winVec)
  } else {
    winList[[i]] = winVec 
  }
}




# make winner dataset
# second for-loop because we need to have both calib estimates populated
for ( i in 1:length(covars) ){

  rows = dm %>%
    filter( unique %in% winList[[i]] ) %>%
    
    select( unique,
            logRR,
            varlogRR,
            # if adding more covar sets, need to add more calib things here:
            calib1,
            calib2,
            calib3,
            qualSum,
            qual.y.prox2,
            qual.exch2,
            qual.gen2,
            qual.sdb2,
            qual.retention)
  
  varName = paste("calib", i, sep="")
  
  rows = rows %>% add_column( winSet = i, .before = 1) %>%
    # sort by calibrated estimate for THIS winner set
    arrange( desc(rows[[varName]]) )

  
  if ( i == 1 ){
    winDat = rows
  } else {
    winDat = rbind(winDat, rows)
  }
}



winDat = winDat %>% rowwise() %>%
  # take exp and round the log-RRs
  mutate( across( c(logRR, calib1, calib2, calib3),
                            .fns = list( exp = ~round( exp(.), 2 ) ) ) ) %>%
  select(-c(logRR, calib1, calib2, calib3))


winDat$group = NA
winDat$group[ winDat$winSet == 1 ] = "Marginal top 10"
winDat$group[ winDat$winSet == 2 ] = "Conditional (1 covar) top 10"
winDat$group[ winDat$unique %in% c(winList[[1]]) & winDat$unique %in% c(winList[[2]]) ] = "Both"
# winDat$group[ winDat$unique %in% c(winList[[1]])
#               & winDat$unique %in% c(winList[[2]]) &
#                 winDat$unique %in% c(winList[[3]]) ] = "All"

View(winDat)


# **for winSet 2,  I deliberately chose a covariate that is associated with WORSE effect size 
#  so winners by that method tend to be higher-quality wrt that characteristic 
#  they get more of a "boost" because their conditional expectation is lower
winDat %>% group_by(winSet) %>%
  summarise( qualSum = mean(qualSum, na.rm = TRUE),
             qual.y.prox2 = mean(qual.y.prox2),
             meanRR = mean(logRR_exp) )

# plot
library(ggplot2)

# ***just winners by either method, with labels
# point size directly proportional  to precision
ggplot( data = winDat %>% filter(winSet != 3),
        aes( x = calib1_exp,
             y = calib2_exp,
             color = group,
             size = 1/varlogRR,
             label = unique) ) +
  geom_point( alpha = 0.5 ) +
  geom_text(aes(label=unique),hjust=0, vjust=0) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  scale_x_continuous( limits = c(1, 2.25) ) +
  scale_y_continuous( limits = c(1, 1.75) ) +
  theme_bw()  


# whole dataset, no labels
# generally calibrated estimates are smaller when conditioning on risks of bias than without
ggplot( data = dm,
        aes( x = calib1,
             y = calib2,
             color = group) ) +
  geom_point( size = 3, alpha = 0.5 ) + 
  geom_abline(slope = 1, intercept = 0, lty = 2)


# by the conditional method, winners are studies for which:
#  - their conditional residual is large (e.g., if low-quality studies have higher estimates, then high-quality studies with large estimates will do better than low-quality studies with large estimates)
# - they are precise so that they don't get shrunk as much toward the mean

# for this dataset, most quality characteristics are associated with BETTER effect size:
#  exch, gen, sdb, retention
#  but prox is associated with worse effect size
#  so, e.g., winners could be studies with relatively LOW quality but a larger estimate
  
# note that if t2=0 after including all the moderators, all calibrated estimates are equal to mean,
#  so no point in looking for winners

# pretty interesting




################################## OPTIMAL NEXT STUDY ##################################

# we want to do a study that minimizes the variance of the estimate conditional on high quality




# steal some code from the above
i=2  # qual.y.prox only

# work with a small subset of the data so that we ccan actually see the impact of 
#  adding studies
# deliberately have only a few studies with qual.y.prox2 = 1

dm$qual.y.actual = dm$qual.y.prox == "a.Actual"
table(dm$qual.y.actual)

dm$fake = dm$qual.y.prox == "a.Actual"  # SE of 0.0436 for the "fake" coefficient, even though only 2 studies in that category
dm$fake = dm$qual.y.prox == "b.Self-reported"  # SE of 0.0739 for the "fake" coefficient even though many more studies in that category
table(dm$fake)

robu( logRR ~ fake,
      data = dm,
      studynum = authoryear,  
      var.eff.size = varlogRR,
      modelweights = "HIER",  # need this to exactly reproduce AWR's RR = 1.22
      small = TRUE)

# an experiment to see what happens to SE
#  this causes the SE to go from 0.0436 to 0.22
# **so the SEs for the moderator effects are mostly driven by how similar the studies are to one another, 
#  not how many studies there are in that category
#dm$logRR[ dm$qual.y.prox == "a.Actual" ] = c(0.10920, -.33)
dm$logRR[ dm$qual.y.prox == "b.Self-reported" ]


# ellipsis args are passed to add_row, so are portions of the new row
add_new_study = function(.covars, ...){
  
  # if there are ellipsis arguments, then we need to add a row
  if ( length( list(...) ) == 1 ) {
    dm2 = dm %>% add_row(...)
  } else {
    dm2 = dm
  }
  
  # temp only
  .covars = c("qual.y.actual")
  dm2 = dm
  dm2 = dm %>% add_row(authoryear = "Fake study",
                       logRR = 0.1224411,  # exactly at the conditional mean
                       varlogRR = 0.001,
                       qual.y.actual = "a.Actual")
  
  #.covars = unlist( covars[i] )
  
  # linear predictor string
  if ( any( is.na(.covars) )  ) {
    string = "logRR ~ 1"
  } else {
    string = paste( c( "logRR", paste(.covars, collapse = " + ") ), collapse = " ~ " )
  }
  
  # fit meta-regression to dataset with the added study
  m = robu( eval( parse(text = string)),
            data = dm2,
            studynum = authoryear,  
            var.eff.size = varlogRR,
            modelweights = "HIER",  # need this to exactly reproduce AWR's RR = 1.22
            small = TRUE)
  
  # variance-covariance matrix of coefficients
  V = m$VR.r
  
  # estimated mean and se for high-quality studies
  ( est = sum(m$b.r) )
  ( se = sqrt(sum(V)) )
  
}

# effect on SE of estimate for high-quality studies DOES depend on the logRR of the new study
#  I think this is because the SE gets larger if the added study is far away from the conditional mean
# the SE goes down if the new study is at the conditional mean







