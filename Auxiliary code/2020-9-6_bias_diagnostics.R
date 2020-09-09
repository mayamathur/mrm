
# Try simulating simple data without effect modifiers to see if EstVar is biased
# by looking directly at the generated true effects
# use a scenario that had biased EstVar, but remove the effect modifiers
#  so that we can look directly at the true effects (marginal heterogeneity)


library(crayon)
library(dplyr)
library(foreach)
library(doParallel)
library(boot)
library(metafor)
library(robumeta)
library(data.table)
library(purrr)
library(metRology)
library(fansi)
library(MetaUtility)

options(scipen=999)



# helper fns
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"
setwd(code.dir)
source("helper_MRM.R")

# just like scenario 14 MR, but with effect modifiers set to 0
( scen.params = make_scen_params( method = "boot.whole",  # this doesn't mean anything since we have only one "method"
                                  calib.method = "MR",  # "MR" for one-stage or "DL" for two-stage
                                  k = c(20),
                                  b0 = 0, # intercept
                                  # bc = 0.5, # effect of continuous moderator
                                  # bb = 1, # effect of binary moderator
                                  bc = 0, # effect of continuous moderator
                                  bb = 0, # effect of binary moderator
                                  
                                  zc.star = 0.5,  # level of moderator to consider
                                  zb.star = 1,
                                  
                                  zc.ref = 2,  # comparison levels of moderator to consider
                                  zb.ref = 0,
                                  
                                  V = c(.01), # residual variance
                                  muN = NA,  # just a placeholder; to be filled in later
                                  minN = c(50),
                                  sd.w = c(1),
                                  tail = "above",
                                  true.effect.dist = c("normal"),
                                  TheoryP = c(0.05),
                                  start.at = 1 ) )

scen = 1
sim.reps = 500
doBoot = TRUE  # slow bootstrapping to try to bias-correct EstVar and EstMean

# set the number of cores
registerDoParallel(cores=8)



rs = foreach( i = 1:sim.reps, .combine=rbind ) %dopar% {
  
  p = scen.params[ scen.params$scen.name == scen ]
  d = sim_data( k = p$k, 
                b0 = p$b0, # intercept
                bc = p$bc, # effect of continuous moderator
                bb = p$bb, # effect of binary moderator 
                V = p$V,
                muN = p$muN, 
                minN = p$minN,
                sd.w = p$sd.w,
                true.effect.dist = p$true.effect.dist )
  
  # get EstVar
  d.stats = prop_stronger_mr(d,
                             zc.star = p$zc.star,
                             zb.star = p$zb.star,
                             zc.ref = p$zc.ref,
                             zb.ref = p$zb.ref,
                             calib.method = p$calib.method )
  
  # simpler way to get EstVar (don't condition on effect modifiers)
  m = robu( yi ~ 1, 
            data = d, 
            studynum = 1:nrow(d),
            var.eff.size = vyi )
  bhat0 = m$b.r[1]
  t2 = m$mod_info$tau.sq
  
  # Phat as in prop_stronger_mr, but not conditioning on the covariates
  # should be very similar to the one in d.stats, but not exactly the same
  calib = c(bhat0) + sqrt( c(t2) / ( c(t2) + d$vyi) ) * ( d$yi - c(bhat0) )
  Phat1 = mean(calib > p$q)
  
  # Phat using the true (unknown) variance but the estimated mean
  calib = c(bhat0) + sqrt( c(p$V) / ( c(p$V) + d$vyi) ) * ( d$yi - c(bhat0) )
  Phat2 = mean(calib > p$q)
  
  # use ONLY real parameters
  calib = c(p$b0) + sqrt( c(p$V) / ( c(p$V) + d$vyi) ) * ( d$yi - c(p$b0) )
  Phat3 = mean(calib > p$q)

  # try to estimate the bias in bhat0 and EstVar with bootstrap
  if ( doBoot == TRUE ) {
    boot.reps = 1000
    boot.res = boot( data = d, 
                     parallel = "multicore",
                     R = boot.reps, 
                     statistic = function(original, indices) {
                       b = d[indices,]
                       
                       tryCatch({
                         
                         mb = robu( yi ~ 1, 
                                    data = b, 
                                    studynum = 1:nrow(b),
                                    var.eff.size = vyi )
                         bhat0.bt = mb$b.r[1]
                         t2.bt = mb$mod_info$tau.sq
                         return( c(bhat0.bt, t2.bt) )
                         
                       }, error = function(err){
                         return( c(NA, NA) )
                       })
                       
                     } )
    bt.means = as.numeric( colMeans( boot.res$t ) )
    bt.bias = bt.means - c(bhat0, t2)
    # sanity check for how boot is calculating the bias
    # bhat0 - bt.means[1]
    
    EstMeanBtCorr = bhat0 - bt.means[1]
    # @note the truncation at 0, which may limit the bias correction's impact
    EstVarBtCorr = max( t2 - bt.means[2], 0 )
    EstVarBtCorrUntrunc = t2 - bt.means[2]
    
    # Phat using bias-corrected mean and variance
    calib = c(EstMeanBtCorr) + sqrt( c(EstVarBtCorr) / ( c(EstVarBtCorr) + d$vyi) ) * ( d$yi - c(EstMeanBtCorr) )
    PhatBtCorr = mean(calib > p$q)
    
    # correct the mean only
    calib = c(EstMeanBtCorr) + sqrt( c(t2) / ( c(t2) + d$vyi) ) * ( d$yi - c(EstMeanBtCorr) )
    PhatBtCorrMeanOnly = mean(calib > p$q)
    
  }
  
  if (doBoot == FALSE) {
    EstMeanBtCorr = EstVarBtCorr = PhatBtCorr = NA
  }

  
  
  return( data.frame( VEmp = var(d$Mi),
                      EstVar = d.stats$t2,
                      EstVar2 = m$mod_info$tau.sq,
                      EstVarBtCorr = EstVarBtCorr,
                      EstVarBtCorrUntrunc = EstVarBtCorrUntrunc,
                      
                      EstMean = d.stats$bhat0,
                      EstMean2 = bhat0,
                      EstMeanBtCorr = EstMeanBtCorr,
                    
                      Phat = d.stats$Phat,
                      Phat1 = Phat1,
                      Phat2 = Phat2,
                      Phat3 = Phat3,
                      PhatBtCorr = PhatBtCorr,
                      PhatBtCorrMeanOnly = PhatBtCorrMeanOnly
                      ) )
}
  
# **Result 1: EstVar and EstMean still biased upward! 
# but bias disappears if using k=500
# Result 2: even EstVar2 is biased (using naive model)
# Result 3: if we calibrate using the true mean and variance (Phat3), bias in Phat disappears
#  and the intermediate Phat2, which only uses the true variance, is in between

colMeans(rs)

# Result 4: the bootstrap bias correction seems to help with the estimated mean but 
#  not EstVar. With EstVar, it actually OVERCORRECTS the estimate, i.e., the boot mean is way too high. 

# Result 5: Based on EstVarBtCorrUntrunc, the reason for the bootstrap's failure to correct EstVar is NOT related to the asymmetrical truncation; presumably it's due
#  to the small sample size and non-pivotality of the estimator. 

# Result 6: If I use the bias-corrected EstMean but not the (badly) bias-corrected EstVar to construct Phat (i.e., PhatBtCorrMeanOnly), this seems to help a bit (changes estimate from 0.0766 to 0.066 vs. truth = 0.05), so reduces relative bias from 1.53 to 1.32. 





