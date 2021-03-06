
# audited 2020-6-17


# to update
# can't rely on bootstrap notes for bca.success anymore because 
#   now that only happens when the resampling fails, not the CIs
#   should use NAs in each respective CI instead


# because Sherlock 2.0 restores previous workspace
rm( list = ls() )

# are we running locally?
run.local = FALSE



######################################## FOR CLUSTER USE ########################################
if (run.local == FALSE) {
  
  # load command line arguments
  args = commandArgs(trailingOnly = TRUE)
  jobname = args[1]
  scen = args[2]  # this will be a letter
  
  # get scen parameters
  setwd("/home/groups/manishad/MRM")
  scen.params = read.csv( "scen_params.csv" )
  p = scen.params[ scen.params$scen.name == scen, ]
  
  print(p)
  
  
  # simulation reps to run within this job
  # this need to match n.reps.in.doParallel in the genSbatch script
  sim.reps = 100
  # was 5,000 in NPPhat
  boot.reps = 5000
  
  
  # EDITED FOR C++ ISSUE WITH PACKAGE INSTALLATION
  library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
  library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
  library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
  library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
  library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
  library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
  library(robumeta, lib.loc = "/home/groups/manishad/Rpackages/")
  library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
  library(purrr, lib.loc = "/home/groups/manishad/Rpackages/")
  library(metRology, lib.loc = "/home/groups/manishad/Rpackages/")
  library(fansi, lib.loc = "/home/groups/manishad/Rpackages/")
  library(MetaUtility, lib.loc = "/home/groups/manishad/Rpackages/")
  library(ICC, lib.loc = "/home/groups/manishad/Rpackages/")
  library(cfdecomp, lib.loc = "/home/groups/manishad/Rpackages/")
  
  # for use in ml load R
  # install.packages( c("metRology"), lib = "/home/groups/manishad/Rpackages/" )
  
  path = "/home/groups/manishad/MRM"
  setwd(path)
  source("bootfuns.R")
  source("helper_MRM.R")
  
  # set the number of cores
  registerDoParallel(cores=16)
  
  # ##### Write Blank CSV File #####
  # # this records that the rep started in case there is a problem with the bootstrapping
  # placeholder = data.frame( TrueMean = NA,
  #                           EstMean = NA, 
  #                           
  #                           TrueVar = NA,
  #                           EstVar = NA,
  # 
  #                           # for "star" level of moderators
  #                           Phat = NA,
  #                           PhatLo = NA,
  #                           PhatHi =  NA,
  #                           
  #                           # for reference level of moderators
  #                           PhatRef =  NA,
  #                           PhatRefLo =  NA,
  #                           PhatRefHi =  NA,
  #                    
  #                           # for the difference
  #                           #TheoryDiff = p$TheoryDiff, 
  #                           Diff =  NA,
  #                           DiffLo =  NA,
  #                           DiffHi =  NA,
  #                           
  #                           # method of calculating CI: exponentiate logit or not?
  #                           Method =  NA,
  #                           
  #                           # CI performance
  #                           CoverPhat =  NA,
  #                           CoverPhatRef =  NA,
  #                           CoverDiff =  NA,
  #                           
  #                           PhatCIWidth =  NA,
  #                           PhatRefCIWidth =  NA,
  #                           DiffCIWidth =  NA,
  #                           
  #                           Note = "Sim failure")
  # 
  # 
  # placeholder$scen.name = scen
  # placeholder = merge( placeholder, scen.params, by = "scen.name" )
  # 
  # setwd("/home/groups/manishad/MRM/sim_results/long")
  # write.csv( placeholder, paste( "long_results", jobname, ".csv", sep="_" ) )
  # # this will be overwritten if the rep finished successfully
}



######################################## FOR LOCAL USE ########################################
if ( run.local == TRUE ) {
  rm(list=ls())
  
  
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
  library(ICC)
  library(cfdecomp)
  
  # helper fns
  code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"
  setwd(code.dir)
  source("bootfuns.R")
  source("helper_MRM.R")
  
  
  # # just one scenario
  # # 14 MR
  # ( scen.params = make_scen_params( method = "no.ci",  # "boot.whole", "no.ci"
  #                                   calib.method = "MR bt both correct",
  #                                   #calib.method = "MR bt mn correct",  # "MR" for one-stage, "DL" for two-stage, "MR bt mn correct", "MR bt var correct", "MR bt both correct"
  #                                   k = c(20),
  #                                   b0 = 0, # intercept
  #                                   bc = 0.5, # effect of continuous moderator
  #                                   bb = 1, # effect of binary moderator
  # 
  #                                   zc.star = 0.5,  # level of moderator to consider
  #                                   zb.star = 1,
  # 
  #                                   zc.ref = 2,  # comparison levels of moderator to consider
  #                                   zb.ref = 0,
  # 
  #                                   V = c( .01 ), # residual variance
  #                                   muN = NA,  # just a placeholder; to be filled in later
  #                                   minN = c(50),
  #                                   sd.w = c(1),
  #                                   tail = "above",
  #                                   true.effect.dist = c("normal"),
  #                                   TheoryP = c(0.05),
  #                                   start.at = 1 ) )
  
  
  # debug cluster error
  ( scen.params = make_scen_params( method = c("bt.reg", "bt.cl"),  # "bt.reg", "bt.cl", "no.ci"
                                    calib.method = "MR",
                                    #calib.method = "MR bt mn correct",  # "MR" for one-stage, "DL" for two-stage, "MR bt mn correct", "MR bt var correct", "MR bt both correct"
                                    k = c(100),
                                    m = 100, # @NEW,
                                    #m = 100,
                                    
                                    b0 = 0, # intercept
                                    bc = 0, # effect of continuous moderator
                                    bb = 0, # effect of binary moderator
                                    
                                    zc.star = 0.5,  # level of moderator to consider
                                    zb.star = 1,
                                    
                                    zc.ref = 2,  # comparison levels of moderator to consider
                                    zb.ref = 0,
                                    
                                    V = c( .2 ), # residual variance
                                    Vzeta = .2 * 0.8, # between-cluster variance (@NEW)
                                    #Vzeta = 0,
                                    
                                    muN = NA,  # just a placeholder; to be filled in later
                                    minN = c(50),
                                    sd.w = c(1),
                                    tail = "above",
                                    true.effect.dist = c("normal"),
                                    TheoryP = c(0.2),
                                    start.at = 1 ) )
  
  
  # # full set of scenarios
  # ( scen.params = make_scen_params( method = "boot.whole",
  #                                   calib.method = "MR",
  #                                   
  #                                   k = rev(c(10, 20, 50, 100, 150)),
  #                                   b0 = 0, # intercept
  #                                   bc = 0.5, # effect of continuous moderator
  #                                   bb = 1, # effect of binary moderator
  #                                   
  #                                   zc.star = 0.5,  # "active" level of moderator to consider
  #                                   zb.star = 1,
  #                                   
  #                                   zc.ref = 2,  # reference levels of moderator to consider
  #                                   zb.ref = 0,
  #                                   
  #                                   # Previous choices:
  #                                   # zc.star = 0.5,  # "active" level of moderator to consider
  #                                   # zb.star = 1,
  #                                   # 
  #                                   # zc.ref = 2,  # reference levels of moderator to consider
  #                                   # zb.ref = 0,
  #                                   
  #                                   V = c( 0.5^2, 0.2^2, 0.1^2 ), # residual variance
  #                                   muN = NA,  # just a placeholder; to be filled in later
  #                                   minN = c(50, 800),
  #                                   sd.w = c(1),
  #                                   tail = "above",
  #                                   true.effect.dist = c("normal", "expo"), # # "expo", "normal", "unif2", "t.scaled"
  #                                   TheoryP = c(0.05, 0.1, 0.2, 0.5),
  #                                   start.at = 1 ) )
  
  # just to see it
  data.frame(scen.params)
  
  n.scen = nrow(scen.params)
  
  #as.data.frame(scen.params)
  
  
  # sim.reps = 500  # reps to run in this iterate; leave this alone!
  # boot.reps = 1000
  sim.reps = 100
  boot.reps = 1000  # ~~ temp only
  
  
  library(foreach)
  library(doParallel)
  library(dplyr)
  library(boot)
  library(purrr)
  library(robumeta)
  library(MetaUtility)
  
  # # ~~~ DEBUGGING: FOR CLUSTER
  # # EDITED FOR C++ ISSUE WITH PACKAGE INSTALLATION
  # library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
  # setwd("/home/groups/manishad/RRR")
  # source("functions_RRR.R")
  
  # set the number of cores
  registerDoParallel(cores=8)
  
  scen = 1
}



########################### RUN SIMULATION ###########################

for ( scen in scen.params$scen.name ) {
  rep.time = system.time({
    rs = foreach( i = 1:sim.reps, .combine=rbind ) %dopar% {
      # for debugging:
      #for ( i in 1:sim.reps ) {
      
      # extract simulation params for this scenario (row)
      # exclude the column with the scenario name itself (col) 
      p = scen.params[ scen.params$scen.name == scen, names(scen.params) != "scen.name"]
      
      
      # true average effect size for this combination of moderators
      TrueMean = p$b0 + ( p$bc * p$zc.star ) + ( p$bb * p$zb.star )
      
      ##### Simulate Dataset #####
      # d = sim_data( k = p$k, 
      #               b0 = p$b0, # intercept
      #               bc = p$bc, # effect of continuous moderator
      #               bb = p$bb, # effect of binary moderator 
      #               V = p$V,
      #               muN = p$muN, 
      #               minN = p$minN,
      #               sd.w = p$sd.w,
      #               true.effect.dist = p$true.effect.dist )
      
      # @NEW
      d = sim_data2( k = p$k, 
                     m = p$m,
                     b0 = p$b0, # intercept
                     bc = p$bc, # effect of continuous moderator
                     bb = p$bb, # effect of binary moderator 
                     V = p$V,
                     Vzeta = p$Vzeta,
                     muN = p$muN, 
                     minN = p$minN,
                     sd.w = p$sd.w,
                     true.effect.dist = p$true.effect.dist )
      
      ##### Get Meta-Regressive Phat for This Dataset #####
      d.stats = prop_stronger_mr(d,
                                 zc.star = p$zc.star,
                                 zb.star = p$zb.star,
                                 zc.ref = p$zc.ref,
                                 zb.ref = p$zb.ref,
                                 calib.method = p$calib.method )
      
      # estimated mean at level "star" of effect modifiers
      EstMean = d.stats$bhat0 + ( p$bc * p$zc.star ) + ( p$bb * p$zb.star )
      
      
      ##### Phat Difference #####
      # Phat difference for two levels of moderators
      PhatDiff = d.stats$Phat.diff
      
      ##### Bootstrap #####
      if ( p$method %in% c("bt.reg", "bt.cl") ) {
        
        Note = NA
        tryCatch({
          
          # this is just the resampling part, not the CI estimation
          boot.res = my_boot( data = d, 
                              parallel = "multicore",
                              R = boot.reps, 
                              statistic = function(original, indices) {
                                # @ for clustering, need to use cluster_bt here
                                
                                # @NEW
                                # bm
                                # either use regular bootstrap or cluster bootstrap as appropriate
                                
                                if ( p$method == "bt.reg" ) {
                                  b = original[indices,]
                                }
                                
                                if ( p$method == "bt.cl" ){
                                  # resample CLUSTERS with replacement (keep number of clusters the same)
                                  #  and retain all observations within the resampled clusters
                                  # so total N could be different in the bootstrapped sample
                                  b = cluster.resample(data = original,
                                                       cluster.name = "cluster",
                                                       # number of CLUSTERS to resample
                                                       size = length( unique(original$cluster) ) )
                                }
                                
                                tryCatch({
                                  b.stats = prop_stronger_mr(dat = b,
                                                             zc.star = p$zc.star,
                                                             zb.star = p$zb.star,
                                                             zc.ref = p$zc.ref,
                                                             zb.ref = p$zb.ref,
                                                             #calib.method = "MR",
                                                             calib.method = p$calib.method
                                  )
                                  
                                  # only return the 3 stats of interest
                                  c( as.numeric(b.stats["Phat"]),
                                     as.numeric(b.stats["Phat.ref"]),
                                     as.numeric(b.stats["Phat.diff"]),
                                     as.numeric(b.stats["t2"]),
                                     truncLogit( as.numeric(b.stats["Phat"]) ) # transformed Phat
                                  )
                                }, error = function(err){
                                  rep(NA, 5)
                                })
                                
                              } )
          boot.res
          # for debugging
          #head( boot.res$t )
          
          # #@temp
          # hist(boot.res$t[,1])
          # hist( logit(boot.res$t[,1]) )
          # # doesn't really work -- still has huge point masses from 0s and 1s
          
          
          # boot diagnostics
          bt.means = as.numeric( colMeans(boot.res$t, na.rm = TRUE) )
          bt.sds = apply( boot.res$t, 2, function(x) sd(x, na.rm = TRUE) )
          
          # get CIs for each estimand individually in case some work and others don't
          tryCatch({
            CI = boot.ci(boot.res, type = "bca", index = 1)
            # put in nice vector format
            PhatBootCIs = c( CI[[4]][4], CI[[4]][5] )
          }, error = function(err){
            PhatBootCIs <<- c(NA, NA)
          } )
          
          
          tryCatch({
            CI = boot.ci(boot.res, type = "bca", index = 2)
            PhatRefBootCIs = c( CI[[4]][4], CI[[4]][5] )
            PhatRefBootSD = sd( boot.res$t[,2] )
          }, error = function(err){
            PhatRefBootCIs <<- c(NA, NA)
          } )
          
          tryCatch({
            CI = boot.ci(boot.res, type = "bca", index = 3)
            DiffBootCIs = c( CI[[4]][4], CI[[4]][5] )
          }, error = function(err){
            DiffBootCIs <<- c(NA, NA)
          } )
          
          tryCatch({
            CI = boot.ci(boot.res, type = "bca", index = 5)
            truncLogitBootCIs = c( CI[[4]][4], CI[[4]][5] )
          }, error = function(err){
            truncLogitBootCIs <<- c(NA, NA)
          } )
          
          # this part happens only if bootstrapping fails completely
          # not just CIs
        }, error = function(err){
          # one list item for each stat of interest (3),
          #  and one sub-entry for lower/upper CI limit
          n.ests = 5  # needs to match what's returned in boot.res
          PhatBootCIs <<- c(NA, NA)
          PhatRefBootCIs <<- c(NA, NA)
          DiffBootCIs <<- c(NA, NA)
          truncLogitBootCIs <<- c(NA, NA)
          bt.means <<- rep(NA, n.ests)
          bt.sds <<- rep(NA, n.ests)
          #boot.median <<- NA
          Note <<- paste("Resampling failed completely: ", err$message, sep="")
          #browser()
        } )  # end of the big tryCatch loop for the whole boot() call
        
      }  # end part for both bootstrap methods
      
      
      if ( p$method == "no.ci" ) {
        n.ests = 5
        Note = NA
        PhatBootCIs = c(NA, NA)
        PhatRefBootCIs = c(NA, NA)
        DiffBootCIs = c(NA, NA)
        truncLogitBootCIs <<- c(NA, NA)
        bt.means = rep(NA, n.ests)
        bt.sds = rep(NA, n.ests)
      }
      
      
      ##### Write Results #####
      rows = data.frame( 
        TrueMean = TrueMean,
        # estimated mean at level "star" of effect modifiers
        EstMean = EstMean, 
        
        TrueVar = p$V,
        EstVar = d.stats$t2,
        EstVarBtMn = bt.means[4],
        
        # ICC of population effects within clusters
        ICCpop = d$icc[1],
        # number of clusters (could be <m for reasons described in helper code)
        nClusters = length(unique(d$cluster)),
        VzetaEmp = var( d$zeta1[ !duplicated(d$cluster) ] ),
        
        # for "star" level of moderators
        Phat = d.stats$Phat,
        PhatLo = PhatBootCIs[1],
        PhatHi = PhatBootCIs[2],
        PhatBtMn = bt.means[1],
        PhatBtSD = bt.sds[1],
        
        LogitPhatBtMn = bt.means[5],
        truncLogitLo = truncLogitBootCIs[1],
        truncLogitHi = truncLogitBootCIs[2],
        
        # for reference level of moderators
        PhatRef = d.stats$Phat.ref, 
        PhatRefLo = PhatRefBootCIs[1],
        PhatRefHi = PhatRefBootCIs[2],
        PhatRefBtMn = bt.means[2],
        PhatRefBtSD = bt.sds[2],
        
        # for the difference
        Diff = PhatDiff,
        DiffLo = DiffBootCIs[1],
        DiffHi = DiffBootCIs[2],
        DiffBtMn = bt.means[3],
        DiffBtSD = bt.sds[3],
        
        # method of calculating CI
        Method = p$method,
        
        # CI performance
        CoverPhat = covers(p$TheoryP, PhatBootCIs[1], PhatBootCIs[2]),
        CoverPhatRef = covers(p$TheoryP.ref, PhatRefBootCIs[1], PhatRefBootCIs[2]),
        CoverDiff = covers(p$TheoryDiff, DiffBootCIs[1], DiffBootCIs[2]),
        
        PhatCIWidth = PhatBootCIs[2] - PhatBootCIs[1],
        PhatRefCIWidth = PhatRefBootCIs[2] - PhatRefBootCIs[1],
        DiffCIWidth = DiffBootCIs[2] - DiffBootCIs[1],
        
        Note = Note)
      
      
      ##### Write Results #####
      
      # add in scenario parameters
      rows$scen.name = scen
      rows = as.data.frame( merge(rows, scen.params,
                                  by = "scen.name") )
      rows
      
    }  ### end foreach loop
    
  } )[3]  # end timer
  
  if ( scen == scen.params$scen.name[1] ) rs2 = rs
  else rs2 = rbind(rs2, rs)
}

# trying with more sim.reps
# compare bt.reg to bt.cl
rs2 %>% group_by(method) %>%
  summarise( btFail = mean(is.na(PhatLo) ),
             
             PhatMn = mean(Phat, na.rm = TRUE),
             TheoryP = TheoryP[1],
             
             CoverPhat = mean(CoverPhat, na.rm = TRUE),
             CIWidth = mean(PhatCIWidth, na.rm = TRUE),
             
             MnPhatBtSD = mean(PhatBtSD, na.rm = TRUE),
             PhatSDEmp = sd(Phat, na.rm = TRUE) )



head(rs)
table(is.na(rs$PhatLo))
mean(rs$CoverPhat, na.rm = TRUE)  #**92% without clusters; 63% with 50 clusters; 54% with 5 clusters; 60% with m=k WHICH DOES NOT MAKE SENSE; 95% with m=k but forcing use of regular bootstrap so cluster boot is at fault

mean(rs$Phat, na.rm = TRUE)  # unbiased even with clusters
mean(rs$PhatBtSD, na.rm = TRUE); sd(rs$Phat, na.rm = TRUE)  # the boot SD is an underestimate when there are clusters


# bm: want to make sure CIs are reasonable when generating clusters
#  in scenarios that work well without clustering
# debug the evil finding of 60% coverage when m=k since this invokes the cluster boot
#  but does NOT actually have clustering

# just tried reducing clusters to 5 instead of 50

# first running a "safe" scenario


# uncorrected Phat and 2 bias-corrected versions
mean(rs$Phat); rs$TheoryP[1]  
mean( rs$Phat - (rs$PhatBtMn - rs$Phat) )
expit( mean( truncLogit(rs$Phat) - (rs$LogitPhatBtMn - truncLogit(rs$Phat) ) ) )

mean(rs$EstVar); rs$V[1]
mean( rs$EstVar - (rs$EstVarBtMn - rs$EstVar) )

mean(rs$Diff)
mean(rs$EstMean); rs$TrueMean[1]
mean(rs$EstVar); rs$V[1]

# running this one bad scenario while saving additional info
#  mostly interested in the boot means here

# time in seconds
rep.time
rs$rep.time = rep.time

# rs$PhatBtMn
# 
# ######### @TEMP ONLY
# 
# # look at logit distribution
# ggplot( ) +
#   geom_histogram( data = rs,
#                   aes( x = truncLogit(Phat) ),
#                   alpha = 0.3 ) +
#   
#   # distribution of bootstrap means
#   geom_histogram( data = rs,
#                   aes( x = LogitPhatBtMn ),  # actual bootstrap mean
#                   alpha = 0.3,
#                   color = "blue") +
#   
#   # black line: truth
#   geom_vline( data = rs,
#               aes(xintercept = truncLogit(rs$TheoryP[1]) ),
#               lty = 2) +
#   
#   # red line: mean of Phats
#   geom_vline( data = rs,
#               aes(xintercept = mean( truncLogit(rs$Phat), na.rm = TRUE)),
#               color = "red",
#               lty = 2) +
#   theme_classic()
# 
# 
# # these should be very close, but not the same due to truncation of logit
# mean(rs$Phat)  # 0.0797 with t2==0 condition; 0.09 with the condition
# mean( expit( truncLogit(rs$Phat) ), na.rm = TRUE )
# 
# # these are different because means are involved
# mean(rs$PhatBtMn, na.rm = TRUE)
# mean( expit(rs$LogitPhatBtMn), na.rm = TRUE )  # this one is closer to the truth
# 
# # whoa...second one sucks! 
# mean( rs$Phat - (rs$PhatBtMn - rs$Phat), na.rm = TRUE ) # bias correction #1
# mean( expit( truncLogit(rs$Phat) - ( rs$LogitPhatBtMn - truncLogit(rs$Phat) ) ), na.rm = TRUE ) # bias correction #2
# 
# 
# rs$V[1]
# mean(rs$EstVar)
# hist(rs$EstVar)  # 0.01186237 with t2==0 condition
# # bias correction actually makes the variance even worse...
# mean( rs$EstVar - (rs$EstVarBtMn - rs$EstVar), na.rm=TRUE ) # bias correction
# 
# # issue of discarding t2=0 sim iterates?
# # bm
# 
# ###### END OF TEMP

########################### WRITE LONG RESULTS  ###########################
if ( run.local == FALSE ) {
  setwd("/home/groups/manishad/MRM/sim_results/long")
  write.csv( rs, paste( "long_results", jobname, ".csv", sep="_" ) )
}