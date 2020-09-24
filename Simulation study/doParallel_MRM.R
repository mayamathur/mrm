
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
  sim.reps = 500
  # used boot.reps=s5,000 in NPPhat
  # just reduced to 1,000
  # MR bt mn both correct still times out with 5:00:00 at 1000 boot.reps
  # JUST TO LOOK AT TIMEOUT ISSUE:
  # for largest scenario (k=150) with method MR and boot.reps=50 and sim.reps=100, one sbatch took 15 min
  #  so boot.reps=1,000 should be about 5 hrs
  # reducing sim.reps to 50 should be about 2.5 hrs
  boot.reps = 1000
  
  
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
  library(tidyr, lib.loc = "/home/groups/manishad/Rpackages/")
  
  # for use in ml load R
  # install.packages( c("ICC", "cfdecomp", "tidyr"), lib = "/home/groups/manishad/Rpackages/" )
  
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
  library(tidyr)
  
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
  
  
  # # debug cluster error
  # ( scen.params = make_scen_params( method = c("bt.smart"),  # "bt.smart" or "no.ci"
  #                                   calib.method = c("MR"),
  #                                   #calib.method = "MR bt mn correct",  # "MR" for one-stage, "DL" for two-stage, "MR bt mn correct", "MR bt var correct", "MR bt both correct"
  #                                   k = c(150),
  #                                   m = c(75), # @NEW,
  #                                   #m = 100,
  #                                   
  #                                   b0 = 0, # intercept
  #                                   bc = 0, # effect of continuous moderator
  #                                   bb = 0, # effect of binary moderator
  #                                   
  #                                   zc.star = 0.5,  # level of moderator to consider
  #                                   zb.star = 1,
  #                                   
  #                                   zc.ref = 2,  # comparison levels of moderator to consider
  #                                   zb.ref = 0,
  #                                   
  #                                   V = c( .2 ), # residual variance
  #                                   Vzeta = .2 * 0.8, # between-cluster variance (@NEW)
  #                                   #Vzeta = 0,
  #                                   
  #                                   muN = NA,  # just a placeholder; to be filled in later
  #                                   minN = c(50),
  #                                   sd.w = c(1),
  #                                   tail = "above",
  #                                   true.effect.dist = c("normal"),
  #                                   TheoryP = c(0.2),
  #                                   start.at = 1 ) )
  
  
  # full set of scenarios
  # IMPORTANT: METHOD MUST HAVE "BT" IN ITS NAME TO BE RECOGNIZED AS BOOTSTRAPPING
  ( scen.params = make_scen_params( method = "bt.smart",
                                    calib.method = c("MR", "DL"),
                                    
                                    k = rev(c(10, 20, 50, 100, 150)),
                                    m = c(99, -99), # to be filled in later;  this is just to generate 2 levels
                                    b0 = 0, # intercept
                                    bc = 0.5, # effect of continuous moderator
                                    bb = 1, # effect of binary moderator
                                    
                                    zc.star = 0.5,  # "active" level of moderator to consider
                                    zb.star = 1,
                                    
                                    zc.ref = 2,  # reference levels of moderator to consider
                                    zb.ref = 0,
                                    
                                    # Previous choices:
                                    # zc.star = 0.5,  # "active" level of moderator to consider
                                    # zb.star = 1,
                                    #
                                    # zc.ref = 2,  # reference levels of moderator to consider
                                    # zb.ref = 0,
                                    
                                    V = rev( c( 0.8^2, 0.5^2, 0.2^2, 0.1^2, 0.05^2 ) ), # residual variance
                                    Vzeta = NA, # to be filled in
                                    muN = NA,  # just a placeholder; to be filled in later
                                    minN = c(50, 800),
                                    sd.w = c(1),
                                    tail = "above",
                                    true.effect.dist = c("normal", "expo"), # # "expo", "normal", "unif2", "t.scaled"
                                    TheoryP = c(0.05, 0.1, 0.2, 0.5),
                                    start.at = 1 ) )
  
  # define clustering scenarios
  # m = -99 will be no clustering
  # m = 99 will be clustering
  scen.params$clustered = (scen.params$m == 99)
  scen.params$Vzeta[ scen.params$m == -99 ] = 0
  scen.params$Vzeta[ scen.params$m == 99 ] = scen.params$V[ scen.params$m == 99 ] * 0.75
  scen.params$m[ scen.params$m == -99 ] = scen.params$k[ scen.params$m == -99 ]
  scen.params$m[ scen.params$m == 99 ] = scen.params$k[ scen.params$m == 99 ]/2
  
  # sanity check
  scen.params %>% group_by(clustered) %>%
    summarise( mean(m/k),
               mean(Vzeta/V) )
  
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
  
  scen = 261
  data.frame(scen.params %>% filter(scen.name == scen))
}



########################### RUN SIMULATION ###########################


#for ( scen in scen.params$scen.name ) {  # can't use this part on the cluster
  
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
      # simulates potentially clustered data
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
      # internally bias-corrects the meta-regressive mean and heterogeneity
      #  if asked (calib.method)
      d.stats = prop_stronger_mr(d,
                                 zc.star = p$zc.star,
                                 zb.star = p$zb.star,
                                 zc.ref = p$zc.ref,
                                 zb.ref = p$zb.ref,
                                 calib.method = p$calib.method )
      
      # estimated mean at level "star" of effect modifiers
      # this has already been bias-corrected if asked (calib.method)
      EstMean = d.stats$bhat0 + ( p$bc * p$zc.star ) + ( p$bb * p$zb.star )
      
      
      ##### Phat Difference #####
      # Phat difference for two levels of moderators
      PhatDiff = d.stats$Phat.diff
      
      ##### Bootstrap #####

      if ( grepl(pattern = "bt", x=p$method) ) {
        
        Note = NA
        # boot sanity checks that will be overwritten if bootstrap succeeds
        btRows = NA
        btNClusters = NA
        
        tryCatch({
          
          # nest by cluster in case we need to do cluster bootstrap
          # now has one row per cluster
          # works whether there is clustering or not
          # because without clustering, the clusters are 1:nrow(data)
          dNest = d %>% group_nest(cluster)
          
          # this is just the resampling part, not the CI estimation
          boot.res = my_boot( data = dNest, 
                              parallel = "multicore",
                              R = boot.reps, 
                              statistic = function(original, indices) {

                                # bt.smart: allows for either independent or clustered observations
                                #  as long as independent observations are each in own cluster
                                if ( p$method == "bt.smart" ) {
                                  bNest = original[indices,]
                                  b = bNest %>% unnest(data)
                                }
                                
                                tryCatch({
                                  
                                  # simple sanity checks on resampling
                                  btRows = nrow(b)
                                  btNClusters = length(unique(b$cluster))
                                  
                                  b.stats = prop_stronger_mr(dat = b,
                                                             zc.star = p$zc.star,
                                                             zb.star = p$zb.star,
                                                             zc.ref = p$zc.ref,
                                                             zb.ref = p$zb.ref,
                                                             calib.method = p$calib.method )
                                  # @TEMP ONLY - FAKE ERROR IN 50% OF ITERATES
                                  #if ( rbinom( n=1, size=2, prob = .2) == 1 ) stop("Fake error")
                                  
                                  # return the stats of interest
                                  # order of stats has to match indices in CI tryCatch loops below
                                  # and in returned results because of bt.means and bt.sds
                                  c( as.numeric(b.stats["Phat"]),
                                     as.numeric(b.stats["Phat.ref"]),
                                     as.numeric(b.stats["Phat.diff"]),
                                     as.numeric(b.stats["t2"]),
                                     truncLogit( as.numeric(b.stats["Phat"]) ) # transformed Phat
                                  )
                                }, error = function(err){
                        
                                  return( rep(NA, 5) )
                                })
                                
                              } )
          boot.res
          # bm

          
          # boot diagnostics
          bt.pfails =  as.numeric( colMeans( is.na(boot.res$t) ) )  # proportion of boot reps that failed (NAs)
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
          bt.pfails <<- rep(NA, n.ests)
          #boot.median <<- NA
          Note <<- paste("Resampling failed completely: ", err$message, sep="")
          #browser()
        } )  # end of the big tryCatch loop for the whole boot() call
        
      }  # end part for both bootstrap methods
      
      ##### No CI (for faster experimentation) #####
      if ( p$method == "no.ci" ) {
        n.ests = 5
        Note = NA
        PhatBootCIs = c(NA, NA)
        PhatRefBootCIs = c(NA, NA)
        DiffBootCIs = c(NA, NA)
        truncLogitBootCIs <<- c(NA, NA)
        bt.means = rep(NA, n.ests)
        bt.sds = rep(NA, n.ests)
        bt.pfails = rep(NA, n.ests)
        btRows = NA
        btNClusters = NA
      }
      
      # order of things in bt.means, etc.
      # as.numeric(b.stats["Phat"]),
      # as.numeric(b.stats["Phat.ref"]),
      # as.numeric(b.stats["Phat.diff"]),
      # as.numeric(b.stats["t2"]),
      # truncLogit( as.numeric(b.stats["Phat"]) )
      
      ##### Write Results #####
      rows = data.frame( 
        TrueMean = TrueMean,
        # estimated mean at level "star" of effect modifiers
        # will be the bias-corrected one if specified via method argument
        EstMean = EstMean, 
        
        TrueVar = p$V,
        EstVar = d.stats$t2,  # will be the bias-corrected one if specified via method argument
        EstVarBtMn = bt.means[4],
        
        # sanity checks on data generation
        # ICC of population effects within clusters
        ICCpop = d$icc[1],
        # number of clusters (could be <m for reasons described in helper code)
        nClusters = length(unique(d$cluster)),
        VzetaEmp = var( d$zeta1[ !duplicated(d$cluster) ] ),
        
        # boot diagnostics
        btNClusters = btNClusters,
        btRows = btRows,
        
        # for "star" level of moderators
        Phat = d.stats$Phat,
        PhatLo = PhatBootCIs[1],
        PhatHi = PhatBootCIs[2],
        PhatBtMn = bt.means[1],
        PhatBtSD = bt.sds[1],
        PhatBtFail = bt.pfails[1],
        # bm
        
        LogitPhatBtMn = bt.means[5],
        truncLogitLo = truncLogitBootCIs[1],
        truncLogitHi = truncLogitBootCIs[2],
        
        # for reference level of moderators
        PhatRef = d.stats$Phat.ref, 
        PhatRefLo = PhatRefBootCIs[1],
        PhatRefHi = PhatRefBootCIs[2],
        PhatRefBtMn = bt.means[2],
        PhatRefBtSD = bt.sds[2],
        PhatRefBtFail = bt.pfails[2],
        
        # for the difference
        Diff = PhatDiff,
        DiffLo = DiffBootCIs[1],
        DiffHi = DiffBootCIs[2],
        DiffBtMn = bt.means[3],
        DiffBtSD = bt.sds[3],
        DiffBtFail = bt.pfails[3],
        
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


rs$repTime = rep.time
  
# # for local use
#   if ( scen == scen.params$scen.name[1] ) rs2 = rs
#   else rs2 = rbind(rs2, rs)
# }  # end loop over all scens in scen.parameters (for local use)

# # trying with more sim.reps
# # compare bt.reg to bt.cl
# rs2 %>% group_by(scen.name) %>%
#   summarise( btFail = mean(is.na(PhatLo) ),
# 
#              PhatMn = mean(Phat, na.rm = TRUE),
#              TheoryP = TheoryP[1],
# 
#              CoverPhat = mean(CoverPhat, na.rm = TRUE),
#              CIWidth = mean(PhatCIWidth, na.rm = TRUE),
# 
#              MnPhatBtSD = mean(PhatBtSD, na.rm = TRUE),
#              PhatSDEmp = sd(Phat, na.rm = TRUE) )


########################### WRITE LONG RESULTS  ###########################
if ( run.local == FALSE ) {
  setwd("/home/groups/manishad/MRM/sim_results/long")
  write.csv( rs, paste( "long_results", jobname, ".csv", sep="_" ) )
}