
# Why does cluster.resample perform worse than regular bootstrap when 
#  m=k (no clusters)? Shouldn't the resampling algorithms be exactly equivalent in that case?

# THIS VERSION WORKS!!! (with the cluster bootstrapped via nest/unnest)


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
  
 
  # same scenario in screenshot
  ( scen.params = make_scen_params( method = c("bt.reg"),  # "bt.reg", "bt.cl", "no.ci"
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
  
  
 
  
  # just to see it
  data.frame(scen.params)
  
  n.scen = nrow(scen.params)
  

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
      if ( grepl(pattern = "bt", x=p$method) ) {
        
        Note = NA
        tryCatch({
          
          # nest by cluster in case we need to do cluster bootstrap
          # now has one row per cluster
          dNest = d %>% group_nest(cluster)
          
          
          # this is just the resampling part, not the CI estimation
          boot.res = my_boot( data = dNest, 
                              parallel = "multicore",
                              R = boot.reps, 
                              statistic = function(original, indices) {
                                
                          
                                # @NEW
                                # bm
                                # either use regular bootstrap or cluster bootstrap as appropriate
                                
                                if ( p$method == "bt.reg" ) {
                                  # test only
                                  #indices = sample(1:nrow(dNest), replace = TRUE)
                                  bNest = original[indices,]
                                  b = bNest %>% unnest(data)
                                }
                                
                                # # bm
                                # if ( p$method == "bt.reg.mine" ) {
                                #   inds = sample(1:nrow(original), replace = TRUE)
                                #   b = original[inds,]
                                #   
                                #   indices <<- inds
                                # }
                                # 
                                # 
                                # 
                                # if ( p$method == "bt.cl" ){
                                #   
                                #   # # guts of cluster.resample
                                #   # data = d
                                #   # cluster.name = "cluster"
                                #   # size = length( unique(original$cluster) )
                                #   # IDs <- unique(data[, cluster.name])
                                #   # # IDs of clusters sampled with replacement
                                #   # y <- sample(IDs, size, replace = TRUE)
                                #   # # table of counts for how many clusters are to appear 1, 2, 3, ... times in dataset
                                #   # z <- table(table(y))
                                #   # # z[1] is the number of clusters chosen once, e.g., 31
                                #   # # from ALL the clusters, choose 31 of them WITHOUT replacement
                                #   # # so selectID represents the clusters that are to appear once in the ultimate resampled dataset
                                #   # selectID <- sample(IDs, size = z[1], replace = FALSE)
                                #   # # retain all observations whose cluster was in the selected IDs
                                #   # newdata <- data[which(data[, cluster.name] %in% selectID), 
                                #   # ]
                                #   # 
                                #   # # now go through the other counts of how often a cluster could be assigned to appear
                                #   # # e.g. for i=2:
                                #   # # 
                                #   # if (length(z) > 1) {
                                #   #   # i: the number of times that a cluster could be assigned to appear
                                #   #   for (i in 2:length(z)) {
                                #   #     # IDs: the cluster IDs that were NOT assigned to be chosen once (i.e., still eligible for assignment)
                                #   #     # if selectID was length 31 and there are 100 total clusters, this will be length 100-31
                                #   #     IDs2 <- setdiff(IDs, selectID)
                                #   #     # from the remaining eligible clusters, choose the appropriate count of them (z[i]) to appear twice (for i=2) in the resample
                                #   #     selectID2 <- sample(IDs2, size = z[i], replace = FALSE)
                                #   #     # update selectID to reflect that selectID2 are no longer eligible for the next i
                                #   #     selectID <- c(selectID, selectID2)
                                #   #     # rbind the corresponding data to the resample twice (for i=2)
                                #   #     for (j in 1:i) {
                                #   #       newdata <- rbind(newdata, data[which(data[, cluster.name] %in% 
                                #   #                                              selectID2), ])
                                #   #     }
                                #   #   }
                                #   #   return(newdata)
                                #   # }
                                #   
                                #   
                                #   # resample CLUSTERS with replacement (keep number of clusters the same)
                                #   #  and retain all observations within the resampled clusters
                                #   # so total N could be different in the bootstrapped sample
                                #   b = cluster.resample(data = original,
                                #                        cluster.name = "cluster",
                                #                        # number of CLUSTERS to resample
                                #                        size = length( unique(original$cluster) ) )
                                # }
                                
                                tryCatch({
                                  
                                  
                                  
                                  # bootstrap diagnostics
                                  ( btRows = nrow(b) )
                                  ( btNClusters = length(unique(b$cluster)) )
                                  
                                  # compare the yi's in bootstrapped df for the first chosen cluster
                                  # to corresponding cluster in d
                                  # in b, each observation may appear multiple times,
                                  #  but should still have all observations
                                  # table( b$yi[ b$cluster == b$cluster[1]] )
                                  # table( d$yi[ d$cluster == b$cluster[1]] )
                                  
                                  b.stats = prop_stronger_mr(dat = b,
                                                             zc.star = p$zc.star,
                                                             zb.star = p$zb.star,
                                                             zc.ref = p$zc.ref,
                                                             zb.ref = p$zb.ref,
                                                             #calib.method = "MR",
                                                             calib.method = p$calib.method )
                                  
                                  
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
        
        # boot diagnostics
        # @ maybe put in the real doParallel as well?
        btNClusters = btNClusters,
        btRows = btRows,
        
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


table(rs2$nClusters)
table(rs2$btRows)

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