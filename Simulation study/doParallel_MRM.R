
#### NEXT UP: 
# - Run brat scenario locally to see why it had a sim failure


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
  sim.reps = 10
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

  # for use in ml load R
  # install.packages( c("metRology"), lib = "/home/groups/manishad/Rpackages/" )

  path = "/home/groups/manishad/MRM"
  setwd(path)
  source("helper_MRM.R")

  # set the number of cores
  registerDoParallel(cores=16)

  ##### Write Blank CSV File #####
  # this records that the rep started in case there is a problem with the bootstrapping
  placeholder = data.frame( TrueMean = NA,
                            EstMean = NA, 
                            
                            TrueVar = NA,
                            EstVar = NA,
  
                            # for "star" level of moderators
                            Phat = NA,
                            PhatLo = NA,
                            PhatHi =  NA,
                            
                            # for reference level of moderators
                            PhatRef =  NA,
                            PhatRefLo =  NA,
                            PhatRefHi =  NA,
                     
                            # for the difference
                            #TheoryDiff = p$TheoryDiff, 
                            Diff =  NA,
                            DiffLo =  NA,
                            DiffHi =  NA,
                            
                            # method of calculating CI: exponentiate logit or not?
                            Method =  NA,
                            
                            # CI performance
                            CoverPhat =  NA,
                            CoverPhatRef =  NA,
                            CoverDiff =  NA,
                            
                            PhatCIWidth =  NA,
                            PhatRefCIWidth =  NA,
                            DiffCIWidth =  NA,
                            
                            Note = "Sim failure")


  placeholder$scen.name = scen
  placeholder = merge( placeholder, scen.params, by = "scen.name" )

  setwd("/home/groups/manishad/MRM/sim_results/long")
  write.csv( placeholder, paste( "long_results", jobname, ".csv", sep="_" ) )
  # this will be overwritten if the rep finished successfully
}



######################################## FOR LOCAL USE ########################################
if ( run.local == TRUE ) {
  rm(list=ls())
  
  # helper fns
  code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"
  setwd(code.dir)
  source("helper_MRM.R")
  
  # just one scenario
  ( scen.params = make_scen_params( method = "boot.whole",
                                    k = c(50),
                                    b0 = 0, # intercept
                                    bc = 0.5, # effect of continuous moderator
                                    bb = 1, # effect of binary moderator
                                    
                                    zc.star = 0.5,  # level of moderator to consider
                                    zb.star = 1,
                                    
                                    zc.ref = 2,  # comparison levels of moderator to consider
                                    zb.ref = 0,
                                    
                                    V = c( 0.5^2 ), # residual variance
                                    muN = NA,  # just a placeholder; to be filled in later
                                    minN = c(100),
                                    sd.w = c(1),
                                    tail = "above",
                                    true.effect.dist = c("normal"), # # "expo", "normal", "unif2", "t.scaled"
                                    TheoryP = c(0.05),
                                    start.at = 1 ) )
  # just to see it
  data.frame(scen.params)
  
  n.scen = nrow(scen.params)
  
  #as.data.frame(scen.params)
  
  
  # sim.reps = 500  # reps to run in this iterate; leave this alone!
  # boot.reps = 1000
  sim.reps = 100
  boot.reps = 500  # ~~ temp only
  
  
  library(foreach)
  library(doParallel)
  library(dplyr)
  library(boot)
  library(purrr)
  
  
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



########################### RUN SIMULATION (CLUSTER) ###########################

# global parameters for all scenarios
#CI.level = 0.95

# which methods should we run?
# it always runs parametric 
# should list all of them unless we're re-running an existing scenario with a new method
#methods.to.run = c("Boot", "NP ensemble", "NP sign test")


rep.time = system.time({
  rs = foreach( i = 1:sim.reps, .combine=rbind ) %dopar% {
    
    # extract simulation params for this scenario (row)
    # exclude the column with the scenario name itself (col) 
    p = scen.params[ scen.params$scen.name == scen, names(scen.params) != "scen.name"]
    
    # true average effect size for this combination of moderators
    TrueMean = p$b0 + ( p$bc * p$zc.star ) + ( p$bb * p$zb.star )
    
    ##### Simulate Dataset #####
    d = sim_data( k = p$k, 
                  b0 = p$b0, # intercept
                  bc = p$bc, # effect of continuous moderator
                  bb = p$bb, # effect of binary moderator 
                  V = p$V,
                  muN = p$muN, 
                  minN = p$minN,
                  sd.w = p$sd.w,
                  true.effect.dist = p$true.effect.dist )
    
    
    ##### Get Meta-Regressive Phat for This Dataset #####
    d.stats = prop_stronger_mr(d,
                               zc.star = p$zc.star,
                               zb.star = p$zb.star,
                               zc.ref = p$zc.ref,
                               zb.ref = p$zb.ref )
    
    
    EstMean = d.stats$bhat0 + ( p$bc * p$zc.star ) + ( p$bb * p$zb.star )
    
    ##### Phat Difference #####
    # Phat difference for two levels of moderators
    PhatDiff = d.stats$Phat.diff
    
    ##### Bootstrap #####
    # option to not bootstrap
    if ( p$method == "boot.whole" ) {
      
      Note = NA
      tryCatch({
        
        boot.res = boot( data = d, 
                         parallel = "multicore",
                         R = boot.reps, 
                         statistic = function(original, indices) {
                           b = original[indices,]
                           
                           b.stats = prop_stronger_mr(dat = b,
                                            zc.star = p$zc.star,
                                            zb.star = p$zb.star,
                                            zc.ref = p$zc.ref,
                                            zb.ref = p$zb.ref)
                           
                           # only return the two stats of interest
                           c( as.numeric(b.stats["Phat"]),
                              as.numeric(b.stats["Phat.ref"]),
                              as.numeric(b.stats["Phat.diff"]) )
                         } )
                        
        # for debugging
        #head( boot.res$t )
        
        bootCIs = get_boot_CIs(boot.res, "bca", n.ests = 3)
        
        # for one estimate only
        # bootCIs = boot.ci(boot.res, type="bca")
        # boot.lo = bootCIs$bca[4]
        # boot.hi = bootCIs$bca[5]
        # boot.median = median(boot.res$t)  # median Phat in bootstrap iterates
        
      }, error = function(err){
        # one list item for each stat of interest (3),
        #  and one sub-entry for lower/upper CI limit
        n.stats = 3
        bootCIs <<- list( c(NA, NA), c(NA, NA), c(NA, NA) )
        #boot.median <<- NA
        Note <<- paste("BCa failed: ", err$message, sep="")
      } )
      
      
      # write results
      rows = data.frame( 
                      TrueMean = TrueMean,
                      EstMean = EstMean, 
                      # would need to combine the vars for this:
                      #MeanCover = covers( p$mu, summary(m)$ci.lb, summary(m)$ci.ub ),
                      
                      TrueVar = p$V,
                      EstVar = d.stats$t2,
                      # VarCover = covers( p$V, CIs$random["tau^2", "ci.lb"],
                      #                    CIs$random["tau^2", "ci.ub"] ),
                      
                      # for "star" level of moderators
                      Phat = d.stats$Phat,
                      PhatLo = bootCIs[[1]][1],
                      PhatHi = bootCIs[[1]][2],
                      
                      # for reference level of moderators
                      PhatRef = d.stats$Phat,
                      PhatRefLo = bootCIs[[2]][1],
                      PhatRefHi = bootCIs[[2]][2],
 
                      #Phat.bt.med = boot.median,  # note that this is the median of the bootstrap iterates
                      #Phat.bt.bias = boot.median - p$TheoryP, 
                      
                      # for the difference
                      #TheoryDiff = p$TheoryDiff, 
                      Diff = PhatDiff,
                      DiffLo = bootCIs[[3]][1],
                      DiffHi = bootCIs[[3]][2],
                      
                      # method of calculating CI: exponentiate logit or not?
                      Method = p$method,
                      
                      # CI performance
                      CoverPhat = covers(p$TheoryP, bootCIs[[1]][1], bootCIs[[1]][2]),
                      CoverPhatRef = covers(p$TheoryP.ref, bootCIs[[2]][1], bootCIs[[2]][2]),
                      CoverDiff = covers(p$TheoryDiff, bootCIs[[3]][1], bootCIs[[3]][2]),
                      
                      PhatCIWidth = bootCIs[[1]][2] - bootCIs[[1]][1],
                      PhatRefCIWidth = bootCIs[[2]][2] - bootCIs[[2]][1],
                      DiffCIWidth = bootCIs[[3]][2] - bootCIs[[3]][1],
                      
                      Note = Note)
 
    }  # end boot.whole
    
    
    ##### Write Results #####
    
    # add in scenario parameters
    rows$scen.name = scen
    rows = as.data.frame( merge(rows, scen.params,
                                by = "scen.name") )
    rows
    
  }  ### end foreach loop
  
} )[3]  # end timer


head(rs)


# time in seconds
rep.time
rs$rep.time = rep.time

# if ( run.local == TRUE ) {
#   # ~~ COMMENT OUT BELOW PART TO RUN ON CLUSTER
#   # see results
#   analysis.vars = c(
#                     # "TrueMean",
#                     # "EstMean",
#                     # "TrueVar",
#                     # "EstVar",
#     
#                     "TheoryP",
#                     "Phat",
#                     
#                     "TheoryP.ref",
#                     "PhatRef",
#                     
#                     "TheoryDiff",
#                     "Diff",
#                     
#                     "CoverPhat",
#                     "CoverPhatRef",
#                     "CoverDiff")
#   
#   data.frame( rs %>% group_by(Method) %>%
#     summarise_at( analysis.vars,
#                   function(x) mean(x, na.rm = TRUE) ) )
# }

########################### WRITE LONG RESULTS  ###########################
if ( run.local == FALSE ) {
  setwd("/home/groups/manishad/MRM/sim_results/long")
  write.csv( rs, paste( "long_results", jobname, ".csv", sep="_" ) )
}