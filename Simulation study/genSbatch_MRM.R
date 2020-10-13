

########################### SET SIMULATION PARAMETERS MATRIX ###########################

# FOR CLUSTER USE
path = "/home/groups/manishad/MRM"
setwd(path)
source("helper_MRM.R")

library(cli, lib.loc = "/home/groups/manishad/Rpackages/")
library(fansi, lib.loc = "/home/groups/manishad/Rpackages/")
library(utf8, lib.loc = "/home/groups/manishad/Rpackages/")
library(rlang, lib.loc = "/home/groups/manishad/Rpackages/")
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

# in case packages need to be installed
# install.packages("cli", lib = "/home/groups/manishad/Rpackages/")


# # just one scenario
# # 164 MR  was especially upward-biased for Phat and Diff
# ( scen.params = make_scen_params( method = "boot.whole",  # this doesn't mean anything since we have only one "method"
#                                   calib.method = "MR",  # "MR" for one-stage or "DL" for two-stage
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
#                                   true.effect.dist = c("expo"),
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
                                  
                                  # "active" level of moderators to consider
                                  zc.star = 0.5,  
                                  zb.star = 1,

                                  # reference levels of moderator to consider
                                  zc.ref = -0.5,
                                  zb.ref = 0,
                                  
                                  # #Previous choices:
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

# make sure there's a good range here
summary(scen.params$TheoryP)  # this won't change
summary(scen.params$TheoryP.ref)
summary(scen.params$TheoryDiff)
summary(scen.params$q)

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

# #### DEBUGGING
# true.effect.dist = "normal"
# TheoryP = .5
# b0 = 0
# bc = 1
# bb = 1
# 
# zc = 2
# zb = 1
# zc.ref = 2.5
# zb.ref = 0
# 
# V = .25
# ( q = calculate_q(true.effect.dist,
#             TheoryP, 
#             b0, 
#             bc,
#             bb, 
#             zc,
#             zb,
#             V) )
# 
# # and calculate the TheoryP for the reference level, based on the q chosen above
# ( TheoryP.ref = calculate_theory_p(true.effect.dist = true.effect.dist,
#                                  q = q, 
#                                  b0 = b0, 
#                                  bc = bc, 
#                                  bb = bb, 
#                                  zc = zc.ref,
#                                  zb = zb.ref,
#                                  V = V) )
# 
# TheoryP - TheoryP.ref

##### END DEBUGGING



# # just one
# ( scen.params = make_scen_params( method = "boot.whole",
#                                   k = c(100),
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
#                                   V = c( 0.5^2 ), # residual variance
#                                   muN = NA,  # just a placeholder; to be filled in later
#                                   minN = c(100),
#                                   sd.w = c(1),
#                                   tail = "above",
#                                   true.effect.dist = c("normal"), # # "expo", "normal", "unif2", "t.scaled"
#                                   TheoryP = c(0.05),
#                                   start.at = 1 ) )


( n.scen = nrow(scen.params) )
# look at it
head( as.data.frame(scen.params) )

# write the csv file of params (to Sherlock)
write.csv( scen.params, "scen_params.csv", row.names = FALSE )


########################### GENERATE SBATCHES ###########################

# load functions for generating sbatch files
source("helper_MRM.R")

# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 500  # if you want to generate only 1 file, set this to 10
n.reps.in.doParallel = 500
( n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen )


path = "/home/groups/manishad/MRM"

scen.name = rep( scen.params$scen.name, each = ( n.files / n.scen ) )
jobname = paste("job", 1:n.files, sep="_")
outfile = paste("rm_", 1:n.files, ".out", sep="")
errorfile = paste("rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            jobtime = "3:30:00",
                            quality = "normal",
                            node_number = 1,
                            mem_per_node = 64000,
                            mailtype =  "NONE",
                            user_email = "mmathur@stanford.edu",
                            tasks_per_node = 16,
                            cpus_per_task = 1,
                            path_to_r_script = paste(path, "/doParallel_MRM.R", sep=""),
                            args_to_r_script = paste("--args", jobname, scen.name, sep=" "),
                            write_path,
                            stringsAsFactors = F,
                            server_sbatch_path = NA)

generateSbatch(sbatch_params, runfile_path)

n.files

# 1,440


# max hourly submissions seems to be 300, which is 12 seconds/job
path = "/home/groups/manishad/MRM"
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:1600) {
  #system( paste("sbatch -p owners /home/groups/manishad/MRM/sbatch_files/", i, ".sbatch", sep="") )
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/MRM/sbatch_files/", i, ".sbatch", sep="") )
  #Sys.sleep(2)  # delay in seconds
}




######## If Running Only Some Jobs To Fill Gaps ########

# run in Sherlock ml load R
path = "/home/groups/manishad/MRM"
setwd(path)
source("functions_MRM.R")

missed.nums = sbatch_not_run( "/home/groups/manishad/MRM/sim_results/long",
                              "/home/groups/manishad/MRM/sim_results",
                              .name.prefix = "long_results",
                              .max.sbatch.num = 1440 )



setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/MRM/sbatch_files/", i, ".sbatch", sep="") )
}