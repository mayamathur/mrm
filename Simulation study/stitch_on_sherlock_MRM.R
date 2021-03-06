
# to be run by stitch.sbatch

# load command line arguments
args = commandArgs(trailingOnly = TRUE)
start.num = as.numeric( args[1] )  # starting results number to stitch
stop.num = as.numeric( args[2] )  # stopping results number to stitch

path = "/home/groups/manishad/MRM"
setwd(path)
source("helper_MRM.R")

######## STITCH LONG FILES ########

library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
# s = stitch_files(.results.singles.path = "/home/groups/manishad/MRM/sim_results/long",
#                  .results.stitched.write.path = "/home/groups/manishad/MRM/sim_results/overall_stitched",
#                  .name.prefix = "long_results",
#                  .stitch.file.name="stitched.csv")

.results.singles.path = "/home/groups/manishad/MRM/sim_results/long"
.results.stitched.write.path = "/home/groups/manishad/MRM/sim_results/overall_stitched"
.name.prefix = "long_results"
.stitch.file.name="stitched.csv"

# get list of all files in folder
all.files = list.files(.results.singles.path, full.names=TRUE)

# we only want the ones whose name includes .name.prefix
keepers = all.files[ grep( .name.prefix, all.files ) ]

# grab variable names from first file
names = names( read.csv(keepers[1] )[-1] )

# read in and rbind the keepers
tables <- lapply( keepers, function(x) read.csv(x, header= TRUE) )

# # @ temp: get rid of bad ones
# keepTable = unlist( lapply(tables, function(x) "scen.name.in.main" %in% names(x) ) )
# table(keepTable)
# tables = tables[ keepTable == TRUE ]

# @why is it missing the merging variable?
setwd("/home/groups/manishad/MRM")
scen.params = read.csv( "scen_params.csv" )

# keepTable
# FALSE  TRUE 
# 34    16 

s <- do.call(rbind, tables)

names(s) = names( read.csv(keepers[1], header= TRUE) )

if( is.na(s[1,1]) ) s = s[-1,]  # delete annoying NA row
write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )

# are we there yet?
nrow(s) / (1600*500)  # main sims: 1600*500, bias correction sims: 32*3*500
length(unique(s$scen.name))  # main sims: 1600; bias correction sims: 26

##### Look for Missed Jobs #####
# look for missed jobs
missed.nums = sbatch_not_run( "/home/groups/manishad/MRM/sim_results/long",
                              "/home/groups/manishad/MRM/sim_results",
                              .name.prefix = "long",
                              .max.sbatch.num = 1600)

path = "/home/groups/manishad/MRM"

setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/MRM/sbatch_files/", i, ".sbatch", sep="") )
}

##### Move to Desktop #####
# Sherlock -> Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MRM/sim_results/overall_stitched/stitched.csv ~/Desktop




##### Quick Look at Results #####

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


s = s[ ,!is.na(names(s)) ]

s %>% group_by(scen.name) %>%
  mutate(PhatRelBias = mean( abs(Phat - TheoryP[1])/TheoryP[1] ),
         EstVarRelBias = mean( abs(EstVar - V[1])/V[1] ) ) %>%
  group_by(calib.method) %>%
  summarise( n(),
             PhatRelBiasMn = mean(PhatRelBias),
             EstVarRelBiasMn = mean(EstVarRelBias) )
# expect n=16000 for each calib.method since there are 96 total rows (3 each per scenario)

table(is.na(s$PhatLo))
table(is.na(s$DiffLo))

# assumes a single scenario
mean(s$Phat)
table(s$TheoryP)
bias = mean(s$Phat) - s$TheoryP
mean(s$PhatBtMn, na.rm=TRUE); bias  # hope this is equal to the bias


mean(s$Diff)
table(s$TheoryDiff)
bias = mean(s$Diff) - s$TheoryDiff
mean(s$DiffBtMn, na.rm=TRUE); bias  # hope this is equal to the bias





# stitch on Sherlock
# sbatch -p qsu,normal,owners /home/groups/manishad/MRM/stitch_sbatch_files/stitch_4.sbatch
# sacct --jobs=49474291 --format=User,JobID,account,Timelimit,elapsed,ReqMem,MaxRss,ExitCode

# # move it to Desktop
# scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MRM/sim_results/overall_stitched/stitched.csv ~/Desktop
# s.Cyril18
