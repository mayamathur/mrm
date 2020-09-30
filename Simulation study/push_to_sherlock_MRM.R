

####################### CHECK IN ####################### 
# see the sbatches
cd /home/groups/manishad/MRM/sbatch_files

sbatch -p qsu,owners,normal /home/groups/manishad/MRM/sbatch_files/1.sbatch

# check on my running or pending jobs
squeue -u mmathur -t RUNNING
squeue -u mmathur -t PENDING


# see the datasets
vim /home/groups/manishad/MRM/sim_results/long/long_results_job_1_.csv
cd /home/groups/manishad/MRM/sim_results/long
ls -l . | egrep -c '^-'

###### See the Errors #####
vim /home/groups/manishad/MRM/sbatch_files/rm_1507.err

# see the scen parameters
nano /home/groups/manishad/MRM/scen_params.csv

# see the stitched results
nano /home/groups/manishad/MRM/sim_results/overall_stitched/sti*
  
  
  
####################### CODE -> SHERLOCK ####################### 

# push all the individual files
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/2020/Meta-regression\ metrics\ \(MRM\)/Code\ \(git\)/Simulation\ study/* mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MRM

# Sherlock -> Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MRM/sim_results/overall_stitched/stitched.csv ~/Desktop


scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MRM/* ~/Desktop


####################### SHERLOCK -> DESKTOP (DEBUGGING) ####################### 

# move error file to Desktop
scp -r mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MRM/sbatch_files/rm_1250.err ~/Desktop

# move one sbatch file to Desktop
scp -r mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MRM/sbatch_files/2296.sbatch ~/Desktop


####################### RUN SBATCH ####################### 

# run one of them
sbatch -p qsu,normal,owners /home/groups/manishad/MRM/sbatch_files/1.sbatch




####################### RESULTS -> DESKTOP FOR ANALYSIS ####################### 

scp mmathur@login.sherlock.stanford.edu /home/groups/manishad/MRM/results/overall_stitched/stitched.csv ~/Desktop

####################### CLEAN UP ####################### 

# clean up the directory
  rm /home/groups/manishad/MRM/sim_results/long/*
  
  rm /home/groups/manishad/MRM/sim_results/overall_stitched/*
  rm /home/groups/manishad/MRM/sbatch_files/rm*
  rm /home/groups/manishad/MRM/sbatch_files/slurm*
  
  # remove all sbatches
  rm -r /home/groups/manishad/MRM/sbatch_files/*
  