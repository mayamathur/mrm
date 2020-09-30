
# Choose scenarios with highest relative bias to include in the bias-correction
#  extension to simulation study.


setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-26")

agg = fread("*agg_dataset_as_analyzed.csv")
max.scen.name = max(agg$scen.name)


# don't need to do both MR and DL
agg = agg %>% filter(calib.method == "MR")

summary(agg$PhatRelBias)
summary(agg$DiffRelBias)

outcomeVars = c("PhatRelBias", "DiffRelBias", "CoverPhat", "CoverDiff")

badScens = c()
for ( i in 1:length(outcomeVars) ){
  outcome = outcomeVars[i]
  
  # 10 scenarios that are worst on outcome
  if ( grepl(pattern = "Bias", x = outcome) ){
    cutoff = rev( sort( agg[[outcome]] ) )[10]
    newBad = agg$scen.name[ agg[[outcome]] >= cutoff ]
  }
  
  if ( grepl(pattern = "Cover", x = outcome) ){
    cutoff = sort( agg[[outcome]] )[10]
    newBad = agg$scen.name[ agg[[outcome]] <= cutoff ]
  }
  cat( "\nCutoff: ", cutoff )
  badScens = c(badScens, newBad)
}

badScens = unique(badScens)
length(badScens)


toRun = agg$scen.name[ agg$scen.name %in% badScens ]


# full set of scenarios
# taken from genSbatch
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

scen.params = scen.params %>% filter( scen.name %in% toRun )

# modify it so they use methods params and bt both correct
# need to double its size so that each method can be used once
scen.params = rbind(scen.params, scen.params, scen.params)

# rename existing scen variable to avoid confusion, but keep in dataset for merging joy
names(scen.params)[  names(scen.params) == "scen.name" ] = "scen.name.in.main"
scen.params$scen.name = (max.scen.name+1) : (max.scen.name+nrow(scen.params))

# first half will use bias correction; second half will use parameters themselves
scen.params$calib.method = c( rep( "MR bt both correct", nrow(scen.params)/3 ),
                              rep( "params", nrow(scen.params)/3 ),
                              # repeat the main method because of regression to mean
                              rep( "MR", nrow(scen.params)/3 ) )

setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-28 bias corrections")
write.csv(scen.params, "scen_params.csv")

# push the new scen params to Sherlock
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/2020/Meta-regression\ metrics\ \(MRM\)/Simulation\ study\ results/2020-9-28\ bias\ corrections/scen_params.csv mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MRM






