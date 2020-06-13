
# recode for ease of analysis
binary_recode = function(x,
                         yes.level){
  
  x2 = rep(NA, length(x))
  x2[ !is.na(x) ] = 0
  x2[ !is.na(x) & x == yes.level ] = 1
  x2
}


########################### FN: PHAT FOR META-REGRESSION ###########################

get_phat_mathur = function(dat,
                         q){
  

  # BE CAREFUL ABOUT REORDERING OR ADDING VARIABLES HERE - WILL AFFECT BETA'Z BELOW
  mb = robu( logRR ~ #randomized +  # too collinear with exch
               qual.y.prox2 +
               low.miss +
               qual.exch2 +
               qual.gen2 +
               qual.sdb2 +
               qual.prereg2 +
               qual.public.data2,
             data = dat,
             studynum = authoryear,  # ~~~ clustering
             var.eff.size = varlogRR )

  t2b = m$mod_info$tau.sq
  # 
  # ##### Consider a Hypothetical Study with Optimal Risks of Bias #####
  # # design matrix of only the moderators
  Zb = as.matrix( dat %>% select(qual.vars) )

  # moderator coefficients
  # exclude intercept
  bhat.b = as.matrix( mb$b.r[ 2:( length(qual.vars) + 1 ) ], ncol = 1 )

  dat$linpredZ = Zb %*% bhat.b
  
  # ~~~~ FOR COMPARISON ONLY - use the original estimates
  #dat$linpredZ = Zb %*% bhat
  
  
  ##### Try Shifting the yis Themselves to Use Existing Package and Sims #####
  dat$yi.shift = dat$logRR - dat$linpredZ  # shifted to have moderators set to 0
  ens.shift = MetaUtility::calib_ests(yi = dat$yi.shift,
                                      sei = sqrt(dat$varlogRR) )
  
  # sum because all quality vars are coded such that 1 is good
  q.shift = q - ( sum(mb$b.r) - mb$b.r[1] )  # remove the intercept from sum(m$b.r)
  
  # ~~~ FOR COMPARISON ONLY - use original estimate
  
  mean(ens.shift > q.shift)
}


############################# FN: GET BOOT CIs FOR A VECTOR OF ESTIMfATES #############################


# list with first entry for b and second entry for t2
# n.ests: how many parameters were estimated?
get_boot_CIs = function(boot.res, type, n.ests) {
  bootCIs = lapply( 1:n.ests, function(x) boot.ci(boot.res, type = type, index = x) )
  
  # list with first entry for b and second entry for t2
  # the middle index "4" on the bootCIs accesses the stats vector
  # the final index chooses the CI lower (4) or upper (5) bound
  bootCIs = lapply( 1:n.ests, function(x) c( bootCIs[[x]][[4]][4],
                                             bootCIs[[x]][[4]][5] ) )
}

