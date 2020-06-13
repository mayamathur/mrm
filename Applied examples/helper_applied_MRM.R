
# recode for ease of analysis
binary_recode = function(x,
                         yes.level){
  
  x2 = rep(NA, length(x))
  x2[ !is.na(x) ] = 0
  x2[ !is.na(x) & x == yes.level ] = 1
  x2
}


########################### FN: PHAT FOR META-REGRESSION ###########################


get_phat_ritchie = function(dat,
                           q,
                           z,
                           z0,
                           return.meta = FALSE){
  
  
  # BE CAREFUL ABOUT REORDERING OR ADDING VARIABLES HERE - WILL AFFECT BETA'Z BELOW
  mboot = robu( yi ~ age.fu,
                data = dat, 
                studynum = study,  # ~~~ clustering
                var.eff.size = vi )
  
  t2 = mboot$mod_info$tau.sq
  
  mods = c("age.fu")
  n.mods = length(mods)
  
  # linear predictor for being low on all ROB criteria
  # ~~~ report this somewhere?
  sum(mboot$b.r[ 2:(2+n.mods-1)] )
  
  
  ##### Consider a Hypothetical Study with Optimal Risks of Bias #####
  # design matrix of only the moderators
  Z = as.matrix( dat %>% select(mods) )
  head(Z)
  
  # moderator coefficients
  # exclude intercept
  bhat = as.matrix( mboot$b.r[2:(2+n.mods-1)], ncol = 1 )
  
  
  dat$linpredZ = c(bhat) * dat$age.fu
  
  
  ##### Phat(z) #####
  dat$yi.shift = dat$yi - dat$linpredZ  # shifted to have moderators set to 0
  ens.shift = MetaUtility::calib_ests(yi = dat$yi.shift,
                                      sei = sqrt(dat$vi) )
  
  
  # sum because all quality vars are coded such that 1 is good
  q.shift = q - (bhat*z)  # remove the intercept from sum(m$b.r)
  
  Phat = mean( ens.shift > c(q.shift) )
  
  ##### Phat(z0) #####
  # reference level 
  q.shift.ref = q - (bhat*z0)
  Phat.ref = mean( ens.shift > c(q.shift.ref) )
  
  
  if ( return.meta == FALSE){
    return( c(Phat, Phat.ref, Phat - Phat.ref) )
  } else {
    return( list(mboot,
                 c(Phat, Phat.ref, Phat - Phat.ref)) )
  }
  
  
}



########################### FN: PHAT FOR META-REGRESSION ###########################


get_phat_bediou = function(dat,
                           q,
                           z,
                           z0){
  
  
  # BE CAREFUL ABOUT REORDERING OR ADDING VARIABLES HERE - WILL AFFECT BETA'Z BELOW
  mboot = robu( yi ~ Training.duration,
            data = dat, 
            studynum = Study.name,  # ~~~ clustering
            var.eff.size = vi )
  
  t2 = mboot$mod_info$tau.sq
  
  mods = c("Training.duration")
  n.mods = length(mods)
  
  # linear predictor for being low on all ROB criteria
  # ~~~ report this somewhere?
  sum(mboot$b.r[ 2:(2+n.mods-1)] )
  
  
  ##### Consider a Hypothetical Study with Optimal Risks of Bias #####
  # design matrix of only the moderators
  Z = as.matrix( dat %>% select(mods) )
  head(Z)
  
  
  # moderator coefficients
  # exclude intercept
  bhat = as.matrix( mboot$b.r[2:(2+n.mods-1)], ncol = 1 )
  
  
  dat$linpredZ = c(bhat) * dat$Training.duration
  
  
  ##### Phat(z) #####
  dat$yi.shift = dat$yi - dat$linpredZ  # shifted to have moderators set to 0
  ens.shift = MetaUtility::calib_ests(yi = dat$yi.shift,
                                      sei = sqrt(dat$vi) )
  
  
  # sum because all quality vars are coded such that 1 is good
  q.shift = q - (bhat*z)  # remove the intercept from sum(m$b.r)
  
  Phat = mean( ens.shift > c(q.shift) )
  
  ##### Phat(z0) #####
  # reference level 
  q.shift.ref = q - (bhat*z0)
  Phat.ref = mean( ens.shift > c(q.shift.ref) )
  
  return( c(Phat, Phat.ref, Phat - Phat.ref) )
  
}




get_phat_mathur = function(dat,
                         q){
  

  # # BE CAREFUL ABOUT REORDERING OR ADDING VARIABLES HERE - WILL AFFECT BETA'Z BELOW
  # mb = robu( logRR ~ #randomized +  # too collinear with exch
  #              qual.y.prox2 +
  #         
  #              qual.exch2 +
  #              qual.gen2 +
  #              qual.sdb2 +
  #              qual.prereg2 +
  #              qual.public.data2,
  #            data = dat,
  #            studynum = authoryear,  # ~~~ clustering
  #            var.eff.size = varlogRR )
  
  mb = robu( logRR ~ #randomized +  # too collinear with exch
               qual.y.prox2,
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
get_boot_CIs = function(boot.res, type = "bca", n.ests) {
  bootCIs = lapply( 1:n.ests, function(x) boot.ci(boot.res, type = type, index = x) )
  
  # list with first entry for b and second entry for t2
  # the middle index "4" on the bootCIs accesses the stats vector
  # the final index chooses the CI lower (4) or upper (5) bound
  bootCIs = lapply( 1:n.ests, function(x) c( bootCIs[[x]][[4]][4],
                                             bootCIs[[x]][[4]][5] ) )
}

