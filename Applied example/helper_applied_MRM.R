
# code audited 2020-6-17

# recode for ease of analysis
binary_recode = function(x,
                         yes.level){
  
  x2 = rep(NA, length(x))
  x2[ !is.na(x) ] = 0
  x2[ !is.na(x) & x == yes.level ] = 1
  x2
}



########################### FN: PHAT FOR META-REGRESSION ###########################

get_phat_hu = function(dat,  # dataset
                       q,  # threshold of interest
                       z,  # level of interest for effect modifiers
                       z0,  # reference level of effect modifiers
                       return.meta = FALSE){  # should the meta-regression results be returned? 
  
  # # # TEST ONLY
  # # dat = dh
  # z = c(1, 8)  # SWS, NOT conditioning, 8 hrs of sleep
  # z0 = c(0, 2)  # not SWS, conditioning, 2 hrs of sleep
  
  ##### Fit Meta-Regression #####
  # regress effect size on age at F/U
  mod = robu( yi ~ sws + Sleep.Length,
              data = dat, 
              studynum = study,  
              var.eff.size = vi )
  # coefficient estimates, not including intercept
  bhat = mod$b.r[2:3]
  # estimated residual heterogeneity
  t2 = mod$mod_info$tau.sq
  
  dat$linpredZ = ( bhat[1] * dat$sws ) + ( bhat[2] * dat$Sleep.Length ) 
  
  ##### Phat(z) #####
  # calculate point estimates shifted to "set" effect modifiers to 0
  # i.e., Equation (S.2) in Appendix
  dat$yi.shift = dat$yi - dat$linpredZ  # shifted to have moderators set to 0
  
  # one-stage method
  ens.shift = c(mod$b.r[1]) + sqrt( c(t2) / ( c(t2) + dat$vi) ) * ( dat$yi.shift - c(mod$b.r[1]) )
  
  # for plotting purposes
  # same residual, but adding to the study's own linpred
  ens.unshift = c(mod$b.r[1] + dat$linpredZ) + sqrt( c(t2) / ( c(t2) + dat$vi) ) * ( dat$yi.shift - c(mod$b.r[1]) )
  
  # regular ensemble
  ens.std = MetaUtility::calib_ests(yi = dat$yi,
                                    sei = sqrt(dat$vi) )
  
  # calculate threshold shifted to "set" effect modifiers to 0
  q.shift = q - sum(bhat*z)
  
  # sample proportion of shifted estimates greater than shifted threshold
  Phat = mean( ens.shift > c(q.shift) )
  
  ##### Phat(z0) #####
  # reference level 
  q.shift.ref = q - sum(bhat*z0)
  Phat.ref = mean( ens.shift > c(q.shift.ref) )
  
  ##### Return Results #####
  if ( return.meta == FALSE){
    return( c(Phat, Phat.ref, Phat - Phat.ref) )
  } else {
    return( list( mod,
                  c(Phat, Phat.ref, Phat - Phat.ref),
                  ens.shift,
                  ens.unshift,
                  ens.std,
                  q.shift,
                  q.shift.ref) )
  }
}





get_phat_mathur = function(.dat,
                           .return.meta = FALSE) {
  # BE CAREFUL ABOUT REORDERING OR ADDING VARIABLES HERE - WILL AFFECT BETA'Z BELOW
  
  string = paste( c( "logRR", paste(qual.vars, collapse = " + ") ), collapse = " ~ " )
  
  m = robu( eval( parse(text = string)),
            data = .dat,
            studynum = authoryear,  # ~~~ clustering
            var.eff.size = varlogRR )
  
  t2 = m$mod_info$tau.sq
  
  # linear predictor for being low on all ROB criteria
  # ~~~ report this somewhere?
  exp( sum(m$b.r[2:7]) )
  
  # bm :)
  
  ##### Consider a Hypothetical Study with Optimal Risks of Bias #####
  # design matrix of only the moderators
  Z = as.matrix( .dat %>% select(qual.vars) )
  head(Z)
  
  # confirm same ordering
  colnames(Z); m
  
  # moderator coefficients
  # exclude intercept
  bhat = as.matrix( m$b.r[ 2:( length(qual.vars) + 1 ) ], ncol = 1 )
  int = m$b.r[1]  # intercept
  
  .dat$linpredZ = Z %*% bhat
  
  # linear predictor for each study but without intercept
  exp(.dat$linpredZ)
  
  
  ##### Shift the yis Themselves to Use Existing Package and Sims #####
  .dat$yi.shift = .dat$logRR - .dat$linpredZ  # shifted to have moderators set to 0
  
  # calibrated estimate, shifted to set effect modifiers to 0
  calib.shift = c(int) + sqrt( c(t2) / ( c(t2) + .dat$varlogRR) ) * ( .dat$yi.shift - c(int) )
  
  # threshold, shifted to set effect modifiers to 0
  # would need to be modified for other datasets
  q = log(1.1)
  q.shift = q - ( sum(bhat) )  # sum because levels of interest are always 1
  # note: below assumes we are considering effects ABOVE the threshold
  Phat = mean( calib.shift > c(q.shift) )
  
  ##### Return Results #####
  if ( .return.meta == FALSE){
    return( Phat )
  } else {
    return( list( m,
                  Phat) )
  }

}




########################### FN: PHAT FOR META-REGRESSION ###########################

# get_phat_ritchie = function(dat,  # dataset
#                             q,  # threshold of interest
#                             z,  # level of interest for effect modifiers
#                             z0,  # reference level of effect modifiers
#                             return.meta = FALSE){  # should the meta-regression results be returned? 
#   
#   ##### Fit Meta-Regression #####
#   # regress effect size on age at F/U
#   mod = robu( yi ~ age.fu,
#               data = dat, 
#               studynum = study,  
#               var.eff.size = vi )
#   # coefficient estimate for age at F/U
#   bhat = mod$b.r[2]
#   # estimated residual heterogeneity
#   t2 = mod$mod_info$tau.sq
#   
#   # calculate beta_1'Z
#   dat$linpredZ = c(bhat) * dat$age.fu
#   
#   ##### Phat(z) #####
#   # calculate point estimates shifted to "set" effect modifiers to 0
#   # i.e., Equation (S.2) in Appendix
#   dat$yi.shift = dat$yi - dat$linpredZ  # shifted to have moderators set to 0
#   
#   # # the calib_ests version
#   # ens.shift = MetaUtility::calib_ests(yi = dat$yi.shift,
#   #                                     sei = sqrt(dat$vi) )
#   # the meta-regressive version
#   ens.shift = c(mod$b.r[1]) + sqrt( c(t2) / ( c(t2) + dat$vi) ) * ( dat$yi.shift - c(mod$b.r[1]) )
#   
#   # calculate threshold shifted to "set" effect modifiers to 0
#   q.shift = q - (bhat*z)
#   
#   # sample proportion of shifted estimates greater than shifted threshold
#   Phat = mean( ens.shift > c(q.shift) )
#   
#   ##### Phat(z0) #####
#   # reference level 
#   q.shift.ref = q - (bhat*z0)
#   Phat.ref = mean( ens.shift > c(q.shift.ref) )
#   
#   ##### Return Results #####
#   if ( return.meta == FALSE){
#     return( c(Phat, Phat.ref, Phat - Phat.ref) )
#   } else {
#     return( list(mod,
#                  c(Phat, Phat.ref, Phat - Phat.ref)) )
#   }
# }


############################# FN: GET BOOT CIs FOR A VECTOR OF ESTIMATES #############################

# n.ests: how many parameters were estimated?
get_boot_CIs = function(boot.res, type = "bca", n.ests) {
  bootCIs = lapply( 1:n.ests, function(x) boot.ci(boot.res, type = type, index = x) )
  
  # list with first entry for b and second entry for t2
  # the middle index "4" on the bootCIs accesses the stats vector
  # the final index chooses the CI lower (4) or upper (5) bound
  bootCIs = lapply( 1:n.ests, function(x) c( bootCIs[[x]][[4]][4],
                                             bootCIs[[x]][[4]][5] ) )
}

############################# MISC #############################

my_ggsave = function(name,
                     width,
                     height,
                     .results.dir = results.dir,
                     .overleaf.dir = overleaf.dir) {
  setwd(.results.dir)
  ggsave( name,
          width = width, 
          height = width)
  
  setwd(.overleaf.dir)
  ggsave( name,
          width = width, 
          height = width)
}

