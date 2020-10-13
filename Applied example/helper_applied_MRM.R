
# code audited 2020-6-17

# recode for ease of analysis
binary_recode = function(x,
                         yes.level){
  
  x2 = rep(NA, length(x))
  x2[ !is.na(x) ] = 0
  x2[ !is.na(x) & x == yes.level ] = 1
  x2
}

########################### FNS: PHAT CI ONLY (FOR 1-CDF PLOTS) ###########################

# return just the CI for Hu example (for making the 1-CDF plot)
phat_ci_hu = function(.dat, .q){
  datNest = .dat %>% group_nest(study)
  
  boot.res = boot( data = datNest, 
                   parallel = "multicore",
                   R = boot.reps, 
                   statistic = function(original, indices) {
                     bNest = original[indices,]
                     b = bNest %>% unnest(data)
                     
                     get_phat_hu(dat = b,
                                 q = .q,
                                 z = z,
                                 z0 = z0,
                                 return.meta = FALSE)[1]
                   } )
  
  # order of stats:
  # Phat, Phat.ref, Phat - Phat.ref
  bootCIs = get_boot_CIs(boot.res, n.ests = 1)[[1]]
  return( data.frame( lo = bootCIs[1], hi = bootCIs[2] ) )
}

# phat_ci_hu(dh, 0)


phat_ci_mathur = function(.dat, .q, .covars){
  datNest = .dat %>% group_nest(authoryear)
  
  # @sourced from sim study script
  boot.res = my_boot( data = datNest, 
                   parallel = "multicore",
                   R = boot.reps, 
                   statistic = function(original, indices) {
                     bNest = original[indices,]
                     b = bNest %>% unnest(data)
                     
                     tryCatch({
                       get_phat_mathur(.dat = b,
                                       .q = .q,
                                       .covars = .covars)[1]
                     }, error = function(err){
                       return(NA)
                     })
                     
                   } )
  
  # boot.res = boot( data = datNest, 
  #                  parallel = "multicore",
  #                  R = boot.reps, 
  #                  statistic = function(original, indices) {
  #                    bNest = original[indices,]
  #                    b = bNest %>% unnest(data)
  #                    
  #                    get_phat_mathur(.dat = b,
  #                                    .q = .q,
  #                                    .covars = .covars)[1]
  #                  } )
  
  # order of stats:
  # Phat, Phat.ref, Phat - Phat.ref
  bootCIs = get_boot_CIs(boot.res, n.ests = 1)[[1]]
  return( data.frame( lo = bootCIs[1], hi = bootCIs[2] ) )
}

# phat_ci_mathur( .dat = dm, .q = log(1.1), .covars = covars[[1]] )



########################### FNS: PHAT FOR META-REGRESSION ###########################

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
              var.eff.size = vi,
              modelweights = "HIER")
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
                           .covars,
                           .q,
                           .return.meta = FALSE) {

  string = paste( c( "logRR", paste(.covars, collapse = " + ") ), collapse = " ~ " )
  
  m = robu( eval( parse(text = string)),
            data = .dat,
            studynum = authoryear,  
            var.eff.size = varlogRR,
            modelweights = "CORR")  # CORR because many studies had shared control groups
  
  t2 = m$mod_info$tau.sq
  
  
  ##### Consider a Hypothetical Study with Optimal Risks of Bias #####
  # design matrix of only the moderators
  Z = as.matrix( .dat %>% select(.covars) )
  head(Z)
  
  # confirm same ordering
  colnames(Z); m
  
  # moderator coefficients
  # exclude intercept
  bhat = as.matrix( m$b.r[ 2:( length(.covars) + 1 ) ], ncol = 1 )
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
  q.shift = .q - ( sum(bhat) )  # sum because levels of interest are always 1
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
          height = height,
          units = "in")
  
  setwd(.overleaf.dir)
  ggsave( name,
          width = width, 
          height = height,
          units = "in")
}


# for reproducible manuscript-writing
# expects global variables "results.dir" and "overleaf.dir"
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
update_result_csv = function( name,
                              section = NA,
                              value = NA,
                              print = FALSE ) {
  setwd(results.dir)
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  if ( "stats_for_paper.csv" %in% list.files() ) {
    .res = read.csv( "stats_for_paper.csv",
                     stringsAsFactors = FALSE,
                     colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% .res$name) ) .res[ .res$name %in% name, ] = new.rows
    else .res = rbind(.res, new.rows)
  }
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    .res = new.rows
  }
  
  write.csv( .res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  # also write to Overleaf
  setwd(overleaf.dir)
  write.csv( .res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  if ( print == TRUE ) {
    View(.res)
  }
}

