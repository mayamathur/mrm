
library(robumeta)
library(metafor)
data(hierdat)

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


get_phat = function(dat,
                            q,
                            z,
                            z0,
                            return.meta = FALSE){
  
  mod = robu( effectsize ~ age,
                data = dat, 
                studynum = studyid,  # ~~~ clustering
                var.eff.size = var )
  bhat = mod$b.r[2]  
  t2 = mod$mod_info$tau.sq

  dat$linpredZ = c(bhat) * dat$age
  
  ##### Phat(z) #####
  dat$yi.shift = dat$effectsize - dat$linpredZ  # shifted to have moderators set to 0
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
    return( list(mod,
                 c(Phat, Phat.ref, Phat - Phat.ref)) )
  }
  
  
}



z = 50
z0 = 10
( stats = get_phat(dat = dr,
                           q = q,
                           z = z,
                           z0 = z0,
                           return.meta = TRUE) )

# look at meta-regression coefficients
stats[[1]]

# and Phats
stats[[2]]


boot.res = boot( data = dr, 
                 parallel = "multicore",
                 R = boot.reps, 
                 statistic = function(original, indices) {
                   b = original[indices,]
                   
                   get_phat_ritchie(dat = b,
                                    q = q,
                                    z = z,
                                    z0 = z0,
                                    return.meta = FALSE)
                   
                 } )

( bootCIs = get_boot_CIs(boot.res, n.ests = 3) )