
# audited the post-NPPhat parts 2020-6-17


#@are we still going to use these?
truncLogit <- function(p) {
  p[p==0] = 0.001
  p[p==1] = 0.999
  log(p/(1-p))
}

expit = function(x) {
  exp(x) / (1 + exp(x))
}

############################# FN: MAKE SUMMARY TABLE FOR ANALYSIS #############################

my_summarise = function(dat){
  round( dat %>% summarise( n.scens = n(),
                            
                            PhatBias = mean(PhatBias, na.rm = TRUE),
                            Phat2Bias = mean(Phat2Bias, na.rm = TRUE),
                            
                            PhatRelBias = mean(PhatRelBias, na.rm = TRUE),
                            Phat2RelBias = mean(Phat2RelBias, na.rm = TRUE),
                            
                            PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
                            Phat2AbsBias = mean(Phat2AbsBias, na.rm = TRUE),
                            
                            MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
                            MinCoverPhat = min(CoverPhat, na.rm = TRUE),
                            
                            DiffBias = mean(DiffBias, na.rm = TRUE),
                            Diff2Bias = mean(Diff2Bias, na.rm = TRUE),
                            
                            DiffRelBias = mean(DiffRelBias, na.rm = TRUE),
                            Diff2RelBias = mean(Diff2RelBias, na.rm = TRUE),
                            
                            DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
                            Diff2AbsBias = mean(Diff2AbsBias, na.rm = TRUE),
                            
                            MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
                            MinCoverDiff = min(CoverDiff, na.rm = TRUE),
                            
                            # for comparison
                            EstMeanRelBias = mean(EstMeanRelBias, na.rm = TRUE),
                            EstVarRelBias = mean(EstVarRelBias, na.rm = TRUE) ),
         2 )
}

############################# FNS FOR BOOTSTRAPPING #############################

# # list with first entry for b and second entry for t2
# # n.ests: how many parameters were estimated?
# # NOT IN USE ANYMORE
# get_boot_CIs = function(boot.res, type, n.ests) {
#   bootCIs = lapply( 1:n.ests, function(x) boot.ci(boot.res, type = type, index = x) )
#   
#   # list with first entry for b and second entry for t2
#   # the middle index "4" on the bootCIs accesses the stats vector
#   # the final index chooses the CI lower (4) or upper (5) bound
#   bootCIs = lapply( 1:n.ests, function(x) c( bootCIs[[x]][[4]][4],
#                                              bootCIs[[x]][[4]][5] ) )
# }



# draw cluster bootstrap sample
# assumes cluster variable is named "cluster"
# this is Davison & Hinkley's recommendation 
#  see section 3.8 in "Further Topics" chapter
cluster_bt = function(.dat, .clustervar){
  
  .dat$cluster = .dat[[.clustervar]]
  
  # resample clusters, leaving observations intact
  #https://stats.stackexchange.com/questions/46821/bootstrapping-hierarchical-multilevel-data-resampling-clusters
  # see answer by dash2
  cluster.ids = data.frame(cluster = sample(.dat$cluster, replace = TRUE))
  datb = .dat %>% inner_join(cluster.ids, by = 'cluster')
  return(datb)
}


########################### FN: PHAT FOR META-REGRESSION ###########################

# includes the meta-regression part since we want to bootstrap that process
# calib.method: "DL", "MR", "MR bt mn correct", "MR bt var correct", "MR bt both correct", "params" (latter bypasses meta-regressive parameter estimation and calibrates using the true parameters as a benchmark)
prop_stronger_mr = function(dat,
                            zc.star,
                            zb.star,
                            # if also looking at difference:
                            zb.ref = NA,
                            zc.ref = NA,
                            calib.method = "DL",
                            simple.output = FALSE) {  # for ease with boot() fn
  # # test only
  # dat = d
  # zc.star = .5
  # zb.star = 1
  # zb.ref = 0
  # zc.ref = -0.5
  # calib.method = "MR bt mn correct"
  
  yi = dat$yi
  vyi = dat$vyi
  Zc = dat$Zc
  Zb = dat$Zb
  
  
  if ( calib.method != "params" ) {
    # fit meta-regression
    m = robu( yi ~ Zc + Zb, 
              data = dat, 
              studynum = 1:nrow(dat),
              var.eff.size = vyi )
    bhat0 = m$b.r[1]
    bhatc = m$b.r[2]
    bhatb = m$b.r[3]
    t2 = m$mod_info$tau.sq
  }
  
  if ( calib.method == "params" ) {
    bhat0 = p$b0
    bhatc = p$bc
    bhatb = p$bb
    t2 = p$V
  }
  
  # point estimates shifted to have Z = 0
  dat$yi.shift = yi - (bhatc*Zc + bhatb*Zb)  
  
  ##### Two-Stage Calibration Method ("DL") #####
  # use regular meta-analysis on the shifted point estimates
  if ( calib.method == "DL" ){
    ens.shift = calib_ests(yi = dat$yi.shift,
                           sei = sqrt(vyi) )
  }
  
  ##### One-Stage Calibration Method ("MR") #####
  # directly use the meta-regressive estimates in Equation (1)
  if ( calib.method == "MR" ) {
    ens.shift = c(bhat0) + sqrt( c(t2) / ( c(t2) + vyi) ) * ( dat$yi.shift - c(bhat0) )
  }
  
  
  if ( calib.method %in% c("MR bt mn correct", "MR bt var correct", "MR bt both correct" ) ) {
    
    tryCatch({
      boot.res = boot( data = dat, 
                       parallel = "multicore",
                       R = boot.reps, 
                       statistic = function(original, indices) {
                         b = dat[indices,]
                         
                         mb = robu( yi ~ Zc + Zb, 
                                    data = b, 
                                    studynum = 1:nrow(b),
                                    var.eff.size = vyi )
                         bhat0.bt = mb$b.r[1]
                         bhatc.bt = mb$b.r[2]
                         bhatb.bt = mb$b.r[3]
                         t2.bt = mb$mod_info$tau.sq
                         
                         return( c(bhat0.bt, bhatc.bt, bhatb.bt, t2.bt) )
                       } ) # end boot()
      
      bt.means = as.numeric( colMeans( boot.res$t, na.rm = TRUE ) )
      bt.bias = bt.means - c(bhat0, bhatc, bhatb, t2)
      
      # correct the coefficient estimates
      if ( calib.method %in% c("MR bt mn correct", "MR bt both correct") ) {
        bhat0 = bhat0 - bt.means[1]
        bhatc = bhatc - bt.means[2]
        bhatb = bhatb - bt.means[3]
      }
      
      # correct the variance estimate
      if ( calib.method %in% c("MR bt var correct", "MR bt both correct") ) {
        t2 = t2 - bt.means[4]
      }
      
      #browser()
      ens.shift = c(bhat0) + sqrt( c(t2) / ( c(t2) + vyi) ) * ( dat$yi.shift - c(bhat0) )
      
    }, error = function(err){
      # completely skip attempts at estimation that follow
      Phat <<- NA
      Phat.ref <<- NA
      Phat.diff <<- NA
      bhat0 <<- NA
      bhatc <<- NA 
      bhatb <<- NA 
      t2 <<- NA
    })
    
  }  # end of calib.method == "MR bt mn correct"
  
  # ens.shift might not exist if we were trying to bootstrap-correct it but the bootstrapping failed
  if ( exists("ens.shift") ) {
    # q shifted to set moderators to 0
    q.shift = p$q - (bhatc * zc.star) - (bhatb * zb.star) # remove intercept from sum(m$b.r)
    # @NOTE: ASSUMES TAIL = ABOVE
    Phat = mean(ens.shift > q.shift)
    
    ##### Reference Level and Difference #####
    if ( !is.na(zc.ref) & !is.na(zb.ref) ) {
      
      q.shift.ref = p$q - (bhatc * zc.ref) - (bhatb * zb.ref)
      
      Phat.ref = mean(ens.shift > q.shift.ref)
      Phat.diff = Phat - Phat.ref
      
    } else {
      Phat.ref = NA
      Phat.diff = NA
    }
  }
  
  if ( simple.output == TRUE ) return(Phat)
  if ( simple.output == FALSE ) {
    return( data.frame( Phat = Phat,
                        Phat.ref = Phat.ref,
                        Phat.diff = Phat.diff,
                        bhat0 = bhat0,
                        bhatc = bhatc, 
                        bhatb = bhatb, 
                        t2 = t2) )
    
  }
}


########################### FN: SIMULATE 1 STUDY ###########################

# mu = true effect size as raw mean difference
# V = true variance of true effects
# muN = mean sample size in each study
# minN = minimum sample size 
# sd.w = SD within each group (2-group experiment)

# updated 2020-6-7 from NPPhat code
sim_one_study = function( b0, # intercept
                          bc, # effect of continuous moderator
                          bb, # effect of binary moderator
                          V, 
                          muN,
                          minN,
                          sd.w,
                          true.effect.dist = "normal"
) {
  
  # # TEST ONLY
  # b0 = 0.5 # intercept
  # bc = 0.5 # effect of continuous moderator
  # bb = 1 # effect of binary moderator
  # V = .5
  # muN = 100
  # minN = 50
  # sd.w = 1
  # true.effect.dist = "normal"
  
  # simulate total N for each study
  N = round( runif( n = 1, min = minN, max = minN + 2*( muN - minN ) ) ) # draw from uniform centered on muN
  
  # simulate study-level moderators
  Zc = rnorm( n = 1, mean = 0, sd = 1)
  Zb = rbinom( n = 1, size = 1, prob = 0.5)
  
  # mean (i.e., linear predictor) conditional on the moderators
  mu = b0 + bc*Zc + bb*Zb
  # all that follows is that same as in NPPhat
  
  ##### Draw a Single Population True Effect for This Study #####
  if ( true.effect.dist == "normal" ) {
    Mi = rnorm( n=1, mean=mu, sd=sqrt(V) )
  }
  if ( true.effect.dist == "expo" ) {
    # set the rate so the heterogeneity is correct
    Mi = rexp( n = 1, rate = sqrt(1/V) )
    # now the mean is sqrt(V) rather than mu
    # shift to have the correct mean (in expectation)
    Mi = Mi + (mu - sqrt(V))
  }
  if ( true.effect.dist == "unif2") {
    Mi = runif2( n = 1,
                 mu = mu, 
                 V = V)$x
  }
  if (true.effect.dist == "t.scaled") {
    Mi = rt.scaled(n = 1,
                   df = 3,  # fixed for all scenarios to get very heavy tails
                   mean = mu,
                   sd = sqrt(V))
  }
  
  ###### Simulate Data For Individual Subjects ######
  
  # group assignments
  X = c( rep( 0, N/2 ), rep( 1, N/2 ) )
  
  # simulate continuous outcomes
  # 2-group study of raw mean difference with means 0 and Mi in each group
  # and same SD
  Y = c( rnorm( n = N/2, mean = 0, sd = sd.w ),
         rnorm( n = N/2, mean = Mi, sd = sd.w ) )
  
  # calculate ES for this study using metafor (see Viechtbauer "Conducting...", pg 10)
  require(metafor)
  ES = escalc( measure="SMD",   
               n1i = N/2, 
               n2i = N/2,
               m1i = mean( Y[X==1] ),
               m2i = mean( Y[X==0] ),
               sd1i = sd( Y[X==1] ),
               sd2i = sd( Y[X==0] ) ) 
  yi = ES$yi
  vyi = ES$vi
  
  return( data.frame( Mi, mu, Zc, Zb, yi, vyi ) )
}

# with clustering
sim_one_study2 = function(b0, # intercept
                          bc, # effect of continuous moderator
                          bb, # effect of binary moderator
                          V, 
                          Vzeta, # used to calcuate within-cluster variance
                          zeta1,  # scalar cluster random intercept for this study's cluster
                          muN,
                          minN,
                          sd.w,
                          true.effect.dist = "normal"
) {
  
  # # @test for m=1 case
  # # TEST ONLY
  # b0 = 0.5 # intercept
  # bc = 0.5 # effect of continuous moderator
  # bb = 1 # effect of binary moderator
  # V = .5
  # Vzeta = 0.25
  # zeta1 = -0.2
  # muN = 100
  # minN = 50
  # sd.w = 1
  # true.effect.dist = "normal"
  
  ##### Simulate Sample Size and Fixed Design Matrix for This Study #####
  # simulate total N for this study
  N = round( runif( n = 1, min = minN, max = minN + 2*( muN - minN ) ) ) # draw from uniform centered on muN
  
  # simulate study-level moderators
  Zc = rnorm( n = 1, mean = 0, sd = 1)
  Zb = rbinom( n = 1, size = 1, prob = 0.5)
  
  # mean (i.e., linear predictor) conditional on the moderators and cluster membership
  mu = b0 + zeta1 + bc*Zc + bb*Zb
  # all that follows is that same as in NPPhat, except incorporating clustering as in SAPB
  
  ##### Draw a Single Population True Effect for This Study #####
  if ( true.effect.dist == "normal" ) {
    Mi = rnorm( n=1, mean=mu, sd=sqrt(V - Vzeta) )
  }
  if ( true.effect.dist == "expo" ) {
    # within-cluster variance = total - between
    Vwithin = V - Vzeta
    # set the rate so the heterogeneity is correct
    Mi = rexp( n = 1, rate = sqrt(1/Vwithin) )
    # now the mean is sqrt(V) rather than mu
    # shift to have the correct mean (in expectation)
    Mi = Mi + (mu - sqrt(Vwithin))
  }
  
  ###### Simulate Data For Individual Subjects ######
  # group assignments
  X = c( rep( 0, N/2 ), rep( 1, N/2 ) )
  
  # simulate continuous outcomes
  # 2-group study of raw mean difference with means 0 and Mi in each group
  # and same SD
  Y = c( rnorm( n = N/2, mean = 0, sd = sd.w ),
         rnorm( n = N/2, mean = Mi, sd = sd.w ) )
  
  # calculate ES for this study using metafor (see Viechtbauer "Conducting...", pg 10)
  require(metafor)
  ES = escalc( measure="SMD",   
               n1i = N/2, 
               n2i = N/2,
               m1i = mean( Y[X==1] ),
               m2i = mean( Y[X==0] ),
               sd1i = sd( Y[X==1] ),
               sd2i = sd( Y[X==0] ) ) 
  yi = ES$yi
  vyi = ES$vi
  
  return( data.frame( Mi, mu, zeta1, Zc, Zb, yi, vyi ) )
}

# calculate I^2 from t^2 and N
I2 = function(t2, N) {
  t2 / (t2 + 4/N)
}


########################### FN: SIMULATE 1 WHOLE DATASET ###########################
# Vw = within-study variances
# Vwv = variance of within-study variances
# p1 = P(X=1)
# p.int = P(Y=1 | X=0, U=0), i.e., intercept probability for logistic model

# updated 2020-6-5
sim_data = function( k, 
                     b0, # intercept
                     bc, # effect of continuous moderator
                     bb, # effect of binary moderator 
                     V,
                     muN, 
                     minN,
                     sd.w, 
                     true.effect.dist) {
  
  
  # initialize estimated ES to values that will enter the while-loop
  t2 = 0  
  
  # if RE fit isn't apparently causative, or if denominator is going to be undefined, sample again
  # ~~~~~~ NOTE: NEED TO BE CAREFUL CHOOSING PARAMETERS TO AVOID SYSTEMATICALLY
  # REJECTING LOTS OF SAMPLES WITH LOWER HETEROGENEITY
  # ~~~ MAYBE DON'T NEED TO REJECT 
  #while ( t2 == 0 ) {
  #while ( (M <= 0) | (V == 0) ) {   
  yi = c()
  vyi = c()
  Mi = c()
  mu = c()
  Zb = c()
  Zc = c()
  
  
  # simulate k studies
  for (i in 1:k) {
    study = sim_one_study( b0, # intercept
                           bc, # effect of continuous moderator
                           bb, # effect of binary moderator
                           V = V, 
                           muN = muN,
                           minN = minN,
                           sd.w = sd.w,
                           true.effect.dist = true.effect.dist)
    yi = c( yi, study$yi )  # append this study's ES to the list
    vyi = c( vyi, study$vyi )  # append this study's variance to the list
    Mi = c( Mi, study$Mi )  # append this study's mean to the list
    mu = c( mu, study$mu )
    Zc = c( Zc, study$Zc )
    Zb = c( Zb, study$Zb )
  }
  
  
  #   # fit RE model in order to record t2
  #   temp = rma.uni( yi=yi,
  #                   vi=vyi,
  #                   measure="SMD",
  #                   knha = TRUE,
  #                   method = "REML" )
  #   t2 = temp$tau2
  # }
  
  return( data.frame( Mi, mu, Zc, Zb, yi, vyi ) )
}



# with clustering
# updated 2020-6-5
sim_data2 = function( k, # total number of studies
                      m = k, # number of clusters (m=k implies no clustering)
                      b0, # intercept
                      bc, # effect of continuous moderator
                      bb, # effect of binary moderator 
                      V,
                      Vzeta = 0, # between-cluster variance (must be less than V)
                      muN, 
                      minN,
                      sd.w, 
                      true.effect.dist) {
  
  # # @test for m=k case
  # # TEST ONLY
  # k = 43
  # m = 43
  # b0 = 0.5 # intercept
  # bc = 0.5 # effect of continuous moderator
  # bb = 1 # effect of binary moderator
  # V = .5
  # Vzeta = 0.25
  # muN = 100
  # minN = 50
  # sd.w = 1
  # true.effect.dist = "normal"
  
  
  # initialize estimated ES to values that will enter the while-loop
  t2 = 0  
  
  if ( Vzeta > V ) stop( "Vzeta must be less than or equal to V")
  
  # avoid t2 = 0 samples
  # ~~~~~~ NOTE: NEED TO BE CAREFUL CHOOSING PARAMETERS TO AVOID SYSTEMATICALLY
  # REJECTING LOTS OF SAMPLES WITH LOWER HETEROGENEITY
  # ~~~ MAYBE DON'T NEED TO REJECT THESE
  while ( t2 == 0 ) {
    #while ( (M <= 0) | (V == 0) ) {   
    yi = c()
    vyi = c()
    Mi = c()
    mu = c()
    Zb = c()
    Zc = c()
    
    # randomly assign studies to clusters
    # @different from SAPB method of fixing number of studies in each cluster
    # fine if k isn't divisible by m (number of clusters); clusters will just be unbalanced
    cluster = sample( 1:m, size = k, replace = TRUE )
    
    # generate cluster random intercepts (zeta)
    # these are normal even when true effect dist is exponential
    zeta1 = rnorm( n = m, mean = 0, sd = sqrt( Vzeta ) )  # one entry per cluster
    zeta1i = zeta1[cluster]  # one entry per study
    
    # simulate k studies
    for (i in 1:k) {
      study = sim_one_study2( b0, # intercept
                              bc, # effect of continuous moderator
                              bb, # effect of binary moderator
                              V = V, 
                              Vzeta = Vzeta, 
                              zeta1 = zeta1i[k], # cluster random intercept for this study;s cluster
                              muN = muN,
                              minN = minN,
                              sd.w = sd.w,
                              true.effect.dist = true.effect.dist)
      yi = c( yi, study$yi )  # append this study's ES to the list
      vyi = c( vyi, study$vyi )  # append this study's variance to the list
      Mi = c( Mi, study$Mi )  # append this study's mean to the list
      mu = c( mu, study$mu )
      Zc = c( Zc, study$Zc )
      Zb = c( Zb, study$Zb )
      # @ record clusters and anything else along those lines
    }
    
    # fit RE model in order to record t2
    # @ does this need updating for clusters?
    # @replace with robumeta
    temp = rma.uni( yi=yi,
                    vi=vyi,
                    measure="SMD",
                    knha = TRUE,
                    method = "REML" )
    t2 = temp$tau2
  } # end "while(t2==0)"
  
  return( data.frame( Mi, zeta1i, mu, Zc, Zb, yi, vyi ) )
}

# # test
# d = sim_data2(k = 1500,
#               m = 50,
#               b0 = 0.5, # intercept
#               bc = 0.5, # effect of continuous moderator
#               bb = 1, # effect of binary moderator
#               V = .5,
#               Vzeta = 0.25,
#               muN = 100,
#               minN = 50,
#               sd.w = 1,
#               true.effect.dist = "expo")
# var(d$zeta1i) # should be close to Vzeta (need a lot of clusters for this to work)
# var(d$mu)  # should be close to V
# var(d$Mi)  # should be close to V - Vzeta




##### Fn: Check CI coverage #####
covers = function( truth, lo, hi ) {
  return( (lo <= truth) & (hi >= truth) )
}




########################### FN: MAKE SCENARIO PARAMETERS ###########################


# all arguments that are scen parameters can be vectors
#  for use in expand_grid
make_scen_params = function( method, 
                             calib.method,
                             k,
                             b0, # intercept
                             bc, # effect of continuous moderator
                             bb, # effect of binary moderator
                             
                             zc.star,  # level of moderators to consider
                             zb.star,
                             zc.ref,
                             zb.ref,
                             
                             V,  # variance of true effects
                             muN, # just a placeholder; to be filled in later
                             minN,
                             sd.w,
                             tail,
                             true.effect.dist, # "expo" or "normal"
                             TheoryP,
                             
                             # number to start scenario names 
                             start.at = 1) {
  
  # full set of scenarios
  scen.params = expand.grid( method = method,
                             calib.method = calib.method,
                             k = k,
                             b0 = b0, 
                             bc = bc, 
                             bb = bb, 
                             zc.star = zc.star,
                             zb.star = zb.star,
                             zc.ref = zc.ref,
                             zb.ref = zb.ref,
                             V = V,  # variance of true effects
                             muN = muN, # just a placeholder; to be filled in later
                             minN = minN,
                             sd.w = sd.w,
                             tail = tail,
                             true.effect.dist = true.effect.dist, # "expo" or "normal"
                             TheoryP = TheoryP)
  
  # calculate q to have correct TheoryP for the zc.star and zb.star level of moderators
  scen.params = scen.params %>% rowwise %>%
    mutate( q = calculate_q(true.effect.dist = true.effect.dist,
                            TheoryP = TheoryP, 
                            b0 = b0, 
                            bc = bc, 
                            bb = bb, 
                            zc = zc.star,
                            zb = zb.star,
                            V = V),
            
            # and calculate the TheoryP for the reference level, based on the q chosen above
            TheoryP.ref = calculate_theory_p(true.effect.dist = true.effect.dist,
                                             q = q, 
                                             b0 = b0, 
                                             bc = bc, 
                                             bb = bb, 
                                             zc = zc.ref,
                                             zb = zb.ref,
                                             V = V),
            
            TheoryDiff = TheoryP - TheoryP.ref
    )
  
  
  
  # name the scenarios
  scen.params$scen.name = start.at : ( start.at + nrow(scen.params) - 1 )
  
  # avoid doing all factorial combinations of muN and minN this way
  scen.params$muN = scen.params$minN + 50
  
  return(scen.params)
}


########################### FN: QUANTILE CALCULATOR FOR A DESIRED THEORYP ###########################


# return the threshold q that is the TheoryP^th quantile 
#  when moderators are set to zc.star and zb.star
calculate_theory_p = function(true.effect.dist, 
                              q,
                              b0,
                              bc,
                              bb,
                              zc,   
                              zb,
                              V){
  
  # get the mean for this combination of moderators
  mu = b0 + bc*zc + bb*zb
  
  if ( true.effect.dist == "normal" ) {
    return( 1 - pnorm( q = q,
                       mean = mu,
                       sd = sqrt(V) ) )
  }
  
  if ( true.effect.dist == "expo" ) {
    # we generate from a exponential, then shift to achieve the correct mean, 
    #  so q is the threshold BEFORE shifting
    # here is the data generation code from sim_data:
    # Mi = rexp( n = 1, rate = sqrt(1/V) )
    # Mi = Mi + (mu - sqrt(V))
    # is this first line right?
    #stop("Not tested/written yet")
    q0 = q - ( mu - sqrt(V) )
    return( pexp( q = q0,
                  rate = sqrt(1/V),
                  lower.tail = FALSE) )
  }
  
  if ( true.effect.dist == "unif2" ) {
    stop("unif2 method not tested/written yet")
    # return( qunif2( p = 1 - TheoryP, 
    #                 mu = mu, 
    #                 V = V) )
  }
  
  if (true.effect.dist == "t.scaled") {
    stop("t.scaled method not tested/written yet")
    # from metRology package
    # return( qt.scaled(p = 1 - TheoryP,
    #                   df = 3,
    #                   mean = mu,
    #                   sd = sqrt(V) ) )
  }
  
  else stop("true.effect.dist not recognized.")
}

# # bm
# # sanity check: with a single binary covariate
# q = .5
# b0 = 0.5
# bc = 0
# bb = 0.25
# zc = 0
# zb = 1
# V = 0.25
# 
# calculate_theory_p("expo",
#                    q = q,
#                    b0 = b0,
#                    bc = bc,
#                    bb = bb,
#                    zc = zc,
#                    zb = zb,
#                    V = V)
# 
# d = sim_data(k = 1000,
#          b0 = b0,
#          bc = bc,
#          bb = bb,
#          V = V,
#          muN = 51,
#          minN = 50,
#          sd.w = 1,
#          true.effect.dist = "expo")
# d = d %>% filter( Zb == 1 )  # filter to get the right subgroup
# mean(d$Mi > q)
# mean( (d$Mi - (mu - sqrt(V^2)) ) > (q - (mu - sqrt(V^2))) )


# return the threshold q that is the TheoryP^th quantile 
#  when moderators are set to zc.star and zb.star
calculate_q = function(true.effect.dist, 
                       TheoryP,
                       b0,
                       bc,
                       bb,
                       zc,  # chosen value of 
                       zb,
                       V){
  
  # get the mean for this combination of moderators
  mu = b0 + bc*zc + bb*zb
  
  if ( true.effect.dist == "normal" ) {
    return( qnorm( p = 1 - TheoryP,
                   mean = mu,
                   sd = sqrt(V) ) )
  }
  
  if ( true.effect.dist == "expo" ) {
    # we generate from a exponential, then shift to achieve the correct mean, 
    #  so q0 is the threshold BEFORE shifting
    # here is the data generation code from sim_data:
    # Mi = rexp( n = 1, rate = sqrt(1/V) )
    # Mi = Mi + (mu - sqrt(V))
    q0 = qexp( p = TheoryP, 
               rate = sqrt(1/V),
               lower.tail = FALSE)
    return( q0 + (mu - sqrt(V) ) )
  }
  
  if ( true.effect.dist == "unif2" ) {
    return( qunif2( p = 1 - TheoryP, 
                    mu = mu, 
                    V = V) )
  }
  
  if (true.effect.dist == "t.scaled") {
    # from metRology package
    return( qt.scaled(p = 1 - TheoryP,
                      df = 3,
                      mean = mu,
                      sd = sqrt(V) ) )
  }
  
  else stop("true.effect.dist not recognized.")
}
# # sanity check
# ( q = calculate_q( true.effect.dist = "expo",
#              TheoryP = 0.1, 
#              mu = 0.5,
#              V = 0.25^2 ) )
# Mi = rexp( n = 10000, rate = sqrt(1/(.25^2)) )
# Mi = Mi + (0.5 - sqrt(.25^2))
# sum(Mi > q) / length(Mi)



########################### FN: RETURN FILES THAT AREN'T COMPLETED ###########################

# looks at results files to identify sbatches that didn't write a file
# .max.sbatch.num: If not passed, defaults to largest number in actually run jobs.

sbatch_not_run = function(.results.singles.path,
                          .results.write.path,
                          .name.prefix,
                          .max.sbatch.num = NA ) {
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # extract job numbers
  sbatch.nums = as.numeric( unlist( lapply( strsplit( keepers, split = "_"), FUN = function(x) x[5] ) ) )
  
  # check for missed jobs before the max one
  if ( is.na(.max.sbatch.num) ) .max.sbatch.num = max(sbatch.nums)
  all.nums = 1 : .max.sbatch.num
  missed.nums = all.nums[ !all.nums %in% sbatch.nums ]
  
  # give info
  print( paste("The max job number is: ", max(sbatch.nums) ) )
  print( paste( "Number of jobs that weren't run: ",
                ifelse( length(missed.nums) > 0, length(missed.nums), "none" ) ) )
  
  if( length(missed.nums) > 0 ) {
    setwd(.results.write.path)
    write.csv(missed.nums, "missed_job_nums.csv")
  }
  
  return(missed.nums)
  
}

# missed.nums = sbatch_not_run( "/home/groups/manishad/multTest/sim_results/short",
#                 "/home/groups/manishad/multTest/sim_results",
#                 .name.prefix = "short_results" )
# scp mmathur@sherlock:/share/PI/manishad/multTest/sim_results/missed_job_nums.csv ~/Desktop


########################### SLURM FUNCTIONS ###########################

# These just generate the sbatch files

# DO NOT CHANGE THE INDENTATION IN THE BELOW OR ELSE SLURM 
#  WILL SILENTLY IGNORE THE BATCH COMMANDS DUE TO EXTRA WHITESPACE!!
# DO NOT CHANGE THE INDENTATION IN THE BELOW OR ELSE SLURM 
#  WILL SILENTLY IGNORE THE BATCH COMMANDS DUE TO EXTRA WHITESPACE!!
sbatch_skeleton <- function() {
  return(
    "#!/bin/bash
#################
#set a job name  
#SBATCH --job-name=JOBNAME
#################  
#a file for job output, you can check job progress
#SBATCH --output=OUTFILE
#################
# a file for errors from the job
#SBATCH --error=ERRORFILE
#################
#time you think you need; default is one hour
#SBATCH --time=JOBTIME
#################
#quality of service; think of it as job priority
#SBATCH --qos=QUALITY
#################
#submit to both owners and normal partition
#SBATCH -p normal,owners
#################
#number of nodes you are requesting
#SBATCH --nodes=NODENUMBER
#################
#memory per node; default is 4000 MB
#SBATCH --mem=MEMPERNODE
#you could use --mem-per-cpu; they mean what we are calling cores
#################
#get emailed about job BEGIN, END, and FAIL
#SBATCH --mail-type=MAILTYPE
#################
#who to send email to; please change to your email
#SBATCH  --mail-user=USER_EMAIL
#################
#task to run per node; each node has 16 cores
#SBATCH --ntasks=TASKS_PER_NODE
#################
#SBATCH --cpus-per-task=CPUS_PER_TASK
#now run normal batch commands

ml load R
R -f PATH_TO_R_SCRIPT ARGS_TO_R_SCRIPT")
}



generateSbatch <- function(sbatch_params,
                           runfile_path = NA,
                           run_now = F) {
  
  #sbatch_params is a data frame with the following columns
  #jobname: string, specifies name associated with job in SLURM queue
  #outfile: string, specifies the name of the output file generated by job
  #errorfile: string, specifies the name of the error file generated by job
  #jobtime: string in hh:mm:ss format, max (maybe soft) is 48:00:00 
  #specifies the amoung of time job resources should be allocated
  #jobs still running after this amount of time will be aborted
  #quality: kind of like priority, normal works
  #node_number, integer: the number of nodes (computers w/16 cpus each) to allocate 
  #mem_per_node, integer: RAM, in MB, to allocate to each node
  #mailtype, string: ALL, BEGIN, END, FAIL: what types of events should you be notified about via email
  #user_email string: email address: email address to send notifications
  #tasks_per_node: integer, number of tasks, you should probably use 1
  #cpus_per_task: integer, 1-16, number of cpus to use, corresponds to number of available cores per task
  #path_to_r_script: path to r script on sherlock
  #args_to_r_script: arguments to pass to r script on command line
  #write_path: where to write the sbatch file
  #server_sbatch_path: where sbatch files will be stored on sherlock
  #runfile_path is a string containing a path at which to write an R script that can be used to run
  #the batch files generated by this function. 
  #if NA, no runfile will be written
  #run_now is a boolean specifying whether batch files should be run as they are generated
  
  sbatches <- list()
  if (!is.na(runfile_path)) {
    outfile_lines <- c(paste0("# Generated on ",  Sys.time()))
  }
  for (sbatch in 1:nrow(sbatch_params) ) {
    gen_batch <- sbatch_skeleton()
    #set job name
    if (is.null(sbatch_params$jobname[sbatch])) { 
      gen_batch <- gsub("JOBNAME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBNAME", sbatch_params$jobname[sbatch], gen_batch) 
    }
    #set outfile name
    if (is.null(sbatch_params$outfile[sbatch])) { 
      gen_batch <- gsub("OUTFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("OUTFILE", sbatch_params$outfile[sbatch], gen_batch) 
    }
    #set errorfile name
    if (is.null(sbatch_params$errorfile[sbatch])) { 
      gen_batch <- gsub("ERRORFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ERRORFILE", sbatch_params$errorfile[sbatch], gen_batch) 
    }
    #set jobtime
    if (is.null(sbatch_params$jobtime[sbatch])) { 
      gen_batch <- gsub("JOBTIME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBTIME", sbatch_params$jobtime[sbatch], gen_batch) 
    }
    #set quality
    if (is.null(sbatch_params$quality[sbatch])) { 
      gen_batch <- gsub("QUALITY", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("QUALITY", sbatch_params$quality[sbatch], gen_batch) 
    }
    #set number of nodes
    if (is.null(sbatch_params$node_number[sbatch])) { 
      gen_batch <- gsub("NODENUMBER", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("NODENUMBER", sbatch_params$node_number[sbatch], gen_batch) 
    }
    #set memory per node
    if (is.null(sbatch_params$mem_per_node[sbatch])) { 
      gen_batch <- gsub("MEMPERNODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MEMPERNODE", sbatch_params$mem_per_node[sbatch], gen_batch) 
    }
    #set requested mail message types
    if (is.null(sbatch_params$mailtype[sbatch])) { 
      gen_batch <- gsub("MAILTYPE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MAILTYPE", sbatch_params$mailtype[sbatch], gen_batch) 
    }
    #set email at which to receive messages
    if (is.null(sbatch_params$user_email[sbatch])) { 
      gen_batch <- gsub("USER_EMAIL", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("USER_EMAIL", sbatch_params$user_email[sbatch], gen_batch) 
    }
    #set tasks per node
    if (is.null(sbatch_params$tasks_per_node[sbatch])) { 
      gen_batch <- gsub("TASKS_PER_NODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("TASKS_PER_NODE", sbatch_params$tasks_per_node[sbatch], gen_batch) 
    }
    #set cpus per task
    if (is.null(sbatch_params$cpus_per_task[sbatch])) { 
      gen_batch <- gsub("CPUS_PER_TASK", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("CPUS_PER_TASK", sbatch_params$cpus_per_task[sbatch], gen_batch) 
    }
    #set path to r script
    if (is.null(sbatch_params$path_to_r_script[sbatch])) { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", sbatch_params$path_to_r_script[sbatch], gen_batch) 
    }
    #set args to r script
    if (is.null(sbatch_params$args_to_r_script[sbatch])) { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", sbatch_params$args_to_r_script[sbatch], gen_batch) 
    }
    
    #write batch file
    if (is.null(sbatch_params$write_path[sbatch])) { 
      cat(gen_batch, file = paste0("~/sbatch_generated_at_", gsub(" |:|-", "_", Sys.time()) ), append = F)
    } else { 
      cat(gen_batch, file = sbatch_params$write_path[sbatch], append = F)
    }
    
    if (!is.na(sbatch_params$server_sbatch_path[sbatch])) {
      outfile_lines <- c(outfile_lines, paste0("system(\"sbatch ", sbatch_params$server_sbatch_path[sbatch], "\")"))
    } 
    sbatches[[sbatch]] <- gen_batch
  }
  if (!is.na(runfile_path)) {
    cat(paste0(outfile_lines, collapse = "\n"), file = runfile_path)
  }
  if(run_now) { system(paste0("R -f ", runfile_path)) } 
  
  return(sbatches)
}


########################### FN: STITCH RESULTS FILES ###########################

# given a folder path for results and a common beginning of file name of results files
#   written by separate workers in parallel, stitch results files together into a
#   single csv.

stitch_files = function(.results.singles.path, .results.stitched.write.path=.results.singles.path,
                        .name.prefix, .stitch.file.name="stitched_model_fit_results.csv") {
  
  # .results.singles.path = "/home/groups/manishad/MRM/sim_results/long"
  # .results.stitched.write.path = "/home/groups/manishad/MRM/sim_results/overall_stitched"
  # .name.prefix = "long_results"
  # .stitch.file.name="stitched.csv"
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # grab variable names from first file
  names = names( read.csv(keepers[1] )[-1] )
  
  # read in and rbind the keepers
  tables <- lapply( keepers, function(x) read.csv(x, header= TRUE) )
  s <- do.call(rbind, tables)
  
  names(s) = names( read.csv(keepers[1], header= TRUE) )
  
  if( is.na(s[1,1]) ) s = s[-1,]  # delete annoying NA row
  write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )
  return(s)
}



