
################### TEST CLUSTERED DATA GENERATION VIA SIM_DATA_2 ################### 

##### Look at a Small Dataset ##### 
d = sim_data2(k = 6,
              m = 2,
              b0 = 0, # intercept
              bc = 0, # effect of continuous moderator
              bb = 0, # effect of binary moderator
              V = .5,
              Vzeta = .25,
              muN = 100,
              minN = 50,
              sd.w = 1,
              # CHECK NORM
              true.effect.dist = "expo")
d



##### Generate a Huge Dataset and Do Sanity Checks #####

d = sim_data2(k = 150,
              m = round(150/4),  # on average 4 studies per cluster
              b0 = 0.1, # intercept
              
              # if bc = bb = 0, mu will be static within clusters
              bc = 0.5, # effect of continuous moderator
              bb = 1, # effect of binary moderator
              
              V = .5,
              Vzeta = .5 * .75,
              muN = 100,
              minN = 50,
              sd.w = 1,
              # CHECK NORM
              true.effect.dist = "expo")

d$icc[1]

d$cluster = as.factor(d$cluster)


# plot a few clusters
# red points: zeta (cluster random intercept)
# blue boxplot: linear predictors within cluster (will only vary within clusters if bc or bb != 0)
# black boxplot: study population effects within cluster
temp = d[ d$cluster == sample( unique(d$cluster), size = 5, replace = FALSE ), ]
library(ggplot2)
ggplot( data = temp,
        aes( x = as.factor(cluster),
             # sanity check: plotting zeta1i here should be constant within clusters
             y = Mi ) ) +
  geom_boxplot( outlier.shape = NA ) +
  
  # study linear predictors within cluster
  # only within-cluster variability in these is due to variability in moderators
  geom_boxplot( aes( x = as.factor(cluster),
                     # sanity check: plotting zeta1i here should be constant within clusters
                     y = mu),
                color = "blue",
                fill = "blue",
                alpha = 0.3,
                width = 0.3,
                outlier.shape = NA) +
  
  # cluster random intercepts
  geom_point( aes( x = as.factor(cluster),
                   y = zeta1),
              size = 3,
              color = "red") +
  
  theme_classic()


length(unique(d$zeta1))  # should equal m
var( d$zeta1[ !duplicated(d$cluster) ] ) # should be close to Vzeta (need a lot of clusters for this to work)
var(d$mu)  # should be close to V

# variance of Mi conditional on clusters
t = d %>% group_by(cluster) %>%
  summarise( var(Mi) )

mean(t$`var(Mi)`) # should be close to V - Vzeta @NOT TRUE??

d$icc[1]


