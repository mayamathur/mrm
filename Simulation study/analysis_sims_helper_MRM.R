


my_violins = function(xName,
                      yName,
                      hline = NA,
                      xlab,
                      ylab,
                      yTicks = NA,
                      prefix = NA ) {
  
  # # test only
  # xName = "true.effect.dist"
  # yName = "PhatRelBias"
  # hline = 0
  # xlab = "Distribution"
  # ylab = "Relative bias in estimated percentage above q"
  # yTicks = seq(0, 5, 1)
  # # ylab = bquote( hat(P)[q] ~ "relative bias" )
  # # ylab = quote( hat(P)[>q] )
  # # ylab = quote(lambda[>{q}])
  # # quote(lambda[>{x}])
  # # quote(P[50 > {q}])
  
  #browser()
  
  agg$X = as.factor( agg[[xName]] )
  agg$Y = agg[[yName]]
  
  p = ggplot( data = agg,
              aes(x = X,
                  y = Y
                  #fill = as.factor(k)
              ) ) +
    
    geom_violin(fill = "orange", alpha = 0.4) +
    geom_boxplot(width=0.1, fill = "white", outlier.shape = NA) +
    scale_fill_brewer(palette="RdBu") +
    theme_classic() +
    xlab(xlab) +
    ylab(ylab) +
    theme(axis.title.x = element_text(size=16),
          axis.title.y = element_text(size = 14),
           axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14))
  
  if ( !is.na(hline) ) p = p + geom_hline(yintercept = hline,
                                          lty = 2,
                                          color = "gray")
  
  if ( any( !is.na(yTicks) ) ) p = p + scale_y_continuous( breaks = yTicks,
                                                           limits = c( min(yTicks), max(yTicks) ) )
  
 
  # save to local directory and Overleaf
  string = paste( "plot_", xName, "_vs_", yName, ".pdf", sep = "" )
  if ( !is.na(prefix) ) string = paste( prefix, string, sep = "_" )
  my_ggsave( name = string,
             .width = 8,
             .height = 5)
}




my_ggsave = function(name,
                     .width,
                     .height,
                     .results.dir = figures.results.dir,
                     .overleaf.dir = overleaf.dir) {
  
  for ( dir in c(.results.dir, .overleaf.dir) ) {
    setwd(dir)
    ggsave( name,
            width = .width, 
            height = .height,
            device = "pdf" )
  }
}
