library(lattice)


isMineralSoilMaterial <- function(soc, clay, saturation = TRUE) {
  
  # ensure reasonable input
  saturation <- as.logical(saturation)
  if(is.na(saturation)) {
    stop('`saturation must be logical`', call. = FALSE)
  }
  
  # 1. not saturated for 30+ days (cumulative)
  if(! saturation) {
    
    # there is only one test
    test <- soc < 20
    
    res <- data.frame(
      final = test
    )
    
  } else {
    # 2. saturated for 30+ days (cumulative)  
    
    # a. high-clay soil material
    test.a <- (soc < 18) & (clay >= 60)
    
    # b. no-clay soil material
    test.b <- (soc < 12) & (clay < 1)
    
    # c. everything in-between via sliding-scale
    test.c <- (soc < (12 + clay * 0.1)) & (clay < 60)
    
    # OR(a, b, c)
    final <- test.a | test.b | test.c
    
    # save pieces for later inspection
    res <- data.frame(
      clause.a = test.a,
      clause.b = test.b,
      clause.c = test.c,
      final = final
    )
    
  }
  
  return(res)
}



# check rules
d <- expand.grid(
  soc = seq(4, 26, by = 0.2),
  clay = seq(-5, 71, by = 0.2)
)

d$mineral <- factor(isMineralSoilMaterial(d$soc, d$clay)$final)

str(d)


levelplot(
  mineral ~ clay * soc, 
  data = d, 
  asp = 1, 
  xlab = 'Clay Content (%)',
  ylab = 'Soil Organic Carbon (%)',
  main = 'Mineral Soil Material',
  col.regions = c(grey(0.85), 'royalblue'),
  colorkey = FALSE,
  scales = list(alternating = 3, tick.number = 20),
  panel = function(...) {
    panel.levelplot(...)
    panel.abline(h = 12)
    panel.abline(h = 18)
    panel.abline(v = 0)
    panel.abline(v = 60)
  }
)



