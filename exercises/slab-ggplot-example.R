library(aqp)
library(ggplot2)

data(loafercreek, package = "soilDB")

# investigate parent material kind
table(loafercreek$pmkind)

# subset to two possible pmkind (reducing noise)
x <- subset(loafercreek, pmkind %in% c('residuum', 'colluvium & residuum'))

# slice-wise aggregation over groups | variables
# note "fm" argument
s <- slab(
  x, 
  fm = pmkind ~ clay + phfield + total_frags_pct,
  slab.structure = 0:100,
  slab.fun = function(x) quantile(x, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
)

# include group information to style lines / ribbons
ggplot(s, aes(x = top, y = X50., group = pmkind)) +
  # plot median
  geom_line(aes(color = pmkind), lwd = 1) +
  # plot 10th & 90th quantiles
  geom_ribbon(aes(ymin = X10., ymax = X90., x = top, fill = pmkind, color = pmkind), alpha = 0.2) +
  # invert depths
  xlim(c(100, 0)) +
  # flip axis
  coord_flip() +
  # each panel gets own x-axis scale (different units)
  facet_wrap(~ variable, scales = "free_x") + 
  # better colors
  scale_color_brewer(palette = 'Set1') +
  scale_fill_brewer(palette = 'Set1') + 
  # x / y labels (note these are reversed)
  xlab('Depth (cm)') + 
  ylab('Median bounded by 10th|90th percentile') +
  # less visual clutter
  theme_minimal()


