
# https://github.com/dusadrian/venn

library(venn)


ragg::agg_png(filename = 'logical-example-01.png', width = 1000, height = 1000, scaling = 1.75)

par(mfrow = c(2, 2), xpd = NA, mar = c(0, 0, 0, 0))

# A & B
d <- c('11')
venn(d, snames = 'A, B', box = FALSE, zcolor = cols, sncs = 1.5)
title('A and B', line = -2, cex.main = 2)

# A | B
d <- c('11', '10', '01')
venn(d, snames = 'A, B', box = FALSE, zcolor = cols, sncs = 1.5)
title('A or B', line = -2, cex.main = 2)

# A ! B
d <- c('10')
venn(d, snames = 'A, B', box = FALSE, zcolor = cols, sncs = 1.5)
title('A not B', line = -2, cex.main = 2)

# A xor B
d <- c('10', '01')
venn(d, snames = 'A, B', box = FALSE, zcolor = cols, sncs = 1.5)
title('A xor B', line = -2, cex.main = 2)

dev.off()


ragg::agg_png(filename = 'logical-example-02.png', width = 600, height = 600, scaling = 1.5)

par(mfrow = c(1, 1), xpd = NA, mar = c(0, 0, 0, 0))

# A xor B
d <- c('100', '110', '010')
venn(d, snames = 'A, B, C', box = FALSE, zcolor = cols, sncs = 1.5)
title('(A or B) not C', line = -2, cex.main = 2)

dev.off()










