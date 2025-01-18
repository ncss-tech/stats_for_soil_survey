library(aqp)

set.seed(456)

d <- list(
  id = "P",
  name = c(c("Oe", "Oa", "A", "Cg1", "Cg2")),
  depths = c(28, 30, 50, 100, 250),
  soil_color = c("10YR 2/2", "10YR 2/1", "7.5YR 2.5/1", "10Y 4/1", "2.5Y 5/1"),
  soc = c(30, 32, 4, 0.8, 0.5),
  bdy = aqp::hzDistinctnessCodeToOffset(c(
    "gradual", "gradual", "clear", "diffuse", "clear"
  ))
)

ohz <- aqp::quickSPC(d, ) |> 
  aqp::perturb(n = 10, boundary.attr = "bdy") |> 
  trunc(0, 200)

ohz$soc <- pmax(0.1, jitter(ohz$soc, abs(log10(ohz$soc) * (ohz$bottom - ohz$top))))
ohz$soc[runif(nrow(ohz)) > 0.97] <- NA

plot(ohz)
plot(ohz, color='soc')

save(ohz, file ="../data/book/02/ohz.rda")
