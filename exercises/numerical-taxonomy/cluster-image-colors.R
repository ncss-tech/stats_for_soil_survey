library(terra)
library(cluster)
library(ape)
library(farver)
library(MASS)

# ## prepare an example image
# library(magick)
# 
# im <- image_read('e:/personal/misc_work_images/a_nice_day_in_valley_springs.jpg')
# im <- image_resize(im, geometry = '400x', filter = 'cubic')
# image_write(im, path = 'photo.png', format = 'PNG')


r <- rast('photo.jpg')

plotRGB(r)
names(r) <- c('r', 'g', 'b')

# copy / update with LAB color coordinates
r.lab <- r
values(r.lab) <- convert_colour(values(r), from = 'rgb', to = 'lab', white_from = 'D65')
names(r.lab) <- c('l', 'a', 'b')

plot(r.lab)

s <- spatSample(r.lab, size = 500, method = 'random', as.points = TRUE)

plotRGB(r)
points(s)


s.lab <- as.data.frame(s)

d <- compare_colour(from = s.lab[, c('l', 'a', 'b')], to = s.lab[, c('l', 'a', 'b')],, from_space = 'lab', method = 'CIE2000')
d <- as.dist(d)

cl <- pam(d, k = 4, diss = TRUE)

s.lab$cl <- factor(cl$clustering)


m <- lda(cl ~ l + a + b, data = s.lab)

predfun <- function(model, data) {
  predict(model, data)$class
}

p <- predict(r.lab, m, fun = predfun)

s.rgb <- convert_colour(s.lab[, c('l', 'a', 'b')], from = 'lab', to = 'rgb', white_from = 'D65')
medoid.cols <- rgb(s.rgb[cl$medoids, ], maxColorValue = 255)

colorspace::swatchplot(medoid.cols)

plot(p, col = medoid.cols)








