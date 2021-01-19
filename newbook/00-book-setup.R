## get bookdown
# install.packages('bookdown')

## for proper html_dependency (CSS files) need >2.9.0
## or the interactive plots will not render in bookdown
## (note: they should work fine in regular e.g. Rstudio sessions)
# remotes::install_github('r-spatial/mapview')

options(bookdown.clean_book = TRUE)

# remove ../book
bookdown::clean_book()

# create ../book
bookdown::render_book(".")