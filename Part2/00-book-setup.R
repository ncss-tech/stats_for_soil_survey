## get bookdown
# install.packages('bookdown')

## for proper html_dependency (CSS files) need >2.9.0
## or the interactive plots will not render in bookdown
## (note: they should work fine in regular e.g. Rstudio sessions)
# remotes::install_github('r-spatial/mapview')


## leaftlet-related issues:
# https://github.com/rstudio/leaflet/issues/732
# remotes::install_github('rstudio/leaflet')

## leafem too?
# remotes::install_github('r-spatial/leafem')


## TODO: 
# * R code to setup local copies of large files required by chapters
# * integrate Yosemite MAST modeling code + appendix
# * convert citation in LM, GLM, Tree models chapters to bibtex



options(bookdown.clean_book = TRUE)

# remove ../book
bookdown::clean_book()
file.remove("s4ssbook.Rmd")

# create ../book2
bookdown::render_book(".")


