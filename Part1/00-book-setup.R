## get bookdown
# install.packages('bookdown')

options(bookdown.clean_book = TRUE)

# remove ../book
bookdown::clean_book()
file.remove("s4ssbook.Rmd")

# create ../book
bookdown::render_book(".")
