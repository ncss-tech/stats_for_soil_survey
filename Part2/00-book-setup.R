## get bookdown
# install.packages('bookdown')

options(bookdown.clean_book = TRUE)

# remove ../book
bookdown::clean_book()
unlink("s4ssbook.rds")

# create ../book2
bookdown::render_book(".")


