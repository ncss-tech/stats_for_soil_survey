# install.packages('bookdown')

options(bookdown.clean_book = TRUE)

# remove _book
bookdown::clean_book()

# create _book
bookdown::render_book(".")
