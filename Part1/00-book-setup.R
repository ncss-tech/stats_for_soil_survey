## get bookdown and book development dependencies
if (!requireNamespace("rmarkdown")) {
  install.packages("rmarkdown")
}

if (!requireNamespace("knitr")) {
  install.packages("knitr")
}

if (!requireNamespace("bookdown")) {
  install.packages("bookdown")
}

if (!requireNamespace("pak")) {
  install.packages("pak")
  pak::pak_install_extra()
}

options(bookdown.clean_book = TRUE)

# remove ../book
bookdown::clean_book()
file.remove("s4ssbook.Rmd")

# create ../book
bookdown::render_book(".")
