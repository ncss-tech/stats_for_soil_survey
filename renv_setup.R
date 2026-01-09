# setup for a clean R environment for building book
renv::init(
  bare = TRUE,
  bioconductor = TRUE
)

# install pak
install.packages("pak")

# get `packages` from index.Rmd
packages <- ...

pak::pkg_install(packages)

# get book building dependencies
install.packages(c("bookdown", "rmarkdown", "knitr"))

