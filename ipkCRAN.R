# ipkCRAN: a helper fuction for installing required packages from CRAN
# - p: vector of package names
# - up: logical - upgrade installed packages? Default: TRUE
ipkCRAN <- function(p, up = TRUE){
  message('installing packages from CRAN...')
  if (up) {
    # default is to re-install everything
    install.packages(p, dependencies = TRUE)
  } else {
    # but if up != TRUE install just what is needed
    new.pkg <- p[! (p %in% installed.packages()[, "Package"])]
    if (length(new.pkg) > 0) {
      install.packages(new.pkg, dependencies = TRUE)
    }
  }
  # finally, check and see if any failed
  missing.pkg <- p[! (p %in% installed.packages()[, "Package"])]
  if (length(missing.pkg) > 0) { 
    warning(sprintf('\033[31mOne or more packages failed to install!\033[39m\n%s',
                    sprintf("Restart R then try `\033[35minstall.packages(c(%s))\033[39m`",
                            paste0(sprintf('"%s"', missing.pkg), collapse = ","))), call. = FALSE)
  }
}
