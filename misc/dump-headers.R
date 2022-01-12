# ---
#   title: "searchable index to on-line course materials"
# author: "Andrew G. Brown"
# date: "2/10/2021"
# output: html_document
# ---
#   
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```
# 
# help in coming up with the key words 
# and which course or presentation these key words could be found
# 
# For each presentation you have delivered
# send a list of key words from the training 
# course/presentation/module/resource would provide training/information on that key word. 



f <- list.files(path = "newbook", pattern = "[0-9]+-.*\\.Rmd",
                full.names = TRUE)

knitr::opts_chunk$set(list(echo = FALSE, eval = FALSE))
lapply(f, knitr::knit)
lapply(gsub("Rmd","md",basename(f)), function(x) {
  l <- readLines(x)
  idx <- grep("^###+", l)
  return(l[idx])
})
