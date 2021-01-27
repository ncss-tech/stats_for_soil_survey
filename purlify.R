# recreates R files from specific Rmd files in chapters folder
# combines into a .zip for convenient access later

# need this because utils::zip() doesn't reliably work on Gov Windows 10 machines
library(zip)

# manual selection of files to purl()
Rmd.files <- c("chapters/0_pre-class-assignment/pre-class-assignment.Rmd", 
  "chapters/1_introduction/1_introduction.Rmd", 
  "chapters/2_data/2a_appendix_data_types.Rmd", 
  "chapters/2_data/2a_tabular_data.Rmd", 
  "chapters/2_data/2b_spatial_data.Rmd",
  "chapters/2_data/genhz_homework.Rmd", 
  "chapters/3_sampling/3_sampling.Rmd", 
  "chapters/4_exploratory_analysis/4_exploratory_analysis.Rmd", 
  "chapters/5_clustering_and_ordination/chapter-content.Rmd",
  "chapters/6_linear_models/6_Linear_models.Rmd", 
  "chapters/7_generalized_linear_models/7_generalized_linear_models.Rmd", 
  "chapters/8_Tree_models/treemodels.Rmd", 
  "chapters/9_uncertainty/class-accuracy-uncertainty.Rmd", 
  "chapters/9_uncertainty/Uncert_val.Rmd", 
  "chapters/10_final_project/10_final_project.Rmd", 
  "chapters/10_final_project/final-project-example.Rmd"
)

sapply(Rmd.files, function(ff) {
  
  fname <- gsub(basename(ff), pattern = "\\.Rmd", replacement = "")
  
  #knitr::purl(input=ff, output=paste0(dirname(ff), "/", fname,"_withdoc.R"), documentation = 2)
  knitr::purl(input=ff, output=paste0(dirname(ff), "/", fname,".R"), documentation = 0)
  
})



# combine data/purled-chapters.zip
R.files <- gsub('.Rmd', '.R', Rmd.files, fixed = TRUE)
zip::zip(zipfile = 'data/purled-chapters.zip', files = R.files)


# consider deleting .R files so as not to pollute the repo and create conflicts?
# unlink(R.files, recursive = FALSE, force = FALSE, expand = FALSE)



