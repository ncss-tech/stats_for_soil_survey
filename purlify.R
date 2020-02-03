# knitr::purl-ify

# recreates R files from Rmd files in chapters folder

# f <- list.files(recursive = TRUE)
# 
# # get any .Rmd in chapters directory that is not a presentation or index
# f.sub <- f[grepl(f, pattern = "chapters.*\\.Rmd$") & !grepl(f, pattern="pre[zs]|index")]

# manual selection of files to purlify
f.sub <- c("chapters/0_pre-class-assignment/pre-class-assignment.Rmd", 
  "chapters/1_introduction/1_introduction.Rmd", 
  "chapters/2_data/2a_appendix_data_types.Rmd", 
  "chapters/2_data/2a_tabular_data.Rmd", 
  "chapters/2_data/2b_spatial_data.Rmd",
  "chapters/2_data/genhz_homework.Rmd", 
  "chapters/3_sampling/3_sampling.Rmd", 
  "chapters/4_exploratory_analysis/4_exploratory_analysis.Rmd", 
  "chapters/5_clustering_and_ordination/chapter-content.Rmd",
  "chapters/6_linear_models/6_Linear_models.Rmd", 
  "chapters/6_linear_models/rms-examples.Rmd", 
  "chapters/7_generalized_linear_models/7_generalized_linear_models.Rmd", 
  "chapters/8_Tree_models/treemodels.Rmd", 
  "chapters/9_uncertainty/class-accuracy-uncertainty.Rmd", 
  "chapters/9_uncertainty/Uncert_val.Rmd", 
  "chapters/10_final_project/10_final_project.Rmd", 
  "chapters/10_final_project/final-project-example.Rmd"
)

lapply(as.list(f.sub), function(ff) {
  
  fname <- gsub(basename(ff), pattern = "\\.Rmd", replacement = "")
  
  #knitr::purl(input=ff, output=paste0(dirname(ff), "/", fname,"_withdoc.R"), documentation = 2)
  knitr::purl(input=ff, output=paste0(dirname(ff), "/", fname,".R"), documentation = 0)
  
})
