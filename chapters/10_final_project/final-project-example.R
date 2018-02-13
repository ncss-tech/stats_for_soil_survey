#' ---
#' title: "Project Name"
#' author: "Your Name"
#' date: "`r Sys.Date()`"
#' ---
#' 

#' # Introduction
#' A narrative that includes **markdown** formatting.
#' 
#' ## Second Level Heading
#' 
#' ### Third Level Heading
#' 
#' A List of things
#' 
#'    * a
#'    * b
#'    * c

#+ warning=FALSE, message=FALSE
library(aqp)
library(soilDB)

x <- fetchNASIS(from='pedons')

#+ fig.width=8, fig.height=5, warning=FALSE, message=FALSE
par(mar=c(1,1,4,2))
plot(x[1:10, ], label='pedon_id')
title(main='10 Pedons from Local NASIS Selected Set')


plot(x[1:10, ], label='pedon_id', color='clay')


str(x)
