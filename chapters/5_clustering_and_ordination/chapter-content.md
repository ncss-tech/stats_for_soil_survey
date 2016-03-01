# Numerical Taxonomy
D.E. Beaudette  
March 2016  





<!-- This document is based on `aqp` version 1.9.7 and `sharpshootR` version 0.9.6. -->

TODO:
 
 * [problems assoc. with too many dimensions](https://en.wikipedia.org/wiki/Clustering_high-dimensional_data)
 * links to unsupervised classification
 * spatial clustering with fanny
 * think: distance metric, characteristics, standardization, clustering algorithm, number of classes...
 * fuzzy clustering of hz attr -> plot on profile
 * missing data: bain of numerical taxonomy... strategies
 * color clustering of entire profiles
 * profile_compare() details
 * add discussion points to each section
 * add questions
 * adapt to use 'pedons' through entire document for simple use of student's data
 * mean silhouette width as cluster number selection



# Introduction

Nearly every aspect of soil survey involves the question: "*is A more similar to B or to C?*". The quantification of *similarity* within a collection of horizons, pedons, components, map units, or even landscapes represents an exciting new way to enhance the precision and accuracy of our day to day work. After completing this module you should be able to *quantitatively* organize objects based on measured or observed characteristics of those objects in a consistent and repeatable manner. Perhaps you will find a solution to the long-standing "similar / dissimilar" question.

![alt.text](static-figures/soils-by-pigments.png)


## Objectives
* Learn essential vocabulary used in the field of numerical taxonomy, review some of the literature
* Gain experience with R functions and packages commonly used for clustering / ordination
* Learn how to create and interpret a distance matrix, and appropriate distance metrics
* Learn how to create and interpret a dendrogram
* Lean the basics and application of heirarchical clustering methods
* Lean the basics and application of partitioning clustering methods
* Learn the basics and application of ordination methods
* Apply skils to a range of soil, vegetation, and similar data sources
* Apply techniques from numerical taxonomy to addressing the "similar/disimilar" question


# A whirlwind tour

## Similarity, disimilarty, and distance

There are shelves of books and many thousands of academic articles describing the theory and applications of "clustering" and "ordination" methods. This body of knowledge is commonly described as the field of numerical taxonomy [@Sneath1973]. Central to this field is the quantification of *similarity* among "individuals" based on a relevant set of "characteristics". Individuals are typically described as rows of data with a single characteristic per column. For example:


Table: A matrix of data: soil horizons (individuals) and associated characteristics.

 name    clay    sand     Mg     Ca     CEC_7 
------  ------  ------  ------  -----  -------
  A       21      46     25.7    9.0    23.0  
 ABt      27      42     23.7    5.6    21.4  
 Bt1      32      40     23.2    1.9    23.7  
 Bt2      55      27     44.3    0.3    43.0  

Quantitative measures of similarity are more conveniently expressed (why?) as distance, or dissimilarity. In the simplest case, dissimilarity can be computed as the shortest distance between individuals in property-space. Another name for the shortest linear distance between points is the [**Euclidean distance**](https://en.wikipedia.org/wiki/Euclidean_distance). Evaluated in two dimensions, between individuals $p$ and $q$ the Euclidean distance is calculated:

$$D(p,q) = \sqrt{(p_{1} - q_{1})^{2} + (p_{2} - q_{2})^{2}}$$

where $p_{1}$ is the 1st characteristic (or dimension) of individual $p$. There are many other ways to define "distance" (e.g. *distance metrics*), but we will cover those later.

Using sand and clay percentages from the data above, dissimilarity is represented as the length of the line connecting any two individuals in property space:

<img src="chapter-content_files/figure-html/unnamed-chunk-2-1.png" title="" alt="" width="480" style="display: block; margin: auto;" />

A matrix of all pair-wise distances (the [**distance matrix**](https://en.wikipedia.org/wiki/Distance_matrix)) looks something like this:

|    |    A|  ABt|  Bt1|  Bt2|
|:---|----:|----:|----:|----:|
|A   |  0.0|  <b>7.2</b>| 12.5| 38.9|
|ABt |  <b>7.2</b>|  0.0|  5.4| <b>31.8</b>|
|Bt1 | 12.5|  5.4|  0.0| 26.4|
|Bt2 | 38.9| <b>31.8</b>| 26.4|  0.0|

Note that this is the "full" form of the [**distance matrix**](https://en.wikipedia.org/wiki/Distance_matrix), with 0s on the diagonal (e.g. the distance between individual 'A' and itself is 0) and upper and lower "triangles" symmetric. The lower triangle is commonly used by most algorithms to encode pair-wise distances.

|    |    A|  ABt|  Bt1|
|:---|----:|----:|----:|
|ABt |  <b>7.2</b>|  |  |
|Bt1 | 12.5|  5.4|  |
|Bt2 | 38.9| <b>31.8</b>| 26.4|

Interpretation is simple: individual "A" is more like "ABt" than "Bt1". It is important to note that quantification of disimilarity (distance) among individuals is always relative: *"X is more like Y, as compared to Z"*. More on this later.


## Standardization of characteristics

Euclidean distance doesn't make much sense when characteristics do not share a common unit of measure, range of values, or when some characteristics are categorical vs. continuous. For example, distances are distorted when computed from clay (%) and exchangeable Ca (cmol/kg):

<img src="chapter-content_files/figure-html/unnamed-chunk-3-1.png" title="" alt="" width="480" style="display: block; margin: auto;" />

In this example, exchangeable Ca contributes less to the distance between individuals than clay content, effectively down-weighting the importance of Ex-Ca. Typically, characteristics are given equal weight (why?).

**Standardization** of the data matrix solves the problem of unequal ranges or units of measure, typically by subtraction of the mean and division by standard deviation:

$$x_{scaled} = \frac{x - mean(x)}{sd(x)}$$

There are many other **standardization** methods which we will cover later. The new data matrix looks like this:


name     clay    sand      Mg      Ca   CEC_7
-----  ------  ------  ------  ------  ------
A       -0.86    0.88   -0.35    1.23   -0.47
ABt     -0.45    0.40   -0.55    0.36   -0.63
Bt1     -0.12    0.15   -0.60   -0.59   -0.40
Bt2      1.43   -1.43    1.49   -1.00    1.49

Using the standardized data matrix, distances computed in the property space of clay and exchangeable calcium are unbiased by the unique central tendency or spread of each character.

<img src="chapter-content_files/figure-html/unnamed-chunk-5-1.png" title="" alt="" width="480" style="display: block; margin: auto;" />

It is rare that the question of "dissimilarity" can be answered with only two characteristcs (dimensions). Euclidean distance can be extended to an arbitrary number of $n$ dimensions:

$$D(p,q) = \sqrt{ \sum_{i=1}^{n}{(p_{i} - q_{i})^{2}} }$$

where $i$ is one of $n$ total characteristics. It is hard to imagine what distance "looks like" when there are > 3 dimensions, so lets look at the distance matrix calculated using all five characteristics.


          A    ABt    Bt1    Bt2
----  -----  -----  -----  -----
A      0.00   1.10   2.11   4.77
ABt    1.10   0.00   1.06   4.17
Bt1    2.11   1.06   0.00   3.61
Bt2    4.77   4.17   3.61   0.00

We can now begin to describe disimilarity between individuals using an arbitrary number of (relevant!) characteristics and make statements like "The A horizon is roughly 2x more similar to the ABt horizon than it is to the Bt horizon". While this may be a trivial example, the utility of generalizing these methods to soil survey operations should be obvious.

### Review and discuss

 * What is are the "data matrix" and "distance matrix"?
 * What is standardization and why is it important?
 * What do you think is the largest impediment to creating a distance matrix from our NASIS and KSSL data?
 * **Key point:** each characteristic is its own *dimension* in property-space:
     + {sand, clay, CEC} = 3 dimensions
     + {sand, clay, CEC, OC, horzion depth} = 5 dimensions
     + ... simple to define in code, but hard to vizualize
   

## Visualizing pair-wise distances: the dendrogram

[Dendrograms](http://en.wikipedia.org/wiki/Dendrogram) are a convenient way visualizaing [pair-wise distances](http://hymenoptera.tamu.edu/courses/ento601/pdf/Sokal_1966.pdf) among individuals from a distance matrix. Disimilarity between branches is proportional to the level at which branches merge: branching at higher levels (relative to the root of the tree) suggests greater dissimilarity, branching at lower levels suggests greater similarity. Consider the previous example, where distance between individuals was defined in terms of sand and clay percentages:

<img src="chapter-content_files/figure-html/unnamed-chunk-7-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

Interpretation is simple. Euclidean distance in property-space is directly proportional to branching height in the corrosponding dendrogram. Visualizing the geometry of pair-wise distances in > 3 dimensions is difficult, however, a the dendrogram can conveniently summarize a distance matrix created from an arbitrary number of characteristics. It is important to note that some information about pair-wise distances is lost in the dendrogram in the form of distortion: distortion is greatest near the terminal "leaves" of the dendrogram. This phenomena is analogous to the distortion generated by a map projection--it is impossible to flatten a higher-dimensional entity to a lower-dimensional form without causing distortion.

<img src="chapter-content_files/figure-html/unnamed-chunk-8-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

There isn't much difference between these two figures, because most of the characteristics in this example dataset are highly correlated with soil texture. There are some more important details on how individuals are connected into larger and larger groups within the dendrogram, more on this later.


### Review and discuss
 * Any questions about dendrogram interpretation? They will be used extensively in this chapter.
 * If you were explaining how to interpret a dendrogram to someone, where would you start: roots or leaves? Why?


## Cluster analysis: finding groups in data
[**Cluster analysis**](https://en.wikipedia.org/wiki/Cluster_analysis) is a massive topic that deals with the seemingly simple task of finding "useful" groups within a dataset. This topic and the methods used are also refered to as "unsupervised classification" in remote sensing and GIS circles. All of the available algorithms will find groups in a give dataset, however, it is up to the subject expert to determine:
 
 1. suitable characteristics and standardization method
 2. appropriate clustering algorithm
 3. criteria used to determine the "right" number of clusters
 4. interpretation of the final grouping based on subject knowledge
 5. &rarr; *possibly starting over at step 1...*

### Using color to communicate the results of a clustering or ordination exercise
Note that the widespread use of color in the following examples are not aestetic. Clors are convenient for tri-variate data-spaces because our eyes can automatically integrate the information into a self-consistent set of classes.

### Hierarchical clustering

<img src="chapter-content_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

#### Methods

There are two main types of [**hierarchical clustering**](https://en.wikipedia.org/wiki/Hierarchical_clustering):

 * **agglomerative**: start with individuals and iteratively combine into larger and larger groups
 * **divisive**: start with all individuals and iteratively split into smaller and smaller goups

Both methods are strongly influenced by choice of **standardization** method and **distance metric**. Both methods require a full, pair-wise distance matrix as input. This can limit [**hierarchical clustering**](https://en.wikipedia.org/wiki/Hierarchical_clustering) to datasets that can be fit into memory.

The agglomerative methods also depends on the choice of a [**linkage criterion**](https://en.wikipedia.org/wiki/Hierarchical_clustering#Linkage_criteria). Some of these criteria include:

 * single linkage
 * complete linkage
 * Ward's method
 * weighted average linkage
 * flexible linkage

More on these criteria later.

##### Review and discuss
 * The simplicity and lack of decisions make the **divisive** method convenient for most work. 
 * The top-down approach is similar to the way in which we describe soil morphology and taxonomy.
 * Method selection: think about it, don't go "fishing".



### Partitioning or centroid/medoid clustering

<img src="chapter-content_files/figure-html/unnamed-chunk-10-1.png" title="" alt="" width="576" style="display: block; margin: auto;" />

#### Methods



##### Review and discuss


## Ordination: visualization in a reduced space

|name |  clay|  sand|    Mg|    Ca| CEC_7|
|:----|-----:|-----:|-----:|-----:|-----:|
|A    | -0.41|  0.21|  0.06|  0.44| -0.23|
|ABt  |  0.04| -0.07| -0.06| -0.13| -0.38|
|Bt1  |  0.41| -0.21| -0.09| -0.74| -0.16|
| <b>...</b>  |  <b>...</b>| <b>...</b>| <b>...</b>| <b>...</b>| <b>...</b>|

<img src="chapter-content_files/figure-html/unnamed-chunk-11-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

<img src="chapter-content_files/figure-html/unnamed-chunk-12-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />



# Learn by doing

## Setup the R session
Install R packages as needed. Open a new R script file to use as you follow along.

```r
# load libraries
library(aqp)
library(soilDB)
library(sharpshootR)
library(cluster)
library(ape)
library(RColorBrewer)
library(vegan)
library(MASS)
library(colorspace)
```

### Data sources

Most of the examples used in this module come from the following sources:

1. built-in data sets from the `aqp` and `soilDB` packages ("sp4" and "loafercreek")
2. results from [`fetchNASIS()`](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/fetchNASIS-mini-tutorial.html?root=aqp): pedon data from the local NASIS selected set
3. results from [`fetchKSSL()`](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/KSSL-demo.html?root=aqp): lab characterization data from the SoilWeb snapshot
4. results from [`fetchOSD()`](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/OSD-dendrogram.html?root=aqp): basic morphologic and taxonmic data from the SoilWeb snapshot 
5. results from [`SDA_query()`](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/SDA-tutorial.html?root=aqp): *live* SSURGO spatial and tabular data from [Soil Data Access](http://sdmdataaccess.nrcs.usda.gov/)
6. data from SSR 2, as CSV, downloaded from class [GitHub site](https://github.com/ncss-tech/stats_for_soil_survey/tree/master/data/clustering_and_ordination)


In most cases, you can edit the examples and swap-in just about any data that are or have been upgraded to a [`SoilProfileCollection` object](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/aqp-intro.html?root=aqp). For example, pedons from **your** local NASIS selected set can be loaded with `fetchNASIS()`.




#### Your turn
Tinker with some `SoilProfileCollection` objects:

 * Get some data using one of the methods listed above, if you need help see the manual pages for examples (`?fetchKSSL`) or the [SoilProfileCollection tutorial](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/aqp-intro.html?root=aqp).
 * Determine the number of profiles and horizons within the collection.
 * View and extract some *site* and *horizon* attributes.
 * Generate some soil profile sketches.


## More on the distance matrix and how to make one

### Options

* `dist()` (base R), simple and fast, limited number of distance metrics
* `daisy()` ([cluster package](https://cran.r-project.org/web/packages/cluster/index.html)), better selection of distance metrics, simple standardization, my go-to function
* `vegdist` ([vegan package](https://cran.r-project.org/web/packages/vegan/index.html)), many distance metrics, primarily designed for species composition data




```r
# get some example data from the aqp package
data('sp4', package = 'aqp')
# subset select rows and columns
sp4 <- sp4[1:4, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]

# compare distance functions
round(dist(sp4[, -1], method = 'euclidean'))
```

```
##    1  2  3
## 2  8      
## 3 15  7   
## 4 48 44 39
```

```r
round(daisy(sp4[, -1], stand = TRUE, metric = 'euclidean'), 2)
```

```
## Dissimilarities :
##      1    2    3
## 2 1.45          
## 3 2.73 1.36     
## 4 6.45 5.65 4.91
## 
## Metric :  euclidean 
## Number of objects : 4
```

```r
round(vegdist(sp4[, -1], method = 'gower'), 2)
```

```
##      1    2    3
## 2 0.19          
## 3 0.32 0.16     
## 4 0.96 0.84 0.69
```

### Distance calculations with categorical data


```r
# SoilTaxonomyDendrogram()
# component.adj.matrix()
```


## Hierachrical clustering

### Agglomerative methods


```r
# hclust()
# agnes()
```

### Divisive methods


```r
# dianna()
```

### Advanced plotting functions


```r
# as.phylo()
# other ways to plot dendrogram
```

## Partitioning or centroid clustering


```r
# 1D example
# 2D example
```

### Hard classes


```r
# pam()
# clara()
```

### Fuzzy classes


```r
# fanny()
```

### How many clusters?


```r
# silhouette()
```



## Interpretation / application


```r
# Hmisc::
# varclus()
# naclus()
```


## Ordination (Non-metric multidimensional scaling)

### Sammon's non-linear mapping

### nMDS with the `vegan` package

### Interpretation and goodness of fit

[Shephard diagram](http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/goodness.metaMDS.html)

```r
# MASS library
d.shep <- Shepard(d, d.sammon$points)
plot(d.shep, pch='.')
lines(d.shep$x, d.shep$yf, type = 'S', col='blue', lwd=2)

## vegan library
# compute distance between ES
d <- metaMDSdist(m[, -1])

# try ordination: non-metrix multidimensional scaling with default distance metric and standardization
# use all columns except first: the ES ID
nmds <- metaMDS(m[, -1])

# evaluate nmMDS fit
stressplot(nmds)

# GOF by site, smaller = better
hist(goodness(nmds))
```



# Practical applications

Review:
 * [SoilProfileCollection object tutorial](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/aqp-intro.html?root=aqp)


## Pair-wise distances between soil profiles 

[relevant paper](http://dx.doi.org/10.1016/j.cageo.2012.10.020)

[relevant slides](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/presentations/AQP-num_soil_classification.pdf?root=aqp)


```r
# init example data
data(sp4)
depths(sp4) <- id ~ top + bottom

# eval dissimilarity:
# using Ex-Ca:Mg and CEC at pH 7
# with no depth-weighting (k=0)
# to a maximum depth of 40 cm
d <- profile_compare(sp4, vars=c('ex_Ca_to_Mg', 'CEC_7'), k=0, max_d=40)

# check distance matrix:
round(d, 1)
```

```
## Dissimilarities :
##                colusa glenn kings mariposa mendocino napa san benito shasta shasta-trinity
## glenn            13.5                                                                     
## kings            16.0  12.7                                                               
## mariposa          8.4  11.3  16.5                                                         
## mendocino        11.5   8.0  16.4     15.0                                                
## napa             30.4  24.1  29.4     29.2      21.6                                      
## san benito       25.7  20.6  26.3     28.2      15.8 18.0                                 
## shasta           17.2  13.3   8.7     17.6      17.1 33.7       22.2                      
## shasta-trinity    6.4  16.6  22.3      9.6      16.5 29.8       27.2   23.3               
## tehama           28.7  22.9  27.9     27.3      20.0  8.8       15.1   31.4           27.9
## 
## Metric :  mixed ;  Types = I, I 
## Number of objects : 10
```

```r
# cluster via divisive method
clust <- diana(d)
```


```r
# vizualize dissimilarity matrix via hierarchical clustering
par(mar=c(0,0,3,0))
plotProfileDendrogram(sp4, clust, dend.y.scale = max(d), scaling.factor = (1/max(d) * 10), y.offset = 2, width=0.15, cex.names=0.45, color='ex_Ca_to_Mg', col.label='Exchageable Ca to Mg Ratio')
```

<img src="chapter-content_files/figure-html/unnamed-chunk-26-1.png" title="" alt="" width="768" style="display: block; margin: auto;" />



## Pair-wise distances between subgroup level taxa


```r
# define a vector of series
s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'drummer', 'musick', 'zook', 'argonaut', 'PALAU')

# get and SPC object with basic data on these series
s <- fetchOSD(s.list)

# graphical check
par(mar=c(0,0,2,0))
plot(s) ; title('Selected Pedons from Official Series Descriptions', line=0)
```

<img src="chapter-content_files/figure-html/unnamed-chunk-27-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

```r
# check structure of some site-level attributes
head(site(s))[, c('id', 'soilorder', 'suborder', 'greatgroup', 'subgroup')]
```



id         soilorder     suborder   greatgroup      subgroup            
---------  ------------  ---------  --------------  --------------------
AMADOR     inceptisols   xerepts    haploxerepts    typic haploxerepts  
ARGONAUT   alfisols      xeralfs    haploxeralfs    mollic haploxeralfs 
CECIL      ultisols      udults     kanhapludults   typic kanhapludults 
DRUMMER    mollisols     aquolls    endoaquolls     typic endoaquolls   
HANFORD    entisols      orthents   xerorthents     typic xerorthents   
KLAMATH    mollisols     aquolls    cryaquolls      cumulic cryaquolls  


```r
par(mar=c(0,1,1,1))
# plot dendrogram + profiles
d <- SoilTaxonomyDendrogram(s, scaling.factor = 0.01)
```

<img src="chapter-content_files/figure-html/unnamed-chunk-28-1.png" title="" alt="" width="1152" style="display: block; margin: auto;" />

Check resulting distance matrix.

```r
d
```




## Soil color
moist colors


```r
# extract horizon data from select OSDs in above example
h <- horizons(s)

# convert Munsell color notation to RGB
rgb.data <- munsell2rgb(h$hue, h$value, h$chroma, return_triplets = TRUE)

# check
head(rgb.data)
```

         r           g           b
----------  ----------  ----------
 0.4360624   0.3706674   0.2969745
 0.5589675   0.4673350   0.3566388
 0.5589675   0.4673350   0.3566388
 0.7719679   0.6774631   0.4899754
 0.3940324   0.2499977   0.1668267
 0.4309729   0.2327690   0.0977103

```r
# remove NA
rgb.data <- na.omit(rgb.data)

# retain unique colors
rgb.data <- unique(rgb.data)

# convert RGB colors to CIE LAB color system
lab.data <- as(with(rgb.data, RGB(r, g, b)), 'LAB')

# visualize colors in LAB coordinates
pairs(lab.data@coords, col='white', bg=rgb(rgb.data), pch=21, cex=2)
```

<img src="chapter-content_files/figure-html/unnamed-chunk-30-1.png" title="" alt="" width="576" style="display: block; margin: auto;" />


```r
# create distance matrix from LAB coordinates
d <- daisy(lab.data@coords, stand = TRUE)

# divisive heirarcical clustering
d.hclust <- as.hclust(diana(d))

# convert to phylo class for nicer plotting
p <- as.phylo(d.hclust)

# perform nMDS on distance matrix
d.sammon <- sammon(d)

# setup multi-figure page
par(mfcol=c(1,2), mar=c(0,0,2,0), bg=grey(0.95))

# plot fan-style dendrogram
plot(p, font=2, cex=0.5, type='fan', show.tip.label=FALSE, main='Dendrogram Representation')
# add colors at dendrogram tips
tiplabels(pch=21, cex=4, col='white', bg=rgb(rgb.data))

# plot nMDS ordination
plot(d.sammon$points, type='n', axes=FALSE, xlab='', ylab='', asp=1, main='nMDS Ordination')
abline(h=0, v=0, col='black', lty=3)
points(d.sammon$points, bg=rgb(rgb.data), pch=21, cex=3.5, col='white')
```

<img src="chapter-content_files/figure-html/unnamed-chunk-31-1.png" title="" alt="" width="1152" style="display: block; margin: auto;" />

## Component interpretations
[here](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/SDA-cointerp-tutorial.html?root=aqp)





```r
library(reshape2)
# set list of component names, same as soil color example
s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 
            'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'drummer', 
            'musick', 'zook', 'argonaut', 'PALAU')

# set list of relevant interpretations
interp.list <- c('ENG - Construction Materials; Topsoil', 
'ENG - Sewage Lagoons', 'ENG - Septic Tank Absorption Fields', 
'ENG - Unpaved Local Roads and Streets')

# compose query
q <- paste0("SELECT UPPER(compname) as compname, mrulename, AVG(interplr) as interplr_mean
FROM component INNER JOIN cointerp ON component.cokey = cointerp.cokey
WHERE compname IN ", format_SQL_in_statement(s.list), "
AND seqnum = 0
AND mrulename IN ", format_SQL_in_statement(interp.list), "
AND interplr IS NOT NULL
GROUP BY compname, mrulename;")

# send query
x <- SDA_query(q)

# reshape long -> wide
x.wide <- dcast(x, compname ~ mrulename, value.var = 'interplr_mean')
knitr::kable(x.wide, digits = 3, caption="Mean Fuzzy Ratings for Select Soil Series")
```



Table: Mean Fuzzy Ratings for Select Soil Series

compname    ENG - Construction Materials; Topsoil   ENG - Septic Tank Absorption Fields   ENG - Sewage Lagoons   ENG - Unpaved Local Roads and Streets
---------  --------------------------------------  ------------------------------------  ---------------------  --------------------------------------
AMADOR                                      0.000                                 1.000                  1.000                                   0.732
ARGONAUT                                    0.050                                 1.000                  1.000                                   0.996
CECIL                                       0.419                                 0.663                  0.855                                   0.282
DRUMMER                                     0.000                                 1.000                  1.000                                   1.000
HANFORD                                     0.667                                 0.988                  1.000                                   0.212
KLAMATH                                     0.000                                 1.000                  1.000                                   1.000
MOGLIA                                      0.000                                 1.000                  0.400                                   1.000
MUSICK                                      0.167                                 1.000                  1.000                                   0.909
PALAU                                       0.011                                 1.000                  0.864                                   1.000
PARDEE                                      0.000                                 1.000                  1.000                                   1.000
PENTZ                                       0.004                                 1.000                  1.000                                   0.739
REDDING                                     0.039                                 1.000                  1.000                                   0.892
SYCAMORE                                    0.787                                 0.947                  0.759                                   0.875
WILLOWS                                     0.010                                 1.000                  0.894                                   1.000
YOLO                                        0.826                                 0.885                  0.633                                   0.730
ZOOK                                        0.006                                 1.000                  1.000                                   1.000


```r
# note: component name and series name have been converted to upper case
# sort rows of fuzzy ratings based on profiles from OSDs
new.order <- match(x.wide$compname, profile_id(s))
x.wide <- x.wide[new.order, ]

# copy ids to row.names so that they are preserved in distance matrix
row.names(x.wide) <- x.wide$compname

# create distance matrix
d <- daisy(x.wide[, -1])

# divisive hierarchical clustering
clust <- diana(d)
```


```r
par(mar=c(2,0,2,0))
plotProfileDendrogram(s, clust, dend.y.scale = 1.5, scaling.factor = 0.004, y.offset = 0.1, width=0.15, cex.names=0.45)
title('Component Similarity via Select Fuzzy Ratings')
mtext('Profile Sketches are from OSDs', 1)
```

<img src="chapter-content_files/figure-html/unnamed-chunk-34-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />



## Diagnostic features
[here](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/diagnostic-property-plot.html?root=aqp)


```r
# load some example NASIS data
data(loafercreek, package='soilDB')

# cut-down to a subset
loafercreek <- loafercreek[1:20, ]

# get depth class
sdc <- getSoilDepthClass(loafercreek)
site(loafercreek) <- sdc

# diagnostic properties to consider, no need to convert to factors
v <- c('lithic.contact', 'paralithic.contact', 'argillic.horizon', 
       'cambic.horizon', 'ochric.epipedon', 'mollic.epipedon', 'very.shallow',
       'shallow', 'mod.deep', 'deep', 'very.deep')


x <- diagnosticPropertyPlot(loafercreek, v, k=5, grid.label='bedrock_kind', dend.label = 'taxonname')
```

<img src="chapter-content_files/figure-html/unnamed-chunk-35-1.png" title="" alt="" width="672" style="display: block; margin: auto;" />


## GIS Data

TODO: give credit to SEKI crew


```r
library(plyr)
library(reshape2)
library(vegan)
library(cluster)

# init a temp file
tf <- tempfile()
# download compressed CSV to temp file
setInternet2(TRUE)
download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/clustering_and_ordination/seki-mu-gis-samples.csv.gz', destfile = tf, quiet = TRUE)
# read-in from compressed CSV to data.frame object
x <- read.csv(gzfile(tf), stringsAsFactors = FALSE)

# check
head(x)

# get terrain variable names
vars <- c('elev', 'solar', 'slope', 'tci', 'ppt', 'maat', 'tpi', 'tri', 'pi')

x.long <- melt(x, id.vars = 'MUSYM', measure.vars = vars)

x.summary <- ddply(x.long, c('MUSYM', 'variable'), .fun=plyr::summarize, m=median(value, na.rm = TRUE))

head(x.summary)

x.wide <- dcast(x.summary, MUSYM ~ variable, value.var = 'm')
row.names(x.wide) <- x.wide$MUSYM

head(x.wide, 2)

# generate lookup table of MUSYM - MU groups
lut <- unique(x[, c('MUSYM', 'gensym')])

# join data matrix with lut
x.wide <- join(x.wide, lut, by='MUSYM', type='left', match='first')
x.wide$gensym <- factor(x.wide$gensym, levels = c("riparian", "xx10", "xx20", "xx30", "xx40", "xx50"), labels = c("riparian", "basins and cirques", "glacial valleys", "bedrock controlled mountain slopes and valley walls", "non bedrock controlled slopes", "plateaus, till plains, outwash plains"))

head(x.wide, 2)

# eval signatures from LHS samples
nmds <- metaMDS(x.wide[, vars], distance = 'gower', autotransform = FALSE, wascores=FALSE)

cols <- c('RoyalBlue', 'black', 'DarkGreen', 'orange3', 'red', 'brown')
site.colors <- cols[as.numeric(x.wide$gensym)]

# plot
par(mar=c(1,1,1,1))
# clusters connected with orange lines
fig <- ordiplot(nmds, type='none', cex.axis=0.75, axes=FALSE)
abline(h=0, v=0, lty=2, col='grey')
text(fig, "sites", cex=0.65, font=2, col=site.colors)
legend('bottomleft', legend=levels(x.wide$gensym), col=cols, pch=15, pt.cex=2, bty='n', cex=1)
title('Map Unit Terrain Signatures')
```

<img src="chapter-content_files/figure-html/unnamed-chunk-36-1.png" title="" alt="" width="768" style="display: block; margin: auto;" />


## Species composition

TODO: give credit to CA630 ESD crew


```r
# library(reshape2)
# library(vegan)

# read species data from online CSV
# init a temp file
tf <- tempfile()
setInternet2(TRUE)
download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/clustering_and_ordination/ca630-22A-species.csv', destfile = tf, quiet = TRUE)

x <- read.csv(tf, stringsAsFactors = FALSE)

# check: long-format
head(x)
```



site_id          ecosite  ecostat   stratum   plantsym    cover
--------------  --------  --------  --------  ---------  ------
07CA630DWB006       2224  4F        SM        CEMO2         0.1
07CA630DWB006       2224  4F        SL        ERCA6         0.1
07CA630DWB006       2224  4F        TS        QUKE          0.1
07CA630DWB006       2224  4F        TR        PISA2         0.1
07CA630DWB006       2224  4F        TR        PIPO          0.1
07CA630DWB006       2224  4F        F         HIAL2         0.1

```r
# keep only tree species
idx <- which(grepl('^T', x$stratum))
x <- x[idx, ]

# add stratum as a suffix for tree species (^T*)
x$symbol <- ifelse(grepl('^T', x$stratum), paste0(x$plantsym, '/', x$stratum), x$plantsym)

# convert "long" format to full community matrix, "wide" format using the mean pct cover
# 1 row / ES ID, each column is a species
m <- dcast(x, site_id ~ symbol, value.var = 'cover', fun.aggregate = mean)

# assign abbreviated user site ID to row names
row.names(m) <- gsub('CA630', '', m$site_id) 

# convert to matrix, excluding the first column (site id)
m <- as.matrix(m[, -1])

# replace NA in the community matrix with 0
m[is.na(m)] <- 0

# check: looks good, except missing data (NA) should be 0
head(m, 1)[, 1:20]
```

```
##   ABCO/TM   ABCO/TR   ABCO/TS   ABCO/TT  ACMA3/TM  ACMA3/TR  ACMA3/TS   AECA/TR   AECA/TS  ALNUS/TR 
##         0         0         0         0         0         0         0         0         0         0 
##   ARMA/TR   ARME/TR   ARME/TS CADE27/TM CADE27/TR CADE27/TS CADE27/TT  CONU4/TR  CONU4/TS  CORNU/TR 
##         0         0         0         0         0         0         0         0         0         0
```

```r
# compute distance between ES
d <- metaMDSdist(m)
```

```
## Square root transformation
## Wisconsin double standardization
## Using step-across dissimilarities:
## Too long or NA distances: 260 out of 4095 (6.3%)
## Stepping across 4095 dissimilarities...
## Connectivity of distance matrix with threshold dissimilarity 1 
## Data are connected
```

```r
# try ordination: non-metrix multidimensional scaling with default distance metric and standardization
nmds <- metaMDS(m)
```

```
## Square root transformation
## Wisconsin double standardization
## Run 0 stress 0.2027479 
## Run 1 stress 0.2162792 
## Run 2 stress 0.2279006 
## Run 3 stress 0.2320883 
## Run 4 stress 0.2052938 
## Run 5 stress 0.2032479 
## Run 6 stress 0.2107288 
## Run 7 stress 0.2052294 
## Run 8 stress 0.2191159 
## Run 9 stress 0.2038073 
## Run 10 stress 0.2220374 
## Run 11 stress 0.225853 
## Run 12 stress 0.205042 
## Run 13 stress 0.2189405 
## Run 14 stress 0.2158423 
## Run 15 stress 0.2050603 
## Run 16 stress 0.2114478 
## Run 17 stress 0.2031161 
## ... procrustes: rmse 0.01376868  max resid 0.07253014 
## Run 18 stress 0.2321677 
## Run 19 stress 0.2066686 
## Run 20 stress 0.2118672
```


```r
# evaluate nmMDS fit
par(mar=c(5,5,1,1))
stressplot(nmds, cex=0.5)
```

<img src="chapter-content_files/figure-html/unnamed-chunk-38-1.png" title="" alt="" width="480" style="display: block; margin: auto;" />


```r
# plot
par(mar=c(1,1,1,1), mfcol=c(1,2))

# "species"
fig <- ordiplot(nmds, type='none', axes=FALSE)
abline(h=0, v=0, lty=2, col='grey')
text(fig, "species", col="blue", cex=0.5)
title('Species', line=-0.5)

# "sites"
# re-use the ordination object 'fig'
ordiplot(fig, type='none', axes=FALSE)
abline(h=0, v=0, lty=2, col='grey')
text(fig, "sites", col="black", cex=0.5)
title('Sites', line=-0.5)
```

<img src="chapter-content_files/figure-html/unnamed-chunk-39-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />



----------------------------
This document is based on `aqp` version 1.9.7 and `soilDB` version 1.7 and `sharpshootR` version 0.9.6.
----------------------------


# References

























