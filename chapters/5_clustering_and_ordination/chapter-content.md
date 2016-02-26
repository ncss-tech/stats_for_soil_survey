# Numerical Taxonomy
D.E. Beaudette  
March 2016  





<!-- This document is based on `aqp` version 1.9.7 and `sharpshootR` version 0.9.6. -->



# Introduction

Nearly every aspect of soil survey involves the question: "*is A more similar to B or to C?*". The quantification of *similarity* within a collection of horizons, pedons, components, map units, or even landscapes represents an exciting new way to enhance the precision and accuracy of our day to day work. After completing this module you should be able to replicate (some of) our innate ability to organize objects based on measured or observed characteristics of those objects.

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


## A whirlwind tour

### Similarity, disimilarty, and distance

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

A matrix of all pair-wise distances (the **distance matrix**) looks something like this:

|    |    A|  ABt|  Bt1|  Bt2|
|:---|----:|----:|----:|----:|
|A   |  0.0|  <b>7.2</b>| 12.5| 38.9|
|ABt |  <b>7.2</b>|  0.0|  5.4| <b>31.8</b>|
|Bt1 | 12.5|  5.4|  0.0| 26.4|
|Bt2 | 38.9| <b>31.8</b>| 26.4|  0.0|

Note that this is the "full" form of the **distance matrix**, with 0s on the diagonal (e.g. the distance between individual 'A' and itself is 0) and upper and lower "triangles" symmetric. Tpically only the lower triangle is used to describe pair-wise distances.

|    |    A|  ABt|  Bt1|
|:---|----:|----:|----:|
|ABt |  <b>7.2</b>|  |  |
|Bt1 | 12.5|  5.4|  |
|Bt2 | 38.9| <b>31.8</b>| 26.4|

Interpretation is simple: individual "A" is more like "ABt" than "Bt1". It is important to note that quantification of disimilarity (distance) among individuals is always relative: *"X is more like Y, as compared to Z"*. More on this later.


### Standardization of characteristics

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

**Review**

 * What is are the "data matrix" and "distance matrix"?
 * What is standardization and why is it important?
   

### Visualizing pair-wise distances: the dendrogram

[Dendrograms](http://en.wikipedia.org/wiki/Dendrogram) are a convenient way visualizaing [pair-wise distances](http://hymenoptera.tamu.edu/courses/ento601/pdf/Sokal_1966.pdf) among individuals from a distance matrix. Disimilarity between branches is proportional to the level at which branches merge: branching at higher levels (relative to the root of the tree) suggests greater dissimilarity, branching at lower levels suggests greater similarity. Consider the previous example, where distance between individuals was defined in terms of sand and clay percentages:

<img src="chapter-content_files/figure-html/unnamed-chunk-7-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

Interpretation is simple. Euclidean distance in property-space is directly proportional to branching height in the corrosponding dendrogram. Visualizing the geometry of pair-wise distances in > 3 dimensions is difficult, however, a the dendrogram can conveniently summarize a distance matrix created from an arbitrary number of characteristics.

<img src="chapter-content_files/figure-html/unnamed-chunk-8-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

There isn't much difference between these two figures, because most of the characteristics in this example dataset are highly correlated with soil texture. More on this later.


### Cluster analysis: finding groups

<img src="chapter-content_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />


### Ordination: visualization in a reduced space

|name |  clay|  sand|    Mg|    Ca| CEC_7|
|:----|-----:|-----:|-----:|-----:|-----:|
|A    | -0.41|  0.21|  0.06|  0.44| -0.23|
|ABt  |  0.04| -0.07| -0.06| -0.13| -0.38|
|Bt1  |  0.41| -0.21| -0.09| -0.74| -0.16|
| <b>...</b>  |  <b>...</b>| <b>...</b>| <b>...</b>| <b>...</b>| <b>...</b>|

<img src="chapter-content_files/figure-html/unnamed-chunk-10-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

<img src="chapter-content_files/figure-html/unnamed-chunk-11-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />



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
```

### Example data and following along with your own data


## More on the distance matrix and how to make one


```r
# dist()
# daisy()
# vegdist()
# vegemite and related
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

## Partitioning into clusters


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

### MDS with the `vegan` package

### Interpretation




# Practical applications

Review:
 * [SoilProfileCollection object tutorial](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/aqp-intro.html?root=aqp)


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

<img src="chapter-content_files/figure-html/unnamed-chunk-23-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />

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

<img src="chapter-content_files/figure-html/unnamed-chunk-24-1.png" title="" alt="" width="1152" style="display: block; margin: auto;" />

Check resulting distance matrix.

```r
d
```


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
# vizualize dissimilarity matrix via hierarchical clustering
par(mar=c(0,0,3,0))
plotProfileDendrogram(sp4, d, dend.y.scale = max(d), scaling.factor = (1/max(d) * 10), y.offset = 2, width=0.15, cex.names=0.45, color='ex_Ca_to_Mg', col.label='Exchageable Ca to Mg Ratio')
```

<img src="chapter-content_files/figure-html/unnamed-chunk-27-1.png" title="" alt="" width="768" style="display: block; margin: auto;" />



## Soil color
moist or dry?


```r
library(colorspace)

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

<img src="chapter-content_files/figure-html/unnamed-chunk-28-1.png" title="" alt="" width="576" style="display: block; margin: auto;" />


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
par(mfcol=c(1,2), mar=c(0,0,2,0))

# plot fan-style dendrogram
plot(p, font=2, cex=0.5, type='fan', show.tip.label=FALSE, main='Dendrogram Representation')
# add colors at dendrogram tips
tiplabels(pch=16, cex=3, col=rgb(rgb.data))

# plot nMDS ordination
plot(d.sammon$points, type='n', axes=FALSE, xlab='', ylab='', asp=1, main='nMDS Ordination')
abline(h=0, v=0, col='black', lty=3)
points(d.sammon$points, bg=rgb(rgb.data), pch=21, cex=3, col='white')
```

<img src="chapter-content_files/figure-html/unnamed-chunk-29-1.png" title="" alt="" width="1152" style="display: block; margin: auto;" />

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
q <- paste0("SELECT compname, mrulename, AVG(interplr) as interplr_mean
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
Amador                                      0.000                                 1.000                  1.000                                   0.732
Argonaut                                    0.050                                 1.000                  1.000                                   0.996
Cecil                                       0.419                                 0.663                  0.855                                   0.282
Drummer                                     0.000                                 1.000                  1.000                                   1.000
Hanford                                     0.667                                 0.988                  1.000                                   0.212
Klamath                                     0.000                                 1.000                  1.000                                   1.000
Moglia                                      0.000                                 1.000                  0.400                                   1.000
Musick                                      0.167                                 1.000                  1.000                                   0.909
Palau                                       0.011                                 1.000                  0.864                                   1.000
Pardee                                      0.000                                 1.000                  1.000                                   1.000
Pentz                                       0.004                                 1.000                  1.000                                   0.739
Redding                                     0.039                                 1.000                  1.000                                   0.892
Sycamore                                    0.787                                 0.947                  0.759                                   0.875
Willows                                     0.010                                 1.000                  0.894                                   1.000
Yolo                                        0.826                                 0.885                  0.633                                   0.730
Zook                                        0.006                                 1.000                  1.000                                   1.000


```r
# create distance matrix
d <- daisy(x.wide[, -1])

# copy component names
m <- as.matrix(d)
dimnames(m) <- list(x.wide$compname, x.wide$compname)

# back to distance matrix
d <- as.dist(m)
```


```r
par(mar=c(2,0,2,0))
plotProfileDendrogram(s, d, dend.y.scale = 1.5, scaling.factor = 0.004, y.offset = 0.1, width=0.15, cex.names=0.45)
title('Component Similarity via Select Fuzzy Ratings')
mtext('Profile Sketches are from OSDs', 1)
```

<img src="chapter-content_files/figure-html/unnamed-chunk-32-1.png" title="" alt="" width="960" style="display: block; margin: auto;" />



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

<img src="chapter-content_files/figure-html/unnamed-chunk-33-1.png" title="" alt="" width="672" style="display: block; margin: auto;" />


## Species composition


## Component relation graphs
[here](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/component-relation-graph.html?root=aqp)



# References

























