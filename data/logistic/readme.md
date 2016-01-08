#Covariates
This document provides a short description of the column headings used in the wv_transect_editedforR .csv file. 

|Abbreviation |Measures                                      |Unit                                        |Source                                         |
|:------------|:---------------------------------------------|:-------------------------------------------|:----------------------------------------------|
|FID          |unique ID                                     |unitless                                    |NA                                             |
|X            |latitude                                      |meters                                      |GPS                                            |
|Y            |longitude                                     |meters                                      |GPS                                            |
|NASIS_Pedo   |NASIS Pedon ID                                |text                                        |NA                                             |
|overstory    |list of veg symbols observed in the overstory |text                                        |Field notes                                    |
|undrstory    |list of veg symbols observed in the overstory |text                                        |Field notes                                    |
|Overtype     |conifer, hardwood, or mixed forest            |text                                        |Field notes                                    |
|Underconifer |understory conifer presence (y or n)          |text                                        |Field notes                                    |
|Oi           |Oi horizon depth                              |cm                                          |Field notes                                    |
|Oe           |Oe horizon depth                              |cm                                          |Field notes                                    |
|Oa           |Oa horizon depth                              |cm                                          |Field notes                                    |
|Otot         |total O horizon thickness                     |cm                                          |Field notes                                    |
|epipedon     |epipedon                                      |text                                        |Field notes                                    |
|spodint      |spodic intensity rating                       |index; 0=non-spodic, 2=Spodosol             |Field notes                                    |
|subgroup     |soil taxonomic subgroup                       |text                                        |Field notes                                    |
|order        |soil taxonomic order                          |text                                        |Field notes                                    |
|ps           |soil taxonomic particle size                  |text                                        |Field notes                                    |
|drainage     |soil drainage class                           |text                                        |Field notes                                    |
|series       |soil series                                   |text                                        |Field notes                                    |
|taxon        |series, family, or taxadjunct                 |text                                        |Field notes                                    |
|slope        |slope                                         |percent                                     |Field notes                                    |
|surfacetex   |surface texture                               |text                                        |Field notes                                    |
|stoniness    |surface stoniness class                       |text                                        |Field notes                                    |
|depthclass   |soil depth class                              |text                                        |Field notes                                    |
|bedrockdepth |depth to bedrock                              |cm                                          |Field notes                                    |
|depth_cm     |total depth described in the field            |cm                                          |Field notes                                    |
|aspect       |slope aspect recorded in the field            |degrees                                     |Field notes                                    |
|hillslope    |hillslope profile position                    |text                                        |Field notes                                    |
|slopeshape   |slopeshape                                    |text; v=convex, l=linear, c=concave         |Field notes                                    |
|tipmound     |tip and mound microtopography index           |unitless; 0=none observed, 3=very hummocky) |Field notes                                    |
|musym        |map unit symbol                               |text                                        |SSURGO                                         |
|asym         |area symbol                                   |text                                        |SSURGO                                         |
|rainfall     |mean annual precipitation                     |inches                                      |PRISM 30 year normals 1981-2010                |
|geology      |bedrock geologic formation                    |text                                        |USGS Bedrock Geology 1:250,000                 |
|aachn        |altitude above channel network                |m                                           |Derivative of 10m DEM; calculated in SAGA      |
|dem10m       |elevation                                     |m                                           |10m NED                                        |
|downslpgra   |down slope distance gradient                  |m                                           |Derivative of 10m DEM; calculated in SAGA      |
|eastness     |relative eastness                             |degrees; ranges from -1(west) to 1(east)    |Derivative of 10m DEM and aspect               |
|greenrefl    |landsat geocover                              |                                            |Landsat Geocover                               |
|landsatb1    |landsat band 1                                |                                            |Landsat                                        |
|landsatb2    |landsat band 2                                |                                            |Landsat                                        |
|landsatb3    |landsat band 3                                |                                            |Landsat                                        |
|landsatb7    |landsat band 7                                |                                            |Landsat                                        |
|maxc100      |maximum curvature                             |unitless                                    |Derivative of 10m DEM; calculated in SAGA      |
|maxent       |maxent spruce prediction model                |probability; ranges from 0(low) to 100(high)|WV DNR                                         |
|minc100      |minimum curvature                             |unitless                                    |Derivative of 10m DEM; calculated in SAGA      |
|mirref       |mid infrared reflectance                      |                                            |Landsat Geocover                               |
|ndvi         |normalized difference vegetation index        |unitless; ranges from 0(low) to 1(high)     |Derivative of Landsat near infrared & red bands|
|northeastn   |relative northeastness                        |degrees; ranges from -1(sw) to 1(ne)        |Derivative of 10m DEM and aspect               |
|northness    |relative northness                            |degrees; ranges from -1(south) to 1(north)  |Derivative of 10m DEM and aspect               |
|northwestn   |relative northwestness                        |degrees; ranges from -1(se) to 1(nw)        |Derivative of 10m DEM and aspect               |
|planc100     |plan curvature                                |unitless                                    |Derivative of 10m DEM; calculated in SAGA      |
|proc100      |profile curvature                             |unitless                                    |Derivative of 10m DEM; calculated in SAGA      |
|protection   |morphometric protection index                 |unitless                                    |Derivative of 10m DEM; calculated in SAGA      |
|relpos11     |relative slope position                       |unitless                                    |Derivative of 10m DEM; calculated in SAGA      |
|slp50        |slope height                                  |m                                           |Derivative of 10m DEM                          |
|solar        |potential incoming solar radiation            |kWh/m2                                      |Derivative of 10m DEM                          |
|tanc75       |tangential curvature                          |unitless                                    |Derivative of 10m DEM; calculated in SAGA      |
   