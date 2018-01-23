




Working with tabular data in R
========================================================
transition: none
width: 1024
height: 800
css: custom.css


Jay Skovlin, Dylan Beaudette, Stephen Roecker

<br><br><br><br><br><br><br><br>
<span style="color: white; font-size:50%;">This document is based on `aqp` version 1.15 and `soilDB` version 2.0`.</span>


Objectives
========================================================

- Learn more about R and how to inspect objects and data types
- Use the soilDB package to load NASIS pedon data into R
- Learn about the checks run on data loaded by the fetchNASIS() function
- Understand the structure of data stored in a Soil Profile Collection (SPC)
- Learn ways to filter and subset SPC data in R
- Learn how functions can be used to bundle operations
- Review additional data that is accessible via extended data functions


The SoilDB Package
========================================================

What if you could extract, organize, and visualize data from NASIS and many other commonly used soil database sources with a couple of lines of code? 

Relevant Data Sources
========================================================

![](2a_tabular_data-figure/soilDB_figure.png)


Convenience 'fetch' Functions in the SoilDB Package
========================================================

- **fetchNASIS(from='pedons')**
    - Gets NASIS pedon/horizon data from a local NASIS database.
    
- **fetchNASIS(from='components')**
    - Gets selected NASIS map unit and component daa from a local NASIS database.
      + [NASIS Component Data](http://ncss-tech.github.io/AQP/soilDB/NASIS-component-data.html)
    

Convenience 'fetch' Functions for lab data sources
========================================================

- **fetchNASISLabData()**
    - Gets KSSL laboratory pedon/horizon layer data from a local NASIS database.
- **fetchKSSL()**
    - Gets KSSL data from the SoilWeb system via BBOX, MLRA, or series name query.
        + [KSSL Data Demo](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html)
        + [Water Retention Curve Development from KSSL Data](http://ncss-tech.github.io/AQP/soilDB/fetchKSSL-VG-demo.html)
- **fetchRaCA()**
    - Gets Rapid Carbon Assessment (RaCA) data by State, geographic bounding-box, RaCA site ID, or series query from the SoilWeb system.
        + [RaCA Data Demo](http://ncss-tech.github.io/AQP/soilDB/RaCA-demo.html)  
        
 
Convenience 'fetch' Functions for other data sources
========================================================       
- **SDA_query**
    - Submits queries to the Soil Data Access system.
        + [Soil Data Access Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html)
        
- **fetchOSD()**
    - Fetches a limited subset of horizon- and site-level attributes for named soil series from the SoilWeb system.
    
- **fetchPedonPC()**
    - Fetches commonly used site and horizon data from a PedonPC v.5 database.
    
Convenience 'fetch' Functions for soil climate-related sources
======================================================== 

- **fetchSCAN()**
    - Queries soil and climate data from USDA-NRCS SCAN Stations.
      + [A Unified Interface to SCAN/SNOTEL Data](http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.html)
      
- **fetchHenry()**
    - Downloads data from the Henry Mount Soil Climate Database.
        + [Henry Mount Soil Climate Database Tutorial](http://ncss-tech.github.io/AQP/soilDB/Henry-demo.html)


Why do all of this?  
========================================================

<img src="2a_tabular_data-figure/pedons_a-1.png" title="plot of chunk pedons_a" alt="plot of chunk pedons_a" style="display: block; margin: auto;" />

Importance of Pedon Data
========================================================

- Pedon data is very important for future work
- We've got a lot of data to work with and likely much more to bring online
- Archiving quality observations of soils made in the past, present, and future is difficult work
and we will need many different tools to help us tackle the job

Common Issuse with Pedon Data
========================================================
- Digging and making observations of soil is difficult!
- Our confidence in the observations often weakens with the depth of material described.
- How can we address this?
  + Use a cutoff depth, for example 100 cm, can be used to truncate observations to a zone of greater confidence.
  + Show the relative confidence of the data with depth.

SoilDB Package - How does it work and what does it do?
========================================================
- extracts data from a NASIS selected set via Structured Query Language (SQL)
- Basic data checks are run within the fetch functions
- data are assembled into a combined site-level and horizon-level data structure within a custom R object called a `Soil Profile Collection (SPC)`.
- The [`SoilProfileCollection`](http://ncss-tech.github.io/AQP/aqp/aqp-intro.html) class simplifies the process of working with collections of data associated with soil profiles, e.g., site-level data, horizon-level data, spatial data, diagnostic horizon data, metadata, etc. 

========================================================
<img src="2a_tabular_data-figure/structure_diagram_a-1.png" title="plot of chunk structure_diagram_a" alt="plot of chunk structure_diagram_a" style="display: block; margin: auto;" />

Limitations
========================================================
- `fetchNASIS()` is not comprehensive
- It does not pull all of the data for every table related to pedon data from NASIS
- Queries the most commonly used pedon and horizon data.  
- The nested complexity of the NASIS data structure is simplified and flattened in the resulting SPC object.  
    + Higher level functions like `fetchNASIS()` bundle a series of lower level functions that get specific parts of the data structure. 


Data Checks Run by the fetchNASIS() Function
========================================================

- **Inconsistent horizon boundaries**. Pedons with inconsistent horizon boundaries are not loaded.  
    + the commonly occurs when a bottom depth of a horizon does not match the next upper depth of the horizon below it.


|hzname | top|bot |
|:------|---:|:---|
|A      |   0|30  |
|Bt1    |  38|56  |
|Bt2    |  56|121 |
|Bk     | 121|135 |
|R      | 135|    |
Note the issue above. The bottom depth of the A horizon and the upper depth of the Bt1 horizon should be the same: either 30 or 38 cm.

========================================================

- **Missing lower horizon depths.** Offending horizons are fixed by replacing the missing bottom depth with the top depth plus 2 cm. In the case of the profile shown above, a bottom depth of 137 cm would be inserted where the depth is missing.


|hzname | top| bot|
|:------|---:|---:|
|A      |   0|  30|
|Bt1    |  38|  56|
|Bt2    |  56| 121|
|Bk     | 121| 135|
|R      | 135| 137|

- **Presence of multiple map datums**. Results reported to the user and the data are not modified.

- **Sites missing pedon records**. Data without corresponding horizons are not loaded.


Find errors and fix them in NASIS?
========================================================
If errors are detected when loading data using fetchNASIS(), these "get" functions can trace them back to the corresponding records in NASIS:

- **get('sites.missing.pedons', envir=soilDB.env)**
    - Returns user site ID's for sites missing pedons.
  
- **get('dup.pedon.ids', envir=soilDB.env)**
    - Returns pedon ID's for sites with duplicate pedon ID's.
  
- **get('bad.pedon.ids', envir=soilDB.env)**
    - Returns user pedon ID's for pedons with inconsistent horizon depths.
    
- **get('bad.horizons', envir=soilDB.env)**
    - Returns a dataframe of horizon-level information for pedons with inconsistent horizon depths.
 - TODO: show an example of this output

Options That Can Be Set Within fetchNASIS()
========================================================

There are two default options that can be set within `fetchNASIS(rmHzErrors = TRUE, nullFragsAreZero = TRUE)`.  

- **rmHzErrors = TRUE/FALSE** 
    - Setting this value to **TRUE** (the default) enables checks for horizon depth consistency. Consider setting this argument to **FALSE** if you aren't concerned about horizon-depth errors or if you know that your selected set contains many combination horizons (e.g., consisting of E/Bt horizons or similar two-part horizons described individually for the same depth range). Note that any pedons flagged as having horizon-depth errors (rmHzErrors = TRUE) are omitted from the data returned by `fetchNASIS()`.
    
- **nullFragsAreZero = TRUE/FALSE**
    - Setting this value to **TRUE** (the default) converts null entries for rock fragment volumes to 0. This is typically the right assumption because rock fragment data are typically populated only when observed. If you know that your data contain a combination of omitted information (e.g. no rock fragment volumes are populated) then consider setting this argument to **FALSE.**
  
For more information on the data checks and adjusting the default options to `fetchNASIS()` function, see the following resource: [**Tips on Getting Data from NASIS into R**](http://ncss-tech.github.io/AQP/soilDB/fetchNASIS-mini-tutorial.html).



========================================================

