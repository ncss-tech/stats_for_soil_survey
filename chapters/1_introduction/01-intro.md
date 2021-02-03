---
title: Chapter 1 Introduction to R and RStudio
author: Stephen Roecker, Skye Wills, Katey Yoast and Tom D'Avello 
date: "2021-02-02"
output:
  html_document:
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: no
editor_options: 
  chunk_output_type: console
---



![](figure/logo.jpg)

## Outline

1. Course Overview
    1. Review Course Objectives
    2. Why is this training needed?
    3. Why is course organized this way?
2. What is R?
    1. Why should I use R?
    2. What can R do?
3. How do I get started?
    1. RStudio interface
    2. What are packages?
    3. How to navigate the Help tab
    4. How to save files
4. Manipulating data
    1. Loading & viewing data
    2. Filtering, transforming, merging, aggregating and reshaping data
    3. Exporting data
    
    
## Course Overview

### Course Objectives

- Develop solutions to investigate soil survey correlation problems and update activities.
- Evaluate investigations for interpretive results and determine how to proceed.
- Summarize data for population in NASIS.
- Analyze spatial data to investigate soil-landscape relationships 
- Help to pursue the question "why" 


### Why is this training needed?

- Long standing goal of the Soil Science Division to have a course in statistics [(Mausbach, 2003)](https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/nrcs142p2_051833.pdf)
- Opportunities to learn these techniques are limited, especially at the undergraduate level [(Hennemann and Rossiter, 2004)]([http://www.css.cornell.edu/faculty/dgr2/Docs/ChaAm/ChaAmKeynoteHennemann.pdf)
- Consistent methodology (data analysis, data population, sampling design, etc.)
- There is continually a greater need to use these techniques:
    - Mapping of lands at high production rates ([MacMillan et al., 2007](https://www.sciencedirect.com/science/article/pii/S0016706107001152); [Kempen et al., 2012](https://acsess.onlinelibrary.wiley.com/doi/10.2136/sssaj2011.0424); [Brevik et al., 2016](https://www.sciencedirect.com/science/article/pii/S034181621630220X))
    - Ecological Sites [(Maynard et al., 2019)](dl.sciencesocieties.org/publications/sssaj/abstracts/83/3/666)
    - Soil survey refinement (disaggregation) ([Chaney et al., 2016](https://www.sciencedirect.com/science/article/pii/S0016706116301434); [Ramcharan et al., 2017](https://acsess.onlinelibrary.wiley.com/doi/10.2136/sssaj2017.04.0122))



### Why is course organized this way?

- Our best judgment for assembling into **24** hours what could be **6** University level courses
- Mixture of slides and script enabled web pages is new for NRCS
- The web content is a long-term investment and should serve as a permanent reference
- Feel free to provide guidance for improving the class for future offerings



## What is R?

R is a free, open-source software and programming language developed in 1995 at the University of Auckland as an environment for statistical computing and graphics [(Ikaha and Gentleman, 1996)](https://www.stat.auckland.ac.nz/~ihaka/downloads/R-paper.pdf). Since then R has become one of the dominant software environments for data analysis and is used by a variety of scientific disiplines, including soil science, ecology, and geoinformatics ([Envirometrics CRAN Task View](https://cran.r-project.org/web/views/Environmetrics.html); [Spatial CRAN Task View](https://cran.r-project.org/web/views/Spatial.html)). R is particularly popular for its graphical capabilities, but it is also prized for it's GIS capabilities which make it relatively easy to generate raster-based models. More recently, R has also gained several packages which are designed specifically for analyzing soil data.

1. a software environment: 
    + statistics
    + graphics
    + programming
    + calculator
    + GIS
2. a language to explore, summarize, and model data
    + functions = verbs
    + objects = nouns

</br>

![](static-figures/rproject.png){ width=80% }

</br>


### Why Should I Learn R?

While the vast majority of people use Microsoft Excel for data analysis, R offers numerous advantages, such as:

1. Cost. R is free! [("Free as in free speech, not free beer.")](https://www.gnu.org/philosophy/free-sw.html)

2. [Reproducible Research](http://christophergandrud.github.io/RepResR-RStudio/) (*self-documenting, repeatable*)
    + repeatable: 
        + code + output in a single document *('I want the right answer, not a quick answer' - Paul Finnell)*
        + easier the next time ([humorous example](https://www.youtube.com/watch?time_continue=1&v=s3JldKoA0zw))
        + numerous Excel horror stories of scientific studies gone wrong exist ([TED Talk](https://www.youtube.com/watch?v=dXKbkpilQME))
    + scalable: applicable to small or large problems

3. R in a Community
    + [Numerous Discipline Specific R Groups](https://cran.r-project.org/web/views/)
    + [Numerous Local R User Groups (including R-Ladies Groups)](https://jumpingrivers.github.io/meetingsR/r-user-groups.html#north-america)
    + [Stack Overflow](https://stackoverflow.com/)

4. Learning Resources *(quantity and quality)*
    + [R books](https://www.r-project.org/doc/bib/R-books.html)
    + [(Free Online) R Books](https://bookdown.org/)
    
5. R is 'becoming' the new norm (paradigm shift?) "If we don't accept these challenges, other who are less qualified will; and soil scientists will be displaced by apathy." [(Arnold and Wilding, 1992)](https://dl.sciencesocieties.org/publications/books/abstracts/sssaspecialpubl/spatialvariabil/1)


While some people find the use of a commandline environment daunting, it is becoming a necessary skill for scientists as the volume and variety of data has grown. Thus scripting or programming has become a third language for many scientists, in addition to their native language and disipline specific terminology. Other popular programming languages include: SQL (i.e. NASIS), Python (i.e. ArcGIS), and JavaScript. 

</br>

![*ODBC and GDAL link R to nearly all possible formats/interfaces*](static-figures/triangle.png){ width=50% }

</br>


### What can R do?

### Packages
* Base R (*functionality is extended through packages*)
    + basic summaries of quantitative or qualitative data
    + data exploration via graphics
    + [GIS](https://cran.r-project.org/web/views/Spatial.html) data processing and analysis

* Soil Science R Packages
    + [aqp](https://github.com/ncss-tech/aqp) - visualization, aggregation, classification
    + [soilDB](https://github.com/ncss-tech/soilDB) - access to commonly used soil databases
    + [soilReports](https://github.com/ncss-tech/soilReports) - handful of report templates
    + [soiltexture](http://soiltexture.r-forge.r-project.org/) - textural triangles

* [Ecology](https://cran.r-project.org/web/views/Environmetrics.html) R packages
    + [vegan](http://vegan.r-forge.r-project.org/) - ordination, diversity analysis, etc.
    + [dismo](http://rspatial.org/sdm/) -  species distribution modeling


#### Soil Science Applications

##### Create Maps

![](static-figures/ssurgo_timeline.png){ width=80% }


##### Draw Soil Profiles

![](01-intro_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


##### Draw Depth Plots

![](01-intro_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


##### Estimate the Range in Characteristics (RIC)


|variable |genhz | pct10| median| pct90|
|:--------|:-----|-----:|------:|-----:|
|clay     |A     |    13|     16|    22|
|clay     |BAt   |    16|     19|    25|
|clay     |Bt1   |    18|     24|    32|
|clay     |Bt2   |    22|     30|    44|
|clay     |Cr    |    15|     15|    15|
|phfield  |A     |     6|      6|     7|
|phfield  |BAt   |     5|      6|     6|
|phfield  |Bt1   |     5|      6|     7|


## RStudio: An Integrated Development Environment (IDE) for R

RStudio is an integrated development environment (IDE) that allows you to interact with R more readily. RStudio is similar to the standard RGui, but is considerably more user friendly. It has more drop-down menus, windows with multiple tabs, and many customization options. The first time you open RStudio, you will see three windows. A forth window is hidden by default, but can be opened by clicking the **File** drop-down menu, then **New File,** and then **R Script**. Detailed information on using RStudio can be found at at [RStudio's Website](https://support.rstudio.com/hc/en-us/sections/200107586-Using-RStudio).
 

![](static-figures/ch1_rstudio2.png)  


RStudio Windows / Tabs  | Location    | Description                                  |
------------------------|-------------|----------------------------------------------|
Console Window          | lower-left  | location were commands are entered and the output is printed |
Source Tabs             | upper-left  | built-in text editor                         |
Environment Tab         | upper-right | interactive list of loaded R objects         |
History Tab             | upper-right | list of key strokes entered into the Console |
Files Tab               | lower-right | file explorer to navigate C drive folders    |
Plots Tab               | lower-right | output location for plots                    |
Packages Tab            | lower-right | list of installed packages                   |
Help Tab                | lower-right | output location for help commands and help search window |
Viewer Tab              | lower-right | advanced tab for local web content           |



## Rcmdr (R Commander): A Graphical User Interface for R

While we recommend the use of RStudio for some of the reasons listed above, many people new to R (or infrequent users) might benefit from a graphical user interface (GUI) that allows the user to run basic functions using a point and click interface. 

Luckily for beginners R has the R Commander (Rcmdr) GUI, which is similiar to [JMP](https://www.jmp.com/en_us/learning-library/using-jmp.html). Rcmdr was created by [John Fox](http://socserv.socsci.mcmaster.ca/jfox/Misc/Rcmdr/index.html) for his introductory statistics students so they could see how the software worked without learning a large number of function names and arguments. Rcmdr is a great way to begin familiarizing yourself with R and statistics within a GUI environment. 

Regretable we know of no GUI that allows users to perform the majority of soil survey applications demonstrated in this course, and thus won't Rcmdr won't be covered. For those who wish to pursue Rcmdr, alternative instructions can be viewed at [Andy Chang & G. Jay Kerns website](http://gchang.people.ysu.edu/r/R_Instructions.htm).

To take a quick peak at Rcmdr, it can be opened by **entering** the following command into the R console.


```r
install.packages(Rcmdr)
library(Rcmdr)
```

![](static-figures/ch1_rcmdr.png)  


## R basics  

- R is command-line driven. It requires you to type or copy-and-paste commands after a command prompt (>) that appears when you open R. This is called the "Read-Eval-Print-Loop" or REPL. After typing a command in the R console and pressing **Enter** on your keyboard, the command will run. 

- If your command is not complete, R issues a continuation prompt (signified by a plus sign: `+`). 

- R is case sensitive. Make sure your spelling and capitalization are correct.  
 
- Commands in R are also called functions. The basic format of a function in R is: `object <- function.name(argument_1 = data, argument_2 = TRUE)`.  

- The up arrow (^) on your keyboard can be used to bring up previous commands that you've typed in the R console. 

- Any text that you do not want R to act on (such as comments, notes, or instructions) needs to be preceded by the `#` symbol (a.k.a. hash-tag, comment, pound, or number symbol).  R ignores the remainder of the script line following `#`. 




```r
# Addition
1 + 1
```

[1] 2

```r
# Multiplication
10 * 10
```

[1] 100

```r
# Compute Logarithm
log10(100)
```

[1] 2

```r
# Print Text
"Hello World"
```

[1] "Hello World"

```r
# Combine a List of Values
c(1, 2)
```

[1] 1 2

```r
# Create sequence of values
1:10
```

 [1]  1  2  3  4  5  6  7  8  9 10

```r
# Loading built-in datasets
data(npk)

# Plot histogram
hist(npk$yield)
```

![](01-intro_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# Assignment
test1 <- 1
test2 <- "Hello World"

# Print
print(test1)
```

[1] 1

```r
test2
```

[1] "Hello World"



## Managing Packages

Packages are collections of additional functions that can be loaded on demand. They commonly include example data that can be used to demonstrate those functions. Although R comes with many common statistical functions and models, most of our work requires additional packages.



### Installing Packages

To use a package, you must first install it and then load it. These steps can be done at the command line or using the Packages Tab. Examples of both approaches are provided below. R packages only need to be installed once (until R is upgraded or re-installed). Every time you start a new R session, however, you need to load every package that you intend to use in that session.  

Within the **Packages** tab you will see a list of all the packages currently installed on your computer, and 2 buttons labeled either "Install" or "Update". To install a new package simply select the **Install** button. You can enter install one or more than one packages at a time by simply separating them with a comma.   

![](static-figures/ch1_package_window.png)  

![](static-figures/ch1_install_example.png)

To find out what packages are installed on your computer, use the following commands:


```r
library() 

# or

installed.packages()
```

One useful package for soil scientists is the `soiltexture` package. It allows you to plot soil textural triangles. The following command shows how to install this package if you do not have currently have it downloaded:  


```r
# CRAN (static version)
installed.packages(c("aqp", "soilDB", "soilReports", "soiltexture"))

# GitHub (development version)
devtools::install_github("ncss-tech/soilDB", dependencies = FALSE, upgrade_dependencies = FALSE, build = FALSE)
```



### Loading Packages

Once a package is installed, it must be loaded into the R session to be used. 

![](static-figures/ch1_load_packages.png){ width=60% }


```r
library(soilDB)
```

You can also load packages using the **Packages Tab**, by **checking** the box next to the package name. Documentation about the soiltexture package is available from the help functions in R. 


```r
help(package = "soilDB")
```

This help command sends you to a webpage.



## Getting Help 

R has [extensive documentation](https://cran.r-project.org/manuals.html), numerous [mailing lists](https://www.r-project.org/mail.html), and [countless books](https://www.r-project.org/doc/bib/R-books.html) (many of which are free and listed at end of each chapter for this course). 

To learn more about the function you are using and the options and arguments available, learn to help yourself by taking advantage of some of the following help functions in RStudio:

1. Use the Help tab in the lower-right Window to search commands (such as hist) or topics (such as histogram). 

![](static-figures/ch1_help_tab.png)

2. Type `help(read.csv) or ?read.csv` in the Console window to bring up a help page. Results will appear in the Help tab in the lower right-hand window. Certain functions may require quotations, such as `help("+")`.


```r
# Help file for a function
help(read.csv) # or ?read.csv

# Help files for a package
help(package = "soiltexture")
```


## Documenting your work  

RStudio's Source Tabs serve as a built-in text editor. Prior to executing R functions at the Console, commands are typically written down (or scripted). Scripting is essentially showing your work. 

The sequence of functions necessary to complete a task are scripted in order to document or automate a task. 

While scripting may seems cumbersome at first, it ultimately saves time in the long run, particularly for repetitive tasks ([humorous YouTube Video on Scripting](https://www.youtube.com/watch?time_continue=1&v=s3JldKoA0zw)).

Benefits include:

- allows others to reproduce your work, which is the foundation of science
- serves as instruction/reminder on how to perform a task
- allows rapid iteration, which saves time and allows the evaluation of incremental changes
- reduces the chance of human error

### Basic Tips for Scripting

To write a script, simply open a new R script file by clicking **File>New File>R Script**. Within the text editor **type** out a sequence of functions.

 - Place each function (e.g. `read.csv()`) on a separate line.
 - If a function has a long list of arguments, place each argument on a separate line.
 - A command can be excuted from the text editor by placing the cursor on a line and typing **Crtl + Enter**, or by **clicking** the Run button.
 - An entire R script file can be excuted by **clicking** the Source button. 

![](static-figures/ch1_text_editor.png)



## Organizing your work

When you first begin a project you should create a new folder and place within it all the data and code associated with the project. This vastly simplifies the process of accessing your files from R, but is it also a good habitat in that it makes it easy pickup where you left off later and find your data. Within R your project folder is also know as your working directory. This directory will be the default location your plots and other R output are saved. 

Essentially, you want to have the inputs for your code to be found in the working directory so that you can refer to them using [relative file paths](https://excelquick.com/r-programming/importing-data-absolute-and-relative-file-paths-in-r/). Relative file paths make it easier if you move the folder containing your script(s) around. Or, if you share it with someone else, they will have little issue getting your code to work on their own file system. 

**NOTE:** _Beware when specifying any file paths_ that **R** uses forward slashes `/` instead of back slashes `\`. Back slashes are reserved for use as an [escape character](https://campus.datacamp.com/courses/string-manipulation-in-r-with-stringr/string-basics?ex=4).


### Setting the Working Directory

Before you begin working in R, you should set your working directory to your project folder; for example, "C:\\workspace2\\projectx...". 

To change the working directory in RStudio, select main menu **Session >> Set Working Directory >> ...**. Or, from the "Files" tab click **More >> Set As Working Directory** to use the _current location of the "Files" tab_ as your working directory.

![](static-figures/ch1_setwd.png)


Setting the working directory can also be done via the Console with the `setwd()` command:


```r
setwd("C:/workspace2")
```


To check the file path of the current working directory (which should now be "C:\\workspace2"), type:


```r
getwd()
```


### RStudio Projects (.Rproj files)

You can also manage your working directory using RStudio Projects. An RStudio Project file (_.Rproj_) is analogous to, for example, a _.mxd_ file for ArcMap. It contains information about the specific settings you may have set for a "project".
  
You open or create projects using the drop down menu in the top right-hand corner of the RStudio window (_shown below_)

![RStudio Project Menu](static-figures/rstudio_projectdropdown.png)

Here is what a typical Project drop-down menu looks like:

![RStudio Project Menu (expanded)](static-figures/rstudio_projectdropdown2.png)

 * You can create new projects from existing or new directories with "New Project...".

 * When you click "Open Project...", your working directory is _automatically set to the .Rproj file's location_ -- this is _extremely_ handy 

 * Any projects you have created/used recently will show up in the "Project List"


<!-- Too much for first chapter...
### More RStudio Project Features

Another neat thing about using Projects is that RStudio will provide additional tabs depending on the contents of your working directory. 

For example, if your project folder contains a Git repository (_.git_ hidden directory), a Git tab (_below image, left_) is available for version control. Also, if the directory contains source code for an R package, the "Build" tab  (_below image, right_) provides commands from the `devtools` package. 

![RStudio Projects - Git Repository and Build Tabs](static-figures/rstudio_projecttabs.png) -->


## Saving your work  

In R, you can save several types of files to keep track of the work you do. The file types include: workspace, script, history, and graphics. It is important to save often because R, like any other software, may crash periodically. 

Such problems are especially likely when working with large files. You can save your workspace in R via the command line or the File menu.  




#### R script (.R)

An R script is simply a text file of R commands that you've typed. 

You may want to save your scripts (whether they were written in R Editor or another program such as Notepad) so that you can reference them in the future, edit them as needed, and keep track of what you've done. 

To save R scripts in RStudio, simply **click the save button** from your R script tab. Save scripts with the .R extension. 

R assumes that script files are saved with only that extension. If you are using another text editor, you won't need to worry about saving your scripts in R. You can open text files in the RStudio text editor, but beware copying and pasting from Word files as discussed below.  

![](static-figures/ch1_save_script.png)  

To open an R script, **click the file icon**.  

![](static-figures/ch1_file_icon.png)


#### Microsoft Word Files

Using Microsoft Word to write or save R scripts is generally a bad idea.

Certain keyboard characters, such as quotations "", are not stored the same in Word (e.g. they are "left" and "right" handed). The difference is hard to distinguish, but will not run in R. 

Also, pasting your R code or output into Wword documents manually is not reproducible, so while it may work in a pinch, it ultimately costs you time. 

You can use the `word_document` Rmarkdown template to automatically "Knit" `.docx` files from R code using a template, which is very handy for quickly getting a nice looking document!

#### R Markdown (.Rmd)

Stub about the basic benefits of Rmarkdown for reproducibility, interactive reports like Shiny, but also "Notebooks" in RStudio.

This document is made in bookdown! You can make websites with blogdown, etc. You can knit visually appealing and high-quality documents into rich HTML, PDF or Word documents.

These are all based off of the powerful pandoc engine and the tools in the Rmarkdown ecosystem.

<!--
## This is not something we should encourage people to do. Not reproducible. We tell them to turn off this "feature" in the pre-course and I sure hope they do because it is a monstrous waste of their and our debugging time. 
## History is the only thing they should save, unless they explicitly are selecting the objects to save and using e.g. save(), saveRDS() / load() etc. -AGB

#### Workspace (.Rdata)  

The R workspace consists of all the data objects you've created or loaded during your R session. When you quit R by either typing `q()` or exiting out of the application window, R will prompt you to save your workspace.

If you choose yes, R saves a file named .RData to your working directory. The next time you open R and reload your .Rdata workspace, all of your data objects will be available in R and all of the commands that you've typed will be accessible by using the up-arrow and down-arrow keys on your keyboard. 

You can also save or load your workspace at any time during your R session from the menu by clicking **Session>Save Workspace As..**, or the save button on the **Environment Tab**.   

![](static-figures/ch1_save_workspace.png)    

The R command for saving your workspace is:


```r
save.image(file="workspace.RData")
```
-->

#### R history (.Rhistory) 

An R history file is a copy of all your key strokes. You can think of it as brute force way of saving your work. It can be useful if you didn't document all your steps in an R script file.

Like an R file, an Rhistory file is simply a text file that lists all of the commands that you've executed. It does not keep a record of the results. 

To load or save your R history from the History Tab click the **Open File** or **Save** button. If you load an Rhistory file, your previous commands will again become available with the up-arrow and down-arrow keys.

![](static-figures/ch1_save_history.png) 

You can also use the command line to load or save your history.  


```r
savehistory(file = "sand.Rhistory")  
loadhistory(file = "sand.Rhistory")  
history(max.show=Inf) #displays all previous commands
```

#### R Graphics   

Graphic outputs can be saved in various formats. 
 

|Format          |Function                    |
|:---------------|:---------------------------|
|pdf             |pdf("graphic.pdf")          |
|window metafile |win.metafile("graphic.wmf") |
|png             |png("graph.png")            |
|jpeg            |jpeg("graph.jpg")           |
|bmp             |bmp("graph.bmp")            |
|postscript      |postscript("graph.ps")      |

To save a graphic: (1) Click the **Plots Tab** window, (2) click the **Export** button, (3) **Choose** your desired format, (3) **Modify** the export settings as you desire, and (4) click **Save**.  

![](static-figures/ch1_save_plot.png)  

The R command for saving a graphic is:  


```r
png(file = "npk_yield.png")
plot(npk$yield)
dev.off()
```

The first line of this command creates a blank file named sand with a JPEG extension.  The second line plots the data object that you want to create a graphic of (here it is conveniently the same name as the JPEG file we are creating). The third line closes the graphics device.  


## Exercise 1

Using the examples discussed thus far as a guide, demonstrate your mastering of the material by performing the following tasks.

1. Create an R script file, demonstrate 3 basic R functions, and comment (`#`) your code. 
2. Install the FedData R package from CRAN and [GitHub](https://github.com/ropensci/FedData). Save the commands in your R script file.
3. Load the FedData R package and read the help file for the `get_ssurgo` function within the FedData package. What is the 1st input/argument? Save the R command in your R script. 
4. Save your R script, and forward to your instructor.



## Loading Data

R can load a variety of data formats, however tabular data is by far the most common, and what we will spend of the majority of our time working with. Typically tabular data is stored in spreadsheets (e.g. .txt, .csv, .xlsx), databases (e.g. NASIS), or webpages (.html). Within R tabular data is stored as a `data.frame`. 



#### Text files

Text files are a preferable format for storing and transferring small datasets. One basic command for importing text files into R is `read.csv()`. The command is followed by the file name or URL and then some optional instructions for how to read the file.

These files can either be imported into R by clicking the **Import Dataset >> From Text** buttons from the Environment tab, or by typing the following command into the R console:


```r
# from working directory
sand <- read.csv("C:/workspace2/sand_example.csv")
```


```r
# from URL
sand <- read.csv("https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/sand_example.csv") 
```

![](static-figures/sand_readcsv.png) 



#### Excel files

R can import Excel files, but generally speaking it is a bad idea to use Excel. Excel has a dangerous default which automatically converts data with common notations to their standard format without warning or notice. For example, the character "11-JUN" entered into a cell automatically becomes the date 6/11/2021, even though the data is still displayed as 11-JUN. The only way to avoid this default behavior is to manually import your data into Excel via the **Data Tab>Get External Data Ribbon**, and manually set the data type of all your columns to text. Failure to do so has resulted in numerous retracted research articles ([Washington Post Article](https://www.washingtonpost.com/news/wonk/wp/2016/08/26/an-alarming-number-of-scientific-papers-contain-excel-errors/?utm_term=.9352a35dca6f)). Warnings aside, Excel files are a very common and are a format most people are familiar with. Therefore we will illustrate how to bring them into R.

Download the sand Excel dataset from GitHub at [https://github.com/ncss-tech/stats_for_soil_survey/blob/master/data/Pre-course/R_sand/sand_example.xlsx](https://github.com/ncss-tech/stats_for_soil_survey/blob/master/data/Pre-course/R_sand/sand_example.xlsx)

Excel datasets can either be imported into R by clicking the **Import Dataset >> From Excel** buttons from the Environment tab, or by typing the following command into the R console:  


```r
library(readxl)

sand_example <- read_excel("sand_example.xlsx")
```

![](static-figures/sand_readxl.png) 



#### NASIS (Web) Reports

NASIS provides a plethora of reports, many of which can be read into R for analysis. The `soilDB` R package provides a series of function to read data from NASIS either using a local database connection or via HTML web reports. Similar functions also exist for accessing tabular data from Soil Data Access. More details on `soilDB` will be provided in the next chapter, but now we'll illustrate how to access some example datasets for manipulating tabular data.


```r
library(soilDB)

# get projects
prj <- get_project_from_NASISWebReport(mlrassoarea = "11-IND", fiscalyear = 2020)

# get legends
leg <- get_legend_from_NASISWebReport(mlraoffice = "Indi%", areasymbol = "%")

# get map units
mu  <- get_mapunit_from_NASISWebReport(areasymbol = c("IN001", "IN11%"))
```



## Data manipulation

Before we can do any sort of analysis, we often need to manipulate our data somehow. Estimates varying, but an analyst typically spend 80% of their time manipulating data, and only 20% actually analyzing or modeling. Tasks generally involve filtering, transforming, merging, aggregating, and reshaping data. 

R has many functions and packages for manipulating data frames, but within the past several years a family of packages, known as the `tidyverse`, have been developed to simplify interacting with data frames (or tibbles). Within the `tidyverse` the most commonly used packages are `dplyr` and `tidyr`. Many of the tidyverse function names are patterned after SQL syntax.

<!--
An important principle of tabular data is that it should be ["normalized'](https://en.wikipedia.org/wiki/Database_normalization) or ["tidy"](https://r4ds.had.co.nz/tidy-data.html). This simply means that rows should hold observations, and columns should hold variables for each observation. 
-->

We will review the most common functions you need to know in order to accomplish the majority of data manipulation tasks.


### Viewing and Removing Data

Once a file is imported, it is imperative that you check to ensure that R correctly imported your data. Make sure numerical data are correctly imported as numerical, that your column headings are preserved, etc. To view the data simply **click** on the mu dataset listed in the Environment tab. This will open up a separate window that displays a spreadsheet like view.

![](static-figures/ch1_view_dataframe.png)


Additionally you can use the following functions to view your data in R.

Function  | Description                                         |
----------|-----------------------------------------------------|
`print()` | prints the entire object (avoid with large tables)  |
`head()`  | prints the first 6 lines of your data               |
`str()`   | shows the data structure of an R object             |
`names()` | lists the column names (i.e., headers) of your data |
`ls()`    | lists all the R objects in your workspace directory |

Try entering the following commands to view the `mu` dataset in R: 


```r
str(mu)

names(mu)

head(mu)

ls()
```

A data object is anything you've created or imported and assigned a name to in R. The Environment tab allows you to see what data objects are in your R session and expand their structure. Right now sand should be the only data object listed. If you wanted to delete all data objects from your R session, you could **click the broom icon** from the Environments tab. Otherwise you could type:


```r
# Remove all R objects
rm(list = ls(all = TRUE)) 

# Remove individual objects
rm(mu, leg, sand)
```

![](static-figures/ch1_clear_workspace.png)



### Filtering or Subsetting Data

When analyzing data in NASIS, filtering is typically accomplished by loading your selected set with only the records you're interested in. However, it is often useful or necessary to subset your data after it's loaded. This can allow you to isolate interesting records within large datasets. For these reasons R has numerous options/functions for filtering data.

Data frames can be filtered by both columns and rows, using either **names**, **position** (e.g. column 1, row 5), or **logical** indices (e.g. `TRUE`/`FALSE`). Another particularly useful feature is the use of **pattern matching** which uses regular expressions to select data, which is similar to the `LIKE` statement from SQL.

**Filtering with names and numerical indices


```r
# Filtering with names
mu$areasymbol                             # select column names using $
mu[, c("areasymbol", "musym")]            # select column names using []
mu[c("1", "2"), ]                         # select row names using []
mu[c("1", "2"), c("areasymbol", "musym")] # select column and row names using []


# Filtering by position
mu[1, ]          # select first row
mu[, 1]          # select first column
mu[2, 2]         # select second row and second column
mu[c(1, 2, 3), ] # select multiple rows
mu[c(-1, -2), ]  # drop multiple rows
```

**Logical Operators**
  - `==` R uses a double equal sign as "equal-to" in SQL
  - `!=` Not-equal-to
  - `<, >, <=, >=` Less than, greater than, less than or equal to, and greater than or equal
  - `&` Equivalent to `AND` in SQL and Soil Taxonomy, must match both conditions
  - `|` Equivalent to `OR` in SQL and Soil Taxonomy, must match at least one condition
  - `%in%` Equivalent to `IN ()` in SQL (e.g. `mu$areasymbol %in% c("IN001", "IN111"`)
  - `grepl()` equivlant to `LIKE` in SQL (e.g. `grepl("IN%", mu$areasymbol)`)
  

**Filtering with logicals**


```r
# Standard evaluation with base R []

# Filtering with logicals
mu[mu$areasymbol == "IN001", ]                # select rows that equal IN001
mu[mu$areasymbol != "IN001", ]                # select rows that do not equal IN001
mu[, names(mu) == "areasymbol"]               # select columns that equal areasymbol
mu[, names(mu) %in% c("areasymbol", "musym")] # select columns that match areasymbol and musym 


# Non-standard evaluation with tidyverse

library(dplyr)

# Filtering rows
filter(mu, areasymbol == "IN001")
filter(mu, areasymbol != "IN001")
filter(mu, areasymbol %in% c("IN001", "IN111"))
filter(mu, muacres > 0)

# Select columns
select(mu, areasymbol, musym)

# Slice  rows
slice(mu, 1:5)
```



### Transforming Data

This allows you to create new columns by convert, compute, or combine data within existing columns.


```r
mu <- mutate(mu, 
             # convert to hectares
             muhectares = muacres * 0.4047,
             # convert muname to TRUE or FALSE if Miami is present using pattern matching
             miami      = grepl("Miami", muname),
             # compute % minor component
             n_minor    = n_component - n_majcompflag,
             # combine columns
             key        = paste(areasymbol, musym)
             )
```


### Sorting Data

Sorting allows you to rearrange your data. Beware R has several similar functions (e.g. `sort` and `order`) for sorting data only work with specific datatypes. The tidyverse function `arrange` is designed to work with data frames.


```r
# sort ascending
arrange(mu, areasymbol, muname)

# sort descending
arrange(mu, desc(areasymbol), desc(muname))
```



### Piping Data

Another particularly useful feature of the tidyverse is the use of **piping** (e.g. `%>%`), which allows functions to be seamlessly strung together, and thus be read from right to left, rather than from inside out or reassigned.


```r
# non-piping example 1
mu_sub <- filter(mu, areasymbol == "IN001")
mu_sub <- mutate(mu_sub, pct_100less = pct_component < 100) 

# non-piping example 2
mu_sub <- mutate(filter(mu, areasymbol == "IN001"), pct_100less = pct_component < 100)

# piping
mu_sub <- mu %>% 
  filter(areasymbol == "IN001") %>% 
  mutate(pct_100less = pct_component < 100)
```



### Merging/Joining or Combining Data

** Joining**

When working with tabular data you often have 2 or more tables you need to join. There several ways to join tables, in particular which direction to join and which columns to join on. 


```r
# inner join
leg_mu <- inner_join(leg, mu, by = c("liid", "areasymbol"))

# left join
leg_mu <- left_join(leg, mu, by = c("liid"))

# right_join
leg_mu <- right_join(leg, mu, by = "liid")
```


** Combining**

If your tables have the same structure (e.g. columns), or length and order you may simply combine them. For example, if you have two different mapunit tables.


```r
# combine rows
rbind(mu, mu)
rbind(mu, leg) # won't work


# combine columns
cbind(mu, mu)  # beware combine tables with duplicate column names
cbind(mu, areasymbol_2 = mu$areasymbol)
cbind(mu, leg) # won't work
```


### Aggregating or Grouping Data

Because soil data has multiple dimensions (e.g. properties and depths) and levels of organization (e.g. many to one relationships), it is often necessary to aggregate it. For example, when we wish to make a map we often need to aggregate over components and then map units. Depending on the data type this aggregation may involve taking a weighted average or selecting the dominant condition.

The `group_by` function defines the groups over which we wish to `summarize` the data.



```r
mu_agg <- mu %>% 
  group_by(grpname, areasymbol) %>%
  summarize(sum_muacres = sum(muacres),
            n_musym     = length(musym)
            )
```



### Reshaping Data

Typically data is stored in what is known as a **wide** format, where each column contains a different variable (e.g. depth, clay, sand, rocks). However, sometimes it is necessary to reshape or pivot to a **long** format, where each variable/column is compressed into 2 new rows. One new column contains the old column names, while another new column contains the values from the old columns. This is particularly useful when combining multiple variables into a single plot.


```r
library(tidyr)

# Simplify mu example dataset
mu2 <- mu %>%
  select(grpname, areasymbol, musym, muacres, n_component, pct_hydric) %>%
  slice(1:5)
print(mu2)

# Pivot long
mu2_long <- pivot_longer(mu2, cols = c(muacres, n_component, pct_hydric))
print(mu2_long)

# Pivot wide
mu2_wide <- pivot_wider(mu2_long, names_from = name)
print(mu2_wide)
```



### Exporting Data

To export data from R, use the command `write.csv()` or `write.dbf()` functions. Since we have already set our working directory, R automatically saves our file into the working directory.  


```r
write.csv(mu_agg, file = "mu_agg.csv")


library(foreign)

write.dbf(mu_agg, file = "mu_agg.dbf")
```



## Exercise 2

1. Create a new R script file.
2. Load a 2021 project table from `mlrassoarea = "2-SON"` and a mapunit table from `areasymbol = c("CA630", "CA649")`.
3. Filter your project table for rows where the `projectapprovedflag == TRUE`.
4. Calculate the acreage of hydric soils for each map unit by multiplying `muacres * pct_hydric`.
5. Join your filtered project table to the mapunit table using a left join.
6. Aggregate the total acreage of hydric soils each soil survey area.
7. Save your R script and forward to your instructor.



## Review

Given what you now know about R, try to answer the following questions:

1. Can you think of a situation where an existing hypothesis or conventional wisdom was not repeatable?

2. What are packages?

3. What is GitHub?

4. Where can you get help?

5. What is a data frame?

6. What are 3 ways you can manipulate a data frame?



## Additional Reading (Introduction)

* Introductory R Books
    + [R for Data Science](https://r4ds.had.co.nz/index.html)
    + [RStudio Cheatsheets](https://rstudio.com/resources/cheatsheets/)
    + [Quick-R](https://www.statmethods.net/)
* Advanced DSM R Books
    + [Predictive Soil Mapping with R](https://envirometrix.github.io/PredictiveSoilMapping/)
    + [Using R for Digital Soil Mapping (not free)](http://www.springer.com/us/book/9783319443256)
* Soil Science R Applications
    + [aqp and soilDB tutorials](http://ncss-tech.github.io/AQP/)
    + [ISRIC World Soil Information Example Training Courses](https://www.isric.org/utilise/capacity-building/training-courses#examplecourses)
    + [ISRIC World Soil Information YouTube Channel](https://www.youtube.com/channel/UCNi1XYjdXWF9eAjvG40KqWg)
    + [OpenGeoHub Courses](https://opengeohub.org/course)
    + [OpenGeoHub YouTube Channel](https://www.youtube.com/channel/UC6HFFFYiV4zEYJlQMIXemWA/featured)
    + [David Rossiter's Cornell Homepage](http://www.css.cornell.edu/faculty/dgr2/)
    + [Pierre Roudier](https://pierreroudier.github.io/teaching/index.html)
* Soil Sciences and Statistics Review Articles
    + Arkely, R., 1976. Statistical Methods in Soil Classification Research. Advances in Agronomy 28:37-70. [https://www.sciencedirect.com/science/article/pii/S0065211308605520](https://www.sciencedirect.com/science/article/pii/S0065211308605520)
    + Mausbach, M., and L. Wilding, 1991. Spatial Variability of Soils and Landforms. Soil Science Society of America, Madison. [https://dl.sciencesocieties.org/publications/books/tocs/sssaspecialpubl/spatialvariabil](https://dl.sciencesocieties.org/publications/books/tocs/sssaspecialpubl/spatialvariabil)
    + Wilding, L., Smeck, N., and G. Hall, 1983.  Spatial Variability and Pedology. In : L. Widling, N. Smeck, and G. Hall (Eds). Pedogenesis and Soil Taxonomy I. Conceps and Interactions. Elseiver, Amsterdam, pp. 83-116. [https://www.sciencedirect.com/science/article/pii/S0166248108705993](https://www.sciencedirect.com/science/article/pii/S0166248108705993)

----------------------------
 This document is based on `aqp` version 1.27, `soilDB` version 2.6.0, and `sharpshootR` version 1.7.


## References (Introduction)

Brevik, E.C., J.A. Homburg, B.A. Miller, T.E. Fenton, J.A. Doolittle, and S.J. Indorante, 2016. Selected highlights in American soil science history from the 1980s to the mid-2010s. Catena 146:128-146.

Chaney, N., E. Wood, A.B. McBratney, J.W. Hempel, T.W. Nauman, C.W. Brungard, and N.P. Odgers, 2016. POLARIS: A 30-meter probabilistic soil series maps of the contiguous United States. Geoderma 274(15)54-67. [https://www.sciencedirect.com/science/article/pii/S0016706116301434](https://www.sciencedirect.com/science/article/pii/S0016706116301434)

Hennemann, G.R., and Rossiter, DG., 2004. Training needs for the next generation of soil surveyors. International Conference on Innovative Techniques in Soil Survey; 22-26 March 2004, Cha-Am, Thailand. [http://www.css.cornell.edu/faculty/dgr2/Docs/ChaAm/ChaAmKeynoteHennemann.pdf](http://www.css.cornell.edu/faculty/dgr2/Docs/ChaAm/ChaAmKeynoteHennemann.pdf)

Kempen, B., D. Brus, J. Stoorvogel, G. Heuvelink, F. de Vries, 2012. Efficiency Comparison of Conventional and Digital Soil Mapping for Updating Soil Maps. Geoderma 76(6)2095-2115. [https://acsess.onlinelibrary.wiley.com/doi/10.2136/sssaj2011.0424](https://acsess.onlinelibrary.wiley.com/doi/10.2136/sssaj2011.0424)

Ihaka, R., and R. Gentleman. 1996. R: A language for data analysis and graphics. Journal of Computational and Graphical Statistics 5(3):399–314. [https://www.stat.auckland.ac.nz/~ihaka/downloads/R-paper.pdf](https://www.stat.auckland.ac.nz/~ihaka/downloads/R-paper.pdf)

MacMillian, R., D. Moon, and R. Coupe, 2007. Automated predictive ecological mapping in a Forest Region in B.C., Canada, 2001-2005. Geoderma 140(4)353-373. [www.sciencedirect.com/science/article/pii/S0016706107001152](www.sciencedirect.com/science/article/pii/S0016706107001152)

Mausbach, 2003. The Importance of Statistical Documentation - Keeping Soil Survey Information Relevant in the 21st Century. 2003 National Cooperative Soil Survey Conference, Plymouth, MA. [https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/nrcs142p2_051833.pdf](https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/nrcs142p2_051833.pdf)

Ramcharan, A., T. Hengl, T. Nauman, C. Brungard, S. Waltman, S. Wills, and J. Thompson, 2017. Soil Property and Class Mas of the Conterminous United States at 100-Meter Spatial Resolution. Soil Science Society of America Journal, 82(1)186-201. [https://acsess.onlinelibrary.wiley.com/doi/10.2136/sssaj2017.04.0122](https://acsess.onlinelibrary.wiley.com/doi/10.2136/sssaj2017.04.0122)


