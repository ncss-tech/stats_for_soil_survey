# andrew's answer to Ch1 Ex2

library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(soilDB)

# GET project table for 2-SON and fiscal year 2021
prj <- get_project_from_NASISWebReport("2-SON", fiscalyear = 2021)

# GET the approved projects (projectapprovedflag == TRUE)
approved <- filter(prj, projectapprovedflag)

# GET approved project mapunits and their area symbols
prjmu <- get_projectmapunit_from_NASISWebReport(projectname = approved$projectname)
area_symbols <- na.omit(unique(prjmu$areasymbol))

# GET mapunit table from all areasymbols where projects are approved
mu <- get_mapunit_from_NASISWebReport(areasymbol = area_symbols)

# CALCULATE number of hydric acres in each dmuiid
# note: pct_hydric contains whole number values, need to convert to proportion divide by 100
dmuiid_hydric <-  mutate(mu, hydric_acres = muacres * pct_hydric /  100)

# CALCULATE TOTAL hydric acres by areasymbol
hydric_by_area <- dmuiid_hydric %>%
  group_by(areasymbol) %>% 
  summarize(total_hydric = sum(hydric_acres, na.rm = TRUE))

# LEFT JOIN FY2021 approved project mapunit TO hydric mapunits, 
#   group by areasymbol, sum hydric acres by areasymbol (just project mapunits!!!)
hydric_in_projects <- left_join(prjmu, dmuiid_hydric) %>%
  group_by(areasymbol) %>%
  summarize(project_hydric = sum(hydric_acres, na.rm = TRUE))

# USE purrr::map_df to supply several areasymbols to 
#   get_legend_from_NASISWebReport and return combined data.frame
leg <- purrr::map_df(area_symbols, function(x) {
  get_legend_from_NASISWebReport(areasymbol = x, mlraoffice = "%")
})

# LEFT JOIN legend to hydric by area and hydric by project
#   then CALCULATE the % of total area that is hydric, AND % in projects 2021 that is hydric
res <- left_join(leg, hydric_by_area, by = "areasymbol") %>%
  left_join(hydric_in_projects, by = "areasymbol") %>%
  mutate(across(total_hydric:project_hydric,
           function(x) x / areaacres * 100,
           .names = "{.col}_pct"))

# display selected columns and summaries
select(res, areasymbol, ssastatus, areaacres, n_lmapunitiid, total_hydric:project_hydric_pct) %>%
  arrange(desc(total_hydric_pct))
