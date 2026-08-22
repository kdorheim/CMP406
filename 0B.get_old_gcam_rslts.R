# Extract the results from the "older version" of GCAM, aka the master branch 
# that this CMP is being merged into. For CMP406 we are aiming to merge into. 
# This script assumes that the output dir from GCAM master runs on pic have
# been donloaded and are up to date.

# 0. Set Up --------------------------------------------------------------------
# Load the required CRAN packages
library(dplyr)
library(ggplot2)
library(rgcam)


# There are several R packages that are not on CRAN, make sure that the correct
# versions are being installed/called. 
# TODO there should be a better way to handle this. 
remotes::install_github("jgcri/hector@gcam-integrationv3")
library(hector)
devtools::load_all("/Users/dorh012/Documents/2025/GCAM2Hector")

GCAM_DB_DIR <- "~/Documents/GCAM-WD/CMPs/CMP406/old-GCAM"


# 1. Extract Climate Results ---------------------------------------------------

prj_file <- file.path("data", "old_gcam.dat")
# FYI the get_all_queries function comes from my GCAM2Hector package.
get_all_queries(db_dir = GCAM_DB_DIR, 
                db_name = "database_basexdbGCAM", 
                prj_file = prj_file)

fetch_GCAM_vs_hector(prj_file = prj_file) %>%  
  mutate(version = "old") %>% 
  write.csv(file = file.path("data", "old_gcam_climate.csv"), row.names = FALSE)


# 2. Extract Emissions ---------------------------------------------------------

# Load the mapping file to convert from 
"data/GCAM_hector_emissions_map.csv" %>% 
  read.csv -> 
  mapping_df

# Extract emissions so can look at the transition period, this does not include 
# CO2 emissions since those are funky! 
prj_data <- loadProject(prj_file)

getQuery(prj_data, "emissions by region") %>% 
  left_join(mapping_df, by = join_by(ghg)) %>% 
  mutate(value = value * unit.conv) %>% 
  summarise(value = sum(value), .by = c(hector.units, scenario, hector.name, year)) %>% 
  select(year, value, variable = hector.name, units = hector.units, scenario) %>%  
  mutate(source = "GCAM") %>% 
  mutate(version = "old") -> 
  gcam_emiss


write.csv(gcam_emiss, file = file.path("data", "old_extracted_gcam_emiss.csv"), row.names = FALSE)




