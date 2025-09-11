# Extract the results from the GCAM database! 

# 0. Set Up --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(GCAM2Hector)
library(rgcam)


# 1. Extract Old GCAM ----------------------------------------------------------
prj_file <- file.path("data", "old_gcam.dat")

get_all_queries(db_dir = "Gv8-Hv3.2", 
                db_name = "database_basexdbGCAM", 
                prj_file = prj_file)

fetch_GCAM_vs_hector(prj_file = prj_file) %>% 
  filter(scenario == "GCAM") %>% 
  mutate(version = "G-Hv3.2") -> 
  old_output


# 2. Extract New GCAM ----------------------------------------------------------
prj_file <- file.path("data", "new_gcam.dat")

get_all_queries(db_dir = "Gv8-Hv3.5", 
                db_name = "database_basexdb", 
                prj_file = prj_file)

fetch_GCAM_vs_hector(prj_file = prj_file) %>% 
  mutate(scenario = "GCAM") %>% 
  mutate(version = "G-Hv3.5") -> 
  new_output

# 3. Save Results --------------------------------------------------------------

old_output %>% 
  bind_rows(new_output) %>% 
  write.csv(file = file.path("data", "gcam_climate.csv"), row.names = FALSE)


