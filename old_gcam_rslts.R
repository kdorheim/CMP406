# Extract the results from the old GCAM database! 

# 0. Set Up --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
#library(GCAM2Hector)
library(rgcam)


#remotes::install_github("jgcri/hector@dev")
library(hector)
devtools::load_all("/Users/dorh012/Documents/2025/GCAM2Hector")


# 1. Extract Climate Results ---------------------------------------------------

prj_file <- file.path("data", "gcam-old.dat")
db_dir <- "~/Documents/GCAM-WD/CMPs/CMP406/Gv8-Hv3.2"

get_all_queries(db_dir = db_dir, 
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


gcam_emiss %>% 
  filter(variable == EMISSIONS_BC()) %>% 
  ggplot() + 
  geom_point(aes(year, value))


write.csv(gcam_emiss, file = file.path("data", "old_extracted_gcam_emiss.csv"), row.names = FALSE)




