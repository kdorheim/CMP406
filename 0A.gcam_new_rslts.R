# Extract the results from the GCAM database! This script assumes that 
# GCAM has already been run locally with the new model development we
# would like to asses as part of the CMP. 

# 0. Set Up --------------------------------------------------------------------
# Load the required CRAN packages
library(dplyr)
library(ggplot2)
library(here)

# The JGCRI packages
library(rgcam)
library(hector)

# DIR where the GCAM DB lives. 
GCAM_DB_DIR <- here::here("dev-GCAM")
DATA_DIR <- here::here("data")

# 1. Extract Emissions ---------------------------------------------------------

# Load the mapping file to convert from GCAM units and variable to hector. 
here("data", "auxiliary", "GCAM_hector_emissions_map.csv") %>% 
  read.csv -> 
  emission_mapping 

# Connect to GCAM database 
conn       <- localDBConn(GCAM_DB_DIR, "database_basexdb")
prj_file   <- file.path(GCAM_DB_DIR, "gcam_db.dat")
scn_names  <- listScenariosInDB(conn)$name
query_file <- here("data", "auxiliary", "hector-queries.xml")

lapply(scn_names, function(name){
  gcam_data <- addScenario(conn = conn,
                           proj = prj_file,
                           scenario = name, 
                           queryFile = query_file)
  return(invisible())
})

# Extract emissions so can look at the transition period from historical to future. 
prj_data <- loadProject(prj_file)

# First do the non CO2 emissions. 
getQuery(prj_data, "nonCO2 emissions by region") %>% 
  left_join(emission_mapping, by = join_by(ghg)) %>% 
  mutate(value = value * unit.conv) %>% 
  summarise(value = sum(value), .by = c(hector.units, scenario, hector.name, year)) %>% 
  select(year, value, variable = hector.name, units = hector.units, scenario) %>%  
  mutate(source = "GCAM") %>% 
  mutate(version = "new") -> 
  gcam_non_CO2emiss

# Now let's take a look at the CO2 emissions 
getQuery(prj_data,  "CO2 emissions by region") %>%  
  left_join(emission_mapping, by = join_by(ghg)) %>% 
  mutate(value = value * unit.conv) %>% 
  summarise(value = sum(value), .by = c(hector.units, scenario, hector.name, year)) %>% 
  select(year, value, variable = hector.name, units = hector.units, scenario) %>%  
  mutate(source = "GCAM") %>% 
  mutate(version = "new") -> 
  gcam_CO2emiss

# Now let's take a look at the LUC emissions 
getQuery(prj_data,  "luc_emissions") %>%  
  mutate(ghg = "luc_emissions") %>% 
  left_join(emission_mapping, by = join_by(ghg)) %>% 
  mutate(value = value * unit.conv) %>% 
  summarise(value = sum(value), .by = c(hector.units, scenario, hector.name, year)) %>% 
  select(year, value, variable = hector.name, units = hector.units, scenario) %>%  
  mutate(source = "GCAM") %>% 
  mutate(version = "new") -> 
  gcam_LUCemiss

# Combine into a single data frame 
gcam_emissions <- rbind(gcam_non_CO2emiss, gcam_CO2emiss, gcam_LUCemiss)


# 2. Extract Hector Output -----------------------------------------------------
# Queries that are more hector focused rather than gcam emissions. 
hector_queries <- c("CO2_concentration", "RF_aci", "RF_OC", "RF_H2O_strat",              
                    "RF_O3_trop", "RF_BC", "RF_SO2", "RF_NH3",                    
                    "RF_N2O", "RF_CH4", "RF_CO2", "RF_tot",                    
                    "gmst")

# Extract all of the GCAM queries and format. 
lapply(hector_queries, function(q){
  
  getQuery(projData = prj_data, query = q) %>% 
    mutate(variable = q, version = "new", source = "GCAM") %>% 
    rename(units = Units)
  
}) %>% 
  bind_rows -> 
  hector_rslts

# 3. Save Output  --------------------------------------------------------------

# Format the extracted results into a data frame that we can plot in a latter 
# stage. 
gcam_emissions %>% 
  bind_rows(hector_rslts) %>% 
  filter(year > 1975) -> 
  rslts

write.csv(rslts, file = file.path("data", "GCAM_new_rslts.csv"), 
          row.names = FALSE)




