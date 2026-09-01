# Extract the results from the old data bases,this script assumes that the 
# GCAM databases have been downloaded from pic. This is what we are going 
# to be comparing the dev results to. 

# 0. Set Up --------------------------------------------------------------------
# Load the required CRAN packages
library(dplyr)
library(ggplot2)
library(here)

# The JGCRI packages
library(rgcam)
library(hector)

# DIR where the GCAM DB lives. 
GCAM_DB_DIR <- here::here("master-GCAM")
DATA_DIR <- here::here("data")

# 1. Main Chunk  ------------------------------------------------------------------

# Load the mapping file to convert from GCAM units and variable to hector. 
here("data", "auxiliary", "GCAM_hector_emissions_map.csv") %>% 
  read.csv -> 
  emission_mapping 

# Connect to the hector query. 
query_file <- here("data", "auxiliary", "hector-queries.xml")


# These results are structured differently, which each SSP being saved
# into a different directory. 
bd_names <- c(paste0("GCAM_SSP", 1:5), "GCAM_DB")

# The data frame to save all the results in 
rslts <- data.frame()

for(db in bd_names){
  
  # Helper message 
  print(db)
  
  # Connect to GCAM database 
  conn       <- localDBConn(GCAM_DB_DIR, db)
  prj_file   <- file.path(GCAM_DB_DIR, paste0(db, "_db.dat"))
  scn_names  <- listScenariosInDB(conn)$name
  
  # Extract the scenarios and query files 
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
    mutate(version = "old") -> 
    gcam_non_CO2emiss
  
  # Now let's take a look at the CO2 emissions 
  getQuery(prj_data,  "CO2 emissions by region") %>%  
    left_join(emission_mapping, by = join_by(ghg)) %>% 
    mutate(value = value * unit.conv) %>% 
    summarise(value = sum(value), .by = c(hector.units, scenario, hector.name, year)) %>% 
    select(year, value, variable = hector.name, units = hector.units, scenario) %>%  
    mutate(source = "GCAM") %>% 
    mutate(version = "old") -> 
    gcam_CO2emiss
  
  
  # Now let's take a look at the LUC emissions 
  getQuery(prj_data,  "luc_emissions") %>%  
    mutate(ghg = "luc_emissions") %>% 
    left_join(emission_mapping, by = join_by(ghg)) %>% 
    mutate(value = value * unit.conv) %>% 
    summarise(value = sum(value), .by = c(hector.units, scenario, hector.name, year)) %>% 
    select(year, value, variable = hector.name, units = hector.units, scenario) %>%  
    mutate(source = "GCAM") %>% 
    mutate(version = "old") -> 
    gcam_LUCemiss
  
  
  # Combine into a single data frame 
  gcam_emissions <- rbind(gcam_non_CO2emiss, gcam_CO2emiss, gcam_LUCemiss)
  
  # Queries that are more hector focused rather than gcam emissions. 
  hector_queries <- c("CO2_concentration", "RF_aci", "RF_OC", "RF_H2O_strat",              
                      "RF_O3_trop", "RF_BC", "RF_SO2", "RF_NH3",                    
                      "RF_N2O", "RF_CH4", "RF_CO2", "RF_tot",                    
                      "gmst")
  
  # Extract all of the GCAM queries and format. 
  lapply(hector_queries, function(q){
    
    getQuery(projData = prj_data, query = q) %>% 
      mutate(variable = q, version = "old", source = "GCAM") %>% 
      rename(units = Units)
    
  }) %>% 
    bind_rows -> 
    hector_rslts
  
  # Save the results 
  out <- bind_rows(hector_rslts, gcam_emissions)
  rslts <- rbind(rslts, out)
  
}


# Format and save results 

rslts %>% 
  mutate(scenario = if_else(scenario == "GCAM", "Reference", scenario)) %>% 
  filter(year > 1975) -> 
  rslts

write.csv(rslts, file = file.path("data", "GCAM_old_rslts.csv"), 
          row.names = FALSE)



