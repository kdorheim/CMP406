# Run the old version of Hector with the old input files. To get a comparison 
# of how history in GCAM is going to change. 


# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
remotes::install_github("jgcri/hector@main")
library(hector)

DATES <- 1750:2100
HIST_DATES <- 1750:2005


VARS <- c(GLOBAL_TAS(), RF_TOTAL(), RF_CO2(), RF_VOL(), RF_CH4(), 
          CONCENTRATIONS_CH4(), "TAU_OH", CONCENTRATIONS_N2O(), RF_N2O())

# 1. Run old hector ------------------------------------------------------------

# Run all of the SSP scenarios just incase we want to include them... 
inis <- list.files(system.file(package = "hector", "input"), 
                   pattern = "ssp", full.names = TRUE)

lapply(inis, function(ini){
  scn <- gsub(basename(ini), replacement = "", pattern = "hector_|.ini")
  
  hc <- newcore(ini)
  run(hc)
  fetchvars(hc, DATES, VARS) %>% 
    mutate(scenario = scn) %>% 
    mutate(source = "hector", 
           version = "V3.2.0") -> 
    old
  
  return(old)
  
}) %>% 
  bind_rows -> 
  full_out



# Run gcam-history 
ini <- here::here("inputs", "old", "hector-gcam.ini")
hc <- newcore(ini)
run(hc, runtodate = max(HIST_DATES))
fetchvars(hc, HIST_DATES, VARS) %>% 
  mutate(scenario = "gcam-hist") %>% 
  mutate(source = "hector", 
         version = "V3.2.0") -> 
  old_hist


# 2. Save Results ------------------------------------------------------------

out <- rbind(full_out, old_hist)
write.csv(out, file = file.path("data", "hector_v32_rslts.csv"), row.names = FALSE)

