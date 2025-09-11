# Run the new version of Hector with the new input files. To get a comparison 
# of how history in GCAM is going to change. 


# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
# Problem is that this version of Hector is not calibrated yet! 
remotes::install_github("jgcri/hector@dev")
library(hector)

DATES <- 1750:2100
HIST_DATES <- 1750:2022

VARS <- c(GLOBAL_TAS(), RF_TOTAL(), RF_CO2(), RF_VOL(), RF_CH4(), 
          CONCENTRATIONS_CH4(), "TAU_OH", CONCENTRATIONS_N2O(), RF_N2O())

# 1. SSP Runs ------------------------------------------------------------

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
           version = "V3.4.9") -> 
    old
  
  return(old)
  
}) %>% 
  bind_rows -> 
  full_out

# 1. GCAM historical  ----------------------------------------------------------

# Run gcam-history 
ini <- here::here("inputs", "new", "hector-gcam.ini")
hc <- newcore(ini)
run(hc, runtodate = max(HIST_DATES))
fetchvars(hc, HIST_DATES, VARS) %>% 
  mutate(scenario = "gcam-hist") %>% 
  mutate(source = "hector", 
         version = "V3.4.9") -> 
  old_hist


# 2. Save Results ------------------------------------------------------------

out <- rbind(full_out, old_hist)
write.csv(out, file = file.path("data", "hector_v349_rslts.csv"), row.names = FALSE)

