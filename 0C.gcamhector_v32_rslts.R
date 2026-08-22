# Run the old version of Hector with the old input files. To get a comparison 
# of how history in GCAM is going to change. 
# PROBLEM the precise V3.2.0 version was not tagged properly so this is not 
# reproducible. This should not be re run. 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
remotes::install_github("jgcri/hector@a8247808f58df0e620bd61dfc91096b827f6d84b")
#library(hector)

DATES <- 1750:2100
HIST_DATES <- 1750:2005


VARS <- c(GLOBAL_TAS(), RF_TOTAL(), RF_CO2(), RF_VOL(), RF_CH4(), 
          CONCENTRATIONS_CH4(), "TAU_OH", CONCENTRATIONS_N2O(), RF_N2O(), 
          CONCENTRATIONS_CO2(), HEAT_FLUX(), GMST())

# 1. Run old hector ------------------------------------------------------------

if(FALSE){
  
  # Run all of the SSP scenarios just in case we want to include them... 
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
  
  
  # Recall that GCAM users are going to be looking at the temp anomaly relative
  # to the 1850:1900 reference period, let's make sure that we normalize those temp 
  # results. 
  old_hist %>% 
    filter(year %in% 1850:1900) %>% 
    filter(variable == GLOBAL_TAS()) %>% 
    summarise(ref = mean(value), .by = variable) -> 
    ref_value 
  
  rbind(full_out, old_hist) %>% 
    full_join(ref_value) %>% 
    replace(is.na(.), 0) %>% 
    mutate(value = value - ref) %>% 
    select(-ref) -> 
    out
  
  
  # 2. Save Results --------------------------------------------------------------
  
  write.csv(out, file = file.path("data", "hector_v32_rslts.csv"), row.names = FALSE)
}


# 3. Idealized Runs ------------------------------------------------------------

# From the hector run archive get the v3.2.0 release results for the idealized 
# scenarios. 
url("https://zenodo.org/records/17459384/files/output-V3.2.0.csv") %>% 
  read.csv() %>%  
  filter(scenario %in% c("abruptx4CO2", "abruptx2CO2" ,"1pctCO2")) %>% 
  filter(variable %in% c(RF_TOTAL(), GLOBAL_TAS())) %>% 
  pivot_longer(cols = starts_with("X")) %>% 
  mutate(year = as.integer(gsub(x = name, pattern = "X", replacement = ""))) %>% 
  select(version, scenario, variable, units, value, year) -> 
  idealized_v32

write.csv(idealized_v32, file = file.path("data", "hector_v320_idealized_rslts.csv"), row.names = FALSE)

