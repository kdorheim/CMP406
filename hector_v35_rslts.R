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
          CONCENTRATIONS_CH4(), "TAU_OH", CONCENTRATIONS_N2O(), RF_N2O(), 
          CONCENTRATIONS_CO2(), HEAT_FLUX())

# Using the input 
# TODO there should be a better way to do this incase needs updating 
PARS <- c("diff" = 0.687, "beta" = 	0.785, "q10_rh" = 1.245)

# Helper function that sets an active Hector core with user defined parameters
# Args
#   hc: active hector core
#   pars: vector of the hector parameter values
# Returns: an active hector core with the new parameter values
my_setvar_fxn <- function(hc, pars = PARS){
  
  print(pars)
  
  
  for(i in 1:length(pars)){
    
    var <- names(pars)[[i]]
    val <- pars[[i]]
    
    setvar(core = hc, dates = NA, var = var,
           values = val, unit = getunits(var))
    reset(hc)
    
  }
  
  return(hc)
  
  
}

# 1. SSP Runs ------------------------------------------------------------

# Run all of the SSP scenarios just incase we want to include them... 
inis <- list.files(system.file(package = "hector", "input"), 
                   pattern = "ssp", full.names = TRUE)

lapply(inis, function(ini){
  scn <- gsub(basename(ini), replacement = "", pattern = "hector_|.ini")
  

  hc <- my_setvar_fxn(newcore(ini))
  
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

# 2. GCAM historical  ----------------------------------------------------------

# Run gcam-history 
ini <- here::here("inputs", "new", "hector-gcam.ini")
hc <- newcore(ini)
run(hc, runtodate = max(HIST_DATES))
fetchvars(hc, HIST_DATES, VARS) %>% 
  mutate(scenario = "gcam-hist") %>% 
  mutate(source = "hector", 
         version = "V3.4.9") -> 
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


# 3. Idealized Inputs ---------------------------------------------------------
devtools::load_all("~/Documents/Hector-WD/hector/")

variables <- c("global_tas", "gmst", "land_tas", "RF_tot",
               "CO2_concentration", "RF_CO2", "NPP", "veg_c",
               "soil_c", "detritus_c", "sst", "heatflux", 
               "HL_sst", "LL_sst" , "HL_ocean_c", "LL_ocean_c", 
               "IO_ocean_c", "DO_ocean_c", "CH4_concentration",
               "HL_ocean_uptake", "ocean_uptake")    





c("inputs/hector_abruptx4CO2.ini", 
  "inputs/hector_abruptx2CO2.ini", 
  "inputs/hector_1pctCO2.ini") %>% 
  lapply(function(ini){
    
    scn <- gsub(x = basename(ini), replacement = "", pattern = "hector_|.ini")
    hc <- my_setvar_fxn(newcore(ini, name = scn))
    run(hc, 2100)
    fetchvars(hc, 1750:2100, variables)
    
  }) %>% 
  bind_rows -> 
  idealized_v35

idealized_v35 %>% 
  mutate(version = "V3.5.0", 
         source = "hector") -> 
  idealized_v35


# z. Save Results ------------------------------------------------------------

out <- rbind(full_out, old_hist)
write.csv(out, file = file.path("data", "hector_v349_rslts.csv"), row.names = FALSE)
write.csv(idealized_v35, file = file.path("data", "hector_v349_idealized_rslts.csv"), row.names = FALSE)
