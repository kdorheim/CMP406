# Run the new version of Hector with the new input files and the input parameter 
# values, this will help us get a sense of how things are changing.... 

# 0. Set Up --------------------------------------------------------------------
HECTOR_TAG <- "3.5.5"

library(dplyr)
library(ggplot2)
remotes::install_github(paste0("jgcri/hector@v", HECTOR_TAG))
library(hector)

# Some custom helper functions that are used to calculate the summary stats 
# we compare with the AR6 benchmarks. 
source("0.AR6_benchmark_fxns.R")

# Set up some paths
WIRTE_TO   <- here::here("data")
DEV_INPUTS <- here::here("inputs", "dev")
INPUTS     <- here::here("inputs")

# Data and variables to use 
DATES      <- 1750:2100
HIST_DATES <- 1750:2022
VARS       <- c("global_tas", "RF_tot", "RF_CO2", "RF_vol", "RF_CH4", "CH4_concentration", "TAU_OH", "N2O_concentration",
                "RF_N2O", "CO2_concentration", "heatflux", "gmst", "RF_BC", "RF_OC", "RF_NH3", "RF_SO2",
                "RF_aci", "RF_O3_trop", "RF_H2O_strat", "RF_albedo", "RF_misc")


# Define the parameters used in the gcam-hector coupling. 
list.files(DEV_INPUTS, pattern = "hector_params.csv", full.names = TRUE) %>% 
  read.csv -> 
  PARS


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

# Run all of the SSP scenarios just in case we want to include them... 
inis <- list.files(system.file(package = "hector", "input"), 
                   pattern = "ssp", full.names = TRUE)

lapply(inis, function(ini){
  scn <- gsub(basename(ini), replacement = "", pattern = "hector_|.ini")
  
  
  hc <- my_setvar_fxn(newcore(ini))
  
  run(hc)
  fetchvars(hc, DATES, VARS) %>% 
    mutate(scenario = scn) %>% 
    mutate(source = "hector", 
           version = paste0("V", HECTOR_TAG)) -> 
    old
  
  return(old)
  
}) %>% 
  bind_rows -> 
  full_out

# 2. GCAM historical  ----------------------------------------------------------

# Run gcam-history 
ini <- file.path(DEV_INPUTS, "hector-gcam.ini")
hc <- newcore(ini)
run(hc, runtodate = max(HIST_DATES))
fetchvars(hc, HIST_DATES, VARS) %>% 
  mutate(scenario = "gcam-hist") %>% 
  mutate(source = "hector", 
         version = paste0("V", HECTOR_TAG)) -> 
  gcam_hist

# # Recall that GCAM users are going to be looking at the temp anomaly relative
# # to the 1850:1900 reference period, let's make sure that we normalize those temp 
# # results. 
# gcam_hist %>% 
#   filter(year %in% 1850:1900) %>% 
#   filter(variable == GLOBAL_TAS()) %>% 
#   summarise(ref = mean(value), .by = variable) -> 
#   ref_value 
# 
# gcam_hist %>% 
#   filter(variable == GLOBAL_TAS()) %>% 
#   full_join(ref_value) %>% 
#   replace(is.na(.), 0) %>% 
#   mutate(value = value - ref) %>% 
#   select(-ref) -> 
#   normalized_global_tas
# 
# gcam_hist %>% 
#   filter(!variable == GLOBAL_TAS()) %>% 
#   bind_rows(normalized_global_tas) -> 
#   gcam_hist

# 3. Idealized Inputs ---------------------------------------------------------
variables <- c("global_tas", "gmst", "land_tas", "RF_tot",
               "CO2_concentration", "RF_CO2", "NPP", "veg_c",
               "soil_c", "detritus_c", "sst", "heatflux", 
               "HL_sst", "LL_sst" , "HL_ocean_c", "LL_ocean_c", 
               "IO_ocean_c", "DO_ocean_c", "CH4_concentration",
               "HL_ocean_uptake", "ocean_uptake")    


data.frame(inis = list.files(DEV_INPUTS, pattern = "ini", full.names = TRUE)) %>% 
  filter(!grepl(pattern = "gcam", x = inis)) %>% 
  pull(inis) %>% 
  lapply(function(ini){
    print(ini)
    scn <- gsub(x = basename(ini), replacement = "", pattern = "hector_|.ini")
    hc <- my_setvar_fxn(newcore(ini, name = scn))
    run(hc, hc$enddate)
    fetchvars(hc, 1750:hc$enddate, variables)
    
  }) %>% 
  bind_rows -> 
  idealized_v35

idealized_v35 %>% 
  mutate(version = paste0("V", HECTOR_TAG), 
         source = "hector") -> 
  idealized_v35


# 4. AR6 Benchmark ----------------------------------------------------------

# Create a temp file to save the hector results in that are required for the 
# AR6 results. 
temp_file <- tempfile()

rbind(full_out, idealized_v35, gcam_hist) %>% 
  select(version, value, year, variable, scenario, units) %>% 
  mutate(year = paste0("X", year)) %>% 
  pivot_wider(names_from = year) %>%  
  mutate(version = HECTOR_TAG) -> 
  o 

write.csv(o, file = temp_file, row.names = FALSE)
ar6_out <- get_AR6_benchmarks(file = temp_file)

# Remove the temp file as part of clean up!
file.remove(temp_file)

# z. Save Results --------------------------------------------------------------

out <- rbind(full_out, gcam_hist)
write.csv(out, file = file.path(WIRTE_TO, "hector_v355_rslts.csv"), row.names = FALSE)
write.csv(idealized_v35, file = file.path(WIRTE_TO, "hector_v355_idealized_rslts.csv"), row.names = FALSE)
write.csv(ar6_out, file = file.path(WIRTE_TO, "hector_v355_AR6_rslts.csv"), row.names = FALSE)
