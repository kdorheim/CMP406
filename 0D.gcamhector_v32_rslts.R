# Process the older stand alone hector version V32, this will help us determine 
# if the changes in hector dev/paramterization has lead to a cooler or warmer 
# hector. This is a bit funky because the calibration of gcam-hector in this case 
# was the same as the V3.2.0 release (which is not necessarily the case) so 
# here we are able to pull from the hector run archive! This script does require 
# that the gcamhector output stream existis. 
# 
# V3.2.0 has some R dependency issues so if you want to run hector locally 
# the easiest thing to do is to check out v3.2.0-4bc2383 build and run 
# from command line. 
# 
# 
# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

source("0.AR6_benchmark_fxns.R")


# This version of hector does not matter, since we are just using it for the 
# helper functions. 
library(hector) 

DATES <- 1750:2100
HIST_DATES <- 1750:2005


VARS       <- c("global_tas", "RF_tot", "RF_CO2", "RF_vol", "RF_CH4", "CH4_concentration", "TAU_OH", "N2O_concentration",
                "RF_N2O", "CO2_concentration", "heatflux", "gmst", "RF_BC", "RF_OC", "RF_NH3", "RF_SO2",
                "RF_aci", "RF_O3_trop", "RF_H2O_strat", "RF_albedo", "RF_misc")

# 1. hector-run-archive data set -----------------------------------------------
url("https://zenodo.org/records/17459384/files/output-V3.2.0.csv") %>%
  read.csv() -> 
  hector_output

# 1. SSPs ----------------------------------------------------------------------

hector_output %>% 
  filter(grepl(pattern = "ssp", x = scenario)) %>% 
  mutate(variable = if_else(variable == "FCH4", "RF_CH4", variable)) %>% 
  pivot_longer(cols = starts_with("X")) %>%
  mutate(year = as.integer(gsub(x = name, pattern = "X", replacement = ""))) %>%
  filter(variable %in% VARS) %>% 
  select(version, scenario, variable, units, value, year) %>% 
  mutate(source = "hector") -> 
  ssp_v32

# 2. hector-gcam ---------------------------------------------------------------

here("master-GCAM") %>% 
  list.files("gcamhector_out", full.names = TRUE) %>% 
  read.csv(comment.char = "#") %>% 
  filter(spinup == 0) %>% 
  mutate(variable = if_else(variable == "FCH4", "RF_CH4", variable)) %>% 
  filter(year %in% HIST_DATES) %>% 
  filter(variable %in% VARS) %>% 
  select(variable, units, value, year) %>% 
  mutate(version = "3.2.0",
         source = "hector", 
         scenario = "gcam-hist") -> 
  gcamhist

# 3. Idealized Runs ------------------------------------------------------------

# From the hector run archive get the v3.2.0 release results for the idealized
# scenarios.
hector_output %>% 
  filter(scenario %in% c("abruptx4CO2", "abruptx2CO2" ,"1pctCO2")) %>%
  filter(variable %in% c(RF_TOTAL(), GLOBAL_TAS(), CONCENTRATIONS_CO2())) %>%
  pivot_longer(cols = starts_with("X")) %>%
  mutate(year = as.integer(gsub(x = name, pattern = "X", replacement = ""))) %>%
  select(version, scenario, variable, units, value, year) %>% 
  mutate(source = "hector") %>% 
  mutate(version = paste0("V", version)) -> 
  idealized_v32


# 4. AR6 Outputs ---------------------------------------------------------------

# Calculate the AR6 results. 
# Create a temp file to save the hector results in that are required for the 
# AR6 results. 
ssp_v32 %>% 
  filter(scenario == "ssp245") %>% 
  mutate(scenario = if_else(scenario == "ssp245", "gcam-hist", scenario)) -> 
  fake_gcam_hist
  
rbind(ssp_v32, idealized_v32, fake_gcam_hist) %>% 
  select(version, value, year, variable, scenario, units) %>% 
  mutate(year = paste0("X", year)) %>% 
  pivot_wider(names_from = year) %>%  
  mutate(version = "3.2.0") -> 
  o 

temp_file <- tempfile()

write.csv(o, file = temp_file, row.names = FALSE)
AR6_rslts <- get_AR6_benchmarks(file = temp_file)

# Remove the temp file as part of clean up!
file.remove(temp_file)

# 5. Save Outputs --------------------------------------------------------------

bind_rows(gcamhist, ssp_v32) %>% 
  mutate(version = paste0("V", version)) -> 
  out 

write.csv(out, file = file.path("data", "hector_v320_rslts.csv"), row.names = FALSE)
write.csv(idealized_v32, file = file.path("data", "hector_v320_idealized_rslts.csv"), row.names = FALSE)
write.csv(AR6_rslts, file = file.path("data", "hector_v320_AR6_rslts.csv"), row.names = FALSE)

