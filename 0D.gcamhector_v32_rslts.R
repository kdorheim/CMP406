# Process the older stand alone hector version V32, this will help us determine 
# if the changes in hector dev/paramterization has lead to a cooler or warmer 
# hector. This is a bit funky because the calibration of gcam-hector in this case 
# was the same as the V3.2.0 release (which is not necessarily the case) so 
# here we are able to pull from the hector run archive! This script does require 
# that the gcamhector output stream existis. 
# 
# V3.2.0 has some R dependency issues so if you want to run hector locally 
# the easiest thing to do is to check out v3.2.0-12-g4bc2383 build and run 
# from command line. 
# 
# 
# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)


# This version of hector does not matter, since we are just using it for the 
# helper functions. 
library(hector) 

DATES <- 1750:2100
HIST_DATES <- 1750:2005


VARS <- c(GLOBAL_TAS(), RF_TOTAL(), RF_CO2(), RF_VOL(), "RF_CH4",
          CONCENTRATIONS_CH4(), "TAU_OH", CONCENTRATIONS_N2O(), RF_N2O(),
          CONCENTRATIONS_CO2(), HEAT_FLUX(), GMST())

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
  filter(variable %in% c(RF_TOTAL(), GLOBAL_TAS())) %>%
  pivot_longer(cols = starts_with("X")) %>%
  mutate(year = as.integer(gsub(x = name, pattern = "X", replacement = ""))) %>%
  select(version, scenario, variable, units, value, year) %>% 
  mutate(source = "hector") %>% 
  mutate(version = paste0("V", version)) -> 
  idealized_v32


# 4. AR6 Outputs ---------------------------------------------------------------

url("https://zenodo.org/records/17459384/files/AR6_benchmarks-V3.2.0.csv") %>%
  read.csv() %>% 
  select(-commit) -> 
  AR6_rslts


# 5. Save Outputs --------------------------------------------------------------

bind_rows(gcamhist, ssp_v32) %>% 
  mutate(version = paste0("V", version)) -> 
  out 

write.csv(out, file = file.path("data", "hector_v320_rslts.csv"), row.names = FALSE)
write.csv(idealized_v32, file = file.path("data", "hector_v320_idealized_rslts.csv"), row.names = FALSE)
write.csv(AR6_rslts, file = file.path(WIRTE_TO, "hector_v320_AR6_rslts.csv"), row.names = FALSE)

