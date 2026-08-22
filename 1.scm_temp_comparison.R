# Hector, Magicc, Fair comparison 


# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)

# Read in the new data
"data/hector_v350_rslts.csv" %>% 
  read.csv() %>% 
  filter(scenario == "gcam-hist") %>% 
  filter(variable == GLOBAL_TAS()) -> 
  new

# Read in the old data! 
"old-GCAM/gcam-hector-outputstream.csv" %>% 
  read.csv(comment.char = "#") %>% 
  mutate(scenario = "gcam-hist", 
         version = "V3.2.0", 
         source = "hector") %>% 
  filter(spinup == 0) %>% 
  select(scenario, year, variable, value, units, source, version) %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(scenario == "gcam-hist") %>% 
  filter(year <= 2020) -> 
  hector_temp_data


# Read in the maggic & fair data for the comparison. 
c("data/hector_fair.csv", "data/hector_magicc.csv") %>% 
  lapply(read.csv) %>% 
  bind_rows %>% 
  filter(scenario == "ssp119") %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(year <= 2015) %>%
  filter(model !=  "hector 3.5.0") %>% 
  select(-scenario) %>% 
  distinct %>% 
  mutate(scenario = "hist") %>% 
  rename(version = model) -> 
  other_scm_data


bind_rows(new, hector_temp_data, other_scm_data) -> 
  temp_data
  

temp_data %>% 
  filter(year %in% 1850:1900) %>% 
  summarise(ref = mean(value), .by = c(scenario, variable, source, version)) -> 
  ref_vals

temp_data %>% 
  left_join(ref_vals, by = join_by(scenario, variable, source, version)) %>%
  mutate(value = value - ref) %>% 
  select(year, variable, value, version) -> 
  to_plot


to_plot %>% 
  filter(year >= 1850) %>% 
  ggplot(aes(year, value, color = version)) + 
  geom_line() +
  labs(title = "GMAT Comparison", y = "temp anomoly deg C (relative to 1850-1900)", 
       x = NULL)


