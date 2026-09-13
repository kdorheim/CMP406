# This script does a series of where only one input is changed at a time, which 
# let's us quantify the effect the change in emissions has on total RF and gmst. 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(tidyr)
library(scales)
library(here)
library(ggpp)
library(patchwork)


# Important note, that the specific hector version does not matter here since 
# we are just using the helper functions. 
library(hector) 

# Define the location where to write the files out to 
MAIN_FIGS <- here::here("figs")
SUP_FIGS  <- here::here("figs", "supplemental_figs")
dir.create(MAIN_FIGS, showWarnings = FALSE)
dir.create(SUP_FIGS, showWarnings = FALSE)

# Figure settings 
theme_set(theme_bw() + theme(legend.title = element_blank()))
ggplot_colors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, c = 100, l = 65)[1:n]
}

my_colors <- ggplot_colors(n = 2)
COLOR_SCHEME <- c("old" = my_colors[1], "new" = my_colors[2], 
                  "V3.2.0" = my_colors[1], "V3.5.5" = my_colors[2], 
                  "cmip7" = "black", 
                  "BerkeleyEarthGlobal" = "black", 
                  "HadCRUT5Global" = "black", 
                  "NOAAGlobalTempGlobal" = "black")
# Define the GCAM color palette 
gcam_color_palette <- ggplot_colors(n = 6)
GCAM_COLOR_SCHEME <- c(gcam_color_palette)
names(GCAM_COLOR_SCHEME) <- c(paste0("GCAM_SSP", 1:5), "Reference")
GCAM_COLOR_SCHEME <- c(GCAM_COLOR_SCHEME, "pre-gcam hist." = "black")

# Global options for plotting 
TYPE <- "png"
WIDTH <- 6 
HEIGHT <- 4

# Options for cleaning up the environment 
CLEAN_UP <- FALSE


## 1A. Load Data  ---------------------------------------------------------------
here("inputs", "dev") %>% 
  list.files(pattern = "default_inputs|gcam_emissions", full.names = TRUE) %>% 
  lapply(function(f){
    
    read.csv(f, comment.char = ";") %>% 
      pivot_longer(-Date, names_to = "variable") %>% 
      rename(year = Date) 
    
  }) %>% 
  bind_rows %>% 
  mutate(source = "new") -> 
  new_inputs

here("inputs", "old") %>% 
  list.files(pattern = "default_emissions|gcam_emissions", full.names = TRUE) %>% 
  lapply(function(f){
    
    read.csv(f, comment.char = ";") %>% 
      pivot_longer(-Date, names_to = "variable") %>% 
      rename(year = Date) 
    
  }) %>% 
  bind_rows %>% 
  mutate(source = "old") -> 
  old_inputs

# Manually add the natural emissions that were hard coded in the older 
# gcam-hector implementation. 
bind_rows(data.frame(year = unique(old_inputs$year), 
                     variable = NATURAL_CH4(), 
                     value = 335, 
                     source = "old"), 
          data.frame(year = unique(old_inputs$year), 
                     variable = NAT_EMISSIONS_N2O(), 
                     value = 9.7, 
                     source = "old"), 
          data.frame(year = unique(old_inputs$year), 
                     variable = "RF_misc", 
                     value = 0, 
                     source = "old"), 
          old_inputs) -> 
  old_inputs


### 1B. Change in emissions ----------------------------------------------------
# Calculate the RMSE between the new and the old inputs. 
bind_rows(new_inputs, old_inputs) %>% 
  pivot_wider(names_from = source, values_from = value) %>%
  na.omit %>% 
  mutate(SE = (new - old)^2) %>%
  summarise(RMSE = sqrt(mean(SE)), .by = c(variable)) %>%
  mutate(RMSE = signif(RMSE, digits = 3)) %>% 
  mutate(units = getunits(variable)) -> 
  inputs_RMSE_df

# Determine which of the inputs have no change in the values
inputs_RMSE_df %>% 
  filter(RMSE <= 0) %>% 
  pull(variable) %>% 
  paste0(., collapse = ", ")


# Which inputs do have an effects on the historical hector behavior 
inputs_RMSE_df %>% 
  filter(RMSE >= 1e-6) %>% 
  arrange(desc(RMSE)) %>% 
  rename(RMSE_emiss = RMSE) -> 
  change_in_emissions

### 1C. Base Line Results ------------------------------------------------------

ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(ini)
run(hc, runtodate = 2025)
fetchvars(hc, 1745:2025, vars = c(RF_TOTAL(), GMST())) %>% 
  select(year, base = value, output = variable) -> 
  base_line_output


### 1D. Run with New Emissions -------------------------------------------------

# Helper function that quickly sets up a new hector core using the 
# baseline ssp245 emissions 
my_new_core <- function(){
  
  ini <- system.file(package = "hector", "input/hector_ssp245.ini")
  hc <- newcore(ini)
  return(hc)
  
}

# Define the max year to run to and query results for 
MAX_YR <- 2023

# Create a data frame to store the results in. 
output <- data.frame() 

for (em in change_in_emissions$variable){
 
  hc <- my_new_core()
  
  # Select the new emissions to be fed into the hector core. 
  new_inputs %>% 
    filter(variable == em) -> 
    new_emiss_to_use
  
  setvar(core = hc, dates = new_emiss_to_use$year, var = em, 
         values = new_emiss_to_use$value, unit = getunits(em))
  reset(hc)
  run(hc, MAX_YR)
  
  fetchvars(hc, 1745:MAX_YR, vars = c(RF_TOTAL(), GMST())) %>% 
    select(year, new = value, output = variable) %>% 
    mutate(variable = em) -> 
    out 
  
  # clean up, make sure we are not reusing the same core
  shutdown(hc)
  
  output <- rbind(output, out)
  
}

### 1E.Quantify RMSE -----------------------------------------------------------

# Calculate the RMSE 
output %>% 
  full_join(base_line_output, by = join_by(year, output)) %>% 
  mutate(SE = (base - new)^2) %>%
  summarise(RMSE = sqrt(mean(SE)), .by = c(variable, output)) %>%
  na.omit %>% 
  mutate(RMSE = signif(RMSE, digits = 3)) %>% 
  pivot_wider(names_from = output, values_from = RMSE) -> 
  hector_rslt_RMSE

change_in_emissions %>% 
  left_join(hector_rslt_RMSE) %>% 
  na.omit %>% 
  select(input = variable, RMSE_emiss, RF_tot, gmst) %>% 
  arrange(desc(RMSE_emiss)) %>% 
  knitr::kable(digits = 3)







