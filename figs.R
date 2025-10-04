# Script to generated the figures associated with GCAM CMP 406

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(ggpmisc)
library(tidyr)

# the hector version does not matter here, since we are just using the 
# helper functions... 
library(hector) 

# helper function to quickly save and plots 
# Args 
#   plot: ggplot object
#   name: str name of the file 
#   type: .png, indicates the type of file to save 
#   WIDTH/HEIGHT: control the size of the plot 
# Returns: writes the plot to disk
my_ggsave <- function(plot, name, type = ".png", WIDTH = 6, HEIGHT = 4){
  
  fname <- file.path("figs", paste0(name, type))
  ggsave(plot, filename = fname, width = WIDTH, height = HEIGHT)
  
}



# 1. stand alone hector --------------------------------------------------------
## A. data ----------------------------------------------------------------------
# Load the data! 
"data/hector_v32_rslts.csv" %>% 
  read.csv() %>% 
  mutate(variable = if_else(variable == "FCH4", "RF_CH4", variable)) -> 
  old

# This should be updated! 
"data/hector_v349_rslts.csv" %>% 
  read.csv() -> 
  new

new %>% 
  bind_rows(old) -> 
  long_df

# Calculate the difference between variables. 
long_df %>% 
  pivot_wider(names_from = version, values_from = value) %>% 
  # Since the old historical run will end earlier. 
  na.omit %>% 
  mutate(error = `V3.4.9` - `V3.2.0`) -> 
  error_df

# Get the MAE for the variable and scenarios! 
error_df %>% 
  summarise(MAE = mean(abs(error)), .by = c(scenario, variable)) -> 
  MAE_df


## B. historical ----------------------------------------------------------------
# Compare the pre-GCAM historical results (these should be finalized gcam-hector)
# results. 
vars_to_plot <- unique(long_df$variable)

# Calculate the MAE to include on all the plots 
MAE_df %>%
  filter(scenario == "gcam-hist") %>%
  mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) %>% 
  mutate(MAE = signif(MAE, digits = 3)) ->
  tb

tbs <- lapply(split(tb, tb$variable), "[", -c(1,2))

df <- tibble(x = rep(Inf, length(tbs)),
             y = rep(-Inf, length(tbs)),
             variable = factor(vars_to_plot, levels = vars_to_plot, ordered = TRUE),
             tbl = tbs)


# All of the variables 
long_df %>% 
  mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) %>% 
  filter(scenario == "gcam-hist") %>% 
  ggplot(aes(year, value, color = version)) + 
  geom_line() + 
  labs(x = NULL, y = NULL, title = "GCAM Historical") + 
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  geom_table(data = df, aes(x = x, y = y, label = tbl),
             hjust = 1, vjust = 0) + 
  facet_wrap("variable", scales = "free")  -> 
  plot; plot
my_ggsave(plot, name = "gcam-hist", WIDTH = 10, HEIGHT = 10)


# Get the CMIP6 era historical GHG concentrations, note these are not 
# the concentrations we used in calibration 
system.file(package = "hector", "input/tables") %>% 
  list.files(pattern = "ssp245_emiss-constraints_rf.csv", full.names = TRUE) %>% 
  read.csv(comment.char = ";") %>% 
  select(year = Date, 
         CH4_concentration = CH4_constrain, 
         N2O_concentration = N2O_constrain, 
         CO2_concentration = CO2_constrain) %>% 
  pivot_longer(-year, names_to = "variable") %>% 
  filter(year %in% 1750:2015) -> 
  hist_values

long_df %>% 
  filter(scenario == "gcam-hist") %>% 
  filter(variable %in% c(CONCENTRATIONS_CH4(), 
                         CONCENTRATIONS_CO2(), 
                         CONCENTRATIONS_N2O())) -> 
  hector_to_plot 

# The 3 GHGs vs. observations 
ggplot() + 
  geom_line(data = hist_values, aes(year, value)) + 
  geom_line(data = hector_to_plot, aes(year, value, color = version, linetype = version), 
            linewidth = 1) + 
  labs(x = NULL, y = NULL, title = "GCAM Historical") + 
  facet_wrap("variable", scales = "free", ncol = 1) -> 
  plot; plot
my_ggsave(plot, name = "gcam-hist_ghgs", WIDTH = 5, HEIGHT = 5)


# Only [CH4] vs observations
long_df %>% 
  filter(scenario == "gcam-hist") %>% 
  filter(variable %in% CONCENTRATIONS_CH4()) %>% 
  mutate(run = if_else(version == "V3.4.9", "new", "old")) -> 
  hector_to_plot 

ggplot() + 
  geom_line(data = hist_values %>% 
              filter(variable == CONCENTRATIONS_CH4()), aes(year, value)) + 
  geom_line(data = hector_to_plot, aes(year, value, color = run), linetype = 2, 
            linewidth = 1) + 
  labs(x = NULL, y = NULL, title = "GCAM Historical") + 
  facet_wrap("variable", scales = "free", ncol = 1) -> 
  plot; plot
my_ggsave(plot, name = "gcam-hist_ch4", WIDTH = 5, HEIGHT = 5)

# Only [N2O] vs observations
 long_df %>% 
  filter(scenario == "gcam-hist") %>% 
  filter(variable %in% CONCENTRATIONS_N2O()) %>% 
  mutate(run = if_else(version == "V3.4.9", "new", "old")) -> 
  hector_to_plot 

ggplot() + 
  geom_line(data = hist_values %>% 
              filter(variable == CONCENTRATIONS_N2O()), aes(year, value)) + 
  geom_line(data = hector_to_plot, aes(year, value, color = run), linetype = 2, 
            linewidth = 1) + 
  labs(x = NULL, y = NULL, title = "GCAM Historical") + 
  facet_wrap("variable", scales = "free", ncol = 1) -> 
  plot; plot

my_ggsave(plot, name = "gcam-hist_n2o", WIDTH = 5, HEIGHT = 5)


# Natural N2O Emissions 
here::here("inputs/new/default_emissions.csv") %>% 
  read.csv(comment.char = ";") %>% 
  select(year = Date, value = N2O_natural_emissions) %>% 
  mutate(variable = NAT_EMISSIONS_N2O(), 
         source = "new") -> 
  nat_n2o_emiss

ggplot() + 
  geom_hline(aes(yintercept = 9.7, color = "old"), size = 1) + 
  geom_line(data = nat_n2o_emiss, aes(year, value, color = source), size = 0.75) + 
  theme(legend.title = element_blank()) + 
  labs(title = NAT_EMISSIONS_N2O(), 
       y = getunits(NAT_EMISSIONS_N2O()), 
       x = NULL) -> 
  plot; plot

my_ggsave(plot, name = "gcam-hist_n2o_emiss", WIDTH = 5, HEIGHT = 5)



## C. SSP Comparison  ----------------------------------------------------------

# Compare SSP results form old and new 
vars_to_plot <- unique(long_df$variable)
scns_to_plot <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434",
                  "ssp460", "ssp534-over", "ssp585")

# Calculate the MAE to include on all the plots 
MAE_df %>%
  filter(scenario %in% scns_to_plot) %>%
  mutate(MAE = signif(MAE, digits = 3)) ->
  tb

tbs <- lapply(split(tb, tb$variable), "[", -c(2))

df <- tibble(x = rep(-Inf, length(tbs)),
             y = rep(Inf, length(tbs)),
             variable = names(tbs),
             tbl = tbs)


# All of the variables 
long_df %>% 
  mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) %>% 
  filter(scenario %in% scns_to_plot) %>%
  ggplot(aes(year, value, color = version, groupby = interaction(variable, scenario))) + 
  geom_line() + 
  labs(x = NULL, y = NULL, title = "Hector SSP Comparison") + 
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  # geom_table(data = df, aes(x = x, y = y, label = tbl),
  #            hjust = 1, vjust = 0) + 
  facet_wrap("variable", scales = "free")  -> 
  plot; plot
my_ggsave(plot, name = "ssps_allvars", WIDTH = 10, HEIGHT = 10)


long_df %>% 
  filter(variable %in% c(GLOBAL_TAS(), RF_TOTAL(), HEAT_FLUX())) %>% 
  filter(scenario %in% scns_to_plot) %>%
  ggplot(aes(year, value, color = version, groupby = interaction(variable, scenario))) + 
  geom_line() + 
  labs(x = NULL, y = NULL, title = "Hector SSP Comparison") + 
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  geom_table(data = filter(df, variable %in% c(GLOBAL_TAS(), RF_TOTAL(), HEAT_FLUX())), aes(x = x, y = y, label = tbl),
             hjust = 0, vjust = 1) +
  facet_wrap("variable", scales = "free")  -> 
  plot; plot
my_ggsave(plot, name = "ssps_EBM", WIDTH = 10, HEIGHT = 5)


VARS <- c(CONCENTRATIONS_CH4(), CONCENTRATIONS_CO2(), CONCENTRATIONS_N2O())
long_df %>% 
  filter(variable %in% VARS) %>% 
  filter(scenario %in% scns_to_plot) %>%
  ggplot(aes(year, value, color = version, groupby = interaction(variable, scenario))) + 
  geom_line() + 
  labs(x = NULL, y = NULL, title = "Hector SSP Comparison") + 
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  geom_table(data = filter(df, variable %in% VARS), aes(x = x, y = y, label = tbl),
             hjust = 0, vjust = 1) +
  facet_wrap("variable", scales = "free")  -> 
  plot; plot
my_ggsave(plot, name = "ssps_GHGs", WIDTH = 10, HEIGHT = 5)


## D. Idealized ----------------------------------------------------------------

"data/output-V3.2.0.csv" %>% 
  read.csv %>% 
  filter(scenario %in% c("1pctCO2", "abruptx0p5CO2", "abruptx2CO2",
                         "abruptx4CO2", "piControl")) %>%
  pivot_longer(names_to = "year", cols = starts_with("X")) %>% 
  mutate(year = as.integer(gsub(x = year, replacement = "", pattern = "X"))) -> 
  old_idealized_rslts

"data/hector_v349_idealized_rslts.csv" %>% 
  read.csv -> 
  new_idealized_rslts

old_idealized_rslts %>% 
  bind_rows(new_idealized_rslts) %>% 
  filter(scenario %in% c("abruptx4CO2", "abruptx2CO2" ,"1pctCO2")) %>% 
  filter(variable %in% c(RF_TOTAL(), GLOBAL_TAS())) %>% 
  filter(year <= 2050) %>% 
  ggplot(aes(year, value, color = version, linetype = version)) + 
  geom_line(size = 1) + 
  facet_grid(variable ~ scenario, scales="free") + 
  labs(x = NULL, y = NULL) + 
  theme(legend.title = element_blank(), 
        legend.position = "bottom") -> 
  plot; plot

my_ggsave(plot, name = "hector-idealized", WIDTH = 8, HEIGHT = 6)




# 2. GCAM ----------------------------------------------------------------------
## A. data ---------------------------------------------------------------------
file.path("data", "gcam_climate.csv") %>% 
  read.csv -> 
  long_df


# Calculate the difference between variables. 
long_df %>% 
  select(-source) %>% 
  pivot_wider(names_from = scenario, values_from = value) %>% 
  # Since the old historical run will end earlier. 
  na.omit %>% 
  mutate(error = new - old) -> 
  error_df

# Get the MAE for the variable and scenarios! 
error_df %>% 
  summarise(MAE = mean(abs(error)), .by = c(variable)) %>% 
  mutate(scenario = "reference") -> 
  MAE_df

## B. fxn ---------------------------------------------------------------------
# Helper function for plotting, this is not a very robust function, no 
# defensive programming checks. 
# Args 
#   vars_to_plot: vector of the variable names 
#   MAE_df: data frame of the MAE
#   long_df: data frame of the GCAM results to be compared 
# Returns: ggplot figure with the MAE 
my_plot <- function(vars_to_plot, MAE_df, long_df){
  
  # Include the MAE on the plots! 
  MAE_df %>%
    filter(variable %in% vars_to_plot) %>% 
    mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) %>% 
    mutate(MAE = signif(MAE, digits = 3)) ->
    tb
  
  tbs <- lapply(split(tb, tb$variable), "[", -c(1,3))
  
  df <- tibble(x = rep(Inf, length(tbs)),
               y = rep(-Inf, length(tbs)),
               variable = factor(vars_to_plot, levels = vars_to_plot, ordered = TRUE),
               tbl = tbs)
  
  long_df %>% 
    filter(variable %in% vars_to_plot) %>% 
    mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) %>% 
    ggplot(aes(year, value, color = scenario)) + 
    geom_vline(xintercept = 2023, color = "grey") +
    geom_line() + 
    labs(x = NULL, y = NULL, title = "GCAM Reference") + 
    theme(legend.title = element_blank(), legend.position = "bottom") + 
    geom_table(data = df, aes(x = x, y = y, label = tbl),
               hjust = 1, vjust = 0) + 
    facet_wrap("variable", scales = "free")  -> 
    plot
  
  return(plot)
  
}

## C. climate variables  -------------------------------------------------------

# Compare the pre-GCAM historical results (these should be finalized gcam-hector)
# results. 
vars <- c("CO2_concentration", "N2O_concentration", "CH4_concentration")
plot1 <- my_plot(vars_to_plot = vars, MAE_df, long_df); plot1
my_ggsave(plot1, name = "gcam-ghg", WIDTH = 8, HEIGHT = 4)
   
vars <- c("CH4_concentration", "RF_CH4")
plot2 <- my_plot(vars, MAE_df, long_df); plot2
my_ggsave(plot2, name = "gcam-ch4", WIDTH = 8, HEIGHT = 4)


vars <- c("RF_aci", "RF_OC", "RF_BC", "RF_SO2", "RF_NH3")
plot3 <- my_plot(vars, MAE_df, long_df); plot3
my_ggsave(plot3, name = "gcam-aero", WIDTH = 8, HEIGHT = 4)


vars <- c(RF_TOTAL(), GMST())
plot4 <- my_plot(vars, MAE_df, long_df); plot4
my_ggsave(plot4, name = "gcam-EBM", WIDTH = 8, HEIGHT = 4)


# my_plot(RF_SO2(), MAE_df, long_df)
# my_plot(RF_BC(), MAE_df, long_df)
# my_plot(RF_OC(), MAE_df, long_df)
# my_plot(RF_CO2(), MAE_df, long_df)
# my_plot(RF_CH4(), MAE_df, long_df)
# my_plot(CONCENTRATIONS_CH4(), MAE_df, long_df)
# 
# 
# long_df %>% 
#   filter(grepl(pattern = "RF", variable)) %>% 
#   ggplot(aes(year, value, color = scenario)) + 
#   geom_line() + 
#   facet_wrap("variable", scales = "free")



