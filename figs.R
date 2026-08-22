# Script use to generate the figures and stats associated with CMP 406.

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(tidyr)
library(hector) # the hector version does not matter since we are just using the helper fxns. 
library(scales)

# Figure settings 
theme_set(theme_bw() + theme(legend.title = element_blank()))
ggplot_colors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, c = 100, l = 65)[1:n]
}
my_colors <- ggplot_colors(n = 2)
COLOR_SCHEME <- c("old" = my_colors[1], "new" = my_colors[2], 
                  "V3.2.0" = my_colors[1], "V3.5.0" = my_colors[2], 
                  "cmip7" = "black", 
                  "BerkeleyEarthGlobal" = "black", 
                  "HadCRUT5Global" = "black", 
                  "NOAAGlobalTempGlobal" = "black")

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
## A. pre gcam historical ------------------------------------------------------
# Load the data! 
"data/hector_v32_rslts.csv" %>% 
  read.csv() %>% 
  mutate(variable = if_else(variable == "FCH4", "RF_CH4", variable)) -> 
  old

"data/hector_v350_rslts.csv" %>% 
  read.csv() -> 
  new

new %>% 
  bind_rows(old) %>% 
  filter(variable != "gmst") -> 
  long_df

# Calculate the difference between variables. 
long_df %>% 
  pivot_wider(names_from = version, values_from = value) %>%  
  # Since the old historical run will end earlier make 
  # sure that NAs are not included. 
  na.omit %>% 
  mutate(error = `V3.5.0` - `V3.2.0`) -> 
  error_df

# Get the MAE for the variable and scenarios! 
error_df %>% 
  summarise(MAE = mean(abs(error)), .by = c(scenario, variable)) -> 
  MAE_df

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
  facet_wrap("variable", scales = "free") + 
  scale_color_manual(values = COLOR_SCHEME) -> 
  plot; plot
my_ggsave(plot, name = "gcam-hist", WIDTH = 10, HEIGHT = 10)

## B. CMIP7 comparison ---------------------------------------------------------

# CMIP7 ghgs 
"data/C.ghg_data.csv" %>% 
  read.csv() -> 
  cmip7_ghgs


# Get the CMIP7 GHG concentrations to use in the comparisons.  note these are not 
# the concentrations we used in calibration 
long_df %>% 
  filter(scenario == "gcam-hist") %>% 
  filter(variable %in% c(CONCENTRATIONS_CH4(), 
                         CONCENTRATIONS_CO2(), 
                         CONCENTRATIONS_N2O())) -> 
  hector_to_plot 

# The 3 GHGs vs. observations 
ggplot() + 
  geom_line(data = cmip7_ghgs, aes(year, value, color = "cmip7")) + 
  geom_line(data = hector_to_plot, aes(year, value, color = version, linetype = version), 
            linewidth = 1) + 
  labs(x = NULL, y = NULL, title = "GCAM Historical") + 
  facet_wrap("variable", scales = "free", ncol = 1) + 
  scale_color_manual(values = COLOR_SCHEME) + 
  guides(linetype = "none") + 
  theme(legend.title = element_blank()) -> 
  plot; plot
my_ggsave(plot, name = "gcam-hist_ghgs", WIDTH = 5, HEIGHT = 5)


# Only [CH4] vs observations
long_df %>% 
  filter(scenario == "gcam-hist") %>% 
  filter(variable %in% CONCENTRATIONS_CH4()) %>% 
  mutate(run = if_else(version == "V3.5.0", "new", "old")) -> 
  hector_to_plot 

ggplot() + 
  geom_line(data = cmip7_ghgs %>% 
              filter(variable == CONCENTRATIONS_CH4()), aes(year, value, color = "cmip7")) + 
  geom_line(data = hector_to_plot, aes(year, value, color = run), linetype = 2, 
            linewidth = 1) + 
  labs(x = NULL, y = NULL, title = "GCAM Historical") + 
  facet_wrap("variable", scales = "free", ncol = 1) + 
  scale_color_manual(values = COLOR_SCHEME) -> 
  plot; plot
my_ggsave(plot, name = "gcam-hist_ch4", WIDTH = 5, HEIGHT = 5)

# Only [N2O] vs observations
long_df %>% 
  filter(scenario == "gcam-hist") %>% 
  filter(variable %in% CONCENTRATIONS_N2O()) %>% 
  mutate(run = if_else(version == "V3.5.0", "new", "old")) -> 
  hector_to_plot 

ggplot() + 
  geom_line(data = cmip7_ghgs %>% 
              filter(variable == CONCENTRATIONS_N2O()), aes(year, value, color = "cmip7")) + 
  geom_line(data = hector_to_plot, aes(year, value, color = run), linetype = 2, 
            linewidth = 1) + 
  labs(x = NULL, y = NULL, title = "GCAM Historical") + 
  facet_wrap("variable", scales = "free", ncol = 1) +
  scale_color_manual(values = COLOR_SCHEME) -> 
  plot; plot

my_ggsave(plot, name = "gcam-hist_n2o", WIDTH = 5, HEIGHT = 5)


# Natural N2O Emissions 
here::here("inputs/new/default_inputs.csv") %>% 
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
       x = NULL) + 
  scale_color_manual(values = COLOR_SCHEME) -> 
  plot; plot

my_ggsave(plot, name = "gcam-hist_n2o_emiss", WIDTH = 5, HEIGHT = 5)


# Natural CH4 Emissions 
here::here("inputs/new/default_inputs.csv") %>% 
  read.csv(comment.char = ";") %>% 
  select(year = Date, value = CH4N) %>% 
  mutate(variable = NATURAL_CH4(), 
         source = "new") -> 
  nat_ch4_emiss

ggplot() + 
  geom_hline(aes(yintercept = 335, color = "old"), size = 1) + 
  geom_line(data = nat_ch4_emiss, aes(year, value, color = source), size = 0.75) + 
  theme(legend.title = element_blank()) + 
  labs(title =NATURAL_CH4(), 
       y = getunits(NATURAL_CH4()), 
       x = NULL) + 
  scale_color_manual(values = COLOR_SCHEME) -> 
  plot; plot

my_ggsave(plot, name = "gcam-hist_ch4_emiss", WIDTH = 5, HEIGHT = 5)


## C. SSP Comparisons  ----------------------------------------------------------

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
  facet_wrap("variable", scales = "free") + 
  scale_color_manual(values = COLOR_SCHEME) -> 
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
  facet_wrap("variable", scales = "free")  + 
  scale_color_manual(values = COLOR_SCHEME) -> 
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
  facet_wrap("variable", scales = "free") + 
  scale_color_manual(values = COLOR_SCHEME) -> 
  plot; plot
my_ggsave(plot, name = "ssps_GHGs", WIDTH = 10, HEIGHT = 5)


## D. Idealized ----------------------------------------------------------------

read.csv("data/hector_v320_idealized_rslts.csv") %>%  
  mutate(version = "V3.2.0") %>% 
  bind_rows(read.csv("data/hector_v350_idealized_rslts.csv")) -> 
  idealzed_rslts

idealzed_rslts %>% 
  filter(scenario %in% c("abruptx4CO2", "abruptx2CO2" ,"1pctCO2")) %>% 
  filter(variable %in% c(RF_TOTAL(), GLOBAL_TAS())) %>% 
  filter(year <= 2050) %>% 
  ggplot(aes(year, value, color = version, linetype = version)) + 
  geom_line(size = 1) + 
  facet_grid(variable ~ scenario, scales="free") + 
  labs(x = NULL, y = NULL) + 
  theme(legend.title = element_blank(), 
        legend.position = "bottom") + 
  scale_color_manual(values = COLOR_SCHEME) -> 
  plot; plot

my_ggsave(plot, name = "hector-idealized", WIDTH = 8, HEIGHT = 6)

## E. temperature observations ------------------------------------------------

"data/temps(global_(above) and european (be).csv" %>% 
  read.csv(skip = 13) -> 
  obs

names(obs) <- gsub(x = names(obs), pattern = "\\.", replacement = "")
cols_to_save <- c("Year", names(obs)[grepl(x = names(obs), pattern = "Global")])
cols_to_save <- cols_to_save[!grepl(pattern = "Europe", x = cols_to_save)]

obs %>% 
  select(all_of(cols_to_save)) %>% 
  pivot_longer(-Year) %>% 
  rename(year = Year, source = name) -> 
  temp_df

temp_df %>% 
  filter(year %in% 1850:1900) %>% 
  summarise(ref = mean(value), .by = source) -> 
  ref

temp_df %>% 
  left_join(ref, by = join_by(source)) %>% 
  mutate(value = value - ref) %>% 
  na.omit %>% 
  select(year, source, value) -> 
  temp_obs

"old-GCAM/gcam-hector-outputstream.csv" %>% 
  read.csv(comment.char = "#") %>% 
  filter(variable == GMST()) %>% 
  mutate(scenario = "gcam-hist", 
         version = "V3.2.0", 
         source = "hector") %>% 
  select(scenario, year, variable, value, units, source, version) %>% 
  bind_rows(new) %>% 
  filter(variable == GMST()) %>% 
  filter(scenario == "gcam-hist") -> 
  hector_temp_data


# TODO this might need to be removed 

bind_rows("~/Documents/Hector-WD/hector_benchmarking/data/hector_fair.csv" %>% 
  read.csv(), 
  read.csv("~/Documents/Hector-WD/hector_benchmarking/data/hector_magicc.csv")
  ) %>% 
  filter(model != "hector 3.5.0") %>% 
  filter(variable == GMST())
  select(scenario, model) %>% distinct



hector_temp_data %>% 
  filter(year %in% 1850:1900) %>% 
  summarise(ref = mean(value), .by = c(scenario, variable, version)) -> 
  hector_ref_value 

hector_temp_data %>% 
  left_join(hector_ref_value) %>% 
  mutate(value = value - ref) %>% 
  filter(year >= 1850 & year < 2025) -> 
  hector_temp_data

ggplot() + 
  geom_line(data = temp_obs, aes(year, value, color = source), alpha = 0.5) + 
  geom_line(data = hector_temp_data, aes(year, value, color = version), linewidth = 1) + 
  scale_color_manual(values = COLOR_SCHEME) + 
  theme(legend.position = "bottom") + 
  labs(y = "GMST (deg C relative to 1850-1900)", 
       x = NULL,
       title = "Annual Global Mean Surface Temperature vs. Obs") -> 
  plot; plot

my_ggsave(plot, name = "hector-temp_vs_obs", WIDTH = 8, HEIGHT = 6)


temp_obs %>% 
  summarise(obs = mean(value), .by = year) %>% 
  left_join(hector_temp_data) %>% 
  mutate(AE = abs(obs - value)) %>% 
  select(year, scenario, variable, version, AE) %>% 
  filter(year %in% 1980:2015) %>% 
  summarise(MAE = mean(AE), .by = version)


# 2. GCAM ----------------------------------------------------------------------
## A. data ---------------------------------------------------------------------

bind_rows(
  read.csv(file.path("data", "new_gcam_climate.csv")),
  read.csv(file.path("data", "old_gcam_climate.csv"))) -> 
  long_df

# Calculate the difference between variables. 
long_df %>% 
  select(year, value, variable, version) %>% 
  pivot_wider(names_from = version, values_from = value) %>% 
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
    ggplot(aes(year, value, color = version)) + 
    geom_vline(xintercept = 2023, color = "grey") +
    geom_line() + 
    labs(x = NULL, y = NULL, title = "GCAM Reference") + 
    theme(legend.title = element_blank(), legend.position = "bottom") + 
    scale_color_manual(values = COLOR_SCHEME) +
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




