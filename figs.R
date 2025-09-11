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


# 1. Data ----------------------------------------------------------------------
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


# 2. historical ----------------------------------------------------------------
# Compare the pre-GCAM historical results (these should be finalized gcam-hector)
# results. 
vars_to_plot <- unique(long_df$variable)

# Include the MAE on the plots! 
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
  plot

ggsave(plot, filename = file.path("figs", "gcam-hist.pdf"), width = 10, height = 10)


# 3. SSP Comparison  -----------------------------------------------------------
# It is not clear if this is needed and or helpful or not! Especially since the 
# new version of Hector is uncalibrated! That needs to be finnished ASAP. 

