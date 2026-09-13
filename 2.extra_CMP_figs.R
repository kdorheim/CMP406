# This script generates some extra figures that were needed for the CMP, 
# however they are kind of funky so the additional script is needed. 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(tidyr)
library(scales)
library(here)
library(ggpp)
library(patchwork)

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

# 1. F-gases -------------------------------------------------------------------

here::here("master-GCAM", "gcamhector_outputstream.csv") %>% 
  read.csv(comment.char = "#") -> 
  old_data

old_data %>% 
  filter(variable %in% ALL_HALOCARBON_RF()) %>% 
  filter(spinup == 0) %>% 
  summarise(value = sum(value), .by = c(year)) %>% 
  mutate(variable = "Total F-gas RF", source = "V3.2.0") -> 
  old_fgas_rf

here::here("dev-GCAM", "gcam-hector-outputstreamReference.csv") %>% 
  read.csv(comment.char = "#") %>% 
  filter(year <= max(old_fgas_rf$year)) -> 
  new_data

new_data %>% 
  filter(variable %in% ALL_HALOCARBON_RF()) %>% 
  filter(spinup == 0) %>% 
  summarise(value = sum(value), .by = c(year)) %>% 
  mutate(variable = "Total F-gas RF", source = "V3.5.5") -> 
  new_fgas_rf

old_fgas_rf %>% 
  bind_rows(new_fgas_rf) -> 
  to_plot 


# Subset the RMSE data frame 
to_plot %>% 
  pivot_wider(names_from = source, values_from = value) %>%
  na.omit %>% 
  mutate(SE = (`V3.2.0` - `V3.5.5`)^2) %>%
  summarise(RMSE = sqrt(mean(SE)), .by = c(variable)) %>%
  mutate(RMSE = signif(RMSE, digits = 3)) %>% 
  mutate(units = getunits(RF_ALBEDO())) -> 
  RMSE_df_to_plot

# subset and format the RMSE results to be included in the plot. 
tibble(x = -Inf, 
       y = -Inf, 
       label = list(RMSE_df_to_plot)) -> 
  RMSE_tib


to_plot %>% 
  ggplot(aes(year, value, color = source, linetype = source), linewidth = 0.75) + 
  geom_line() + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(x = NULL, title = "Historical ", subtitle = "Total F-gas RF",  y = getunits(RF_ALBEDO())) + 
  geom_table(data = RMSE_tib, aes(x = x, y = y, label = label),
             hjust = 0, vjust = -1) + 
  scale_color_manual(values = COLOR_SCHEME) -> 
  p 

fname <- file.path(MAIN_FIGS, paste0("total_Fgas_RF.", TYPE))
ggsave(plot = p, filename = fname, width = WIDTH, height = HEIGHT)
  
