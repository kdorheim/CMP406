# Compare Hector historical emissions with the GCAM emissions, 
# most interested in seeing if there is a mismatch between hector's 2023 
# values and the GCAM emissions. 

# 0. Set Up --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpmisc)
theme_set(theme_bw())

read.csv("data/new_extracted_gcam_emiss.csv") %>% 
  filter(version == "new") %>% 
  filter(year > 1970) %>% 
  mutate(source = "GCAM") -> 
  gcam_emiss

"inputs/new/gcam_emissions.csv" %>% 
  read.csv(comment.char = ";") %>% 
  pivot_longer(-Date, names_to = "variable") %>% 
  rename(year = Date) %>%  
  mutate(source = "pre-GCAM") -> 
  pre_gcam_emiss

# Data frame for plotting
bind_rows(gcam_emiss, 
          pre_gcam_emiss) %>% 
  filter(!is.na(variable)) %>% 
  # Ignore the daccs & luc emissions since they were not included in the queries
  filter(!variable %in% c("daccs_uptake", "luc_emissions", "luc_uptake")) -> 
  emiss_data


# Helper function that makes a quick plot 
# Args
#   df: data frame of the emissions from GCAM and the pre-GCAM period
#   EMISS: vector name of the emissions to plot 
# Returns: ggplot of the the emissions
# helpful for debugging 
EMISS <- "luc_emissions"
df <- emiss_data

quick_plot <- function(df, EMISS){
  
  print(EMISS)
  df %>% 
    filter(variable == EMISS) %>% 
    filter(year %in% 1950:2100) -> 
    to_plot
  
  # What is the difference between pre GCAM emissions and then the GCAM emissions
  to_plot %>% 
    filter(source == "pre-GCAM" & year == 2023) %>% 
    select(value, source, year, variable) -> 
    final_preGCAM
  
  to_plot %>% 
    filter(source == "GCAM" & year == 2025) %>% 
    select(value, source, year, variable) -> 
    first_GCAM
  
  
  rbind(final_preGCAM, first_GCAM) %>% 
    select(source, year, value, variable) %>% 
    bind_rows(data.frame(source = "difference", year = NA, value = final_preGCAM$value - first_GCAM$value, variable = EMISS))  %>% 
    mutate(value = signif(value, digits = 4)) ->
    tb
  
  tbs <- lapply(split(tb, tb$variable), "[", -c(4))
  
  df <- tibble(x = rep(Inf, length(tbs)),
               y = rep(-Inf, length(tbs)),
               tbl = tbs)
  
  
  ggplot() + 
    geom_vline(xintercept = 2023, alpha = 0.25) +
    geom_line(data = to_plot %>% filter(source != "GCAM"), aes(year, value, color = source), linewidth = 0.75, alpha = 0.75) + 
    geom_point(data = to_plot %>% filter(source == "GCAM"), aes(year, value, color = source), alpha = 0.75) + 
    
    labs(title = EMISS, 
         y = getunits(EMISS), x = NULL, 
         subtitle = "dev GCAM: reference run ") +
  geom_table(data = df , aes(x = x, y = y, label = tbl),
             hjust = 1, vjust = 0) + 
    theme(legend.position = "bottom") + 
    labs(caption = "vertical line 2023 marks the transition from historical pre-GCAM emissions to GCAM generated emissions \n 
       table shows the difference between final year of pre-GCAM emissions & first year of GCAM emissions ")
  

}


# 1. Set Up --------------------------------------------------------------------


lapply(unique(emiss_data$variable), function(emiss_name){
  
  print(emiss_name)
  p <- quick_plot(emiss_data, emiss_name)
  fname <- paste0("figs/emiss/", emiss_name, ".png")
  ggsave(plot = p, filename = fname, width = 8, height = 6)
  
})





