# Compare Hector historical emissions with the GCAM emissions, 
# most interested in seeing if there is a mismatch between hector's 2023 
# values and the GCAM emissions. 

# 0. Set Up --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
theme_set(theme_bw())

read.csv("data/extracted_gcam_emiss.csv") %>% 
  filter(scenario == "new") -> 
  gcam_emiss

"inputs/new/gcam_emissions.csv" %>% 
  read.csv(comment.char = ";") %>% 
  pivot_longer(-Date, names_to = "variable") %>% 
  rename(year = Date) %>%  
  mutate(source = "pre-GCAM") -> 
  pre_gcam_emiss

# Data frame for plotting
emiss_data <- bind_rows(gcam_emiss, pre_gcam_emiss)


# Helper function that makes a quick plot 
# Args
#   df: data frame of the emissions from GCAM and the pre-GCAM period
#   EMISS: vector name of the emissions to plot 
# Returns: ggplot of the the emissions
quick_plot <- function(df, EMISS){
  
  df %>% 
    filter(variable == EMISS) -> 
    to_plot
  
  ggplot() + 
    geom_vline(xintercept = 2023, alpha = 0.75) +
    geom_line(data = to_plot, aes(year, value, color = source), linewidth = 0.75, alpha = 0.75) + 
    labs(title = EMISS, 
         y = getunits(EMISS), x = NULL) 
}


# 1. Set Up --------------------------------------------------------------------


lapply(unique(emiss_data$variable), function(emiss_name){
  
  print(emiss_name)
  p <- quick_plot(emiss_data, emiss_name)
  fname <- paste0("figs/emiss/", emiss_name, ".png")
  ggsave(plot = p, filename = fname, width = 8, height = 6)
  
})
