
# 0. Set Up --------------------------------------------------------------------
# This version of the GCAM2Hector only works with a specific version of Hector 
# aka the version of Hector that is currently coupled with GCAM!
remotes::install_github("jgcri/hector@main")
remotes::install_github("kdorheim/GCAM2Hector@pkg_implementation")

library(hector)
library(dplyr)
library(GCAM2Hector)
library(ggplot2)

GCAM_DIR <- here::here("Gv8-Hv3.2")
OUTPUT_DIR <- here::here("data"); dir.create(OUTPUT_DIR)
TEMP_DIR <- file.path(OUTPUT_DIR, "temp-old"); dir.create(TEMP_DIR)

# List all of the data bases to process for the runs! 
dbs <- list.files(GCAM_DIR, pattern = "database_basexdb")
prj_file <- file.path(TEMP_DIR, "old_gcam.dat")

# 1. Get GCAM Results ----------------------------------------------------------

# For each GCAM runs derive the hector inputs. 
inputs <- data.frame()
for(db_name in dbs){
  prj_file <- file.path(TEMP_DIR, paste0(db_name, "-old_gcam.dat"))
  out <- get_hector_inputs(db_dir = GCAM_DIR, db_name, prj_file = prj_file)
  inputs <- rbind(inputs, out)
}
write.csv(inputs, file.path(OUTPUT_DIR, "old_gcam_inputs.csv"), row.names = FALSE)

hector_rslts <- data.frame()
for(db_name in dbs){
  print(db_name)
  prj_file <- file.path(TEMP_DIR, paste0(db_name, "-old_gcam.dat"))
  out <- fetch_GCAM_vs_hector(prj_file = prj_file)
  hector_rslts <- rbind(hector_rslts, out)
}
write.csv(hector_rslts, file.path(OUTPUT_DIR, "old_hector_rlsts.csv"), row.names = FALSE)

hector_rslts %>% 
  filter(variable == GMST()) %>% 
  ggplot(aes(year, value, color = scenario)) + 
  geom_line()

