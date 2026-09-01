# This script defines all of the functions that can be used to 
# compute the benchmarks corresponding to AR6 (Table 7.SM.4). 
#
# Use the function get_AR6_benchmarks to compute the AR6 benchmark metrics. 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(tidyr)

# 1. Functions  ----------------------------------------------------------------

# Helper function that loads the data needed for the AR6 benchmarking.
# Args
#   file: str name to the run-archive runs output file
# Returns: data.frame of hector results
AR6B.get_data <- function(file){
  
  # Make sure that the file exits
  stopifnot(file.exists(file))
  
  # Load the results 
  full_data <- read.csv(file) 
  
  # Extract the version number
  ver <- unique(full_data$version)
  
  
  if(ver %in% c("3.5.0", "3.2.0")){
    
    aerosol_vars <- c("RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci")
    hist_warming <- c("global_tas")
    other_vars <- c("RF_O3_trop", "RF_H2O_strat", "RF_vol", "RF_albedo", "RF_misc", "heatflux", "CO2_concentration", "RF_CH4", "RF_tot")
    
    VARS <- c(aerosol_vars, hist_warming, other_vars)
    
  } else {
    stopifnot("version not supported by at the momment")
  }
  
  
  full_data %>% 
    # Subset variables to minimize the data
    filter(variable %in% VARS) %>% 
    # Change from wide to long format & make sure that the years are integers. 
    pivot_longer(starts_with("X"), names_to = "year") %>% 
    mutate(year = as.integer(gsub(pattern = "X", replacement = "", x = year))) -> 
    out 
  
  return(out)
  
}


# Helper function to normalize hector output
# Args
#   d: data frame of hector results
#   yrs: vector of reference period
# Returns: data frame of normalized results
normalize_fxn <- function(d, yrs){
  
  # check inputs
  req_cols <- c("variable", "scenario", "value")
  stopifnot({
    d %>%
      select(variable, scenario) %>% distinct %>%
      nrow == 1})
  stopifnot(all(yrs %in% d$year))
  
  
  d %>%
    filter(year %in% yrs) %>%
    pull(value) %>%
    mean ->
    ref_value
  
  d %>%
    mutate(value = value - ref_value) ->
    out
  
  return(out)
}


# Helper function that calculates the TCRE 
# Args
#   rslts: data.frame hector results
# Returns: data.frame of TCRE
AR6B.get_tcre_fxn <- function(rslts){
  
  
  scn <- "ssp585"
  v <- unique(rslts$version)
  
  # Extract the temperature results associated with the SSP585 scenario 
  # which we will use for the tcre calculation. 
  rslts %>% 
    filter(scenario == scn & variable == "global_tas") %>% 
    pull(value) -> 
    temp
  
  # For TCRE we need the annual net CO2 emissions. 
  system.file(package = "hector", "input/tables") %>% 
    list.files(pattern = scn, full.names = TRUE) %>% 
    read.csv(comment.char = ";") %>% 
    filter(Date %in% rslts$year) %>% 
    select("ffi_emissions", "luc_emissions", "luc_uptake", "daccs_uptake") %>% 
    # Change the uptake values to negative values to use in the net emissions 
    # calculation. 
    mutate(luc_uptake = -1 * luc_uptake, 
           daccs_uptake = -1 * daccs_uptake) %>% 
    rowSums %>%  
    cumsum -> 
    net_emiss
  
  # Calculating TCRE via linear regression
  tcre_reg <- lm(temp ~ net_emiss)
  tcre <- tcre_reg$coefficients[2] * 1000
  
  # Format the results as a data frame
  data.frame(version = v, 
             value = tcre[[1]], 
             year = NA, 
             variable = "tcre") -> 
    out
  
  return(out)
  
}

# Helper function that calculates the TCR 
# Args
#   rslts: data.frame hector results
# Returns: data.frame of TCR
AR6B.get_tcr_fxn <- function(rslts){
  
  rslts %>% 
    filter(scenario == "1pctCO2") -> 
    data_1pctCO2
  
  data_1pctCO2 %>% 
    filter(variable == "CO2_concentration" & year == 1750) %>% 
    pull(value) -> 
    pi_co2
  
  # The years to use in the tcr calculation
  yrs <- 1800:2000
  
  data_1pctCO2 %>% 
    filter(variable == "global_tas" & year %in% yrs) -> 
    temp
  
  data_1pctCO2 %>% 
    filter(variable == "CO2_concentration" & year %in% yrs) -> 
    co2
  
  # Finding TCR
  tcr_fit <- lm(temp$value ~ co2$value)
  # The temperature of CO2 after a doubling of CO2 
  tcr <- coefficients(tcr_fit)[["(Intercept)"]] + coefficients(tcr_fit)[["co2$value"]] * pi_co2 * 2
  
  # Format the output
  out <- data.frame(version = unique(data_1pctCO2$version), 
                    value = tcr, 
                    year = NA, 
                    variable = "tcr")
  
  return(out)
  
}

# Helper function convert ocean heat flux to ocean heat content
# Args
#   d: data frame of historical hector results
# Returns: data frame of ocean heat content
AR6B.internal_ohc <- function(data){
  
  # Ocean heat content constants
  OCEAN_AREA <- 5100656e8 * (1 - 0.29) # The total area of the ocean
  W_TO_ZJ <- 3.155693e-14              # Watts to ZJ
  
  yrs <- 1971:2018
  
  data %>% 
    filter(variable == "heatflux") %>% 
    mutate(value = value * OCEAN_AREA * W_TO_ZJ) %>% 
    mutate(variable = "OHC", units = "ZJ") %>%  
    filter(year %in% yrs) %>% 
    summarise(value = mean(value), .by = c(scenario, variable, units)) %>% 
    mutate(value = value * length(yrs)) %>%
    mutate(year = "1971-2018") -> 
    out
  
  return(out)
  
}


# Helper function that gathers all of the historical rf
# Args
#   d: data frame of historical hector results
# Returns: data frame of historical rf benchmarks
AR6B.internal_hist_rf <- function(data){
  
  # Save a copy of the version 
  v <- unique(data$version)
  
  # The mean aerosol RF
  data %>% 
    filter(year %in% 2005:2015) %>% 
    filter(variable %in% c("RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci")) %>% 
    # get the total aerosol RF per YEAR 
    summarise(value = sum(value), .by = c("scenario", "year", "units")) %>% 
    # get the mean aerosol over our period of interest
    summarise(value = mean(value), .by = c(scenario, units)) %>%
    mutate(variable = "total aerosol RF", year = "2005-15") -> 
    aero_rf
  
  # CH4 forcing in 2019
  data %>% 
    filter(variable == "RF_CH4" & year == 2019) -> 
    ch4_rf
  
  # The non GHG RF in 2019 
  non_ghg_vars <- c("RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci", "RF_O3_trop", 
                    "RF_H2O_strat", "RF_vol", "RF_albedo", "RF_misc")
  data %>%  
    filter(year == 2019) %>% 
    filter(variable %in% non_ghg_vars) %>%  
    summarise(value = sum(value), .by = c("scenario", "year", "units")) %>%
    mutate(variable = "non_ghg_ERF") ->
    nonghg_rf
  
  # The wmghg RF
  data %>%  
    filter(year == 2019) %>% 
    filter(variable == "RF_tot") %>% 
    mutate(value = value - nonghg_rf$value, 
           variable = "wmghg RF") -> 
    wmghg_rf
  
  # Format all the historical RF values
  bind_rows(ch4_rf, wmghg_rf, nonghg_rf) %>% 
    mutate(year = as.character(year)) %>% 
    bind_rows(aero_rf) %>% 
    mutate(version = v) ->
    out
  
  return(out)
}


# Helper function that gathers all of the historical temp
# Args
#   d: data frame of historical hector results
# Returns: data frame of historical temp benchmark
AR6B.internal_hist_temp <- function(data){
  
  # historical warming
  data %>% 
    filter(year %in% 1750:2100) %>% 
    filter(variable == "global_tas") %>% 
    normalize_fxn(yrs = 1850:1900) %>%
    filter(year %in% 1995:2014) %>%
    summarise(value = mean(value), .by = c(scenario, variable, units)) %>%
    mutate(variable = "hist. warming",
           year = "1995-2014") ->
    out
  
  return(out)
  
}


# The function that gets all the historical benchmarks 
# TODO there are some values from 2019... which might be a problem 
# Args 
#   rslts: data.frame of hector results 
# Returns: data.frame of the historical benchmarks from AR6
AR6B.get_historical_fxn <- function(rslts){
  
  rslts %>% 
    filter(scenario == "ssp245") %>% 
    filter(year <= 2025) %>% 
    mutate(scenario = "historical") -> 
    data 
  
  
  hist_ohc   <- AR6B.internal_ohc(data)
  hist_rf    <- AR6B.internal_hist_rf(data)
  hist_temp  <- AR6B.internal_hist_temp(data)
  
  
  bind_rows(hist_ohc, 
            hist_rf, 
            hist_temp) %>% 
    mutate(version = unique(data$version)) -> 
    out 
  
  return(out)
  
}


# Helper function that calculates the global mean temp in the 
# future scenarios over near, mid, and long term time horizons. 
# Args
#   d: data frame of  hector results
# Returns: data frame of historical of the future SSP temperature benchmarks
AR6B.future_warming_fxn <- function(rslts){
  
  
  # Normalize temp. data for each ssp scenario. 
  rslts %>% 
    filter(grepl(pattern = "ssp", x = scenario)) %>% 
    filter(variable == "global_tas") %>% 
    split(., .$scenario) %>% 
    lapply(function(d){
      normalize_fxn(d, yrs = 1995:2014)
    }) %>% 
    bind_rows %>% 
    mutate(units = "degC rel. 1995-2014") -> 
    temp
  
  
  temp %>%
    filter(year %in% 2021:2040) %>%
    mutate(year = "2021-2040") %>%
    summarise(value = mean(value),
              .by = c(scenario, year, variable, units, version)) ->
    near_temp
  
  temp %>%
    filter(year %in% 2041:2060) %>%
    mutate(year = "2041-2060") %>%
    summarise(value = mean(value),
              .by = c(scenario, year, variable, units, version)) ->
    mid_temp
  
  temp %>%
    filter(year %in% 2081:2100) %>%
    mutate(year = "2081-2100") %>%
    summarise(value = mean(value),
              .by = c(scenario, year, variable, units, version)) ->
    long_temp
  
  out <- rbind(near_temp, mid_temp, long_temp)
  
  return(out)
  
}


# The function that calculates all of the AR6 benchmarks 
# Args 
#   file: full file path to the output-X.csv to be processed 
# Returns: data.frame of the AR6 benchmark that corresponds to the AR6 (Table 7.SM.4).
get_AR6_benchmarks <- function(file){
  
  # Load the Hector data and keep only the relevant scenarios/variables.
  data <- AR6B.get_data(file) 
  
  # Emergent climate metrics
  TCRE <- AR6B.get_tcre_fxn(data)
  TCR  <- AR6B.get_tcr_fxn(data)
  
  # Historical metrics 
  hist <- AR6B.get_historical_fxn(data)
  
  # Future warming 
  fut_warming <- AR6B.future_warming_fxn(data)
  
  out <- bind_rows(TCRE, TCR, hist, fut_warming)
  return(out)
  
}


