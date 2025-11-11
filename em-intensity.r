library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(AER)
if (!requireNamespace("glmnet", quietly = TRUE)) install.packages("glmnet")
if (!requireNamespace("VIM", quietly = TRUE)) install.packages("VIM")
library(VIM)
library(glmnet)
library(lme4)

setwd("~/OneDrive - agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd")

####### LOADING DATA #######
# Load data
# Economic performance series
df_EPS <- read.csv("/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/OECD.ECO.MAD,DSD_EPS@DF_EPS,1.0+all/OECD.ECO.MAD,DSD_EPS@DF_EPS,1.0+all.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Green growth indicators
df_GG <- read.csv("/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/OECD.ECO.MAD,DSD_EPS@DF_EPS,1.0+all/OECD.ENV.EPI,DSD_GG@DF_GREEN_GROWTH,1.1+all.csv", stringsAsFactors = FALSE, check.names = FALSE)

df_pop <- read.csv("/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/OECD.ECO.MAD,DSD_EPS@DF_EPS,1.0+all/OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,1.0+AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+G20+EU27+OECD+ARG+BRA+BGR.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Exogenous emissions rates
sigma_NICE <- read.csv("/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/exogenous-emissionsrate.csv", stringsAsFactors = FALSE, check.names = FALSE)

GDP_net_BAU_NICE <- read.csv("/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/nice_net_output.csv", stringsAsFactors = FALSE, check.names = FALSE)

GDPpc_net_BAU_NICE <- read.csv("/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/net_output_per_capita.csv", stringsAsFactors = FALSE, check.names = FALSE)

population_BAU_NICE <- read.csv("/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/population.csv", stringsAsFactors = FALSE, check.names = FALSE)

# --- Load and clean ISO3 mapping for country names ---
iso_map <- read.csv(
  "/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/country_iso3_codes.csv",
  stringsAsFactors = FALSE, check.names = FALSE
)
# Clean the `name` column: remove quotes, handle commas and parentheses
iso_map <- iso_map %>%
  rename(orig_name = name) %>%
  mutate(
    # X, Y Z --> Y Z X
    orig_name = ifelse(
      grepl(',', orig_name),
      sapply(strsplit(orig_name, ',\\s*'), function(parts) paste(c(parts[2], parts[1]), collapse = ' ')),
      orig_name
    ),
    # X (Y Z) --> Y Z X
    orig_name = ifelse(
      grepl('\\(', orig_name),
      sapply(
        regmatches(orig_name, regexec('^(.+?) \\((.+)\\)$', orig_name)),
        function(m) if (length(m) == 3) paste(c(m[3], m[2]), collapse = ' ') else orig_name
      ),
      orig_name
    ),
    # Remove quotes
    name_clean = gsub('"', '', orig_name)
  )

# --- Read all TES-country files, clean, map to ISO3, and stack ---
tes_folder <- "/Users/orlando/Library/CloudStorage/OneDrive-agroparistech.fr/DENS/ENS-APT/3A_EEET/AR_PSE/Préparation-mémoire-oecd/data-analysis-oecd/TES-country"
files <- list.files(tes_folder,
                    pattern = "^Total energy supply \\(TES\\) by source - .*\\.csv$",
                    full.names = TRUE)
combined_list <- list()
unmapped <- character()

for (f in files) {
  # Read and drop first 3 rows
  df_tes <- read.csv(f, skip = 3, stringsAsFactors = FALSE, check.names = FALSE)
  # Rename first column
  names(df_tes)[1] <- "TIME_PERIOD"

  # Extract country name from filename
  fname <- basename(f)
  country_raw <- sub(
    '^Total energy supply \\(TES\\) by source - (.*)\\.csv$',
    '\\1',
    fname
  )

  # Map to ISO3
  iso <- iso_map$iso3[match(country_raw, iso_map$name_clean)]
  # Manual mapping for a few remaining names:
  if (is.na(iso)) {
    manual <- c(
      "Czech Republic"              = "CZE",
      "Islamic Republic of Iran"    = "IRN",
      "Korea"                       = "KOR",
      "Kyrgyz Republic"             = "KGZ",
      "People's Republic of China"  = "CHN",
      "Plurinational State of Bolivia" = "BOL",
      "Republic of Turkiye"         = "TUR",
      "Slovak Republic"             = "SVK",
      "United Kingdom"              = "GBR",
      "United States"               = "USA"
      # add more if needed
    )
    if (country_raw %in% names(manual)) {
      iso <- manual[country_raw]
    }
  }
  if (is.na(iso)) {
    unmapped <- c(unmapped, country_raw)
    next
  }

  # Prepend REF_AREA and store
  df_tes$REF_AREA <- iso
  df_tes$countryname <- country_raw
  combined_list[[length(combined_list) + 1]] <- df_tes
}

# Combine all
tes_df <- bind_rows(combined_list)

# --- compute total and shares ---
tes_df <- tes_df %>%
  mutate(
    Total_energy = rowSums(
      across(-c(TIME_PERIOD, REF_AREA, countryname, Units)),
      na.rm = TRUE
    ),
    share_fossil_fuel = (
      coalesce(Coal, 0) +
      coalesce(`Natural gas`, 0) +
      coalesce(Oil, 0)
    ) / Total_energy,
    share_renewables = (
      coalesce(Hydro, 0) +
      coalesce(`Wind, solar, etc.`, 0)
    ) / Total_energy
  )

# Report any unmapped country names
if (length(unmapped) > 0) {
  cat("Unmapped TES country names:\n",
      paste(unique(unmapped), collapse = ", "),
      "\n")
}




############################
####### SHAPING DATA #######
############################

# Columns to inspect
cols_to_check <- c(
  "STRUCTURE", "STRUCTURE_ID", "STRUCTURE_NAME", "ACTION",
  "FREQ", "Frequency of observation", 
  "SEX", "Sexe", "AGE", "Âge", "TIME_HORIZ", "Horizon temporel", "OBS_STATUS", "Statut d'observation", "UNIT_MULT", "Multiplicateur d'unité", "DECIMALS", "Décimales", 
  "Units"
)

for(df_name in c("df_EPS", "df_GG", "df_pop", "tes_df")) {
  df <- get(df_name, envir = .GlobalEnv)
  
  # 1) Check specified columns
  for(col in cols_to_check) {
    if (col %in% names(df)) {
      vals <- unique(df[[col]])
      if (length(vals) > 1) {
        cat(sprintf("%s → column '%s' has multiple values: %s\n",
                    df_name, col, paste(vals, collapse = ", ")))
      } else {
        cat(sprintf("%s → column '%s' single value: %s (dropping)\n",
                    df_name, col, vals))
        df[[col]] <- NULL
      }
    }
  }
  
  # 2) Find fully empty columns
  empty_cols <- names(df)[vapply(df, function(x) all(is.na(x) | x == ""), logical(1))]
  if (length(empty_cols) > 0) {
    cat(sprintf("%s → empty columns: %s (dropping)\n",
                df_name, paste(empty_cols, collapse = ", ")))
    df[, empty_cols] <- NULL
  }
  
  # 3) Put cleaned df back into global env
  assign(df_name, df, envir = .GlobalEnv)
}

# Filter df_GG for selected measures
measures_to_keep <- c(
  "Energy public RD&D budget",
  "Fossil fuel public RD&D budget (exclusing CCS)",
  "Renewable energy public RD&D budget",
  "Environment related government R&D budget",
  "Environment related R&D expenditure",
  "Technology and innovation: R&D",
  "Technology and innovation: Patents",
  "Total fossil fuel support",
  "Fossil fuel general services support",
  "Fossil fuel producer support",
  "Fossil fuel consumer support",
  "Gas support",
  "Petroleum support",
  "Electricity support",
  "Emissions priced above EUR 30 per tonne of CO2",
  "Emissions priced above EUR 60 per tonne of CO2",
  "Emissions priced above EUR 120 per tonne of CO2",
  "Environmental taxes and transfers",
  "Energy related taxes revenue",
  "Petrol tax rate",
  "Diesel tax rate",
  "Feed-in tariffs for solar photovoltaic",
  "Demand-based GHG emissions",
  "Production-based GHG emissions",
  "Demand-based CO2 emissions",
  "Production-based CO2 emissions",
  "CO2 emissions from air transport per unit of GDP",
  "GHG Productivity",
  "Demand-based GHG productivity, GDP per unit of energy-related GHG emissions",
  "Production-based GHG productivity, GDP per unit of energy-related GHG emissions",
  "Demand-based CO2 productivity, GDP per unit of energy-related CO2 emissions",
  "Production-based CO2 productivity, GDP per unit of energy-related CO2 emissions",
  "Energy productivity, GDP per unit of TES",
  "Demand-based GHG productivity relative to disposable income",
  "Demand-based CO2 productivity relative to disposable income",
  "Energy intensity per capita",
  "Demand-based GHG intensity energy-related GHG per capita",
  "Production-based GHG intensity, energy-related GHG per capita",
  "Demand-based CO2 intensity energy-related CO2 per capita",
  "Production-based CO2 intensity, energy-related CO2 per capita",
  "Energy consumption",
  "Renewable electricity generation",
  "Renewable energy supply",
  "Renewable energy supply (excluding solid biofuels)",
  "Real GDP",
  "Real GDP per capita",
  "GDP deflator",
  "Residential electricity price",
  "Industry electricity price",
  "Diesel end-user price",
  "Petrol end-user price",
  "Cropland",
  "Built-up area"
)

# Create filtered data frame
df_GG_filtered <- df_GG[df_GG$Measure %in% measures_to_keep, ]

#–– Build “short” versions of GG and EPS ––
cols_short <- c("REF_AREA","MEASURE","UNIT_MEASURE","ACTIVITY","TIME_PERIOD","OBS_VALUE")
cols_short_EPS <- c("REF_AREA","MEASURE","UNIT_MEASURE","CLIM_POL","TIME_PERIOD","OBS_VALUE")
cols_short_pop <- c("REF_AREA","MEASURE","UNIT_MEASURE","TIME_PERIOD","OBS_VALUE")

df_GG_short  <- df_GG_filtered[, cols_short]
# Combine measure, unit, and activity into a single identifier
df_GG_short$`MESURE-UNIT-ACTIVITY` <- paste(
  df_GG_short$MEASURE,
  df_GG_short$UNIT_MEASURE,
  df_GG_short$ACTIVITY,
  sep = "-"
)
# Drop the original columns
df_GG_short <- df_GG_short %>%
  select(-MEASURE, -UNIT_MEASURE, -ACTIVITY)


df_EPS_short <- df_EPS[, cols_short_EPS]
df_EPS_short$`MESURE-UNIT-ACTIVITY` <- paste(
  df_EPS_short$MEASURE,
  df_EPS_short$UNIT_MEASURE,
  df_EPS_short$CLIM_POL,
  sep = "-"
)
# Drop the original columns
df_EPS_short <- df_EPS_short %>%
  select(-MEASURE, -UNIT_MEASURE, -CLIM_POL)


df_pop_short <- df_pop[, cols_short_pop]
df_pop_short$`MESURE-UNIT-ACTIVITY` <- paste(
  df_pop_short$MEASURE,
  df_pop_short$UNIT_MEASURE,
  sep = "-"
)
# Drop the original columns
df_pop_short <- df_pop_short %>%
  select(-MEASURE, -UNIT_MEASURE)

# Merge GG and EPS vertically
df_short_combined <- rbind(df_EPS_short, df_GG_short, df_pop_short)

#–– Pivot sigma_NICE into REF_AREA / TIME_PERIOD / OBS_VALUE ––
# Rename first column to REF_AREA if needed
names(sigma_NICE)[1] <- "REF_AREA"


# Pivot sigma_NICE into long format
df_sigma_NICE <- pivot_longer(
  sigma_NICE,
  cols      = -REF_AREA,
  names_to  = "TIME_PERIOD",
  values_to = "OBS_VALUE"
)
# Remove leading 'X' from year names
df_sigma_NICE$TIME_PERIOD <- sub("^X", "", df_sigma_NICE$TIME_PERIOD)
# Tag it as its own measure
df_sigma_NICE$`MESURE-UNIT-ACTIVITY` <- "sigma_NICE"


#–– Final vertically‐merged dataset ––
df_short_data <- rbind(df_short_combined, df_sigma_NICE)

# cat("df_short_data created with", nrow(df_short_data), "rows\n")

# 1) Pivot to wide format: one value per `MESURE-UNIT-ACTIVITY`
# Verify there are no duplicate entries for REF_AREA, TIME_PERIOD,`MESURE-UNIT-ACTIVITY`
dup_entries <- df_short_data %>%
  dplyr::count(REF_AREA, TIME_PERIOD, `MESURE-UNIT-ACTIVITY`) %>%
  dplyr::filter(n > 1)
if (nrow(dup_entries) > 0) {
  cat("Error: found duplicate entries for REF_AREA/TIME_PERIOD/MESURE-UNIT-ACTIVITY:\n")
  print(dup_entries)
  stop("Duplicates must be resolved before pivoting.")
}

# Pivot without aggregation since there are no duplicates
wide_df <- df_short_data %>%
  dplyr::distinct(REF_AREA, TIME_PERIOD, `MESURE-UNIT-ACTIVITY`, .keep_all = TRUE) %>%
  tidyr::pivot_wider(
    names_from  = `MESURE-UNIT-ACTIVITY`,
    values_from = OBS_VALUE
  )

# Merge GDP_net_BAU_NICE (Y × 10^6) into wide_df
gdp_df <- GDP_net_BAU_NICE %>%
  transmute(
    REF_AREA         = as.character(country),
    TIME_PERIOD      = as.character(time),
    GDP_net_BAU_NICE = as.numeric(Y) * 1e6
  )

gdppc_df <- GDPpc_net_BAU_NICE %>%
  transmute(
    REF_AREA         = as.character(country),
    TIME_PERIOD      = as.character(time),
    GDPpc_net_BAU_NICE = as.numeric(Y_pc)
  )

population_df <- population_BAU_NICE %>%
  transmute(
    REF_AREA         = as.character(country),
    TIME_PERIOD      = as.character(time),
    population_BAU_NICE = as.numeric(l) * 1e3
  )

tes_df <- tes_df %>%
  mutate(
    REF_AREA         = as.character(REF_AREA),
    TIME_PERIOD      = as.character(TIME_PERIOD)
  )

wide_df <- wide_df %>%
  # ensure TIME_PERIOD is character so the join lines up
  mutate(TIME_PERIOD = as.character(TIME_PERIOD)) %>%
  left_join(gdp_df, by = c("REF_AREA", "TIME_PERIOD")) %>%
  left_join(gdppc_df, by = c("REF_AREA", "TIME_PERIOD")) %>%
  left_join(population_df, by = c("REF_AREA", "TIME_PERIOD")) %>%
  filter(as.integer(TIME_PERIOD) <= 2150 & as.integer(TIME_PERIOD) >= 1995) %>%
  left_join(tes_df, by = c("REF_AREA", "TIME_PERIOD"))

# Create sigma_OECD as (GHG_PBEM / GDP) and keep only valid ratios
wide_df <- wide_df %>%
  mutate(
    `GHG_PBEM-T_CO2E-_T` = `GHG_PBEM-T_CO2E-_T` * 1e6,
    `GDP_R-USD_PPP-_T`   = `GDP_R-USD_PPP-_T` * 1e6,
    sigma_OECD = `GHG_PBEM-T_CO2E-_T` / `GDP_R-USD_PPP-_T`
  ) 

  # Remove unwanted aggregate regions from wide_df
  exclude_areas <- c("W", "EU28", "OECDE", "OECDSO", "EU27_2020", "OECDA", "OECD_KEY_PART", "OECDS", "EU27")
  wide_df <- wide_df %>%
    filter(!REF_AREA %in% exclude_areas)
  wide_df <- wide_df %>%
    arrange(REF_AREA, as.integer(TIME_PERIOD))


#–– Create aggregated OECD region ––
oecd_countries <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT",
  "SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA"
)

oecd_summaries <- wide_df %>%
  filter(REF_AREA %in% oecd_countries) %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    # 1) Totals for GDP and emissions
    total_emissions    = sum(`GHG_PBEM-T_CO2E-_T`, na.rm = TRUE),
    total_GDP_R        = sum(`GDP_R-USD_PPP-_T`,   na.rm = TRUE),
    # 2) Totals for population
    total_population   = sum(`POP-PS`,              na.rm = TRUE),
    # 3) Numerator/denominator for sigma_NICE
    .w_num             = sum(sigma_NICE * GDP_net_BAU_NICE, na.rm = TRUE),
    .w_den             = sum(GDP_net_BAU_NICE,              na.rm = TRUE),
    # 4) Totals for BAU net GDP and population
    total_GDP_net_BAU  = sum(GDP_net_BAU_NICE,    na.rm = TRUE),
    total_pop_BAU      = sum(population_BAU_NICE, na.rm = TRUE),
    # 5) Aggregate energy sources
    total_coal         = sum(Coal, na.rm = TRUE),
    total_gas          = sum(`Natural gas`, na.rm = TRUE),
    total_oil          = sum(Oil, na.rm = TRUE),
    total_hydro        = sum(Hydro, na.rm = TRUE),
    total_other_renew         = sum(`Wind, solar, etc.`, na.rm = TRUE),
    total_nuclear      = sum(Nuclear, na.rm = TRUE),
    total_biofuels      = sum(`Biofuels and waste`, na.rm = TRUE),
    # 6) Total energy
    Total_energy_agg   = total_coal + total_gas + total_oil + total_hydro + total_other_renew + total_nuclear + total_biofuels,
    .groups = "drop"
  ) %>%
  mutate(
    # Compute the two versions of sigma
    sigma_OECD   = total_emissions / total_GDP_R,
    sigma_NICE   = ifelse(.w_den > 0, .w_num / .w_den, NA_real_),
    # GDP per capita
    `GDP_RCAP-USD_PPP_PS-_T` = ifelse(total_population > 0,
                                      total_GDP_R / total_population,
                                      NA_real_),
    # BAU net GDP per capita
    GDPpc_net_BAU_NICE      = ifelse(total_pop_BAU > 0,
                                     total_GDP_net_BAU / total_pop_BAU,
                                     NA_real_),
    # Region label
    REF_AREA      = "OECD",
    # Shares
    share_fossil_fuel = (total_coal + total_gas + total_oil) / Total_energy_agg,
    share_renewables  = (total_hydro + total_other_renew) / Total_energy_agg
  ) %>%
  select(
    REF_AREA,
    TIME_PERIOD,
    `GHG_PBEM-T_CO2E-_T`   = total_emissions,
    `GDP_R-USD_PPP-_T`     = total_GDP_R,
    population_BAU_NICE    = total_pop_BAU,
    GDP_net_BAU_NICE       = total_GDP_net_BAU,  
    `POP-PS`               = total_population,
    Total_energy           = Total_energy_agg,
    share_fossil_fuel,
    share_renewables,
    sigma_NICE,
    sigma_OECD,
    `GDP_RCAP-USD_PPP_PS-_T`,
    GDPpc_net_BAU_NICE
  ) %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.nan(.) | . == 0, NA_real_, .)
  ))

# Append it to your main wide_df
wide_df <- bind_rows(oecd_summaries, wide_df %>%
  filter(REF_AREA != "OECD"))

# Build data for regression: compute ln_GDP_RCAP and log_sigma on wide_df
wide_df <- wide_df %>%
  mutate(
    # Build log versions only where the originals are > 0 and non‐missing
    ln_sigma_NICE = ifelse(!is.na(sigma_NICE) & sigma_NICE > 0,
                           log(sigma_NICE), 
                           NA_real_),
    ln_sigma_OECD = ifelse(!is.na(sigma_OECD) & sigma_OECD > 0,
                           log(sigma_OECD), 
                           NA_real_),
    ln_GDP_RCAP   = ifelse(!is.na(`GDP_RCAP-USD_PPP_PS-_T`) & `GDP_RCAP-USD_PPP_PS-_T` > 0,
                           log(`GDP_RCAP-USD_PPP_PS-_T`), 
                           NA_real_),
    ln_GDP_RCAP_sq = ln_GDP_RCAP^2, 
    efficiency_OECD = sigma_OECD / share_fossil_fuel, 
    POL_STRINGENCY_mean_NMKT_TS = (`POL_STRINGENCY-0_TO_6-TECHSUP` +
    `POL_STRINGENCY-0_TO_6-EPS_NMKT`)/2
  )

#–– Compute growth rates and merge back via left_join ––
growth_df <- wide_df %>%
  arrange(REF_AREA, as.integer(TIME_PERIOD)) %>%
  group_by(REF_AREA) %>%
  mutate(
    gr_sigma_NICE   = sigma_NICE / lag(sigma_NICE) - 1,
    gr_sigma_OECD   = sigma_OECD / lag(sigma_OECD) - 1,
    gr5_sigma_NICE  = sigma_NICE / lag(sigma_NICE, 5) - 1,
    gr5_sigma_OECD  = sigma_OECD / lag(sigma_OECD, 5) - 1,
    gr10_sigma_NICE = sigma_NICE / lag(sigma_NICE, 10) - 1,
    gr10_sigma_OECD = sigma_OECD / lag(sigma_OECD, 10) - 1
  ) %>%
  ungroup() %>%
  select(REF_AREA, TIME_PERIOD, starts_with("gr"))

wide_df <- wide_df %>%
  left_join(growth_df, by = c("REF_AREA", "TIME_PERIOD"))

# Ensure all columns except REF_AREA and TIME_PERIOD are numeric
wide_df <- wide_df %>%
  mutate(
    across(
      .cols = -c(REF_AREA, TIME_PERIOD),
      .fns = as.numeric
    )
  )


#######################################
######### DATAFRAME SUBSET  ###########
#######################################

df_for_Fabrice <- wide_df %>%
  mutate(
    TIME_PERIOD               = as.integer(TIME_PERIOD),
    ln_GDPpc_net_BAU_NICE     = log(GDPpc_net_BAU_NICE)
  ) %>%
  filter(TIME_PERIOD <= 2100) %>%
  select(
    REF_AREA,
    TIME_PERIOD,
    sigma_NICE,
    sigma_OECD,
    ln_sigma_OECD,
    ln_sigma_NICE,
    ln_GDP_RCAP,
    ln_GDP_RCAP_sq,
    ln_GDPpc_net_BAU_NICE,
    `POL_STRINGENCY-0_TO_6-TECHSUP`,
    `POL_STRINGENCY-0_TO_6-EPS`,
    `POL_STRINGENCY-0_TO_6-EPS_NMKT`,
    `POL_STRINGENCY-0_TO_6-EPS_MKT`,
    POL_STRINGENCY_mean_NMKT_TS,
    share_fossil_fuel,
    share_renewables,
    Total_energy, 
    efficiency_OECD
  )

write.csv(df_for_Fabrice, "df_for_Fabrice.csv", row.names = FALSE)


df_2020 <- wide_df %>%
    filter(TIME_PERIOD == "2020") 

# Compare distributions of sigma_NICE and sigma_OECD in 2020
# Pivot to long format for plotting both on same axes
df_2020_long_sigma <- df_2020 %>%
  select(sigma_NICE, sigma_OECD) %>%
  pivot_longer(
    cols = c(sigma_NICE, sigma_OECD),
    names_to  = "sigma_type",
    values_to = "sigma_value"
  )

# Prepare OECD time series
df_oecd <- wide_df %>%
  filter(REF_AREA == "OECD") %>%
  select(TIME_PERIOD, contains("sigma")) %>%
  mutate(
    # Convert TIME_PERIOD to integer for plotting
    TIME_PERIOD = as.integer(TIME_PERIOD),
    # Replace zeros or NaN with NA
    sigma_NICE  = ifelse(is.nan(sigma_NICE) | sigma_NICE == 0, NA_real_, sigma_NICE),
    sigma_OECD  = ifelse(is.nan(sigma_OECD) | sigma_OECD == 0, NA_real_, sigma_OECD)
  ) %>%
  # Drop rows where both sigma columns are NA
  filter(!(is.na(sigma_NICE) & is.na(sigma_OECD)))

# Build a little lookup of MEASURE → descriptive text, UNIT_MEASURE → Unit text, and Activity group
# For df_EPS (climate policy series), map CLIM_POL code to descriptive text
metadata_EPS <- df_EPS %>%
  select(
    MEASURE,
    MeasText     = Measure,
    UNIT_MEASURE,
    UnitText     = `Unit of measure`,
    ActivityCode = CLIM_POL,
    ActivityDesc = `Climate policies`
  ) %>%
  distinct() %>%
  mutate(ActivityType = "Climate policies")

# For df_GG (economic activity series), map ACTIVITY code to descriptive text
metadata_GG <- df_GG %>%
  select(
    MEASURE,
    MeasText     = Measure,
    UNIT_MEASURE,
    UnitText     = `Unit of measure`,
    ActivityCode = ACTIVITY,
    ActivityDesc = `Economic activity`
  ) %>%
  distinct() %>%
  mutate(ActivityType = "Economic Activity")

# Combine EPS and GG metadata and build code key
metadata_desc <- bind_rows(metadata_EPS, metadata_GG) %>%
  mutate(code = paste(MEASURE, UNIT_MEASURE, ActivityCode, sep = "-"))

# Build separate descriptive columns for each activity group
metadata_wide <- full_join(
  metadata_desc %>% select(code, `Climate policies` = ActivityDesc),
  metadata_desc %>% select(code, `Economic activity` = ActivityDesc),
  by = "code"
)

#######################################
####### CORRELATION COMPUTATION #######
#######################################

# Compute correlations vs. sigma_NICE
cors_df <- wide_df %>%
  # Keep only the year 2020, common to all datasets
  filter(TIME_PERIOD == "2020") %>%
  # drop the grouping keys so we only have numeric columns
  select(-REF_AREA, -TIME_PERIOD) %>%
  # compute cor(sigma_NICE, X) for every other column X
  summarise(across(-sigma_NICE,
                   ~ cor(.x, sigma_NICE, use = "pairwise.complete.obs"))) %>%
  pivot_longer(everything(),
               names_to  = "MESURE-UNIT-ACTIVITY",
               values_to = "corr") %>%
  mutate(abs_corr = abs(corr)) %>%
  arrange(desc(abs_corr))

# 3) Top 20 most highly (absolute) correlated measures
top20_measures <- cors_df %>% slice_head(n = 20) %>% pull(`MESURE-UNIT-ACTIVITY`)

# 4) Build a plotting dataframe from wide_df for 2020: sigma_NICE vs top 20 measures
plot_df <- wide_df %>%
  filter(TIME_PERIOD == "2020") %>%
  select(REF_AREA, TIME_PERIOD, sigma_NICE, all_of(top20_measures)) %>%
  pivot_longer(
    cols      = all_of(top20_measures),
    names_to  = "MESURE-UNIT-ACTIVITY",
    values_to = "value"
  )

# 5) Scatterplot: sigma_NICE on x, measure value on y, one panel per measure
ggplot(plot_df, aes(x = sigma_NICE, y = value)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ `MESURE-UNIT-ACTIVITY`, scales = "free_y") +
  labs(
    title = "Top 20 measures most correlated with σ",
    x     = "σ (exogenous emissions rate)",
    y     = "Measure OBS_VALUE"
  ) +
  theme_minimal()


#
# Assemble the top-20 table including correlation coefficient
top20_table <- tibble(code = top20_measures) %>%
  # Join human‐readable measure and unit
  left_join(
    metadata_desc %>% select(code, MeasText, UnitText) %>% distinct(),
    by = "code"
  ) %>%
  # Join the two activity columns
  left_join(metadata_wide, by = "code") %>%
  # Join the raw (non‐absolute) corr values
  left_join(
    cors_df %>% select(`MESURE-UNIT-ACTIVITY`, corr),
    by = c("code" = "MESURE-UNIT-ACTIVITY")
  ) %>%
  # Select and rename final columns
  select(
    Code                   = code,
    Measure                = MeasText,
    Unit                   = UnitText,
    `Economic activity`,
    `Climate policies`,
    `correlation coefficient` = corr
  )

# Print the result
print(top20_table)

# Save the correlation table to CSV
write.csv(top20_table, "correlation-with-sigma.csv", row.names = FALSE)


#######################################
############## PLOTS ##################
#######################################

# Plot the distribution for GHG_DBPROD-USD_KG_CO2E-_T from wide_df
ggplot(wide_df, aes(x = 1/`GHG_DBPROD-USD_KG_CO2E-_T`)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of GHG_DBPROD-USD_KG_CO2E-_T",
    x = "GHG_DBPROD-USD_KG_CO2E-_T (Inverse)",
    y = "Count"
  ) +
  theme_minimal()

# Plot the distribution for GHG_PBPROD-USD_KG_CO2E-_T from wide_df
ggplot(wide_df, aes(x = 1/`GHG_PBPROD-USD_KG_CO2E-_T`)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of GHG_PBPROD-USD_KG_CO2E-_T",
    x = "GHG_PBPROD-USD_KG_CO2E-_T (Inverse)",
    y = "Count"
  ) +
  theme_minimal()

  # Plot the distribution for CO2_DBPROD-USD_CO2-_T from wide_df
ggplot(wide_df, aes(x = 1/`CO2_DBPROD-USD_CO2-_T`)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of CO2_DBPROD-USD_CO2-_T",
    x = "CO2_DBPROD-USD_CO2-_T (Inverse)",
    y = "Count"
  ) +
  theme_minimal()

  # Plot the distribution for CO2_PBPROD-USD_CO2-_T from wide_df
ggplot(wide_df, aes(x = 1/`CO2_PBPROD-USD_CO2-_T`)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of CO2_PBPROD-USD_CO2-_T",
    x = "CO2_PBPROD-USD_CO2-_T (Inverse)",
    y = "Count"
  ) +
  theme_minimal()


  ggplot(wide_df, aes(y = `GHG_PBEM-T_CO2E-_T`/`GDP_R-USD_PPP-_T`, x = log(`GDP_RCAP-USD_PPP_PS-_T`))) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Scatterplot",
    y     = "σ (Production based emissions over GDP) - OECD",
    x     = "log(GDP per capita in USD PPP)"
  ) +
  theme_minimal()



# Overlaid histogram with distinct colors
ggplot(df_2020_long_sigma, aes(x = sigma_value, fill = sigma_type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(
    title = "Distribution of sigma_NICE vs. sigma_OECD (2020)",
    x = "Sigma value",
    y = "Count",
    fill = "Sigma type"
  ) +
  theme_minimal()

# Scatterplot: sigma_OECD vs sigma_NICE for 2020
ggplot(df_2020, aes(x = sigma_NICE, y = sigma_OECD)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  labs(
    title = "Scatterplot of sigma_OECD vs sigma_NICE (2020)",
    x = "sigma_NICE",
    y = "sigma_OECD"
  ) +
  coord_equal() +
  theme_minimal()


# Scatterplot: sigma_OECD vs renewable electricity production 
ggplot(wide_df, aes(x = `RE_NRG-PT_GEN_EL-_T`, y = sigma_OECD)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  labs(
    title = "Scatterplot of Renewable electricy generation vs sigma_OECD",
    x = "Renewable electricy generation",
    y = "sigma_OECD"
  ) +
  theme_minimal()


# 1) Primary plot: σ_NICE and σ_OECD
ylim_sigma <- range(c(df_oecd$sigma_NICE, df_oecd$sigma_OECD), na.rm = TRUE)
plot(
  df_oecd$TIME_PERIOD, df_oecd$sigma_NICE, type = "l", col = "blue",
  xlim = c(1995, 2150), ylim = ylim_sigma,
  xlab = "Year", ylab = "Sigma",
  main = "σ and Growth Rates over Time (OECD)"
)
lines(df_oecd$TIME_PERIOD, df_oecd$sigma_OECD, col = "darkgreen")

# 2) Overlay 5-year growth rates on right axis (only for 5-year intervals)
par(new = TRUE)
# Force growth‐rate axis between -0.05 and 0
ylim_gr <- c(-0.20, -0.05)
# Subset to 5-year periods (e.g., years divisible by 5)
df5 <- df_oecd[df_oecd$TIME_PERIOD %% 5 == 0, ]
plot(
  df5$TIME_PERIOD, df5$gr5_sigma_NICE, type = "p", col = "#ff00bf",
  xlim = c(1995, 2150), ylim = ylim_gr,
  axes = FALSE, xlab = "", ylab = ""
)
points(df5$TIME_PERIOD, df5$gr5_sigma_OECD, col = "purple", pch = 1)

# 3) Add the right‐hand axis and label
axis(side = 4)
mtext("Growth rate", side = 4, line = 3)

# 4) Legend
legend(
  "topright",
  legend = c("σ_OECD", "σ_NICE", "gr σ_OECD", "gr σ_NICE"),
  col    = c("darkgreen", "blue", "purple", "#ff00bf"),
  lty    = c(1, 1, 2, 2),
  bg     = "white"
)

# Save the plot to file
dev.copy(jpeg, filename = "sigma-OECD_region.jpg", width = 800, height = 600)

dev.off()


#######################################
############ REGRESSIONS ##############
#######################################

# (1) Fit simple linear regression without dummies
model_simple <- lm(
  ln_sigma_OECD ~ `POL_STRINGENCY-0_TO_6-TECHSUP` +
                `POL_STRINGENCY-0_TO_6-EPS_MKT` +
                `POL_STRINGENCY-0_TO_6-EPS_NMKT` +
                ln_GDP_RCAP,
  data = wide_df
)
cat("\n\n===== Simple OLS Regression Results =====\n")
print(summary(model_simple))

# (2) Fit with temporal (TIME_PERIOD) and spatial (REF_AREA) dummies
model_dummies <- lm(
  ln_sigma_OECD ~ `POL_STRINGENCY-0_TO_6-TECHSUP` +
                `POL_STRINGENCY-0_TO_6-EPS_MKT` +
                `POL_STRINGENCY-0_TO_6-EPS_NMKT` +
                ln_GDP_RCAP +
                factor(REF_AREA) +
                factor(TIME_PERIOD),
  data = wide_df
)
cat("\n\n===== OLS with Spatial and Temporal Dummies =====\n")
print(summary(model_dummies))

# (3) Fit with EPS and dummies
model_dummies <- lm(
  ln_sigma_OECD ~ `POL_STRINGENCY-0_TO_6-EPS` +
                ln_GDP_RCAP +
                factor(REF_AREA) +
                factor(TIME_PERIOD),
  data = wide_df
)
cat("\n\n===== OLS with Spatial and Temporal Dummies =====\n")
print(summary(model_dummies))


### LARGER REGRESSION ##
# 1.1 Find the code for “Renewable electricity generation”:
renew_code <- metadata_desc %>%
  filter(MeasText == "Renewable electricity generation") %>%
  pull(code)

# 1.2 Find all codes whose MeasText contains “R&D” or “RD&D”
rd_codes <- metadata_desc %>%
  filter(grepl("R&D|RD&D", MeasText, ignore.case = TRUE)) %>%
  pull(code)

# 1.3 Keep only those R&D codes that actually exist in wide_df
rd_codes <- intersect(rd_codes, names(wide_df))

# Double-check what remains
cat("R&D‐related codes in wide_df:\n")
print(rd_codes)

# Build iv_data but first drop any columns that are 100% NA
iv_candidates <- wide_df %>%
  select(
    ln_sigma_OECD,
    all_of(renew_code),
    all_of(rd_codes)
  )

# Count NAs per column and identify those completely NA
na_counts <- sapply(iv_candidates, function(col) sum(is.na(col)))
total_rows <- nrow(iv_candidates)
full_na_cols <- names(na_counts[na_counts == total_rows])

# Print and remove any fully-NA columns
for (col in full_na_cols) {
  cat("Column", col, "is empty, therefore removed from the dataset\n")
  # If it's in rd_codes, drop it
  rd_codes <- setdiff(rd_codes, col)
  # If it's the endogenous variable, warn user
  if (col == renew_code) {
    cat("Warning: endogenous variable", col, "is empty. Cannot proceed.\n")
  }
  # Remove from iv_candidates
  iv_candidates[[col]] <- NULL
}

# Now drop rows with any remaining NAs
iv_data <- iv_candidates %>%
  filter(!if_any(everything(), is.na))

# Construct the formula for ivreg:  # syntax is:   outcome ~ endogenous + controls | instruments
iv_formula <- as.formula(
  paste0(
    "ln_sigma_OECD ~ `", renew_code, "` | ",
    paste0("`", rd_codes, "`", collapse = " + ")
  )
)

iv_fit <- ivreg(iv_formula, data = iv_data)

# Print summary
cat("\n\n>>>>>>> IV regression via AER::ivreg() <<<<<<<\n")
print(summary(iv_fit, diagnostics = TRUE))



# (4) R&D and dummies on sigma_NICE
# Build formula text for all rd_codes
rd_terms <- paste0("`", rd_codes, "`", collapse = " + ")
fmla_rd <- as.formula(
  paste0("ln_sigma_OECD ~ ", rd_terms, " + ln_GDP_RCAP + factor(REF_AREA) + factor(TIME_PERIOD)")
)
model_rd_dummies <- lm(fmla_rd, data = wide_df)
cat("\n\n===== OLS with R&D Predictors and Spatial/Temporal Dummies =====\n")
print(summary(model_rd_dummies))


############## LASSO REGRESSION #################
exclude_pattern <- "^(TIME_PERIOD|REF_AREA|sigma_NICE|ln_sigma_NICE|GDP_net_BAU_NICE|ln_sigma_OECD|gr|CO2|GDP|POP|GHG_DB|GHG_PB)"

# Prepare data for LASSO: include only complete cases
lasso_data <- wide_df %>%
  # keep only years where sigma_OECD is defined (1995–2020)
  filter(
    !is.na(sigma_OECD),
    as.integer(TIME_PERIOD) >= 1995,
    as.integer(TIME_PERIOD) <= 2020
  ) %>%
  # select all columns except those matching exclude_pattern, plus response
  select(-matches(exclude_pattern), sigma_OECD)

# Identify and remove empty or single-valued predictor columns
predictor_cols <- setdiff(names(lasso_data), "sigma_OECD")
cols_to_remove <- character()
for (col in predictor_cols) {
  vals <- lasso_data[[col]]
  # Check if all NA
  if (all(is.na(vals))) {
    cat("Column", col, "is empty (all NA), removing\n")
    cols_to_remove <- c(cols_to_remove, col)
  } else {
    # Check if only one unique non-missing value
    uniq_vals <- unique(vals[!is.na(vals)])
    if (length(uniq_vals) <= 1) {
      cat("Column", col, "has single value", uniq_vals, ", removing\n")
      cols_to_remove <- c(cols_to_remove, col)
    }
  }
}
if (length(cols_to_remove) > 0) {
  lasso_data <- lasso_data %>% select(-all_of(cols_to_remove))
}



#   -----------------------------------------
#   Drop predictors with >60% missing values
predictor_cols <- setdiff(names(lasso_data), "sigma_OECD")
miss_rate <- sapply(lasso_data[predictor_cols], function(x) mean(is.na(x)))
# Identify high-missing predictors
high_miss <- names(miss_rate)[miss_rate > 0.6]
if (length(high_miss) > 0) {
  for (col in high_miss) {
    cat(sprintf("Dropping %s (%.1f%% missing) due to >60%% NA\n",
                col, miss_rate[col] * 100))
  }
  # Keep only low-missing predictors
  keep_cols <- setdiff(predictor_cols, high_miss)
  lasso_data <- lasso_data %>% select(all_of(keep_cols), sigma_OECD)
} else {
  keep_cols <- predictor_cols
}

#–– Add squared features for every predictor column ––
# 'keep_cols' contains the names of all predictor variables after cleaning
lasso_data <- lasso_data %>%
  mutate(
    across(
      .cols = all_of(keep_cols),
      .fns  = ~ .x^2,
      .names = "{.col}_sq"
    )
  )
#   -----------------------------------------

# Impute only the predictor columns and their squared versions; leave sigma_OECD unchanged
lasso_data <- kNN(
  lasso_data,
  variable = c(keep_cols, paste0(keep_cols, "_sq")),
  k = 5,
  imp_var = FALSE
)

# Build design matrix with interactions and squared terms
X <- model.matrix(sigma_OECD ~ (.)^2 - 1, data = lasso_data)

# Response vector
y <- lasso_data$sigma_OECD

# Check dimensions
cat("LASSO data: nrow(X) =", nrow(X), ", length(y) =", length(y), "\n")

#### LASSO VERSION ####
# set.seed(123)
# cv_lasso <- cv_glmnet(X, y, alpha = 1, standardize = TRUE)

#### ELASTIC NET VERSION ####
# Tune elastic net alpha via 10-fold CV
alphas <- seq(0, 1, by = 0.1)
cv_results <- lapply(alphas, function(a) {
  cv_fit <- cv.glmnet(
    X, y,
    alpha = a,
    standardize = TRUE,
    standardize.response = TRUE,
    nfolds = 10
  )
  list(alpha = a,
       cvm   = min(cv_fit$cvm),
       cvobj = cv_fit)
})
# Find alpha with lowest CV error
mse_vals   <- sapply(cv_results, `[[`, "cvm")
best_idx   <- which.min(mse_vals)
best_alpha <- cv_results[[best_idx]]$alpha
cat(sprintf("Best alpha = %.2f (MSE = %.4g)\n", best_alpha, mse_vals[best_idx]))

#
# Final CV fit with best alpha (already includes standardize.response = TRUE)
set.seed(123)
cv_lasso <- cv_results[[best_idx]]$cvobj

# Save CV error curve plot to lasso.jpg
jpeg("lasso.jpg", width = 800, height = 600)
# The cv_lasso object already contains standardize.response = TRUE
plot(cv_lasso)
dev.off()

# Extract coefficients at optimum lambda and save to CSV
lasso_coefs <- coef(cv_lasso, s = "lambda.1se")
coefs_df <- as.data.frame(as.matrix(lasso_coefs))
coefs_df$Variable <- rownames(coefs_df)
colnames(coefs_df)[1] <- "Coefficient"
# Reorder columns
coefs_df <- coefs_df[, c("Variable", "Coefficient")]
write.csv(coefs_df, "lasso_coeff.csv", row.names = FALSE)


lasso_coef_filtered <- coefs_df %>%
  filter(Coefficient != 0)
write.csv(lasso_coef_filtered, "lasso_coeff_nonzero.csv", row.names = FALSE)



# ----------------------
# Post-LASSO OLS on selected predictors via formula
# ----------------------
# Drop intercept
clean_vars <- setdiff(lasso_coef_filtered$Variable, "(Intercept)")

# Build formula string: sigma_OECD ~ var1 + var2 + var1:var2 + var_sq + ...
fmla_str <- paste(
  "sigma_OECD ~", 
  paste(clean_vars, collapse = " + ")
)
fmla <- as.formula(fmla_str)

# Fit the linear model using formula (generates interactions automatically)
glm_post <- lm(fmla, data = lasso_data)


cat("\n\n===== Post-LASSO OLS Results =====\n")
print(summary(glm_post))


# Helper: map code to verbose "Measure (Unit, Activity)" using metadata_desc
code_to_verbose <- function(code) {
  # Remove backticks
  code <- gsub("`", "", code)
  # If it’s a squared term, recursively call and place ² outside parentheses
  if (grepl("_sq$", code)) {
    base_code    <- sub("_sq$", "", code)
    base_verbose <- code_to_verbose(base_code)
    # Represent the squared term as (base_verbose)²
    return(paste0("(", base_verbose, ")²"))
  }
  # Split into components (Measure-Unit-Activity)
  parts <- strsplit(code, "-", fixed = TRUE)[[1]]
  # Rebuild for matching to metadata
  idx <- match(code, metadata_desc$code)
  if (!is.na(idx)) {
    meas <- metadata_desc$MeasText[idx]
    unit <- metadata_desc$UnitText[idx]
    act  <- NA
    # Use correct field for activity if present
    if ("ActivityDesc" %in% names(metadata_desc)) {
      act <- metadata_desc$ActivityDesc[idx]
    }
    out <- meas
    if (!is.na(unit) && nzchar(unit)) out <- paste0(out, " (", unit, ")")
    if (!is.na(act) && nzchar(act)) out <- paste0(out, ", ", act)
    return(out)
  }
  # Fallback: just return code if no match
  return(code)
}

# Map terms to verbose labels using metadata_desc
map_term_verbose <- function(t) {
  if (t == "(Intercept)") return("Intercept")
  if (grepl(":", t, fixed = TRUE)) {
    parts <- strsplit(t, ":", fixed = TRUE)[[1]]
    labels <- sapply(parts, code_to_verbose)
    # Wrap in parentheses only if not already wrapped
    wrapped <- sapply(labels, function(lbl) {
      if (grepl("^\\(", lbl)) lbl else paste0("(", lbl, ")")
    })
    # Build interaction label without redundant parentheses
    return(paste0(wrapped[1], " x ", wrapped[2]))
  }
  # Otherwise, just delegate to code_to_verbose:
  code_to_verbose(t)
}

# Build verbose coefficient table
post_sum <- summary(glm_post)
coef_mat <- post_sum$coefficients  # Estimate, Std. Error, t value, Pr(>|t|)
terms <- rownames(coef_mat)
df_verbose <- data.frame(
  Term        = terms,
  Coefficient = coef_mat[, "Estimate"],
  PValue      = coef_mat[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
)
df_verbose$Signif <- symnum(
  df_verbose$PValue,
  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  symbols   = c("***", "**", "*", ".", " ")
)

# Map to explicit variable names
df_verbose$Label <- sapply(df_verbose$Term, map_term_verbose)
df_out <- df_verbose %>%
  arrange(PValue) %>%
  select(Label, Coefficient, Signif)
write.csv(df_out, "post_lasso_verbose.csv", row.names = FALSE, quote = TRUE)


# ############## SIMPLER REGRESSION #################

# ------ Share of energy, GDP and EPS -------

mod_nrg <- lm(ln_sigma_OECD ~ 
   `POL_STRINGENCY-0_TO_6-EPS` +
   share_fossil_fuel +
   share_renewables +
   ln_GDP_RCAP + 
   ln_GDP_RCAP_sq +
   factor(REF_AREA) + 
   factor(TIME_PERIOD),
 data = wide_df)

summary(mod_nrg)



# =====================================
# Model Comparison and Selection Block
# =====================================

# 1) Policy stringency variables of interest
policy_vars <- c(
  "POL_STRINGENCY-0_TO_6-EPS",
  "POL_STRINGENCY-0_TO_6-EPS_MKT",
  "POL_STRINGENCY-0_TO_6-EPS_NMKT",
  "POL_STRINGENCY-0_TO_6-TECHSUP",
  "POL_STRINGENCY_mean_NMKT_TS", 
  "share_fossil_fuel",
  "share_renewables"
)
# Generate individual lags 1 through 10 for each policy variable
wide_df <- wide_df %>%
  group_by(REF_AREA) %>%
  arrange(as.integer(TIME_PERIOD))
for (pv in policy_vars) {
  for (k in 0:10) {
    lag_name <- if (k == 0) pv else paste0(pv, "_lag", k)
    wide_df <- wide_df %>%
      mutate(
        !!lag_name := dplyr::lag(.data[[pv]], k)
      )
  }
}
wide_df <- wide_df %>% ungroup()

#
# 2) Programmatic generation of regression formulas
FE_options <- list(
  none = "",
  REF  = "factor(REF_AREA)",
  TIME = "factor(TIME_PERIOD)",
  BOTH = "factor(REF_AREA) + factor(TIME_PERIOD)"
)
policy_sets <- list(
  EPS    = "POL_STRINGENCY-0_TO_6-EPS",
  DISAGG = c(
    "POL_STRINGENCY-0_TO_6-EPS_MKT",
    "POL_STRINGENCY-0_TO_6-EPS_NMKT",
    "POL_STRINGENCY-0_TO_6-TECHSUP"
  ),
  NRG = c(
    "share_fossil_fuel",
    "share_renewables"
  ),
  EPSNRG = c(
    "POL_STRINGENCY-0_TO_6-EPS",
    "share_fossil_fuel",
    "share_renewables"
  ),
  DISAGGNRG = c(
    "POL_STRINGENCY-0_TO_6-EPS_MKT",
    "POL_STRINGENCY-0_TO_6-EPS_NMKT",
    "POL_STRINGENCY-0_TO_6-TECHSUP", 
    "share_fossil_fuel",
    "share_renewables"
  )
)
# Helper to generate lag variable names 1–10
dag_lags <- function(vars) {
  unlist(lapply(vars, function(v) sprintf("%s_lag%d", v, 1:10)))
}
# Build formulas list
formulas <- list()
for (fe_name in names(FE_options)) {
  fe_term <- FE_options[[fe_name]]
  for (ps_name in names(policy_sets)) {
    pv <- policy_sets[[ps_name]]
    # Base covariates
    base_terms <- c(
      if (fe_term != "") fe_term,
      "ln_GDP_RCAP", "ln_GDP_RCAP_sq"
    )
    # 2a) No policy
    f_base <- sprintf(
      "ln_sigma_OECD ~ %s",
      paste(base_terms, collapse = " + ")
    )
    formulas[[paste(fe_name, ps_name, "base", sep = "_")]] <- as.formula(f_base)
    # 2b) Interaction with policy set (quote each variable)
    raw_pvs  <- policy_sets[[ps_name]]
    quoted_pvs <- paste0("`", raw_pvs, "`")
    pol_term <- paste(quoted_pvs, collapse = " + ")
    int_term <- sprintf("ln_GDP_RCAP * (%s)", pol_term)
    f_int <- sprintf(
      "ln_sigma_OECD ~ %s + %s",
      paste(base_terms, collapse = " + "),
      int_term
    )
    formulas[[paste(fe_name, ps_name, "int", sep = "_")]] <- as.formula(f_int)
    # 2c) Interaction + lags (quote each lagged variable)
    raw_lags <- dag_lags(policy_sets[[ps_name]])
    quoted_lags <- paste0("`", raw_lags, "`")
    lag_term_str <- paste(quoted_lags, collapse = " + ")
    f_lags <- sprintf(
      "ln_sigma_OECD ~ %s + %s + %s",
      paste(base_terms, collapse = " + "),
      int_term,
      lag_term_str
    )
    formulas[[paste(fe_name, ps_name, "int_lags", sep = "_")]] <- as.formula(f_lags)
  }
}

# 3) Fit each model and compute AIC
model_fits <- lapply(formulas, function(f) lm(f, data = wide_df))
aic_vals   <- sapply(model_fits, AIC)

# 4) Select best model by lowest AIC
best_name  <- names(aic_vals)[which.min(aic_vals)]
cat("Best model by AIC: ", best_name, " (AIC = ", round(aic_vals[best_name], 2), ")\n", sep = "")

# 5) Print summary of best model
cat("\n===== Summary of ", best_name, " =====\n", sep = "")


print(summary(model_fits[[best_name]]))


# =====================================
#  Model Validation: Hold-Out and Rolling Origin
# =====================================
# 1) Hold-out test (last 3 years: 2018–2020)
hold_years <- as.character(2018:2020)
train_df <- wide_df %>% filter(!TIME_PERIOD %in% hold_years)
test_df  <- wide_df %>% filter(TIME_PERIOD %in% hold_years)

hold_model <- update(model_fits[[best_name]], . ~ . - factor(REF_AREA))
test_pred  <- predict(hold_model,     newdata = test_df)

# Compute errors (on ln_sigma_OECD)
rmse_hold <- sqrt(mean((test_df$ln_sigma_OECD - test_pred)^2, na.rm = TRUE))
mae_hold  <- mean(abs(test_df$ln_sigma_OECD - test_pred),    na.rm = TRUE)
cat(sprintf("Hold-out (2018–2020)   RMSE = %.4f, MAE = %.4f\n", rmse_hold, mae_hold))

# 2) Rolling-origin evaluation year by year
years      <- sort(unique(as.integer(wide_df$TIME_PERIOD)))
roll_errors <- data.frame(YEAR = integer(), MSE = double())

# start at year +10 to ensure at least 10 years of training
for (y in (min(years)+10):2019) {
  train_i <- wide_df %>% filter(as.integer(TIME_PERIOD) <= y)
  test_i  <- wide_df %>% filter(as.integer(TIME_PERIOD) == y+1)
  if (nrow(test_i)==0) next
  # Fit without country or time fixed effects
  mod_i <- lm(
    update(
      formulas[[best_name]],
      . ~ . - factor(REF_AREA) - factor(TIME_PERIOD)
    ),
    data = train_i
  )
  pred_i  <- predict(mod_i, newdata = test_i)
  mse_i   <- mean((test_i$ln_sigma_OECD - pred_i)^2, na.rm = TRUE)
  roll_errors <- rbind(roll_errors,
                       data.frame(YEAR = y+1, MSE = mse_i))
}

avg_roll <- mean(roll_errors$MSE, na.rm = TRUE)
cat(sprintf("Rolling-origin average MSE = %.4f\n", avg_roll))
print(roll_errors)


# =========================================
# 1) Monte Carlo Forecast of σ_OECD (2021–2100)
# =========================================
set.seed(123)
# 1) Prepare historical GDP series for OECD
gdppc_hist <- wide_df %>%
  filter(REF_AREA == "OECD", as.integer(TIME_PERIOD) <= 2020) %>%
  arrange(as.integer(TIME_PERIOD)) %>%
  pull(`GDP_RCAP-USD_PPP_PS-_T`)
# Compute annual growth rates
g_hist <- diff(gdppc_hist) / head(gdppc_hist, -1)
mean_g <- mean(g_hist, na.rm = TRUE)
sd_g   <- sd(g_hist,   na.rm = TRUE)

# 2) Simulate N Monte Carlo paths
N    <- 500    # number of simulations
H    <- 80     # years 2021–2100
last_gdppc <- tail(gdppc_hist, 1)

sims <- replicate(N, {
  # draw future growth shocks
  g_draws <- rnorm(H, mean = mean_g, sd = sd_g)
  # cumulative GDP path
  gdppc_fc  <- cumprod(c(last_gdppc, 1 + g_draws))[-1]
  # return vector
  gdppc_fc
})
# sims is H x N matrix

dates <- 2021:2100

# 3) Build a data frame of all simulations
mc_df <- expand.grid(
  YEAR = dates,
  sim  = 1:N
) %>%
  arrange(sim, YEAR) %>%
  mutate(
    GDP_RCAP = as.vector(sims)
  ) %>%
  # Compute regression inputs
  mutate(
    ln_GDP_RCAP    = log(GDP_RCAP),
    ln_GDP_RCAP_sq = ln_GDP_RCAP^2,
    REF_AREA       = "OECD",
    TIME_PERIOD    = as.character(YEAR)
  )

# ==== NICE‐GDPpc ====
# Build a dataframe with known GDPpc_net_BAU_NICE to 2100
NICE_pred_df <- wide_df %>%
  mutate(
    TIME_PERIOD     = as.integer(TIME_PERIOD),
    ln_GDP_RCAP     = log(GDPpc_net_BAU_NICE),
    ln_GDP_RCAP_sq  = ln_GDP_RCAP^2
  ) %>%
  filter(REF_AREA == "OECD", TIME_PERIOD >= 2021 & TIME_PERIOD <= 2100) %>%
  select(where(~ !all(is.na(.)) & !all(. == "")))


# 4) Refitting forecast model: drop only time fixed effects so OECD FE remains
forecast_model <- update(model_fits[[best_name]], . ~ . - factor(TIME_PERIOD))

#
# ---- Ensure forecast covariates exist in newdata (OECD) ----
# Build OECD covariates (policy + energy shares) for 1990–2100 and carry-forward last observed values
oecd_covars <- wide_df %>%
  filter(REF_AREA == "OECD") %>%
  transmute(
    TIME_PERIOD = as.integer(TIME_PERIOD),
    `POL_STRINGENCY-0_TO_6-EPS`,
    `POL_STRINGENCY-0_TO_6-EPS_MKT`,
    `POL_STRINGENCY-0_TO_6-EPS_NMKT`,
    `POL_STRINGENCY-0_TO_6-TECHSUP`,
    share_fossil_fuel,
    share_renewables
  ) %>%
  tidyr::complete(TIME_PERIOD = 1990:2100) %>%
  arrange(TIME_PERIOD) %>%
  tidyr::fill(
    `POL_STRINGENCY-0_TO_6-EPS`,
    `POL_STRINGENCY-0_TO_6-EPS_MKT`,
    `POL_STRINGENCY-0_TO_6-EPS_NMKT`,
    `POL_STRINGENCY-0_TO_6-TECHSUP`,
    share_fossil_fuel,
    share_renewables,
    .direction = "down"
  )

# Generate 1–10 lags (if the selected model includes them)
for (pv in c("POL_STRINGENCY-0_TO_6-EPS",
             "POL_STRINGENCY-0_TO_6-EPS_MKT",
             "POL_STRINGENCY-0_TO_6-EPS_NMKT",
             "POL_STRINGENCY-0_TO_6-TECHSUP",
             "share_fossil_fuel",
             "share_renewables")) {
  for (k in 1:10) {
    lag_name <- paste0(pv, "_lag", k)
    oecd_covars[[lag_name]] <- dplyr::lag(oecd_covars[[pv]], k)
  }
}

# Join these covariates into the Monte Carlo and NICE forecast frames
mc_df <- mc_df %>%
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
  left_join(oecd_covars, by = "TIME_PERIOD")

NICE_pred_df <- NICE_pred_df %>%
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
  left_join(oecd_covars, by = "TIME_PERIOD")

# -- Helper to ensure all regressors requested by forecast_model exist in newdata
ensure_covars_for <- function(newdf) {
  # 1) Which variables does the model expect (term labels)?
  tl <- attr(terms(forecast_model), "term.labels")

  # Strip wrapper 'factor(...)' so we only keep the underlying variable name
  base_terms <- gsub("^factor\\((.+)\\)$", "\\1", tl)

  # 2) Variables we will provide via the forecast frames themselves
  provided_directly <- c("ln_GDP_RCAP", "ln_GDP_RCAP_sq")

  # 3) Compute which additional covariates are missing in newdf
  needed <- setdiff(base_terms, provided_directly)
  missing_vars <- setdiff(needed, names(newdf))

  # 4) If the model needs extra covariates (e.g., energy shares or EPS / their lags),
  #    bring them from oecd_covars by TIME_PERIOD and carry-forwarded values
  if (length(missing_vars) > 0) {
    # Keep only the columns that actually exist in oecd_covars
    miss_in_covars <- setdiff(missing_vars, names(oecd_covars))
    if (length(miss_in_covars) > 0) {
      message("Warning: the following model terms are not present in 'oecd_covars' and will remain missing: ",
              paste(miss_in_covars, collapse = ", "))
    }
    keep_vars <- intersect(missing_vars, names(oecd_covars))
    if (length(keep_vars) > 0) {
      newdf <- newdf %>%
        dplyr::mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
        dplyr::left_join(oecd_covars %>% dplyr::select(TIME_PERIOD, dplyr::all_of(keep_vars)),
                         by = "TIME_PERIOD")
    }
  }

  # 5) Align factor levels for REF_AREA if the model contains country FE
  if (!is.null(forecast_model$xlevels) && "REF_AREA" %in% names(forecast_model$xlevels)) {
    newdf$REF_AREA <- factor(newdf$REF_AREA, levels = forecast_model$xlevels$REF_AREA)
  }

  return(newdf)
}

# 5) Predict ln_sigma and back-transform
mc_df <- ensure_covars_for(mc_df)
# --- Intercept alignment to 2020 sigma_NICE (chain-linking) ------------------
# Anchor target (falls back to sigma_OECD if sigma_NICE is missing)
sigmaNICE_2020 <- wide_df %>%
  dplyr::filter(REF_AREA == "OECD", TIME_PERIOD == "2020") %>%
  dplyr::pull(sigma_NICE) %>%
  .[1]
if (is.na(sigmaNICE_2020)) {
  sigmaNICE_2020 <- wide_df %>%
    dplyr::filter(REF_AREA == "OECD", TIME_PERIOD == "2020") %>%
    dplyr::pull(sigma_OECD) %>%
    .[1]
}
ln_sigmaNICE_2020 <- log(sigmaNICE_2020)

# Use NICE GDPpc for 2020 if available, else observed ln_GDP_RCAP
ln_gdp_2020_NICE <- wide_df %>%
  dplyr::filter(REF_AREA == "OECD", TIME_PERIOD == "2020") %>%
  dplyr::transmute(val = dplyr::if_else(!is.na(GDPpc_net_BAU_NICE),
                                        log(GDPpc_net_BAU_NICE), ln_GDP_RCAP)) %>%
  dplyr::pull(val) %>%
  .[1]

# Build a 2020 anchor row and ensure all covariates required by the model exist
anchor_df_forecast <- tibble::tibble(
  TIME_PERIOD    = 2020L,
  REF_AREA       = "OECD",
  ln_GDP_RCAP    = ln_gdp_2020_NICE,
  ln_GDP_RCAP_sq = ln_gdp_2020_NICE^2
)
# If you defined `ensure_covars_for()` above, run it here to add any missing regressors:
if (exists("ensure_covars_for")) {
  anchor_df_forecast <- ensure_covars_for(anchor_df_forecast)
}

# Model-implied ln(sigma) at 2020 and constant shift to hit sigma_NICE(2020)
ln_sigma_anchor_forecast <- as.numeric(predict(forecast_model, newdata = anchor_df_forecast))
shift_forecast           <- ln_sigmaNICE_2020 - ln_sigma_anchor_forecast
# ----------------------------------------------------------------------------- 
mc_df$ln_sigma_pred <- predict(forecast_model, newdata = mc_df) + shift_forecast
mc_df$sigma_pred    <- exp(mc_df$ln_sigma_pred)

# Same, using NICE GDPpc rather than MC
NICE_pred_df <- ensure_covars_for(NICE_pred_df)
NICE_pred_df$ln_sigma_scen <- predict(forecast_model, newdata = NICE_pred_df) + shift_forecast
NICE_pred_df$sigma_scen    <- exp(NICE_pred_df$ln_sigma_scen)

# 6) Summarize simulation distribution
fc_summary <- mc_df %>%
  group_by(YEAR) %>%
  summarise(
    median = median(sigma_pred, na.rm = TRUE),
    lower  = quantile(sigma_pred, 0.025, na.rm = TRUE),
    upper  = quantile(sigma_pred, 0.975, na.rm = TRUE)
  )

# 7) Actual sigma_NICE and historical sigma_OECD for OECD
sigma_for_oecd_reg <- wide_df %>%
  filter(REF_AREA == "OECD") %>%
  mutate(
    YEAR = as.integer(TIME_PERIOD)
  ) %>%
  select(YEAR, sigma_NICE, sigma_OECD)

# 8) Plot and save to file using ggsave (no dev.off)
forecast_plot <- ggplot() +
  # sigma_NICE
  geom_line(
    data = sigma_for_oecd_reg %>% filter(sigma_NICE > 0),
    aes(x = YEAR, y = sigma_NICE, color = "sigma_NICE"), linewidth = 1
  ) +
  # historical sigma_OECD
  geom_line(
    data = sigma_for_oecd_reg %>% filter(sigma_OECD > 0),
    aes(x = YEAR, y = sigma_OECD, color = "sigma_OECD"), linewidth = 1
  ) +
  # Monte Carlo median forecast
  geom_line(data = fc_summary, aes(x = YEAR, y = median, color = "Forecast median"), linetype = "dashed", linewidth = 1) +
  # Forecast uncertainty ribbon
  geom_ribbon(data = fc_summary, aes(x = YEAR, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.4) +
  # NICE GDPpc scenario
  geom_line(
    data = NICE_pred_df,
    aes(x = TIME_PERIOD, y = sigma_scen, color = "NICE GDPpc projection"),
    linetype = "dotted", linewidth = 1
  ) +
  scale_x_continuous(limits = c(1990, 2100), expand = c(0, 0)) +
  labs(
    title = "Monte Carlo Forecast of σ_OECD (2021–2100)",
    x     = "Year",
    y     = "Sigma",
    color = "Series"
  ) +
  scale_color_manual(
    values = c(
      "sigma_NICE"      = "blue",
      "sigma_OECD"      = "darkgreen",
      "Forecast median" = "red",
      "NICE GDPpc projection" = "black"
    )
  ) +
  theme_minimal()

ggsave("forecast-sigma.jpg", plot = forecast_plot, width = 9, height = 6, units = "in", dpi = 100)


# =========================================
# 2) OECD‑only Model Selection & Forecast w/ Anchor + GDPpc Scenario
# =========================================
# Historical subset for OECD 1990–2020
oecd_hist <- wide_df %>%
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
  filter(REF_AREA == "OECD",
         TIME_PERIOD >= 1990,
         TIME_PERIOD <= 2020) %>%
  select(where(~ !all(is.na(.)) & !all(. == "")))


# For OECD-only forecasting, use a simple GDP per capita quadratic
formulas_no_country_FE <- list(
  simple = ln_sigma_OECD ~ ln_GDP_RCAP + ln_GDP_RCAP_sq
)
# Fit each OECD-only candidate model
oecd_fits <- lapply(formulas_no_country_FE, function(f) lm(f, data = oecd_hist))
# Compute AIC for each model
oecd_aics <- sapply(oecd_fits, AIC)
oecd_best <- names(oecd_aics)[which.min(oecd_aics)]
oecd_model <- oecd_fits[[oecd_best]]
cat("OECD‑only best model: ", oecd_best, "\n")
print(summary(oecd_fits[[oecd_best]]))

# Monte Carlo forecast anchored at 2020
set.seed(123)
gdppc_sigma_for_oecd_reg <- oecd_hist %>% pull(`GDP_RCAP-USD_PPP_PS-_T`)
g_sigma_for_oecd_reg <- diff(gdppc_sigma_for_oecd_reg) / head(gdppc_sigma_for_oecd_reg, -1)
mean_g_OECD <- mean(g_sigma_for_oecd_reg, na.rm=TRUE)
sd_g_OECD   <- sd(g_sigma_for_oecd_reg, na.rm=TRUE)
last_gdppc_OECD <- tail(gdppc_sigma_for_oecd_reg, 1)
N <- 500; H <- 80 # 2021–2100
sims <- replicate(N, cumprod(c(last_gdppc_OECD, 1 + rnorm(H, mean_g_OECD, sd_g_OECD)))[-1])
dates <- 2021:2100
mc_oecd <- expand.grid(TIME_PERIOD = dates, sim = 1:N) %>%
  arrange(sim, TIME_PERIOD) %>%
  mutate(
    ln_GDP_RCAP = log(as.vector(sims)),
    ln_GDP_RCAP_sq = ln_GDP_RCAP^2,
    REF_AREA    = "OECD"
  )
mc_oecd$ln_sigma_pred <- predict(oecd_model, newdata = mc_oecd)
mc_oecd$sigma_pred    <- exp(mc_oecd$ln_sigma_pred)

# Summarize and anchor
fc_oecd <- mc_oecd %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    median = median(sigma_pred, na.rm=TRUE),
    lower  = quantile(sigma_pred, 0.025, na.rm=TRUE),
    upper  = quantile(sigma_pred, 0.975, na.rm=TRUE)
  )
base_val  <- oecd_hist %>% filter(TIME_PERIOD == 2020) %>% pull(sigma_OECD)
anchor    <- tibble(TIME_PERIOD = 2020, median = base_val, lower = base_val, upper = base_val)
fc_oecd   <- bind_rows(anchor, fc_oecd)

# Scenario using known GDPpc_net_BAU_NICE to 2100
scenario_df <- wide_df %>%
  mutate(
    TIME_PERIOD = as.integer(TIME_PERIOD),
    ln_GDP_RCAP = log(GDPpc_net_BAU_NICE),
    ln_GDP_RCAP_sq = ln_GDP_RCAP^2
   ) %>%
  filter(REF_AREA == "OECD",
         TIME_PERIOD >= 2021) %>%
  select(where(~ !all(is.na(.)) & !all(. == "")))

scenario_df$ln_sigma_scen <- predict(oecd_model, newdata = scenario_df)
scenario_df$sigma_scen    <- exp(scenario_df$ln_sigma_scen)

# =========================================
# 3) Hierarchical (multilevel) Model & Projection
# =========================================
# Fit varying‐intercept and slope model up to 2020
## Compute training mean of ln_GDP_RCAP for centering (use only training data, <= 2020)
tmp_data <- wide_df %>%
  filter(as.integer(TIME_PERIOD) <= 2020)
mu_lngdp <- mean(tmp_data$ln_GDP_RCAP, na.rm = TRUE)

## Center main wide_df using the *same* mu_lngdp
wide_df <- wide_df %>%
  mutate(
    c_lGDP  = ln_GDP_RCAP - mu_lngdp,
    c_lGDP2 = c_lGDP^2
  )

hier_mod <- lmer(ln_sigma_OECD ~ c_lGDP + c_lGDP2 + (1 + c_lGDP || REF_AREA), data=wide_df)
cat("\nHierarchical model summary:\n")
print(summary(hier_mod))

# Forecast OECD using GDPpc scenario, center using same mu_lngdp
hier_df <- scenario_df %>%
  mutate(
    c_lGDP  = log(GDPpc_net_BAU_NICE) - mu_lngdp,
    c_lGDP2 = c_lGDP^2
  )

hier_df$ln_sigma_hier <- predict(hier_mod, newdata = hier_df, allow.new.levels = TRUE)
hier_df$sigma_hier    <- exp(hier_df$ln_sigma_hier)


#### This commented part might be wrong ####
# # Build NICE-based GDPpc path for recurrence (OECD, 2021–2100)
# rec_df <- wide_df %>%
#   filter(REF_AREA == "OECD") %>%
#   transmute(
#     TIME_PERIOD    = as.integer(TIME_PERIOD),
#     ln_GDP_RCAP    = log(GDPpc_net_BAU_NICE),
#     ln_GDP_RCAP_sq = ln_GDP_RCAP^2,
#     sigma_NICE     = sigma_NICE
#   ) %>%
#   filter(TIME_PERIOD >= 2021, TIME_PERIOD <= 2100) %>%
#   arrange(TIME_PERIOD)

########################################
####### PREVISION FROM VARIATIONS ######
########################################
# # ----------------------------------------
# # Recurrence-based forecasts: NICE vs MC GDP, both vs β₂-only
# # ----------------------------------------
# sources <- list(
#   NICE = rec_df,
#   MC   = mc_df %>%
#     group_by(TIME_PERIOD) %>%
#     summarise(
#       GDP_RCAP = median(GDP_RCAP, na.rm=TRUE),
#       .groups = 'drop'
#     ) %>%
#     arrange(TIME_PERIOD) %>%
#     mutate(
#       ln_GDP_RCAP    = log(GDP_RCAP),
#       ln_GDP_RCAP_sq = ln_GDP_RCAP^2
#     )

# # Define term modes: both β₁+β₂, or β₂ only
# modes <- list(
#   `β₁+β₂` = list(b1 = beta1, b2 = beta2),
#   `β₂ only` = list(b1 = 0,     b2 = beta2)
# )
# # Build all recurrences
# all_recs <- purrr::map_dfr(names(sources), function(src) {
#   base <- sources[[src]]
#   purrr::map_dfr(names(modes), function(mn) {
#     mm <- modes[[mn]]
#     rec <- base %>% mutate(ln_sigma_rec = NA_real_)
#     # seed with 2020 log-sigma_OECD
#     rec$ln_sigma_rec[1] <- log(
#       sigma_for_oecd_reg$sigma_OECD[sigma_for_oecd_reg$YEAR == 2020]
#     )
#     for(i in 2:nrow(rec)) {
#       d1 <- rec$ln_GDP_RCAP[i]    - rec$ln_GDP_RCAP[i-1]
#       d2 <- rec$ln_GDP_RCAP_sq[i] - rec$ln_GDP_RCAP_sq[i-1]
#       rec$ln_sigma_rec[i] <- rec$ln_sigma_rec[i-1] + mm$b1*d1 + mm$b2*d2
#     }
#     rec <- rec %>% mutate(
#       sigma_rec = exp(ln_sigma_rec),
#       Scenario  = paste(src, mn)
#     )
#     rec
#   })
# })



# sources <- list(
#   NICE = rec_df,
#   MC = mc_df %>%
#     group_by(TIME_PERIOD) %>%
#     summarise(
#       GDP_RCAP = median(GDP_RCAP, na.rm = TRUE),
#       .groups  = 'drop'
#     ) %>%
#     arrange(TIME_PERIOD) %>%
#     mutate(
#       TIME_PERIOD     = as.integer(TIME_PERIOD),
#       ln_GDP_RCAP     = log(GDP_RCAP),
#       ln_GDP_RCAP_sq  = ln_GDP_RCAP^2
#     )
# )
# # Define term modes: both β₁+β₂, or β₂ only
# modes <- list(
#   `β₁+β₂` = list(b1 = beta1, b2 = beta2),
#   `β₂ only` = list(b1 = 0,     b2 = beta2)
# )
# # Build all recurrences
# all_recs <- purrr::map_dfr(names(sources), function(src) {
#   base <- sources[[src]]
#   purrr::map_dfr(names(modes), function(mn) {
#     mm <- modes[[mn]]
#     rec <- base %>% mutate(ln_sigma_rec = NA_real_)
#     # seed with 2021 log-sigma_NICE
#     rec$ln_sigma_rec[1] <- log(rec_df$sigma_NICE[rec_df$TIME_PERIOD == 2021])
#     for(i in 2:nrow(rec)) {
#       d1 <- rec$ln_GDP_RCAP[i]    - rec$ln_GDP_RCAP[i-1]
#       d2 <- rec$ln_GDP_RCAP_sq[i] - rec$ln_GDP_RCAP_sq[i-1]
#       rec$ln_sigma_rec[i] <- rec$ln_sigma_rec[i-1] + mm$b1*d1 + mm$b2*d2
#     }
#     rec <- rec %>% mutate(
#       sigma_rec = exp(ln_sigma_rec),
#       Scenario  = paste(src, mn)
#     )
#     rec
#   })
# })
# # Plot all four recurrence forecasts plus observed
# recurrence_forecast_plot <- ggplot(all_recs, aes(x = TIME_PERIOD, y = sigma_rec, color = Scenario)) +
#   geom_line(size = 1, linetype = "dotted") +
#   # Add observed sigma
#   geom_line(data = oecd_hist %>% filter(sigma_OECD > 0),
#             aes(x = TIME_PERIOD, y = sigma_OECD, color = "Observed", group = 1), linewidth = 1) +
#   geom_line(data = scenario_df %>% filter(sigma_NICE > 0),
#             aes(x = as.integer(TIME_PERIOD), y = sigma_NICE, color = "sigma_NICE", group = 1), linewidth = 1) +
#   scale_color_manual(
#     values = c(
#       "NICE β₁+β₂" = "steelblue",
#       "NICE β₂ only" = "skyblue",
#       "MC β₁+β₂"   = "firebrick",
#       "MC β₂ only" = "salmon",
#       "Observed"   = "darkgreen",
#       "sigma_NICE" = "blue"
#     )
#   ) +
#   labs(
#     title = "Recurrence Forecasts of σ (2021–2100)",
#     x     = "Year",
#     y     = "Sigma",
#     color = "Scenario"
#   ) +
#   scale_x_continuous(limits = c(1995, 2100), expand = c(0, 0)) +
#   theme_minimal()

# # Save the recurrence plot
# ggsave("recurrence-forecast-plot.jpg", plot = recurrence_forecast_plot, width = 10, height = 5, units = "in", dpi = 100)



# =========================================
# Full Plotting of All Forecasts
# =========================================
oecd_forecast_plot <- ggplot() +
  # Observed σ_OECD (historical)
  geom_line(
    data = oecd_hist %>% dplyr::filter(sigma_OECD > 0),
    aes(x = TIME_PERIOD, y = sigma_OECD, color = "Observed", group = 1),
    linewidth = 1
  ) +
  # σ_NICE (exogenous)
  geom_line(
    data = scenario_df %>% dplyr::filter(sigma_NICE > 0),
    aes(x = as.integer(TIME_PERIOD), y = sigma_NICE, color = "sigma_NICE", group = 1),
    linewidth = 1
  ) +
  # OECD-only regression with NICE GDPpc
  geom_line(
    data = scenario_df %>% dplyr::filter(sigma_scen > 0),
    aes(x = TIME_PERIOD, y = sigma_scen, color = "Regression on OECD region only with NICE GDPpc", group = 1),
    linetype = "dotdash", linewidth = 1
  ) +
  # All-regions regression with NICE GDPpc (anchored)
  geom_line(
    data = NICE_pred_df,
    aes(x = TIME_PERIOD, y = sigma_scen, color = "Regression on all regions with NICE GDPpc"),
    linetype = "dotted", linewidth = 1
  ) +
  # OECD-only regression with Monte Carlo GDPpc (median) + ribbon
  geom_line(
    data = fc_oecd,
    aes(x = TIME_PERIOD, y = median, color = "Regression on OECD region only with Monte Carlo GDPpc", group = 1),
    linetype = "dashed", linewidth = 1
  ) +
  geom_ribbon(
    data = fc_oecd,
    aes(x = TIME_PERIOD, ymin = lower, ymax = upper),
    fill = "grey50", alpha = 0.2
  ) +
  # Hierarchical forecast with NICE GDPpc
  geom_line(
    data = hier_df %>% dplyr::filter(sigma_hier > 0),
    aes(x = TIME_PERIOD, y = sigma_hier, color = "Hierarchical Forecast with NICE GDPpc"),
    linetype = "dotdash", linewidth = 1
  ) +
  # All-regions regression with Monte Carlo GDPpc (median) + ribbon
  geom_line(
    data = fc_summary,
    aes(x = YEAR, y = median, color = "Regression on all regions with Monte Carlo GDPpc"),
    linetype = "dashed", linewidth = 1
  ) +
  geom_ribbon(
    data = fc_summary,
    aes(x = YEAR, ymin = lower, ymax = upper),
    fill = "grey80", alpha = 0.4
  ) +
  scale_color_manual(
    values = c(
      "Observed"                                                = "darkgreen",
      "sigma_NICE"                                              = "blue",
      "Hierarchical Forecast with NICE GDPpc"                   = "orange",
      "Regression on OECD region only with NICE GDPpc"          = "purple",
      "Regression on OECD region only with Monte Carlo GDPpc"   = "red",
      "Regression on all regions with Monte Carlo GDPpc"        = "darkred",
      "Regression on all regions with NICE GDPpc"               = "black",
      "Recurrence (NICE β₂ only)"                               = "black"  # keep slot for optional overlay
    )
  ) +
  scale_x_continuous(limits = c(1990, 2100), expand = c(0, 0)) +
  labs(
    title = "OECD Forecasts (1990–2100): σ_OECD, σ_NICE & Scenarios",
    x     = "Year",
    y     = "Sigma",
    color = NULL
  ) +
  theme_minimal()

# Save the plot to file
ggsave("oecd-forecast-sigma.jpg", plot = oecd_forecast_plot, width = 12, height = 6, units = "in", dpi = 120)


## ===============================
## Recurrence forecast on ln(sigma)
## ===============================

## 1) Pull β1, β2 from the selected model
beta_source <- model_fits[[best_name]]
cf <- coef(beta_source)
beta1 <- if ("ln_GDP_RCAP"     %in% names(cf)) unname(cf["ln_GDP_RCAP"])     else 0
beta2 <- if ("ln_GDP_RCAP_sq"  %in% names(cf)) unname(cf["ln_GDP_RCAP_sq"])  else 0
message(sprintf("Using β1 = %.6f, β2 = %.6f from %s", beta1, beta2, best_name))

## 2) Seed: σ_OECD(2020)
base_sigma_2020 <- wide_df %>%
  filter(REF_AREA == "OECD", TIME_PERIOD == "2020") %>%
  pull(sigma_OECD) %>% .[!is.na(.)]
stopifnot(length(base_sigma_2020) > 0)
ln_sigma0 <- log(base_sigma_2020[1])

## 3) Build GDPpc paths (2020–2100)

# 3a) NICE path (uses provided GDPpc_net_BAU_NICE for OECD)
gdp_nice <- wide_df %>%
  filter(REF_AREA == "OECD",
         as.integer(TIME_PERIOD) >= 2020,
         as.integer(TIME_PERIOD) <= 2100) %>%
  transmute(
    TIME_PERIOD  = as.integer(TIME_PERIOD),
    ln_GDP_RCAP  = log(GDPpc_net_BAU_NICE)
  ) %>%
  arrange(TIME_PERIOD) %>%
  filter(!is.na(ln_GDP_RCAP)) %>%
  distinct(TIME_PERIOD, .keep_all = TRUE) %>%
  mutate(ln_GDP_RCAP_sq = ln_GDP_RCAP^2)

# 3b) MC path (median across simulations); if mc_df is missing, fall back gracefully
if (exists("mc_df")) {
  gdp_mc <- mc_df %>%
    mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
    filter(TIME_PERIOD >= 2021, TIME_PERIOD <= 2100) %>%
    group_by(TIME_PERIOD) %>%
    summarise(GDP_RCAP = median(GDP_RCAP, na.rm = TRUE), .groups = "drop") %>%
    arrange(TIME_PERIOD) %>%
    mutate(ln_GDP_RCAP = log(GDP_RCAP)) %>%
    select(TIME_PERIOD, ln_GDP_RCAP)
} else {
  # Fallback: flat (carry) GDPpc after 2020 to avoid breaking the pipeline
  last_ln_gdp <- wide_df %>%
    filter(REF_AREA == "OECD", TIME_PERIOD == "2020") %>%
    pull(ln_GDP_RCAP) %>% .[1]
  gdp_mc <- tibble(
    TIME_PERIOD = 2021:2100,
    ln_GDP_RCAP = last_ln_gdp
  )
}
# prepend 2020 value to MC path
ln_gdp_2020 <- wide_df %>%
  filter(REF_AREA == "OECD", TIME_PERIOD == "2020") %>%
  transmute(TIME_PERIOD = 2020L, ln_GDP_RCAP = ln_GDP_RCAP) %>%
  distinct()
gdp_mc <- bind_rows(ln_gdp_2020, gdp_mc) %>%
  arrange(TIME_PERIOD) %>%
  mutate(ln_GDP_RCAP_sq = ln_GDP_RCAP^2)

## 4) Recurrence on ln(sigma)
recur_ln_sigma <- function(path_df, b1, b2, ln_sigma_start) {
  df <- path_df %>% arrange(TIME_PERIOD)
  df$ln_sigma_rec <- NA_real_
  df$ln_sigma_rec[1] <- ln_sigma_start
  if (nrow(df) >= 2) {
    for (i in 2:nrow(df)) {
      d1 <- df$ln_GDP_RCAP[i]    - df$ln_GDP_RCAP[i - 1]
      d2 <- df$ln_GDP_RCAP_sq[i] - df$ln_GDP_RCAP_sq[i - 1]
      df$ln_sigma_rec[i] <- df$ln_sigma_rec[i - 1] + b1 * d1 + b2 * d2
    }
  }
  df %>% mutate(sigma_rec = exp(ln_sigma_rec))
}

## 5) Build all four scenarios (NICE/MC × {β1+β2, β2 only})
modes   <- list("β₁+β₂" = c(b1 = beta1, b2 = beta2),
                "β₂ only" = c(b1 = 0,     b2 = beta2))
sources <- list("NICE" = gdp_nice, "MC" = gdp_mc)

all_recs <- bind_rows(lapply(names(sources), function(src_nm) {
  path <- sources[[src_nm]]
  bind_rows(lapply(names(modes), function(mode_nm) {
    b <- modes[[mode_nm]]
    out <- recur_ln_sigma(path, b1 = b["b1"], b2 = b["b2"], ln_sigma_start = ln_sigma0)
    out$Scenario <- paste(src_nm, mode_nm)
    out
  }))
}))

## 6) Observed σ and σ_NICE series for the panel
obs_sigma <- oecd_hist %>%
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
  filter(sigma_OECD > 0) %>%
  select(TIME_PERIOD, sigma_OECD)

sigma_nice_series <- wide_df %>%
  filter(REF_AREA == "OECD") %>%
  transmute(TIME_PERIOD = as.integer(TIME_PERIOD), sigma_NICE) %>%
  filter(TIME_PERIOD >= 1990, TIME_PERIOD <= 2100, !is.na(sigma_NICE), sigma_NICE > 0)

## 7) Plot: dotted recurrence curves + observed + σ_NICE
recurrence_forecast_plot <-
  ggplot(all_recs, aes(x = TIME_PERIOD, y = sigma_rec, color = Scenario)) +
  geom_line(linewidth = 1, linetype = "dotted") +
  geom_line(data = obs_sigma,
            aes(x = TIME_PERIOD, y = sigma_OECD, color = "Observed"), linewidth = 1, inherit.aes = FALSE) +
  geom_line(data = sigma_nice_series,
            aes(x = TIME_PERIOD, y = sigma_NICE, color = "sigma_NICE"), linewidth = 1, inherit.aes = FALSE) +
  scale_color_manual(
    values = c(
      "NICE β₁+β₂" = "steelblue",
      "NICE β₂ only" = "skyblue",
      "MC β₁+β₂" = "firebrick",
      "MC β₂ only" = "salmon",
      "Observed" = "darkgreen",
      "sigma_NICE" = "blue"
    )
  ) +
  scale_x_continuous(limits = c(1990, 2100), expand = c(0, 0)) +
  labs(
  x = "Year",
  y = "Sigma",
  title = NULL,
  color = NULL,
  linetype = NULL
) +
  theme_minimal()

ggsave("recurrence-forecast-plot.jpg", plot = recurrence_forecast_plot,
       width = 10, height = 5, units = "in", dpi = 120)

## 8) (Optional) also overlay the NICE β₂ only recurrence in your main plot
if (exists("oecd_forecast_plot") && exists("all_recs")) {
  oecd_forecast_plot <- oecd_forecast_plot +
    geom_line(
      data = dplyr::filter(all_recs, Scenario == "NICE β₂ only"),
      aes(x = TIME_PERIOD, y = sigma_rec, color = "Recurrence (NICE β₂ only)"),
      linetype = "dotted", linewidth = 1
    )
  ggsave("oecd-forecast-sigma.jpg", plot = oecd_forecast_plot,
         width = 12, height = 6, units = "in", dpi = 120)
}

##############################################
####### TABLE OF REGRESSION FOR FABRICE ######
##############################################

# =========================================
# Efficiency Regressions for Fabrice
# =========================================

# 1) Subset to years ≤ 2020
eff_df <- df_for_Fabrice %>%
  filter(as.integer(TIME_PERIOD) <= 2020)

# 2) Define the 11 model formulas
formulas_eff <- list(
  mod1  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP + `POL_STRINGENCY-0_TO_6-EPS` + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod2  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP + `POL_STRINGENCY-0_TO_6-EPS_MKT` + `POL_STRINGENCY-0_TO_6-EPS_NMKT` + `POL_STRINGENCY-0_TO_6-TECHSUP` + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod3  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP + POL_STRINGENCY_mean_NMKT_TS + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod4  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-EPS` + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod5  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-EPS_MKT` + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod6  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-EPS_NMKT` + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod7  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-TECHSUP` + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod8  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP * POL_STRINGENCY_mean_NMKT_TS + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod9  = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP + ln_GDP_RCAP_sq + `POL_STRINGENCY-0_TO_6-EPS_MKT` + `POL_STRINGENCY-0_TO_6-EPS_NMKT` + `POL_STRINGENCY-0_TO_6-TECHSUP` + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod10 = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-EPS_MKT` + ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-EPS_NMKT` + ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-TECHSUP` + factor(REF_AREA) + factor(TIME_PERIOD)"),
  mod11 = as.formula("log(efficiency_OECD) ~ ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-EPS_MKT` + ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-EPS_NMKT` + ln_GDP_RCAP * `POL_STRINGENCY-0_TO_6-TECHSUP` + ln_GDP_RCAP_sq + factor(REF_AREA) + factor(TIME_PERIOD)")
)

# 3) Fit each model
model_eff_fits <- lapply(formulas_eff, function(f) lm(f, data = eff_df))

# 4) Extract tidy coefficient tables + model statistics, excluding fixed effects
if (!requireNamespace("broom", quietly=TRUE)) install.packages("broom")
library(broom)
results_list <- lapply(model_eff_fits, function(m) {
  td <- tidy(m)
  gl <- glance(m)
  # keep only non‐fixed‐effect terms (no intercept, no factor())
  td <- td %>%
    filter(
      !grepl("^factor\\(", term)
    )
  # build result tibble
  tib <- td %>%
    transmute(
      term          = term,
      estimate      = estimate,
      std.error     = std.error,
      p.value       = p.value,
      signif        = symnum(
        p.value,
        cutpoints = c(0,0.001,0.01,0.05,0.1,1),
        symbols   = c("***","**","*","."," ")
      ),
      nobs          = gl$nobs,
      adj.r.squared = gl$adj.r.squared
    )
  tib
})
names(results_list) <- names(formulas_eff)

# 5) Write to an Excel workbook with one sheet per model
if (!requireNamespace("openxlsx", quietly=TRUE)) install.packages("openxlsx")
library(openxlsx)
wb <- createWorkbook()
for (nm in names(results_list)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, results_list[[nm]])
}
saveWorkbook(wb, "Regressions_Efficiency_countrydata.xlsx", overwrite = TRUE)


# =====================================
# THE ONE OF INTERESTS, IN A LOOP
# =====================================

# 1) Policy stringency variables of interest
policy_vars <- c(
  "POL_STRINGENCY-0_TO_6-EPS",
  "POL_STRINGENCY-0_TO_6-EPS_MKT",
  "POL_STRINGENCY-0_TO_6-EPS_NMKT",
  "POL_STRINGENCY-0_TO_6-TECHSUP",
  "POL_STRINGENCY_mean_NMKT_TS"
)
# Generate individual lags 1 through 10 for each policy variable
df_for_Fabrice <- df_for_Fabrice %>%
  group_by(REF_AREA) %>%
  arrange(as.integer(TIME_PERIOD))
for (pv in policy_vars) {
  for (k in 0:10) {
    lag_name <- if (k == 0) pv else paste0(pv, "_lag", k)
    df_for_Fabrice <- df_for_Fabrice %>%
      mutate(
        !!lag_name := dplyr::lag(.data[[pv]], k)
      )
  }
}
df_for_Fabrice <- df_for_Fabrice %>% ungroup()

#
# 2) Programmatic generation of regression formulas
FE_options <- list(
  BOTH = "factor(REF_AREA) + factor(TIME_PERIOD)"
)
policy_sets <- list(
  EPS    = "POL_STRINGENCY-0_TO_6-EPS",
  DISAGG = c(
    "POL_STRINGENCY-0_TO_6-EPS_MKT",
    "POL_STRINGENCY-0_TO_6-EPS_NMKT",
    "POL_STRINGENCY-0_TO_6-TECHSUP"
  ),
  NMKTS = "POL_STRINGENCY_mean_NMKT_TS"
)
# Helper to generate lag variable names 1–10
dag_lags <- function(vars) {
  unlist(lapply(vars, function(v) sprintf("%s_lag%d", v, 1:10)))
}
# Build formulas list
formulas_new <- list()
for (fe_name in names(FE_options)) {
  fe_term <- FE_options[[fe_name]]
  for (ps_name in names(policy_sets)) {
    pv <- policy_sets[[ps_name]]
    # Base covariates
    base_terms <- c(
      if (fe_term != "") fe_term,
      "ln_GDP_RCAP"
    )
    # 2a) No policy
    f_base <- sprintf(
      "ln_sigma_OECD ~ %s",
      paste(base_terms, collapse = " + ")
    )
    formulas_new[[paste(fe_name, ps_name, "base", sep = "_")]] <- as.formula(f_base)
    
    # 2b) lags (quote each lagged variable)
    raw_lags <- dag_lags(policy_sets[[ps_name]])
    quoted_lags <- paste0("`", raw_lags, "`")
    lag_term_str <- paste(quoted_lags, collapse = " + ")
    f_lags <- sprintf(
      "ln_sigma_OECD ~ %s + %s",
      paste(base_terms, collapse = " + "),
      lag_term_str
    )
    formulas_new[[paste(fe_name, ps_name, "lags", sep = "_")]] <- as.formula(f_lags)
  }
}

# 3) Fit each model and compute AIC
model_fits_new <- lapply(formulas_new, function(f) lm(f, data = df_for_Fabrice))
aic_vals_new   <- sapply(model_fits_new, AIC)

# 4) Select best model by lowest AIC
best_name_new  <- names(aic_vals_new)[which.min(aic_vals_new)]
cat("Best model by AIC: ", best_name_new, " (AIC = ", round(aic_vals_new[best_name_new], 2), ")\n", sep = "")

# 5) Print summary of best model
cat("\n===== Summary of ", best_name_new, " =====\n", sep = "")


print(summary(model_fits_new[[best_name_new]]))


# ############################################################
# ## ANCHORED FORECAST LINES + UPDATED OECD FORECAST PLOT  ##
# ## (fix intercept to σ_NICE (OECD, 2020); drop Recurrence) ##
# ############################################################

# ---- Helper: anchor any sigma series to OECD σ_NICE in 2020 ----
sigma_anchor_2020 <- wide_df %>%
  filter(REF_AREA == "OECD", TIME_PERIOD == "2020") %>%
  pull(sigma_NICE) %>% .[1]

if (is.na(sigma_anchor_2020)) {
  # Fallback if σ_NICE is missing at 2020
  sigma_anchor_2020 <- wide_df %>%
    filter(REF_AREA == "OECD", TIME_PERIOD == "2020") %>%
    pull(sigma_OECD) %>% .[1]
}

anchor_sigma <- function(df, sigma_col, anchor_value) {
  s0 <- df[[sigma_col]][1]
  if (is.na(s0) || s0 <= 0) return(df)
  scale <- anchor_value / s0
  df[[sigma_col]] <- df[[sigma_col]] * scale
  df
}

# ---- 0) Common inputs (ensure NICE & MC frames exist) ----
# Ensure OECD covariates (policy + energy shares) exist for 1990–2100, with forward-fill and lags
oecd_covars <- wide_df %>%
  dplyr::filter(REF_AREA == "OECD") %>%
  dplyr::transmute(
    TIME_PERIOD = as.integer(TIME_PERIOD),
    `POL_STRINGENCY-0_TO_6-EPS`,
    `POL_STRINGENCY-0_TO_6-EPS_MKT`,
    `POL_STRINGENCY-0_TO_6-EPS_NMKT`,
    `POL_STRINGENCY-0_TO_6-TECHSUP`,
    share_fossil_fuel,
    share_renewables
  ) %>%
  tidyr::complete(TIME_PERIOD = 1990:2100) %>%
  dplyr::arrange(TIME_PERIOD) %>%
  tidyr::fill(
    `POL_STRINGENCY-0_TO_6-EPS`,
    `POL_STRINGENCY-0_TO_6-EPS_MKT`,
    `POL_STRINGENCY-0_TO_6-EPS_NMKT`,
    `POL_STRINGENCY-0_TO_6-TECHSUP`,
    share_fossil_fuel,
    share_renewables,
    .direction = "down"
  )
# Generate 1–10 lags for each covariate (except TIME_PERIOD)
for (v in setdiff(names(oecd_covars), "TIME_PERIOD")) {
  for (k in 1:10) {
    oecd_covars[[paste0(v, "_lag", k)]] <- dplyr::lag(oecd_covars[[v]], k)
  }
}
# NICE_pred_df already created above; ensure TIME_PERIOD numeric and ordered
NICE_pred_df <- NICE_pred_df %>%
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
  arrange(TIME_PERIOD)

# Monte Carlo: use median GDPpc path for "MC lines"
mc_median <- mc_df %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    ln_GDP_RCAP    = median(ln_GDP_RCAP,    na.rm = TRUE),
    ln_GDP_RCAP_sq = median(ln_GDP_RCAP_sq, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    REF_AREA    = "OECD",
    TIME_PERIOD = as.integer(TIME_PERIOD)
  ) %>%
  arrange(TIME_PERIOD) %>%
  # bring covariates (EPS shares, energy shares and their lags)
  left_join(oecd_covars, by = "TIME_PERIOD")

# ---- 1) Hierarchical forecast with NICE GDPpc, anchored ----
# If not already centered above, build centered GDP terms on training window (≤2020)
center_mean <- wide_df %>%
  filter(as.integer(TIME_PERIOD) <= 2020) %>%
  summarise(m = mean(ln_GDP_RCAP, na.rm = TRUE)) %>% pull(m)

wide_df <- wide_df %>%
  mutate(
    c_lGDP  = ln_GDP_RCAP - center_mean,
    c_lGDP2 = c_lGDP^2
  )

# Fit (or reuse) a hierarchical model with non-random quadratic term
if (!exists("hier_mod") || !inherits(hier_mod, "lmerMod")) {
  hier_mod <- lmer(
    ln_sigma_OECD ~ c_lGDP + c_lGDP2 + (1 | REF_AREA) + (0 + c_lGDP | REF_AREA),
    data    = wide_df %>% filter(as.integer(TIME_PERIOD) <= 2020),
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

# Build prediction frame for the hierarchical model (make sure REF_AREA and centered terms exist)
hier_new <- NICE_pred_df %>%
  dplyr::mutate(
    REF_AREA = factor("OECD", levels = levels(wide_df$REF_AREA)),
    c_lGDP   = ln_GDP_RCAP - center_mean,
    c_lGDP2  = c_lGDP^2
  )

# Predict with the hierarchical model using the prepared frame
hier_pred <- hier_new %>%
  dplyr::mutate(
    ln_hat     = as.numeric(predict(hier_mod, newdata = hier_new, allow.new.levels = TRUE)),
    sigma_hier = exp(ln_hat)
  ) %>%
  dplyr::select(TIME_PERIOD, sigma_hier) %>%
  dplyr::arrange(TIME_PERIOD)

hier_pred <- anchor_sigma(hier_pred, "sigma_hier", sigma_anchor_2020)

# ---- 2) All-regions regression (best model) with NICE & MC, anchored ----
# NICE line
pred_all_nice <- NICE_pred_df %>%
  mutate(
    ln_hat = as.numeric(predict(forecast_model, newdata = .)),
    sigma_all_nice = exp(ln_hat)
  ) %>%
  select(TIME_PERIOD, sigma_all_nice) %>%
  arrange(TIME_PERIOD)

pred_all_nice <- anchor_sigma(pred_all_nice, "sigma_all_nice", sigma_anchor_2020)

# MC line (median GDPpc path)
pred_all_mc <- mc_median %>%
  mutate(
    ln_hat = as.numeric(predict(forecast_model, newdata = .)),
    sigma_all_mc = exp(ln_hat)
  ) %>%
  select(TIME_PERIOD, sigma_all_mc) %>%
  arrange(TIME_PERIOD)

pred_all_mc <- anchor_sigma(pred_all_mc, "sigma_all_mc", sigma_anchor_2020)

# ---- 3) OECD-only regression (simple quadratic) with NICE & MC, anchored ----
oecd_only_model <- lm(
  ln_sigma_OECD ~ ln_GDP_RCAP + ln_GDP_RCAP_sq,
  data = wide_df %>% filter(REF_AREA == "OECD", as.integer(TIME_PERIOD) <= 2020)
)

pred_oecd_only_nice <- NICE_pred_df %>%
  mutate(
    ln_hat = as.numeric(predict(oecd_only_model, newdata = .)),
    sigma_oecd_only_nice = exp(ln_hat)
  ) %>%
  select(TIME_PERIOD, sigma_oecd_only_nice) %>%
  arrange(TIME_PERIOD)

pred_oecd_only_nice <- anchor_sigma(pred_oecd_only_nice, "sigma_oecd_only_nice", sigma_anchor_2020)

pred_oecd_only_mc <- mc_median %>%
  mutate(
    ln_hat = as.numeric(predict(oecd_only_model, newdata = .)),
    sigma_oecd_only_mc = exp(ln_hat)
  ) %>%
  select(TIME_PERIOD, sigma_oecd_only_mc) %>%
  arrange(TIME_PERIOD)

pred_oecd_only_mc <- anchor_sigma(pred_oecd_only_mc, "sigma_oecd_only_mc", sigma_anchor_2020)

# --- Construire un frame de prédiction MC qui contient EXACTEMENT les régress. du modèle ---
# 1) Variables attendues par le modèle (termes hors réponse)
term_labels <- attr(terms(forecast_model), "term.labels")
# enlever factor(...) et éclater les interactions "a:b" en {a,b}
vars_base <- gsub("^factor\\((.+)\\)$", "\\1", term_labels)
vars_base <- unique(unlist(strsplit(vars_base, ":", fixed = TRUE)))

# 2) Ce que mc_df fournit directement (GDPpc)
provided_direct <- c("ln_GDP_RCAP", "ln_GDP_RCAP_sq", "REF_AREA", "TIME_PERIOD")

# 3) Ce qu’on doit récupérer depuis oecd_covars
need_from_covars <- setdiff(vars_base, provided_direct)
covars_for_model <- oecd_covars %>%
  dplyr::select(TIME_PERIOD, dplyr::all_of(need_from_covars))

# 4) Construire le newdata pour MC (garantir types et niveaux)
mc_pred_frame <- mc_df %>%
  dplyr::mutate(
    TIME_PERIOD = as.integer(TIME_PERIOD),
    REF_AREA    = factor("OECD", levels = levels(wide_df$REF_AREA))
  ) %>%
  dplyr::left_join(covars_for_model, by = "TIME_PERIOD")


## -- (B) Lister les régress. attendues par le modèle et préparer la jointure --
term_labels <- attr(terms(forecast_model), "term.labels")
# enlever factor() et éclater les interactions a:b -> {a,b}
vars_base <- unique(unlist(strsplit(gsub("^factor\\((.+)\\)$", "\\1", term_labels), ":", fixed = TRUE)))

# Ce que mc_df fournit déjà
provided_direct <- c("ln_GDP_RCAP","ln_GDP_RCAP_sq","REF_AREA","TIME_PERIOD")

# Ce qu'il faut aller chercher dans oecd_covars
need_from_covars <- setdiff(vars_base, provided_direct)

covars_for_model <- oecd_covars %>%
  dplyr::select(TIME_PERIOD, dplyr::all_of(intersect(need_from_covars, names(oecd_covars))))

## -- (C) Construire mc_pred_frame prêt pour predict() --
mc_pred_frame <- mc_df %>%
  dplyr::mutate(
    TIME_PERIOD = as.integer(TIME_PERIOD),
    REF_AREA    = factor("OECD", levels = levels(wide_df$REF_AREA))
  ) %>%
  dplyr::left_join(covars_for_model, by = "TIME_PERIOD")

## -- (D) Filet de sécurité : si parts d'énergie manquent encore, les injecter --
missing_now <- setdiff(vars_base, names(mc_pred_frame))
if (length(missing_now) > 0) {
  # Si seules les parts d'énergie manquent, on met la dernière valeur observée (2020) en constante
  if (all(missing_now %in% c("share_fossil_fuel", "share_renewables"))) {
    last_shares <- wide_df %>%
      dplyr::filter(REF_AREA == "OECD",
                    !is.na(share_fossil_fuel), !is.na(share_renewables)) %>%
      dplyr::arrange(as.integer(TIME_PERIOD)) %>%
      dplyr::summarise(
        share_fossil_fuel = dplyr::last(share_fossil_fuel),
        share_renewables  = dplyr::last(share_renewables),
        .groups = "drop"
      )
    if (!"share_fossil_fuel" %in% names(mc_pred_frame)) {
      mc_pred_frame$share_fossil_fuel <- last_shares$share_fossil_fuel
    }
    if (!"share_renewables" %in% names(mc_pred_frame)) {
      mc_pred_frame$share_renewables <- last_shares$share_renewables
    }
  } else {
    stop("Manquantes après jointure: ", paste(missing_now, collapse = ", "))
  }
}

## -- (E) (Optionnel) Vérifier qu’on n’a pas d’autres trous sur l’horizon --
# Si des années post-2020 restent NA pour ces parts, on peut aussi forward-fill ici :
mc_pred_frame <- mc_pred_frame %>%
  dplyr::arrange(TIME_PERIOD) %>%
  tidyr::fill(share_fossil_fuel, share_renewables, .direction = "down")
  
# Sanity check : toutes les variables requises sont là ?
missing_now <- setdiff(vars_base, names(mc_pred_frame))
if (length(missing_now) > 0) {
  stop("Manquantes dans mc_pred_frame: ", paste(missing_now, collapse = ", "))
}

# 5) Prédictions + bandes MC
mc_pred_frame$ln_hat <- as.numeric(predict(forecast_model, newdata = mc_pred_frame))
mc_pred_frame$sigma  <- exp(mc_pred_frame$ln_hat)

mc_pred_bands <- mc_pred_frame %>%
  dplyr::group_by(TIME_PERIOD) %>%
  dplyr::summarise(
    lower  = quantile(sigma, 0.10, na.rm = TRUE),
    median = median(sigma,        na.rm = TRUE),
    upper  = quantile(sigma, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(TIME_PERIOD)


# ---- 5) Observed and σ_NICE lines (historical) ----
observed_oecd <- df_oecd %>%
  filter(TIME_PERIOD <= 2020) %>%
  transmute(TIME_PERIOD, sigma_OECD)

sigma_nice_line <- df_oecd %>%
  transmute(TIME_PERIOD, sigma_NICE)

# ---- 6) Build long-format dataframe for forecast lines (without Recurrence series) ----
forecast_lines <- dplyr::bind_rows(
  pred_all_mc %>% mutate(Series = "Regression on all regions with Monte Carlo GDPpc",
                         Value  = sigma_all_mc) %>% select(TIME_PERIOD, Series, Value),
  pred_all_nice %>% mutate(Series = "Regression on all regions with NICE GDPpc",
                           Value  = sigma_all_nice) %>% select(TIME_PERIOD, Series, Value),
  pred_oecd_only_mc %>% mutate(Series = "Regression on OECD region only with Monte Carlo GDPpc",
                               Value  = sigma_oecd_only_mc) %>% select(TIME_PERIOD, Series, Value),
  pred_oecd_only_nice %>% mutate(Series = "Regression on OECD region only with NICE GDPpc",
                                 Value  = sigma_oecd_only_nice) %>% select(TIME_PERIOD, Series, Value),
  hier_pred %>% mutate(Series = "Hierarchical Forecast with NICE GDPpc",
                       Value  = sigma_hier) %>% select(TIME_PERIOD, Series, Value)
)

# ---- 7) Plot: overwrite oecd-forecast-sigma.jpg with a SINGLE legend ----
legend_levels <- c(
  "Hierarchical Forecast with NICE GDPpc",
  "Regression on all regions with Monte Carlo GDPpc",
  "Regression on all regions with NICE GDPpc",
  "Regression on OECD region only with Monte Carlo GDPpc",
  "Regression on OECD region only with NICE GDPpc",
  "Observed",
  "sigma_NICE"
)

# Build ONE long data frame for all lines (so color & linetype share the same mapping)
lines_df <- dplyr::bind_rows(
  forecast_lines %>%
    dplyr::transmute(TIME_PERIOD = TIME_PERIOD,
                     Series      = Series,
                     sigma       = Value),
  observed_oecd %>%
    dplyr::transmute(TIME_PERIOD = TIME_PERIOD,
                     Series      = "Observed",
                     sigma       = sigma_OECD),
  sigma_nice_line %>%
    dplyr::transmute(TIME_PERIOD = TIME_PERIOD,
                     Series      = "sigma_NICE",
                     sigma       = sigma_NICE)
) %>%
  dplyr::arrange(TIME_PERIOD)

# Unified styles
cols <- c(
  "Observed"                                              = "darkgreen",
  "sigma_NICE"                                            = "blue",
  "Regression on all regions with Monte Carlo GDPpc"      = "sienna4",
  "Regression on all regions with NICE GDPpc"             = "black",
  "Regression on OECD region only with Monte Carlo GDPpc" = "red3",
  "Regression on OECD region only with NICE GDPpc"        = "purple",
  "Hierarchical Forecast with NICE GDPpc"                 = "orange3"
)

lts <- c(
  "Observed"                                              = "solid",
  "sigma_NICE"                                            = "solid",
  "Regression on all regions with Monte Carlo GDPpc"      = "dashed",
  "Regression on all regions with NICE GDPpc"             = "dotted",
  "Regression on OECD region only with Monte Carlo GDPpc" = "dashed",
  "Regression on OECD region only with NICE GDPpc"        = "dashed",
  "Hierarchical Forecast with NICE GDPpc"                 = "dashed"
)

oecd_forecast_plot <- ggplot() +
  # Uncertainty ribbon (no legend entry)
  geom_ribbon(data = mc_pred_bands,
              aes(x = TIME_PERIOD, ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.4, show.legend = FALSE) +
  # Single line layer for every series
  geom_line(data = lines_df,
            aes(x = TIME_PERIOD, y = sigma, color = Series, linetype = Series),
            linewidth = 1) +
  scale_color_manual(name = NULL, breaks = legend_levels, values = cols) +
  scale_linetype_manual(name = NULL, breaks = legend_levels, values = lts) +
  scale_x_continuous(limits = c(1995, 2100), expand = c(0, 0)) +
  labs(x = "Year", y = "Sigma") +
  guides(color = guide_legend(ncol = 1),
         linetype = guide_legend(ncol = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Always overwrite the file
ggsave("oecd-forecast-sigma.jpg", oecd_forecast_plot, width = 12, height = 6, dpi = 100)