###                                                                          ###
### This script is intended to aid in the initial review of submitted grab   ###
### data by looking for common errors that can be easy to miss in large      ###
### datasets.                                                                ###
###                                                                          ###

#install.packages("devtools")
#devtools::install_github("TravisPritchardODEQ/AWQMSdata",dependencies = TRUE, force = TRUE, upgrade = FALSE)

### Load tools and packages necessary for this script
library(tidyverse)
library(AWQMSdata)
library(lubridate)
library(readxl)
library(openxlsx)
library(lutz)

### Disable scientific notation
options(scipen = 999999)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### Load files, org and data to be reviewed                                   ###
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### Load one outlier percentage file based on the project you're working on 
# CuBLM
OutPerc <- read_xlsx("//deqlead-lims/SERVERFOLDERS/Third_Party_Data/RPA/InitialDataReviewFiles/OutlierPercentiles_CuBLM_2023-12-31.xlsx")
# Landfill
OutPerc <- read_xlsx("//deqlead-lims/SERVERFOLDERS/Third_Party_Data/Landfill Split Data/InitialDataReviewFiles/OutlierPercentiles_LF_2023-12-31.xlsx")
# RPA Toxics
OutPerc <- read_xlsx("//deqlead-lims/SERVERFOLDERS/Third_Party_Data/RPA/InitialDataReviewFiles/OutlierPercentiles_RPA_2023-12-31.xlsx")

### Load files for unit conversions, no need to change either
UnitConv <- read_xlsx("//deqlab1/Assessment/AWQMS/Validation/NormalizedUnits.xlsx")
source("https://raw.githubusercontent.com/DEQdbrown/AWQMS_Audit/main/FUNCTION_convert_units.R") 

### Load org name and data, these will need changed each time
org <- "CITY_EUGENE(NOSTORETID)"
file <- "//Deqhq1/stormwater/Muni Stormwater Program/Municipal - Phase I/Electronic Data Delivery/2023/Working Copies/Eugene/WorkingCopy_City of Eugene Final 2022-2023 MS4 Data.xlsx"
mloc <- read_xlsx(file, sheet = "Monitoring Locations")
data <- read_xlsx(file, sheet = "Results")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### Prepare data for use                                                      ###
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### Remove any blank rows and rename necessary columns
mloc <- mloc %>%
  rename(MLocID = 'Monitoring Location ID') %>%
  filter(!is.na(MLocID)) %>%
  mutate(timezone = tz_lookup_coords(lat = Latitude, lon = Longitude, method = "accurate")) %>%
  mutate(timezone = if_else(str_detect(timezone, "Boise"), "MST/MDT", "PST/PDT"))

data <- data %>%
  rename(MLocID = 'Monitoring Location ID', date = "Sample Collection Date",
         time = 'Sample Collection Time', tz = "Time Zone", ana_date = "Analytical Start/End Date",
         ana_time = "Analytical Start/End Time", media.type = "Media Type", result = 'Result Value',
         analyte_name = "Analyte Name", collmeth = "Sample Collection Method", MRL = "Reporting Limit Value",
         Result_Unit = "Result Unit") %>%
  mutate(SampleMedia = "Water",
         result = as.character(result),
         result = if_else(str_detect(result, "ND|nd"), paste0("<",MRL), result),
         analyte_name = str_replace(analyte_name, "As", "as"),
         Result_Operator = ifelse(str_detect(result, "<"), "<", NA),
         Result_Operator = ifelse(str_detect(result, ">") & is.na(Result_Operator), ">", Result_Operator),
         result = str_remove(result, "<|>"),
         result = as.numeric(result),
         Sample_Fraction = ifelse(str_detect(analyte_name, "Recover|recover"), "Total Recoverable", NA),
         Sample_Fraction = ifelse(str_detect(analyte_name, "Total|total") & is.na(Sample_Fraction), "Total", Sample_Fraction),
         Sample_Fraction = ifelse(str_detect(analyte_name, "Dissolved|dissolved") & is.na(Sample_Fraction), "Dissolved", Sample_Fraction),
         Char_Speciation = str_extract(analyte_name, "As \\S+|as \\S+"), # pulls the as whatever out of the analyte_name column and puts it in the Char_Speciation column
         Char_Name = ifelse(str_detect(analyte_name, ", "), str_remove(analyte_name, ", .*"), analyte_name), # removes anything after ", " and leaves just the Char_Name
         Char_Speciation = str_remove(Char_Speciation, "\\)$"), # removes the trailing ) from what's pull into the column
         Char_Name = str_remove(Char_Name, "\\s*\\(.*"), # removes anything after " (" and leaves just the Char_Name
         Char_Name = ifelse(str_detect(Char_Name, "temp|Temp"), "Temperature, water", Char_Name),
         Sample_Fraction = case_when(
           Char_Name == "Chloride" & Sample_Fraction == "Dissolved" ~ "Filtered, field", 
           Char_Name == "Chloride" & (is.na(Sample_Fraction) | Sample_Fraction %in% c("Total", "Total Recoverable")) ~ "Filtered, lab",
           TRUE ~ Sample_Fraction),
         Result_Unit = case_when(
           Result_Unit == 'mg/L' ~ 'mg/l',
           Result_Unit == 'ug/L' ~ 'ug/l',
           Result_Unit == 'µg/L' ~ 'ug/l',
           Result_Unit == 'SU' ~ 'None',
           Result_Unit == 's.u.' ~ 'None',
           Result_Unit == 'UG/L' ~ 'ug/l',
           Result_Unit == 'meq/L' ~ 'meq/l',
           Result_Unit == 'UMHOS/CM' ~ 'umhos/cm',
           Result_Unit == 'PH UNITS' ~ 'None',
           TRUE ~ Result_Unit)) %>% 
  relocate(Result_Operator, .before = result) %>%
  relocate(c(Char_Name, Sample_Fraction, Char_Speciation), .after = analyte_name) %>%
  relocate(SampleMedia, .before = media.type) %>%
  filter(!is.na(result))

### Run these two lines, only if the time columns are combo date-time otherwise you'll get an error
data <- data %>%
  mutate(time = format(as.POSIXct(time), format = "%H:%M:%S"), 
         ana_time = format(as.POSIXct(ana_time), format = "%H:%M:%S"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### Stations check                                                            ###
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### AWQMS
stations <- AWQMS_Stations(OrganizationID = org) %>%
  select(MLocID) %>%
  mutate(Source = "AWQMS")

### Mon Loc tab
ml_mloc <- mloc %>%
  distinct(MLocID) %>%
  mutate(Source = "MLoc")

### Results tab
r_mloc <- data %>%
  distinct(MLocID) %>%
  mutate(Source = "Results")

### list of all stations
All_MLoc <- bind_rows(stations, r_mloc, ml_mloc) %>%
  arrange(MLocID)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### Time zone check                                                           ###
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### Check whether sample was collected during DST
### If 0 then all samples collected during standard time
tz_check <- data %>%
  mutate(datetime = ymd_hms(paste(date, time)),
         is_dst = dst(datetime)) %>%
  relocate(c(datetime, is_dst), .before = media.type) %>%
  filter(is_dst == 'TRUE')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### Media type check                                                          ###
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### Check whether the submitted media type will load to AWQMS
### Update when new translations are added
mt_check <- data %>%
  distinct(media.type) %>%
  mutate(InAWQMS = case_when(
    media.type %in% c('Industrial Effluent', 'Municipal Sewage Effluent', 'Municipal Waste',
                      'Surface Water', 'Groundwater', 'Leachate', 'Stormwater') ~ "Yep",
    media.type %in% c('Industrial Wastewater', 'Industrial Wastewater Effluent',
                      'MUNICIPAL', 'Municipal Effluent', 'Municipal Effuent',
                      'MUNICIPAL SEWAGE', 'Municipal Wastewater', 'Municipal Wastewater Effluent',
                      'Municipal Wastewater Influent', 'Municiple Effluent', 'Municiple Sewage Effluent',
                      'River', 'Surfacewater', 'Upstream') ~ "Translation",
    TRUE ~ "Nope"
  ))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### Total vs Dissolved check                                                  ###
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### This section groups the data to be compared
 TD_comp<- data %>%
  filter(!is.na(Sample_Fraction),
         is.na(Result_Operator)) %>%
  mutate(CharMatch = str_c(MLocID, date, collmeth, Char_Name, sep = "-"),
         MRL = as.numeric(MRL),
         result = as.numeric(result)) %>%
  group_by(CharMatch) %>%
  mutate(num = n(),
         group_num = cur_group_id()) %>%
  filter(num > 1) %>%
  ungroup() %>%
  arrange(group_num)

### This section subtracts the dissolved from the total fraction and indicates if dissolved is more or less than five times the MRL
TD_check <- TD_comp %>%
  group_by(group_num) %>%
  arrange(group_num, Sample_Fraction) %>%
  mutate(result_diff = result - lag(result, default = first(result)), # Also subtracts dissolved from itself, so will be 0
         MRL_comp = ifelse(result > 5*MRL, "More", "Less")) %>%
  select(MLocID:Char_Speciation, Result_Operator:Result_Unit, group_num:MRL_comp)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### Outlier check                                                             ###
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### Combines the data with the Unit Conversion file
conv_data <- data %>%
  left_join(UnitConv, c('SampleMedia', 'Char_Name', 'Result_Unit'), relationship = "many-to-many") %>%
  convert_units(unit_col = "Unit_UID", pref_unit_col = "Pref_Unit_UID",
                result_col = 'result') %>%
  relocate(c(Conv_Result, Preferred_Unit, Pref_Unit_UID), .after = Char_Speciation)

### Determines if the results are outliers based on the outlier percentile file chosen
out_check <- conv_data %>%
  left_join(OutPerc, c('SampleMedia', 'ParamUID', 'Char_Name', 'Char_Speciation', 'Sample_Fraction', 'Preferred_Unit')) %>%
  mutate(val_result = case_when(Conv_Result < p01 ~ "Below the 1st percentile",
                                Conv_Result > p99 ~ "Above the 99th percentile",
                                is.na(p01) | is.na(p99) ~ NA))

### Shortens list to only the results that are outliers
outliers <- out_check %>%
  filter(!is.na(val_result),
         !str_detect(Result_Operator, "<")| is.na(Result_Operator)) %>%
  relocate(c(val_result, p01, p99), .after = Pref_Unit_UID)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### WQ Standard check                                                         ###
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### Compares results to WQ Human Health and Aquatic Life criteria
### Must run outlier chunk for this to work
### -99 indicates that a calculation is necessary to determine criteria
### All criteria in ug/l except asbestos which is in fibers/L
WQ_check <- out_check %>%
  filter(!str_detect(Result_Operator, "<")| is.na(Result_Operator)) %>%
  mutate(WQ_exceed = case_when(ToxAL_crit == -99 ~ "Calculation Needed",
                               result > ToxHH_crit ~ "Over HH",
                               result > ToxAL_crit ~ "Over AL",
                               TRUE ~ "Below criteria"
  )) %>%
  filter(WQ_exceed != "Below criteria")

