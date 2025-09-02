################################################################
#                                                              #
#   Extraction of data from the DCF Master and ANNEX 9file DE  #
#                                                              #
################################################################

# Author: Jan-Dag Pohlmann and Ukeme Inyang
# Date: 20.08.2025


#Define path from current data call ANNEX 9 here
annex_path <- "2025_new_integration/2025_Eel_Data_Call_Annex9_Other_Sampling_Data_DE.xlsx"
DCF_file <- "2025_new_integration/Master_2025_08_27_Aal_DCF.xlsx" #without coordinates
DCF_file1 <- "2025_new_integration/Master_2025_05_28_Aal_DCF1.xlsx" #with coordinates
Balance_data <- "2025_new_integration/eel_sex_data.xlsx"

################################ 1. load/install libraries ###################################


# define libraries needed
libs <- c("tidyverse", "readxl", "dplyr", "purrr", "stringr", "lubridate", "writexl", "icesTAF") 

# define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries needed
invisible(lapply(libs, library, character.only = T))

#create directory
mkdir( "2025_new_integration/output")


################################# 2. LOAD TO  BE USED FOR INDIVIDUAL METRICS ################################### 

# extract data from the master sheet of the DCF file
data <- read_excel(DCF_file, 
                   sheet = "Master", col_names = TRUE, guess_max = 15000) %>% filter(qual == 1) %>% mutate(qual = NULL) 
data1 <- read_excel(DCF_file1, 
                   sheet = "Master", col_names = TRUE, guess_max = 15000)

call_ind <-  read_excel(annex_path, 
                            sheet = "new_individual_metrics", col_names = TRUE, guess_max = 15000)

call_samp <-  read_excel(annex_path, 
                                sheet = "sampling_info", col_names = TRUE, guess_max = 15000)

Balance <-  read_excel(Balance_data, 
                         sheet = "data", col_names = TRUE, guess_max = 15000) %>% rename(bal_sex = "Sex")

locations <-  read_excel("2025_new_integration/update_locations.xlsx", 
                       col_names = TRUE, guess_max = 15000)


# replace all excel error messages and placeholders for NA with NA
data <- data %>% mutate(across(everything(), ~ replace(., .%in% c("NA", "#WERT!", "#NV", "", "#BEZUG!", "-"), NA)))
data1 <- data1 %>% mutate(across(everything(), ~ replace(., .%in% c("NA", "#WERT!", "#NV", "", "#BEZUG!", "-"), NA)))
Balance <- Balance %>% mutate(across(everything(), ~ replace(., .%in% c("NA", "#WERT!", "#NV", "", "#BEZUG!", "-"), NA)))

#convert coordinates to decimal
# Function to convert coordinates from DMS (° ' ") format to decimal degrees
dms_to_dd <- function(x) {
  # Extract all numbers (degrees, minutes, seconds) from the string
  nums <- str_match_all(x, "([0-9.]+)")
  dir  <- str_extract(x, "[NSEW]")  # Extract the direction (N, S, E, W)
  vapply(seq_along(nums), function(i) {   # Loop over each input coordinate
    # Convert the extracted parts into numeric values
    parts <- as.numeric(nums[[i]][,2])  # degrees, minutes, seconds
    
    dd <- parts[1] + parts[2]/60 + parts[3]/3600  # Convert to decimal degrees
    
    # Make values negative for South or West
    if (dir[i] %in% c("W","S")) dd <- -dd
    
    dd # Return the decimal degree value
  }, numeric(1))
}

#apply conversion
data1 <- data1 %>% mutate(fisa_x_4326 = dms_to_dd(fisa_x_4326),
                          fisa_y_4326 = dms_to_dd(fisa_y_4326))


#left join coordinates from data1 by Lfd. Nr. & add exact coordinates for balance
data <- left_join(data, data1 %>% select("Lfd. Nr.", "fisa_x_4326", "fisa_y_4326"), by = c("Lfd. Nr.")) 

#left_join sex data from Balance
data <- left_join(data, Balance %>% select("Balance ID", "bal_sex"), by = c("Extra ID (Polen ID, Florian Id)" = "Balance ID" )) %>% 
  mutate(Sex = ifelse(!is.na(bal_sex), bal_sex, Sex),
         bal_sex = NULL)

#add exact coordinates for balance
data <- data %>% mutate(
  fisa_x_4326 = case_when(str_detect(series, regex("balance", ignore_case = TRUE)) ~ 7.399,
                          TRUE ~ fisa_x_4326),
  fisa_y_4326 = case_when(str_detect(series, regex("balance", ignore_case = TRUE)) ~ 53.237,
                          TRUE ~ fisa_y_4326),
  
)


################################## 3. CREATE NEW INDIVIDUAL METRIC TABLE ##################################

#create a new data frame called data_processed with the individual metrics needed for further processing
# edit the original data from Master to ease programming (very horrible headers!)
data_renamed <- data %>%
  rename(fi_id_cou = "Lfd. Nr.",
         fge = "FGE",
         habitat = "Habitat",
         fi_year = "Fangjahr",
         fi_date = "Fangdatum",
         lengthmm = "Length (mm)  corrected",
         weightg = "Mass (g) corrected",
         eye_diam_meanmm = "eye_diam_avg",
         stage = "Stage (s.i.)",
         sex = "Sex",
         ageyear = "Age (y)",
         lenght = "Laenge (cm) gemessen",
         pectoral_lengthmm = "Brustfl.  (mm)",
         anguillicola_intensity = "Anguillicola cr. (n gesamt)",
         eyediam_meanmm = "eye_diam_avg") 

#edit columns to match data call
data_processed <- data_renamed %>% 
  mutate(habitat = case_when(
          str_detect(series, "(?i)balance") ~ "T",
          habitat %in% c("R", "L", "F") ~ "F",                  # recode R, L, F as F
          habitat == "M" ~ "MO",                                # recode M as MO
          TRUE ~ habitat),                                        # otherwise keep original
         sai_emu_nameshort = ifelse(is.na(fge), NA, substr(fge, 1, 4)),
         sai_name = case_when(
           is.na(fge) ~ paste0("DE_", "other_", habitat),
           is.na(habitat) ~ paste0("DE_", substr(fge, 1, 4), "_other"),
           TRUE ~ paste0("DE_", substr(fge, 1, 4), "_", habitat)),
         fi_lfs_code = case_when(
           stage %in% c(1, 2, 3) ~ "Y",
           stage %in% c(4, 5, 6) ~ "S",
           TRUE ~ NA),
         lengthmm = as.numeric(lengthmm),                       
         weightg = as.numeric(weightg),                          
         ageyear = as.integer(ageyear),                          # convert age to integer
         eye_diam_meanmm = as.numeric(eyediam_meanmm),           # convert eye diameter to numeric
         pectoral_lengthmm = as.numeric(pectoral_lengthmm),      # convert pectoral length to numeric
         sex = case_when(
           sex == "(m)" ~ NA,
           sex == "n" ~ NA,
           sex == "sm" ~ "m",
           sex == "F" ~ "f",
           TRUE ~ sex),
         is_female = case_when(
         sex == "f" ~ 1,
         sex == "m" ~ 1,
         sex == "u" ~ 0,
         TRUE ~ NA), 
         method_sex = case_when(
           sex %in% c("m", "f", "u") ~ 1,
           TRUE ~ NA),
         is_differentiated = case_when(                            
           is.na(sex) ~ NA,
           sex %in% c("m", "f") ~ 1,                               
           sex == "u" ~ 0,                                         
           TRUE ~ NA),
         anguillicola_intensity = as.numeric(anguillicola_intensity),
         anguillicola_presence = case_when(
           is.na(anguillicola_intensity) ~ NA,
           anguillicola_intensity > 0 ~ 1,
           anguillicola_intensity == 0 ~ 0),                         
         method_anguillicola = ifelse(is.na(anguillicola_intensity), NA, 1),  
         evex_presence = NA, 
         hva_presence = NA,
         fi_comment = "",
         fi_date = str_replace_all(fi_date, " ", "")
         ) %>% 
  rename("is_female_(1=female,0=male)" = is_female,
         "method_sex_(1=visual,0=use_length)" = method_sex,
         "is_differentiated_(1=differentiated,0_undifferentiated)" = is_differentiated,
         "anguillicola_presence_(1=present,0=absent)" = anguillicola_presence,
         "method_anguillicola_(1=stereomicroscope,0=visual_obs)" = method_anguillicola,
         "evex_presence_(1=present,0=absent)" = evex_presence,
         "hva_presence_(1=present,0=absent)" = hva_presence) 



#Cleaning the date column
data_temp <- data_processed %>%
  mutate(fi_date = if_else(str_count(fi_date, "-") >= 2, str_replace(fi_date, "^((?:[^-]*-){2}?)", function(x) sub("-$", ".", x)), fi_date),
         fi_date_correct = if_else(str_detect(fi_date, "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$"), format(as.Date(fi_date, format = "%d.%m.%Y"), "%d.%m.%Y"), NA_character_),
         fi_date_wrong = if_else(is.na(fi_date_correct), fi_date, NA),
         fi_date_month = if_else(str_detect(fi_date_wrong, "^(0?[1-9]|1[0-2])\\.\\d{4}$"), sprintf("%02d.%s", as.integer(str_extract(fi_date_wrong, "^[0-9]+")),str_extract(fi_date_wrong, "\\d{4}$")),NA),
         fi_date_year = if_else(str_detect(fi_date_wrong, "^\\d{4}$"), fi_date_wrong, NA),
         fi_start_date = if_else(str_detect(fi_date_wrong, "[-/]"), str_extract(fi_date_wrong, "^[^-/]+"), NA),
         fi_start_date_correct =  if_else(str_detect(fi_start_date, "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$"), format(as.Date(fi_start_date, format = "%d.%m.%Y"), "%d.%m.%Y"), NA_character_),
         fi_start_date_wrong =if_else(is.na(fi_start_date_correct), fi_start_date, NA),
         fi_start_date_corrected = if_else(str_detect(fi_start_date_wrong, "^(0?[1-9]|[12][0-9]|3[01])\\.(0?[1-9]|1[0-2])\\.\\d{2}$"), str_replace(fi_start_date_wrong, "^(\\d{1,2}\\.\\d{1,2}\\.)(\\d{2})$", "\\120\\2"), NA),
         fi_end_date = if_else(str_detect(fi_date_wrong, "[-/]"), str_extract(fi_date_wrong, "(?<=[-/]).+$"), NA),
         fi_end_date_correct = if_else(str_detect(fi_end_date, "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$"), format(as.Date(fi_end_date, format = "%d.%m.%Y"), "%d.%m.%Y"), NA_character_),
         fi_end_date_wrong =if_else(is.na(fi_end_date_correct), fi_end_date, NA),
         fi_end_date_corrected = if_else(str_detect(fi_end_date_wrong, "^(0?[1-9]|[12][0-9]|3[01])\\.(0?[1-9]|1[0-2])\\.\\d{2}$"),str_replace(fi_end_date_wrong,"^(\\d{1,2}\\.\\d{1,2}\\.)(\\d{2})$","\\120\\2"),NA),
         only_digits = as.numeric(if_else(str_detect(fi_date, "^\\d+$"), fi_date, NA)),
         only_digits = if_else(only_digits > 36526, only_digits, NA),
         fi_date_serial = (if_else(!is.na(only_digits), format(as.Date(only_digits, origin = "1899-12-30"), "%d.%m.%Y"), NA)),
         only_digits = NULL,
         fi_start_month = if_else(str_detect(fi_start_date_wrong, "^(0?[1-9]|1[0-2])\\.\\d{4}$"), sprintf("%02d", as.integer(str_extract(fi_start_date_wrong, "^[0-9]+"))), NA),
         fi_start_year  = if_else(str_detect(fi_start_date_wrong, "^(0?[1-9]|1[0-2])\\.\\d{4}$"), str_extract(fi_start_date_wrong, "\\d{4}$"), NA),
         fi_end_month = if_else(str_detect(fi_end_date_wrong, "^(0?[1-9]|1[0-2])\\.\\d{4}$"),sprintf("%02d", as.integer(str_extract(fi_end_date_wrong, "^[0-9]+"))), NA),
         fi_end_year = if_else(str_detect(fi_end_date_wrong, "^(0?[1-9]|1[0-2])\\.\\d{4}$"), str_extract(fi_end_date_wrong, "\\d{4}$"), NA),
         fi_start_corrected_fill = if_else(str_detect(fi_start_date_wrong, "^(\\d{1,2})\\.?$") &
             (!is.na(fi_end_date_corrected) | !is.na(fi_end_date_correct)),
           sprintf(
             "%02d.%s.%s",
             as.integer(str_extract(fi_start_date_wrong, "^\\d{1,2}")),
             if_else(!is.na(fi_end_date_corrected),str_extract(fi_end_date_corrected, "(?<=\\.)\\d{1,2}(?=\\.)"),str_extract(fi_end_date_correct, "(?<=\\.)\\d{1,2}(?=\\.)")),
             if_else(!is.na(fi_end_date_corrected), str_extract(fi_end_date_corrected, "\\d{4}$"), str_extract(fi_end_date_correct, "\\d{4}$"))), NA)
         )


#collapse to columns with only start and end year/month/day
data_temp <- data_temp %>% 
  mutate(
    start_day   = case_when(!is.na(fi_date_correct)         ~ as.integer(str_extract(fi_date_correct, "^[0-9]{1,2}")),
                            !is.na(fi_start_date_correct)   ~ as.integer(str_extract(fi_start_date_correct, "^[0-9]{1,2}")),
                            !is.na(fi_start_date_corrected) ~ as.integer(str_extract(fi_start_date_corrected, "^[0-9]{1,2}")),
                            !is.na(fi_start_corrected_fill) ~ as.integer(str_extract(fi_start_corrected_fill, "^[0-9]{1,2}")),
                            !is.na(fi_date_serial)          ~ as.integer(str_extract(fi_date_serial, "^[0-9]{1,2}")),
                            TRUE ~ NA_integer_),
    start_month = case_when(!is.na(fi_date_correct)         ~ as.integer(str_extract(fi_date_correct, "(?<=^\\d{1,2}\\.)\\d{1,2}")),
                            !is.na(fi_start_date_correct)   ~ as.integer(str_extract(fi_start_date_correct, "(?<=^\\d{1,2}\\.)\\d{1,2}")),
                            !is.na(fi_start_date_corrected) ~ as.integer(str_extract(fi_start_date_corrected, "(?<=^\\d{1,2}\\.)\\d{1,2}")),
                            !is.na(fi_start_corrected_fill) ~ as.integer(str_extract(fi_start_corrected_fill, "(?<=^\\d{1,2}\\.)\\d{1,2}")),
                            !is.na(fi_date_serial)          ~ as.integer(str_extract(fi_date_serial, "(?<=^\\d{1,2}\\.)\\d{1,2}")),
                            !is.na(fi_date_month) ~ as.integer(str_extract(fi_date_month, "^\\d{2}")),
                            !is.na(fi_start_month)          ~ as.integer(fi_start_month),
                            TRUE ~ NA_integer_),
    start_year  = case_when(!is.na(fi_date_correct)         ~ as.integer(str_extract(fi_date_correct, "\\d{4}$")),
                            !is.na(fi_start_date_correct)   ~ as.integer(str_extract(fi_start_date_correct, "\\d{4}$")),
                            !is.na(fi_start_date_corrected) ~ as.integer(str_extract(fi_start_date_corrected, "\\d{4}$")),
                            !is.na(fi_start_corrected_fill) ~ as.integer(str_extract(fi_start_corrected_fill, "\\d{4}$")),
                            !is.na(fi_date_serial)          ~ as.integer(str_extract(fi_date_serial, "\\d{4}$")),
                            !is.na(fi_date_month) ~ as.integer(str_extract(fi_date_month, "(?<=\\.)\\d{4}")),
                            !is.na(fi_date_year)            ~ as.integer(fi_date_year),
                            !is.na(fi_start_year)           ~ as.integer(fi_start_year),
                            TRUE ~ NA_integer_),
    end_day     = case_when(!is.na(fi_end_date_correct)    ~ as.integer(str_extract(fi_end_date_correct, "^[0-9]{1,2}")),
                            !is.na(fi_end_date_corrected)  ~ as.integer(str_extract(fi_end_date_corrected, "^[0-9]{1,2}")),
                            TRUE ~ NA_integer_),
    end_month   = case_when(!is.na(fi_end_date_correct)    ~ as.integer(str_extract(fi_end_date_correct, "(?<=^\\d{1,2}\\.)\\d{1,2}")),
                            !is.na(fi_end_date_corrected)  ~ as.integer(str_extract(fi_end_date_corrected, "(?<=^\\d{1,2}\\.)\\d{1,2}")),
                            !is.na(fi_end_month)           ~ as.integer(fi_end_month),
                            TRUE ~ NA_integer_),
    end_year    = case_when(!is.na(fi_end_date_correct)    ~ as.integer(str_extract(fi_end_date_correct, "\\d{4}$")),
                            !is.na(fi_end_date_corrected)  ~ as.integer(str_extract(fi_end_date_corrected, "\\d{4}$")),
                            !is.na(fi_end_year)            ~ as.integer(fi_end_year),
                            TRUE ~ NA_integer_),
  period        = case_when(!is.na(start_day) & !is.na(start_month) & !is.na(start_year) &
                            !is.na(end_day)   & !is.na(end_month)   & !is.na(end_year) ~ 
                                as.integer(as.Date(sprintf("%04d-%02d-%02d", end_year, end_month, end_day)) - as.Date(sprintf("%04d-%02d-%02d", start_year, start_month, start_day))),
                            !is.na(start_day) & !is.na(start_month) & !is.na(start_year) &
                            is.na(end_day)    & is.na(end_month)    & is.na(end_year) ~ 1,
                            TRUE ~ NA_integer_)) %>% 
  select(-c(fi_date_correct:fi_start_corrected_fill))

#write corrected table
write_xlsx(data_temp, "2025_new_integration/output/corrected_dates_1.xlsx")

#update locations for Warnow
data_temp <- data_temp %>% 
  rename(location = "Fangort bzw. Strom km") %>%
  left_join(locations %>% select(location, habitat_new, true_name, subdivision, fisa_x, fisa_y), by = "location") %>% 
  mutate(
    location = if_else(!is.na(true_name), true_name, NA),
    fisa_x_4326 = if_else(!is.na(fisa_x), fisa_x, NA),
    fisa_y_4326 = if_else(!is.na(fisa_y), fisa_y, NA),
    habitat = if_else(!is.na(habitat_new), habitat_new, NA),
  ) %>% 
  select(-true_name, -fisa_x, -fisa_y)

#write corrected table
write_xlsx(data_temp, "2025_new_integration/output/corrected_location_2.xlsx")

# extract Warnow data for validation of subdivision
check_locations <- data_temp %>% 
  filter(sai_emu_nameshort != "Warn") %>% 
  distinct(sai_emu_nameshort, location, fisa_x_4326, fisa_y_4326, habitat) %>% 
  mutate(subdivision = "",
         true_name = "",
         fisa_x = "",
         fisa_y = "") %>% 
  arrange(sai_emu_nameshort, location)

# save for ulrike    
write_xlsx(check_locations, "2025_new_integration/output/check_locations.xlsx")

#edit for ICES datacall
new_individual_metrics <- data_temp %>% 
  mutate(fi_date = case_when(
    !is.na(start_day) & !is.na(start_month) & !is.na(start_year) ~ sprintf("%02d.%02d.%04d", start_day, start_month, start_year),
    is.na(start_day) & !is.na(start_month) & !is.na(start_year) ~ sprintf("01.%02d.%04d", start_month, start_year),
    TRUE ~ NA_character_),
    fi_year = as.integer(start_year),
    fi_comment = case_when(!is.na(start_day) & !is.na(start_month) & !is.na(start_year) & !is.na(period) & period > 1  ~ paste0("fi_date gives starting date of a fishing period, the period was ", period, " days long"),
                           !is.na(start_day) & !is.na(start_month) & !is.na(start_year) & !is.na(period) & period == 1 ~ "fi_date is exact",
                           is.na(start_day) & !is.na(start_month) & !is.na(start_year) & is.na(end_day) & is.na(end_month) ~ "Day of month is unknown and by default set to first of the month. Duration of the fishing period is unknown but was within the specified month",
                           is.na(start_day) & !is.na(start_month) & !is.na(start_year) & is.na(end_day) & !is.na(end_month) & !is.na(end_year) ~ "fi_date gives a starting value but day of month is unknown and by default set to first of the month. Duration of the fishing period is unknown but extends at least to the next month",
                           is.na(start_day) & is.na(start_month) & !is.na(start_year) ~ "Only year of catch is known")
                        ) %>% 
    select(all_of(names(call_ind)))
    



#write for ICES   
write_xlsx(new_individual_metrics, "2025_new_integration/output/new_individual_metrics.xlsx")





########################################## 4. CREATE SAMPLING INFO TABLE #######################################

#create sampling info
sampling_info <- data_renamed %>%
  mutate(
    habitat = case_when(
    str_detect(series, "(?i)balance") ~ "T",
    habitat %in% c("R", "L", "F") ~ "F",                  # recode R, L, F as F
    habitat == "M" ~ "MO",                                # recode M as MO
    TRUE ~ habitat),                                        # otherwise keep original
sai_emu_nameshort = ifelse(is.na(fge), NA, substr(fge, 1, 4)),
sai_emu_nameshort = paste0("DE_",sai_emu_nameshort),
sai_name = case_when(
  is.na(fge) ~ paste0("DE_", "other_", habitat),
  is.na(habitat) ~ paste0("DE_", substr(fge, 1, 4), "_other"),
  TRUE ~ paste0("DE_", substr(fge, 1, 4), "_", habitat))) %>% 
  group_by(sai_name) %>% 
  summarize(
    sai_emu_nameshort = unique(sai_emu_nameshort),
    sai_hty_code = unique(habitat)) %>% 
  mutate(
    sai_area_division = case_when(
      sai_emu_nameshort %in% c("DE_Elbe", "DE_Ems", "DE_Eide", "DE_Rhei", "DE_Wese") ~ "27.4.b",
      sai_emu_nameshort %in% c("DE_Oder", "DE_Warn") ~ "27.3.d",
      sai_emu_nameshort == "DE_Schl" ~ "27.3.b, c",
      TRUE ~ NA),
    sai_samplingobjective = "DCF",
    sai_samplingstrategy = case_when(
      sai_emu_nameshort == "Warn" ~ "scientific sampling & commercial fisheries",
      TRUE ~ "commercial fisheries"),
    sai_protocol = case_when(
      sai_emu_nameshort == "Warn" ~ "Fish mostly from scientific monitoring executed in the river Warnow",
      TRUE ~ "mostly commercial fisheries, some individuals potentially from other survey"),
    sai_qal_id = 2,
    sai_comment = "Summarizes all eels sampled in the respective habitat and EMU under German DCF, not necessarily representative",
    sai_lastupdate = "",
    sai_dts_datasource = "") %>% 
  select(all_of(names(call_samp)))

# write the  sampling info to an xlsx file
write_xlsx(sampling_info, "2025_new_integration/output/sampling_info.xlsx")


