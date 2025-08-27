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

#####---------------------- 1. load/install libraries ----------------------------#####


# define libraries needed
libs <- c("tidyverse", "readxl", "dplyr", "purrr", "stringr", "lubridate", "writexl") 

# define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries needed
invisible(lapply(libs, library, character.only = T))


#####-------------------- 2. LOAD TO  BE USED FOR INDIVIDUAL METRICS -------------------##### 

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


#left join coordinates from data1 by Lfd. Nr.
data <- left_join(data, data1 %>% select("Lfd. Nr.", "fisa_x_4326", "fisa_y_4326"), by = c("Lfd. Nr."))

#left_join sex data from Balance
data <- left_join(data, Balance %>% select("Balance ID", "bal_sex"), by = c("Extra ID (Polen ID, Florian Id)" = "Balance ID" )) %>% 
  mutate(Sex = ifelse(!is.na(bal_sex), bal_sex, Sex),
         bal_sex = NULL)

#write new table for Ulrike
write_xlsx(data, "2025_new_integration/Master_edited.xlsx")

################################## 3. CREATE NEW INDIVIDUAL METRIC TABLE ##################################

#create a new data frame called data_processed with the individual metrics needed for further processing
# edit the original data from Master to ease programming (very horrible headers!)
data_processed <- data %>%
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
data_processed <- data_processed %>% 
  mutate(habitat = case_when(
          habitat %in% c("R", "L", "F") ~ "F",                  # recode R, L, F as F
          habitat == "M" ~ "MO",                                # recode M as MO
          TRUE ~ habitat),                                        # otherwise keep original
         sai_emu_nameshort = ifelse(is.na(fge), NA, substr(fge, 1, 4)),
         sai_name = case_when(
           is.na(fge) ~ paste("DE_", "other_", habitat),
           is.na(habitat) ~ paste("DE_", substr(fge, 1, 4), "_other"),
           TRUE ~ paste("DE_", substr(fge, 1, 4), "_", habitat)),
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
         fi_comment = ""
         ) %>% 
  rename("is_female_(1=female,0=male)" = is_female,
         "method_sex_(1=visual,0=use_length)" = method_sex,
         "is_differentiated_(1=differentiated,0_undifferentiated)" = is_differentiated,
         "anguillicola_presence_(1=present,0=absent)" = anguillicola_presence,
         "method_anguillicola_(1=stereomicroscope,0=visual_obs)" = method_anguillicola,
         "evex_presence_(1=present,0=absent)" = evex_presence,
         "hva_presence_(1=present,0=absent)" = hva_presence) %>% 
  select(all_of(names(call_ind)))



#mutate date and year!!!
#check outliers!



######################## ----------------- 5. CREATE SAMPLING INFO TABLE -------------------########################

# create sampling_info
sampling_info <- data_processed %>% 
  group_by(sai_name) %>% 
  mutate(
    sai_area_division = case_when(
      is.na(sai_emu_nameshort) ~ NA_character_, #keeps column type consistent (avoids warnings)
      sai_emu_nameshort %in% c("DE_Elbe", "DE_Ems", "DE_Eider", "DE_Rhein", "DE_Weser") ~ "27.4.b",
      sai_emu_nameshort %in% c("DE_Oder", "DE_Warno") ~ "27.3.d",
      sai_emu_nameshort == "DE_Schle" ~ "27.3.b, c",
      TRUE ~ NA_character_
    ),
  habitat = unique(habitat)) %>% 
  summarise(sai_name = unique(sai_name),
            #sai_emu_nameshort = unique(sai_emu_nameshort),
            sai_area_division = unique(sai_area_division),
            sai_hty_code = unique(habitat),
            sai_samplingobjective = "DCF",
            sai_protocol = "",
            sai_samplingstrategy = "",
            sai_qal_id = "",
            sai_comment = "",
            )

#change others in habitat to NA in the sai_hty_code 
sampling_info <- sampling_info %>%
  mutate(sai_hty_code = ifelse(sai_hty_code == "other", NA, sai_hty_code))


#####------------------------- 5. PRINT xlsx -------------------------########################
# write the individual metrics and sampling info to an xlsx file
write_xlsx(new_individual_metrics, "new_individual_metrics.xlsx") 
write_xlsx(sampling_info, "sampling_info.xlsx")
# print the new_individual_metrics and sampling_info to the console

