################################################################
#                                                              #
#   Extraction of data from the DCF Master and ANNEX 9file DE  #
#                                                              #
################################################################

# Author: Jan-Dag Pohlmann and Ukeme Inyang
# Date: 20.08.2025


#Define path from current data call ANNEX 9 here
annex_path <- "2025_Eel_Data_Call_Annex9_Other_Sampling_Data_DE.xlsx"
DCF_file <- "Master_2025_08_11_Aal_DCF.xlsx" #without coordinates
DCF_file1 <- "Master_2025_05_28_Aal_DCF1.xlsx" #with coordinates

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
                   sheet = "Master", col_names = TRUE)
data1 <- read_excel(DCF_file1, 
                   sheet = "Master", col_names = TRUE)


# replace all excel error messages and placeholders for NA with NA
data <- data %>% mutate(across(everything(), ~ replace(., .%in% c("NA", "#WERT!", "#NV", "", "#BEZUG!", "-"), NA)))


################################## 3. CREATE NEW INDIVIDUAL METRIC TABLE ##################################

#create a new data frame called data_processed with the individual metrics needed for further processing
# edit the original data from Master to ease programming (very horrible headers!)
data_processed <- data %>%
  rename(ID = "Lfd. Nr.",
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
         fi_lfs_code = "Y/S nach Durif",
         laenge = "Laenge (cm) gemessen",
         pectoral_lengthmm = "Brustfl.  (mm)",
         anguillicola_intensity = "Anguillicola cr. (n gesamt)",
         eyediam_meanmm = "eye_diam_avg",
         ) %>% 
  mutate(fi_id_cou = as.character(ID),
         sai_name = NA,
         sai_emu_nameshort = case_when(
           is.na(fge) ~ NA,                                      # if fge is missing, keep NA
           TRUE ~ paste("DE", substr(fge, 1, 5), sep = "_")
           ),      # otherwise build code,
         habitat = case_when(
           is.na(habitat) ~ "other",                             # rename NA as other
           habitat %in% c("R", "L", "F") ~ "F",                  # recode R, L, F as F
           habitat == "M" ~ "MO",                                # recode M as MO
           TRUE ~ habitat                                        # otherwise keep original
         ),
         fi_date = as.factor(fi_date),                                      # keep fi_date as is
         fi_year = as.factor(fi_year),                                      # keep fi_year as is
         fi_lfs_code = fi_lfs_code,                              # set life stage code to Y/S nach Durif
         fisa_x_4326 = NA,
         fisa_y_4326 = NA,
         lengthmm = as.numeric(lengthmm),                        # convert length to numeric
         weightg = as.numeric(weightg),                          # convert weight to numeric
         ageyear = as.factor(ageyear),                          # convert age to numeric
         eye_diam_meanmm = as.numeric(eyediam_meanmm),            # convert eye diameter to numeric
         pectoral_lengthmm = as.numeric(pectoral_lengthmm),     # convert pectoral length to numeric
         is_female = case_when(
           str_detect(sex, regex("f", ignore_case = TRUE)) ~ "f",  # contains 'f' make female
           str_detect(sex, regex("m", ignore_case = TRUE)) ~ "m",  # contains 'm' make male
           # If sex is empty or unclear, use Stage rules
           stage  == 6 ~ "m",                                      # if stage 6 then it is male
           stage %in% c(4,5) ~ "f",                                # if stage 4,5 then, it is female
           stage == 3 &  laenge  > 45 ~ "f",                       # if stage 3 & length is >45cm then it is female
           TRUE ~ NA
         ), 
         method_sex = case_when(
           str_detect(sex, regex("f|m", ignore_case = TRUE)) ~ 1,   # if inferred from sex, it is 1
           (stage %in% c(4,5,6)) | (stage == 3 & laenge  > 45) ~ 0, # inferred from stage, it is 0
           TRUE ~ NA
         ),
         is_differentiated = case_when(                             # if sex is not NA and not u, then it is differentiated
           is.na(sex) ~ NA,
           sex %in% c("m", "f") ~ 1,                                # if sex is male of female, then it is 1
           sex == "u" ~ 0,                                          # # if sex is u, then it is 0
           TRUE ~ NA),
         anguillicola_presence = as.integer(anguillicola_intensity > 0),      # convert anguillicola_intensity to presence (1) or absence (0)
         anguillicola_intensity = as.numeric(anguillicola_intensity),                     # keep anguillicola_intensity as is
         method_anguillicola = ifelse(!is.na(anguillicola_intensity), 1, 0),  # if anguillicola_intensity is not NA, then it is 1, otherwise 0
         evex_presence = NA, # set evex_presence to NA as it is not available in the data
         hva_presence = NA,
         fi_comment = ""
         ) %>% 
  select(fi_id_cou, sai_name, sai_emu_nameshort, fge, habitat, fi_date, fi_year, fi_lfs_code,fisa_x_4326, 
         fisa_y_4326, lengthmm, weightg, ageyear, eye_diam_meanmm, pectoral_lengthmm, 
         is_female, method_sex, is_differentiated, anguillicola_presence, anguillicola_intensity,
         method_anguillicola, evex_presence, hva_presence, fi_comment) # select only the columns needed for further processing



#fill in sai_name for individual data using the sai_emu_nameshort, habitat and fi_lfs_code, adding DCF and _ in between names
data_processed <- data_processed %>%
  mutate(
    sai_name = pmap_chr(
      list(sai_emu_nameshort, "DCF", habitat, fi_lfs_code),
      ~ str_c(na.omit(c(...)), collapse = "_")  # drop missing parts automatically
    )
  )



######################## ----------------- 4. EXTRACT COORDINATES -------------------########################
# extract coordinates from the DCF file
coordinates <- data1 %>%
  select( "Lfd. Nr.",  "X-coordintes E", "Y-coordintes N") %>%  #extract the columns with coordinates
  rename(fi_id_cou = "Lfd. Nr.",
         fisa_x_4326 = "X-coordintes E",
         fisa_y_4326 = "Y-coordintes N") %>%
  mutate(fi_id_cou = as.character(fi_id_cou),
         fisa_x_4326 = fisa_x_4326, 
         fisa_y_4326 = fisa_y_4326)


#convert to fisa_x_4326 and fisa_y_4326 to degrees
#create a function to convert coordinates from DMS to decimal degrees

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


# apply the conversion function to the coordinates to be input into the new_individual_metrics dataframe
coordinates <-coordinates %>%
  mutate(
    fisa_x_4326 = dms_to_dd(fisa_x_4326),  # convert longitude
    fisa_y_4326 = dms_to_dd(fisa_y_4326)   # convert latitude
  )

# join the coordinates to the new_individual_metrics dataframe
data_processed <- data_processed %>%
  left_join(coordinates, by = "fi_id_cou") %>%
  rename(
    fisa_x_4326 = fisa_x_4326.y,  # rename to avoid confusion
    fisa_y_4326 = fisa_y_4326.y
  ) %>%
  select(-fisa_x_4326.x, -fisa_y_4326.x) # keep the original columns and add the coordinates
  

#extract needed columns from the annex file
new_individual_metrics <- data_processed %>% 
  select(fi_id_cou, sai_name, fi_date, fi_year, fi_lfs_code, fisa_x_4326, 
         fisa_y_4326, lengthmm, weightg, ageyear, eye_diam_meanmm, pectoral_lengthmm, 
         is_female, method_sex, is_differentiated, anguillicola_presence, anguillicola_intensity,
         method_anguillicola, evex_presence, hva_presence, fi_comment) # select only the columns needed for further processing


#rename to match with columns names in the new individual metrics table in annex 9
new_individual_metrics <- new_individual_metrics %>%
  rename("is_female(1=female,0=male)" = is_female,
         "method_sex_(1=visual,0=use_length)" = method_sex,
         "is_differentiated_(1=differentiated,0_undifferentiated)" = is_differentiated,
         "anguillicola_presence_visual(1=present,0=absent)" = anguillicola_presence,
         "evex_presence_(1=present,0=absent)" = evex_presence, 
         "hva_presence_(1=present,0=absent)" = hva_presence,
         "method_anguillicola_(1=stereomicroscope,0=visual_obs)"= method_anguillicola,
  )


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

