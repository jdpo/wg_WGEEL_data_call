#####################################################
#                                                   #
#   Extraction of data from the DCF Master file DE  #
#                                                   #
#####################################################

# Author: Jan-Dag Pohlmann
# Date: 11.08.2022

#ALWAYS UPDATE "annex_path", "DCF_path" and OUTPUT FILES AT THE END OF THE SCRIPT

#Define path from current data call ANNEX 10 here
annex_path <- "input_data/2024/2024_Eel_Data_Call_Annex19_Other_Sampling_Data_DE.xlsx"
DCF_file <- "input_data/2024/2023_11_13_Aal_DCF.xlsx"

#####--------------------- 1. PROVIDE GENERAL COMMENTS -------------------------#####

# Provide a general comment for grouped data of "DCF" series
DCF_group_comment <- "Not necessarily representative for EMU, summarizes all landings sub-samples from single or multiple fishermen in this EMU, which are considered representative of these catches. Hence, quality data does not comprise all available quality data in this EMU, these are reported in seperate (QUAL) series"

# Provide a general comment for BALANCE grouped data
BALANCE_group_comment <- "Data generally considered representative for stow net catches near the estuary, but at the time of reporting was incomplete"

# Provide a general comment for grouped data of "QUAL" series
QUAL_group_comment <- "Not necessarily representative for EMU, summarizes data of all eels sampled for the respective quality indicator within the DCF in the respective year. Samples were likely selected due to pre-defined criteria (e.g. large silver eels), depending on the intention of sampling"

#provide a general comment for individual data
IND_comment <- "Date is not exact and refers to the landing date. Day was always (even if exact date is known) set to 15 for consistency, since most eels are fished and collected over a period of many days and sometimes across a month boundary. In the latter case, the first of the two month is given. Pb and Cd values are provided for dw!"

# provide a comments for sai_info (sai_protocol) if it's a DCF biometry series
DCF_info_protocol <- "sampling of commercial fisheries (gear may vary but usually comprises small/large fykes and/or stow nets); catches are considered non-seklective above 45cm."

# provide a comments for sai_info (sai_protocol) if it's a QUAL series
QUAL_info_protocol <- "Samples usually from commercial fisheries but can include samples from non-representative catches (i.e. qual series provide all avbailable quality data, DCF series provide all data considered representative for catches which may include all or parts of qual data)."

# provide a comments for sai_info (sai_protocol) if it's BALANCE data
BALANCE_info_protocol <- "All samples are collected from a stow net near the estuary year round. Samples are restricted to few years though (not complete and therefore not representative yet), as they are/were taken as part of the BALANCE project."

#####---------------------- 2. load/install libraries ----------------------------#####

# define libraries needed
libs <- c("tidyverse", "readxl") 

# define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries needed
invisible(lapply(libs, library, character.only = T))




#####-------------------- 3. PROCESS INDIVIDUAL DATA -------------------##### 

# read master and vectors with column names (original code for reading csv's included for documentation 
# and convenience when re-running from scratch; col_names_full includes some columns that are not needed in the 
# output but used during processing). Read _excel is for working with xlsx directly.
#load("2022/data_master.RData")
data <- read_excel(DCF_file, 
           sheet = "Master")
#data <- read.table("input_data/2023/2022_08_11_Aal_DCF.xlsx", header = T, sep  = ";")
col_names <- readLines("input_data/col_names.csv")
col_names_full <- readLines("input_data/col_names_full.csv")
col_names_grouped_new <- readLines("input_data/colnames_grouped_new.csv")

# replace all excel error messages and placeholders for NA with NA
data <- data %>% mutate(across(everything(), ~ replace(., .%in% c("NA", "#WERT!", "#NV", "", "#BEZUG!", "-"), NA)))

                            
# edit the original data to ease programming (horrible headers!)
data_processed <- data %>%
  rename(ID = "Lfd. Nr.",
         fge = "FGE",
         habitat = "Habitat",
         fi_year = "Fangjahr",
         month = "Fangmonat",
         lengthmm = "Length (mm)  corrected",
         weightg = "Mass (g) corrected",
         eye_diam_mean_mm = "eye_diam_avg",
         stage = "Stage (s.i.)",
         sex = "Sex",
         ageyear = "Age (y)",
         muscle_lipid_fatmeter_perc = "Fett (%-Fatmeter Eel-1, ggf. korrigiert (aus eel-2?))",
         muscle_lipid_gravimeter_perc = "Fett % (Labor)",
         teq = "PCB  (pg TEQ/g ww)",
         pectoral_lengthmm = "Brustfl.  (mm)",
         anguillicola_intensity = "Anguillicola cr. (n gesamt)",
         pb = "Pb Muskel (µg/kg dw)",
         hg = "Quecksilber Muskel (µg/kg ww)",
         cd = "Cd Muskel (µg/kg dw)",
         rep = "representative") %>% 
  mutate(fi_idcou = as.character(ID),
         sai_name = NA,
         sai_emu_nameshort = ifelse(is.na(fge), NA, paste("DE", substr(fge, 1, 4), sep = "_")),
         habitat = ifelse(is.na(habitat), NA,
                    ifelse(habitat == "R" | habitat == "L" | habitat == "F", "F",
                           ifelse(habitat == "M", "MO", habitat))),
         fi_date = as.Date(paste(fi_year, month, "15",  sep = "-")),
         fi_lfs_code = ifelse(is.na(stage), NA, 
                         ifelse(stage == 4 | stage == 5 | stage == 6, "S", "Y")),
         fi_comment = IND_comment,
         fi_last_update = NA,
         fi_dts_datasource = NA,
         fisa_x_4326 = NA,
         fisa_y_4326 = NA,
         lengthmm = as.numeric(lengthmm),
         weightg = as.numeric(weightg),
         ageyear = as.numeric(ageyear),
         f_m = ifelse(!is.na(stage), 
                      ifelse(stage == 4 | stage == 5, 1,
                        ifelse(stage == 6, 0,
                          ifelse(!is.na(sex), 
                            ifelse(sex == "m", 0, 
                              ifelse(sex == "f", 1, NA)), NA))), NA),
         diff = ifelse(is.na(sex), NA,
                  ifelse(sex == "m" | sex == "f", 1, 
                    ifelse(sex == "u", 0, NA))),
         an_pre = ifelse(is.na(anguillicola_intensity), NA, 
                    ifelse(anguillicola_intensity > 0, 1, 0)),
         sum_6_pcb = NA,
         teq = as.numeric(ifelse(is.na(teq), NA, teq)),
         ev_pre = NA,
         hva_pre = NA,
         hg = as.numeric(hg),
         pb = as.numeric(pb),
         cd = as.numeric(cd),
         muscle_lipid_fatmeter_perc = as.numeric(muscle_lipid_fatmeter_perc),
         muscle_lipid_gravimeter_perc = as.numeric(muscle_lipid_gravimeter_perc),
         pectoral_lengthmm = as.numeric(pectoral_lengthmm)) %>% 
  select(all_of(col_names_full))




#####-------------------- 3. PREPARE INDIVIDUAL BIOMETRY DATA -------------------#####

# filter data for use of biometric data
data_biometry <- data_processed %>% 
  filter(is.na(rep) | rep == 1 | rep == 2,             #### REP 2 is representative for life stage sampled
         !is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(fi_year)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, series, "DCF", habitat, fi_lfs_code, sep = "_")) 


# order biometry dataframe and add a number (not necessary, but useful for checks during programming)
data_biometry <- data_biometry[order(data_biometry$sai_name, data_biometry$fi_year),]
data_biometry$no <- 1:nrow(data_biometry)




#####-------------------- 4. PREPARE INDIVIDUAL QUALITY DATA -------------------#####

# filter data for use of quality data and create seperate data frames for each 
# available quality indicator (to produce seperate series per indicator)
data_teq <- data_processed %>% 
  filter(!is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(teq),
         !is.na(fi_year)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, series, "QUAL", "teq", habitat, fi_lfs_code, sep = "_"))

data_hg <- data_processed %>% 
  filter(!is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(hg),
         !is.na(fi_year)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, series, "QUAL", "hg", habitat, fi_lfs_code, sep = "_"))

data_pb <- data_processed %>% 
  filter(!is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(pb),
         !is.na(fi_year)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, series, "QUAL", "pb", habitat, fi_lfs_code, sep = "_"))

data_cd <- data_processed %>% 
  filter(!is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(cd),
         !is.na(fi_year)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, series, "QUAL", "cd", habitat, fi_lfs_code, sep = "_"))


# combine seperate quality data frames
data_quality <- rbind(data_teq, data_hg, data_pb, data_cd)


# order quality dataframe and add a number (not necessary, but useful for checks during 
# programming and to be consistent with biometry data frame)
data_quality <- data_quality[order(data_quality$sai_name, data_quality$fi_year),]
data_quality$no <- 1:nrow(data_quality)




#####------------------------- 5. COMBINE INDIVIDUAL DATA AND CREATE GROUP DATA FOR OUTPUT -------------------------#####

# combine all individual biometry and quality data (several individuals 
# may occur in several series, with a seperate line for each series)
data_individual_full <- rbind(data_biometry, data_quality)

# summarize individual data to generate output for grouped data
data_grouped <- data_individual_full %>%
  mutate(length_f = ifelse(f_m == 1, lengthmm, NA),
         weight_f = ifelse(f_m == 1, weightg, NA),
         age_f = ifelse(f_m == 1, ageyear, NA),
         length_m = ifelse(f_m == 0, lengthmm, NA),
         weight_m = ifelse(f_m == 0, weightg, NA),
         age_m = ifelse(f_m == 0, ageyear, NA)) %>% 
  group_by(sai_name, fi_year) %>% 
  summarise(sai_name = unique(sai_name),
            sai_emu_nameshort = unique(sai_emu_nameshort),
            gr_year = unique(fi_year),
            grsa_lfs_code = unique(fi_lfs_code),
            gr_number = n(),
            lengthmm = mean(lengthmm, na.rm = T),
            weightg = mean(weightg, na.rm = T),
            ageyear = mean(ageyear, na.rm = T),
            female_proportion = length(f_m[which(f_m == 1)])/length(f_m[which(!is.na(f_m))]),
            differentiated_proportion = length(diff[which(diff == 1)])/length(diff[which(!is.na(diff))]),
            f_mean_lengthmm = mean(length_f, na.rm = T), #WHY IS "mean(length_mm[fm == "1"], na.rm = T)" not working!?!?
            f_mean_weightg = mean(weight_f, na.rm = T),
            f_mean_age = mean(age_f, na.rm = T),
            m_mean_lengthmm = mean(length_m, na.rm = T),
            m_mean_weightg = mean(weight_m, na.rm = T),
            m_mean_ageyear = mean(age_m, na.rm = T),
            anguillicola_proportion = length(an_pre[which(an_pre == 1)])/length(an_pre[which(!is.na(an_pre))]),
            anguillicola_intensity = mean(an_pre, na.rm = T),
            muscle_lipid_fatmeter_perc = mean(muscle_lipid_fatmeter_perc, na.rm = T),
            muscle_lipid_gravimeter_perc = mean(muscle_lipid_gravimeter_perc, na.rm = T),
            sum_6_pcb = mean(sum_6_pcb, na.rm = T),
            teq = mean(teq, na.rm = T),
            evex_proportion = length(ev_pre[which(ev_pre == 1)])/length(ev_pre[which(!is.na(ev_pre))]),
            hva_proportion = length(hva_pre[which(hva_pre == 1)])/length(hva_pre[which(!is.na(hva_pre))]),
            pb = mean(pb, na.rm = T),
            hg = mean(hg, na.rm = T),
            cd = mean(cd, na.rm = T),
            g_in_gy_proportion = NA,
            s_in_ys_proportion = NA,
            habitat = unique(habitat),
            gr_comment = ifelse(grepl("QUAL", sai_name), QUAL_group_comment,
                                ifelse(grepl("BALANCE", sai_name), BALANCE_group_comment,
                                       ifelse(grepl("DCF", sai_name), DCF_group_comment, NA)))) 


# create sai_info
sai_info <- data_grouped %>% 
  group_by(sai_name) %>% 
  mutate(sai_area_division = ifelse(is.na(sai_emu_nameshort), NA, 
                                    ifelse(sai_emu_nameshort == "DE_Elbe" | sai_emu_nameshort == "DE_Ems"| sai_emu_nameshort == "DE_Eide"| sai_emu_nameshort == "DE_Rhei"| sai_emu_nameshort == "DE_Wese", "27.4.b",
                                    ifelse(sai_emu_nameshort == "DE_Oder" | sai_emu_nameshort == "DE_Warn", "27.3.d",
                                           ifelse(sai_emu_nameshort == "DE_Schl", "27.3.b, c", NA)))),
         habitat = unique(habitat)) %>% 
  summarise(sai_name = unique(sai_name),
            sai_emu_nameshort = unique(sai_emu_nameshort),
            sai_area_division = unique(sai_area_division),
            sai_hty_code = unique(habitat),
            sai_samplingobjective = "DCF",
            sai_samplingstrategy = "commercial fisheries",
            sai_protocol = ifelse(grepl("QUAL", sai_name), QUAL_info_protocol,
                                  ifelse(grepl("BALANCE", sai_name), BALANCE_info_protocol,
                                         ifelse(grepl("DCF", sai_name), DCF_info_protocol, NA))),
            sai_qal_id = 1,
            sai_comment = ifelse(grepl("QUAL", sai_name), QUAL_group_comment,
                                 ifelse(grepl("BALANCE", sai_name), BALANCE_group_comment,
                                        ifelse(grepl("DCF", sai_name), DCF_group_comment, NA)))) 
            


#####------------- 6. CLEAN UP -----------------#####

# clean environment

# rename some columns in individual data to be consistent with data call spreadsheet and remove additional columns 
data_individual <- data_individual_full %>%
  rename("is_female_(1=female,0=male)" = f_m,
         "is_differentiated_(1=differentiated,0_undifferentiated)" = diff,
         "anguillicola_presence(1=present,0=absent)" = an_pre,
         "evex_presence_(1=present,0=absent)" = ev_pre,
         "hva_presence_(1=present,0=absent)" = hva_pre) %>% 
  select(all_of(col_names)) 

# remove habitat column from grouped data
data_grouped <- data_grouped %>% 
  select(-habitat, -fi_year)




#####------------------------- 7. COMPARE WITH DB EXISTING AND PREPARE NEW & UPDATED ENTRIES -------------------------#####

##### individual data ##### (for 2023 see seperate script!)

# import individual data from Annex 10 and remove columns that are not needed for integration  (and add lfs if not provided by dc)
individual_old <- read_excel(annex_path, 
                        sheet = "existing_individual_metrics") %>%
  select(-"fi_last update", -fi_dts_datasource) 

# get the database id (fi_id) in the created output (to distinguish update from/new data)
data_individual_temp <- data_individual %>%
  left_join(individual_old %>% select(fi_id, fi_idcou, sai_name), by = c("fi_idcou", "sai_name")) %>% 
  select(names(individual_old)) 

#replace NaN with NA (otherwise they won't be recognized as duplicates)
data_individual_temp[sapply(data_individual_temp, is.nan)] <- NA

#check if df columns are similar  
setdiff(names(individual_old), names(data_individual_temp))
setdiff(names(data_individual_temp), names(individual_old))

# create tables for common, updated and new data
individual_common <- intersect(individual_old, data_individual_temp) # not needed, only to check for consistency
individual_update <- setdiff(data_individual_temp %>% filter(!is.na(fi_id)), individual_old)
individual_new <- data_individual_temp %>% filter(is.na(fi_id))






##### group data #####

# import grouped data from Annex 10 and remove rows that are not in "new data"
group_old <- read_excel(annex_path, 
                      sheet = "existing_group_metrics") %>% 
  select(-gr_last_update, -gr_dts_datasource)


# create a grouped data table with added gr_id (match by year and sai); order same as group_old
data_grouped_temp <- data_grouped %>% 
  left_join(group_old %>% select(sai_name, gr_year, gr_id), by = c("sai_name", "gr_year")) %>% 
  select(names(group_old)) %>% 
  mutate_all(function(x) ifelse(is.nan(x), NA, x))

# check if df's are similar  
setdiff(names(group_old), names(data_grouped_temp))
setdiff(names(data_grouped_temp), names(group_old))

# create tables for common, updated and new data (note, in group_old are 10 series that are not from DCF, hence common & update + 10 is the nrow of group_old)
grouped_common <- intersect(data_grouped_temp, group_old) # not needed, only to check for consistency
grouped_update <- setdiff(data_grouped_temp %>% filter(!is.na(gr_id)), group_old)
grouped_new <- setdiff(data_grouped_temp %>% filter(is.na(gr_id)), group_old) %>% select(all_of(col_names_grouped_new))


##### sampling_info #####

#load existing data in db from Annex 10
sai_old <- read_excel(annex_path, 
                      sheet = "sampling_info") %>% 
  select(-sai_lastupdate, -sai_dts_datasource)

# create file with only changes series_info (if no chjanges create a file with "no changes")
sai_info_update <- anti_join(sai_info, sai_old)
if (nrow(sai_info_update) == 0){sai_info_update <- data.frame("NO_UPDATED_SERIES" = "")}

# create a file with new sampling infos
new_sai <- anti_join(sai_info, sai_old, by = "sai_name")

#####------------------------- 8. PRINT CSVs -------------------------#####

write.table(individual_updated, file = "output_data/data_individual/2024_individual_updated.csv", row.names = FALSE, sep = ";")
write.table(individual_new, file = "output_data/data_individual/2024_individual_new.csv", row.names = FALSE, sep = ";")
write.table(grouped_update, file = "output_data/data_grouped/2024_grouped_update.csv", row.names = FALSE, sep = ";")
write.table(grouped_new, file = "output_data/data_grouped/2024_grouped_new.csv", row.names = FALSE, sep = ";")
write.table(sai_info_update, file = "output_data/sai_info/2024_sai_info_update.csv", row.names = FALSE, sep = ";")
