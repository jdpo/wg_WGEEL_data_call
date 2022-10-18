#####################################################
#                                                   #
#   Extraction of data from the DCF Master file DE  #
#                                                   #
#####################################################

# Author: Jan-Dag Pohlmann
# Date: 11.08.2022


#--------------------- 1. PROVIDE GENERAL COMMENTS -------------------------#

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

#---------------------- 2. load/install libraries ----------------------------#

# define libraries needed
libs <- c("tidyverse") 

# define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries needed
invisible(lapply(libs, library, character.only = T))




#-------------------- 3. PROCESS INDIVIDUAL DATA -------------------# 

# read master and vectors with column names (original code for reading csv's included for documentation 
# and convenience when re-running from scratch; col_names_full includes some columns that are not needed in the 
# output but used during processing)
load("2022/data_master.RData")

#data <- read.csv("data/2022_08_11_Aal_DCF.csv", header = T, sep  = ";")
#col_names <- readLines("data/col_names.csv")
#col_names_full <- readLines("data/col_names_full.csv")


# replace all excel error messages and placeholders for NA with NA
data <- data %>% mutate(across(everything(), ~ replace(., .%in% c("#WERT!", "#NV", "", "#BEZUG!", "-"), NA)))

                            
# edit the original data to ease programming (horrible headers!)
data_processed <- data %>%
  rename(ID = "Lfd..Nr.",
         fge = "FGE",
         habitat = "Habitat",
         fi_year = "Fangjahr",
         month = "Fangmonat",
         length_mm = "Length..mm...corrected",
         weight_g = "Mass..g..corrected",
         eye_diam_mean_mm = "eye_diam_avg",
         stage = "Stage..s.i..",
         sex = "Sex",
         age_year = "Age..y.",
         muscle_lipid_fatmeter_perc = "Fett....Fatmeter.Eel.1..ggf..korrigiert..aus.eel.2...",
         muscle_lipid_gravimeter_perc = "Fett....Labor.",
         teq = "PCB...pg.TEQ.g.ww.",
         pectoral_length_mm = "Brustfl....mm.",
         anguillicola_intensity = "Anguillicola.cr...n.gesamt.",
         pb = "Pb.Muskel..µg.kg.dw.",
         hg = "Quecksilber.Muskel..µg.kg.ww.",
         cd = "Cd.Muskel..µg.kg.dw.",
         rep = "representative") %>% 
  mutate(fi_id = NA,
         sai_name = NA,
         sai_emu_nameshort = ifelse(is.na(fge), NA, paste("DE", substr(fge, 1, 4), sep = "_")),
         habitat = ifelse(is.na(habitat), NA,
                    ifelse(habitat == "R" | habitat == "L" | habitat == "F", "F",
                           ifelse(habitat == "M", "MO", habitat))),
         fi_date = as.Date(paste(fi_year, month, "15",  sep = "-")),
         fi_lfs_code = ifelse(is.na(stage), NA, 
                         ifelse(stage == 4 | stage == 5 | stage == 6, "S", "Y")),
         fi_comment = paste(ID, IND_comment, sep = "_"),
         fi_last_update = NA,
         fi_dts_datasource = NA,
         fisa_x_4326 = NA,
         fisa_y_4326 = NA,
         length_mm = as.numeric(length_mm),
         weight_g = as.numeric(weight_g),
         age_year = as.numeric(age_year),
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
         hg = as.numeric(hg)) %>% 
  select(all_of(col_names_full))




#-------------------- 3. PREPARE INDIVIDUAL BIOMETRY DATA -------------------# 

# filter data for use of biometric data
data_biometry <- data_processed %>% 
  filter(is.na(rep) | rep == 1,             #### CHECK HERE, REP 2 WAS ADDED TO MASTER SHEET!!!
         !is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(fi_year)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, series, "DCF", habitat, fi_lfs_code, sep = "_")) 


# order biometry dataframe and add a number (not necessary, but useful for checks during programming)
data_biometry <- data_biometry[order(data_biometry$sai_name, data_biometry$fi_year),]
data_biometry$no <- 1:nrow(data_biometry)




#-------------------- 4. PREPARE INDIVIDUAL QUALITY DATA -------------------#

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




#------------------------- 5. COMBINE INDIVIDUAL DATA AND CREATE GROUP DATA FOR OUTPUT -------------------------#

# combine all individual biometry and quality data (several individuals 
# may occur in several series, with a seperate line for each series)
data_individual_full <- rbind(data_biometry, data_quality)

# summarize individual data to generate output for grouped data
data_grouped <- data_individual_full %>%
  mutate(length_f = ifelse(f_m == 1, length_mm, NA),
         weight_f = ifelse(f_m == 1, weight_g, NA),
         age_f = ifelse(f_m == 1, age_year, NA),
         length_m = ifelse(f_m == 0, length_mm, NA),
         weight_m = ifelse(f_m == 0, weight_g, NA),
         age_m = ifelse(f_m == 0, age_year, NA)) %>% 
  group_by(sai_name, fi_year) %>% 
  summarise(sai_name = unique(sai_name),
            sai_emu_nameshort = unique(sai_emu_nameshort),
            gr_year = unique(fi_year),
            grsa_lfs_code = unique(fi_lfs_code),
            gr_number = n(),
            length_mm = mean(length_mm, na.rm = T),
            weight_g = mean(weight_g, na.rm = T),
            age_year = mean(age_year, na.rm = T),
            female_proportion = length(f_m[which(f_m == 1)])/length(f_m[which(!is.na(f_m))]),
            differentiated_proportion = length(diff[which(diff == 1)])/length(diff[which(!is.na(diff))]),
            f_mean_length_mm = mean(length_f, na.rm = T), #WHY IS "mean(length_mm[fm == "1"], na.rm = T)" not working!?!?
            f_mean_weight_g = mean(weight_f, na.rm = T),
            f_mean_age_year = mean(age_f, na.rm = T),
            m_mean_length_mm = mean(length_m, na.rm = T),
            m_mean_weight_g = mean(weight_m, na.rm = T),
            m_mean_age_year = mean(age_m, na.rm = T),
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
  mutate(max = max(gr_year),
         min = min (gr_year),
         sai_area_division = ifelse(is.na(sai_emu_nameshort), NA, 
                                    ifelse(sai_emu_nameshort == "DE_Elbe" | sai_emu_nameshort == "DE_Ems"| sai_emu_nameshort == "DE_Eide"| sai_emu_nameshort == "DE_Rhei"| sai_emu_nameshort == "DE_Wese", "27.4.b",
                                    ifelse(sai_emu_nameshort == "DE_Oder" | sai_emu_nameshort == "DE_Warn", "27.3.d",
                                           ifelse(sai_emu_nameshort == "DE_Schl", "27.3.b, c", NA)))),
         habitat = unique(habitat)) %>% 
  summarise(sai_name = unique(sai_name),
            sai_emu_nameshort = unique(sai_emu_nameshort),
            sai_year = unique(min),
            max = unique(max),
            sai_area_division = unique(sai_area_division),
            sai_hty_code = unique(habitat),
            sai_sampling_objective = "DCF",
            sai_samplingstrategy = "commercial fisheries",
            sai_protocol = ifelse(grepl("QUAL", sai_name), QUAL_info_protocol,
                                  ifelse(grepl("BALANCE", sai_name), BALANCE_info_protocol,
                                         ifelse(grepl("DCF", sai_name), DCF_info_protocol, NA))),
            sai_qal_id = "",
            sai_comment = ifelse(grepl("QUAL", sai_name), QUAL_group_comment,
                                 ifelse(grepl("BALANCE", sai_name), BALANCE_group_comment,
                                        ifelse(grepl("DCF", sai_name), DCF_group_comment, NA)))) %>% 
  mutate(sai_year = ifelse(sai_year == max, sai_year, paste(sai_year, max, sep ="-"))) %>% 
  select(-max)
            


#------------------------- 6. CLEAN UP, WRITE CSV FILES AND STORE AN RDATA -------------------------#

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


write.table(data_individual, file = "output/2022_data_individual.csv", row.names = FALSE, sep = ";")
write.table(data_grouped, file = "output/2022_data_grouped.csv", row.names = FALSE, sep = ";")
write.table(sai_info, file = "output/2022_sai_info.csv", row.names = FALSE, sep = ";")
