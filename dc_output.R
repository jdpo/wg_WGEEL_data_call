#####################################################
#                                                   #
#   Extraction of data from the DCF Master file DE  #
#                                                   #
#####################################################

# Author: Jan-Dag Pohlmann
# Date: 11.08.2022

#---------------------- 1. load/install libraries ----------------------------#

# define libraries needed
libs <- c("tidyverse", "stringr") 

# define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries needed
invisible(lapply(libs, library, character.only = T))


#-------------------- 2. PROCESS INDIVIDUAL DATA -------------------# 

# read master
data <- read.csv("data/2022_08_11_Aal_DCF.csv", header = T, sep  = ";")
col_names <- readLines("data/col_names.csv")
col_names_full <- readLines("data/col_names_full.csv")

# replace all excel errror messages with
data <- data %>% mutate(across(everything(), ~ replace(., .%in% c("#WERT!", "#NV", "", "#BEZUG!"), NA)))
                            
                            
# prepare a reduced 
data_processed <- data %>%  
  rename(ID = "Lfd..Nr.",
         fge = "FGE",
         year = "Fangjahr",
         month = "Fangmonat",
         length_mm = "Length..mm...corrected",
         weight_g = "Mass..g..corrected",
         eye_diam_mean_mm = "eye_diam_avg",
         stage = "Stage..s.i..",
         sex = "Sex",
         age_year = "Age..y.",
         muscle_lipid_fatmeter_perc = "Fett....Fatmeter.Eel.1..ggf..korrigiert..aus.eel.2...",
         fat_mus_tot = "Fett....Labor.",
         fat_mus_an = "Fett....hinter.Anus.",
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
         fi_date = as.Date(paste(year, month, "15",  sep = "-")),
         fi_lfs_code = ifelse(is.na(stage), NA, 
                              ifelse(stage == 4 | stage == 5 | stage == 6, "S", "Y")),
         fi_comment = paste(ID, "Date is not exact and refer to the landing date. Day was always (even if exact date is known) set to 15 for consistency, since most eels are fished and collected over a period of many days and sometimes across a month boundary. In the latter case, the first of the two month is given. Pb and Cd values are provided for dw!", sep = "_"),
         fi_last_update = NA,
         fi_dts_datasource = NA,
         fisa_x_4326 = NA,
         fisa_y_4326 = NA,
         length_mm = as.numeric(length_mm),
         weight_g = as.numeric(weight_g),
         age_year = as.numeric(age_year),
         f_m = ifelse(is.na(stage), NA,
                        ifelse(stage == 4 | stage == 5, 1,
                          ifelse(stage == 6, 0,
                            ifelse(is.na(sex), NA,
                             ifelse(sex == "m", 0, 
                              ifelse( sex == "f", 1, NA)))))),
         diff = ifelse(is.na(sex), NA,
                                                                            ifelse(sex == "m" | sex == "f", 1, 
                                                                                   ifelse(sex == "u", 0, NA))),
         an_pre = ifelse(is.na(anguillicola_intensity), NA, 
                                                              ifelse(anguillicola_intensity < 0, 0, 1)),
         
         muscle_lipid_gravimeter_perc = ifelse(is.na(fat_mus_tot) & is.na(fat_mus_an), NA,
                                             ifelse(is.na(fat_mus_an), fat_mus_an, fat_mus_tot)),
         sum_6_pcb = NA,
         teq = as.numeric(ifelse(is.na(teq), NA, teq)),
         ev_pre = NA,
         hva_pre = NA) %>% 
  select(col_names_full)




#------------------------- 3. CREATE sampling_info & summary biometry --------------------------#

data_biometry <- data_processed %>% 
  filter(is.na(rep) | rep == 1,
         !is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, series, "DCF", fi_lfs_code, sep = "_"))

data_biometry <- data_biometry[order(data_biometry$sai_name, data_biometry$year),]

data_biometry_summary <- data_biometry %>% 
  group_by(sai_name, year) %>% 
  summarize(sai_name = unique(sai_name),
            sai_emu_nameshort = unique(sai_emu_nameshort),
            gr_year = unique(year),
            grsa_lfs_code = unique(fi_lfs_code),
            gr_number = n(),
            gr_comment = "",
            gr_last_update = "",
            gr_dts_datasource = "",
            length_mm = mean(length_mm),
            weight_g = mean(weight_g),
            age_year = mean(age_year),
            female_proportion = length(f_m[which(f_m == 1)])/length(f_m[which(!is.na(f_m))]))


            



#------------------------ 4. CREATE sampling_info & summary quality ---------------------------# 

data_teq <- data_processed %>% 
  filter(!is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(teq)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, "QUAL", "teq", fi_lfs_code, sep = "_"))

data_hg <- data_processed %>% 
  filter(!is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(hg)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, "QUAL", "hg", fi_lfs_code, sep = "_"))

data_pb <- data_processed %>% 
  filter(!is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(pb)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, "QUAL", "pb", fi_lfs_code, sep = "_"))

data_cd <- data_processed %>% 
  filter(!is.na(sai_emu_nameshort),
         !is.na(fi_lfs_code),
         !is.na(cd)) %>% 
  mutate(sai_name =  paste(sai_emu_nameshort, "QUAL", "cd", fi_lfs_code, sep = "_"))

data_quality <- rbind(data_teq, data_hg, data_pb, data_cd)
