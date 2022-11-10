setwd("C:/Users/TZ514/OneDrive - Danish Refugee Council/Desktop/Food Insecurity")
library("tidyverse")
library("readr")
library("readxl")
library("stringr")
library("funModeling")
library("dplyr")
library("DataExplorer")
library("lubridate")


#=========================================={ cadre_harmonise_caf_ipc }===========================================#

Base_caf_ipc <- readxl::read_xlsx("cadre_harmonise_caf_ipc.xlsx", sheet = "Feuil1")
names(Base_caf_ipc)
unique(Base_caf_ipc$adm0_name)

dim(Base_caf_ipc)
sort(unique(Base_caf_ipc$adm0_name)) # Pays sont couverts


Select_Base_caf_ipc <- Base_caf_ipc %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5)

head(Select_Base_caf_ipc)

sort(unique(Select_Base_caf_ipc$exercise_label))

Select_Base_caf_ipc1 <- Select_Base_caf_ipc %>% 
  dplyr::mutate(exercise_label_01 = case_when(exercise_label == "Jan-May" ~ 1),
                exercise_label_02 = case_when(exercise_label == "Jan-May" ~ 2),
                exercise_label_03 = case_when(exercise_label == "Jan-May" ~ 3),
                exercise_label_04 = case_when(exercise_label == "Jan-May" ~ 4),
                exercise_label_05 = case_when(exercise_label == "Jan-May" ~ 5),
                
                exercise_label_06 = case_when(exercise_label == "Jun-Aug" ~ 6),
                exercise_label_07 = case_when(exercise_label == "Jun-Aug" ~ 7),
                exercise_label_08 = case_when(exercise_label == "Jun-Aug" ~ 8),
                
                exercise_label_09 = case_when(exercise_label == "Sep-Dec" ~ 9),
                exercise_label_10 = case_when(exercise_label == "Sep-Dec" ~ 10),
                exercise_label_11 = case_when(exercise_label == "Sep-Dec" ~ 11),
                exercise_label_12 = case_when(exercise_label == "Sep-Dec" ~ 12)
                
  ) 




#--------------------------------- Selections des différentes bases en fonction du mois ---------------------
#============= 1
Select_Base_caf_ipc01 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_01, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_01)
df_status(Select_Base_caf_ipc01)

# Suppression des NA
Select_Base_caf_ipc01 <- Select_Base_caf_ipc01 %>% 
  dplyr::filter(!is.na(Month))

#============= 2
Select_Base_caf_ipc02 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_02, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_02)

# Suppression des NA
Select_Base_caf_ipc02 <- Select_Base_caf_ipc02 %>% 
  dplyr::filter(!is.na(Month))

#============== 3
Select_Base_caf_ipc03 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_03, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_03)


# Suppression des NA
Select_Base_caf_ipc03 <- Select_Base_caf_ipc03 %>% 
  dplyr::filter(!is.na(Month))

#============== 4
Select_Base_caf_ipc04 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_04, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_04)

# Suppression des NA
Select_Base_caf_ipc04 <- Select_Base_caf_ipc04 %>% 
  dplyr::filter(!is.na(Month))

#============== 5
Select_Base_caf_ipc05 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_05, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_05)


# Suppression des NA
Select_Base_caf_ipc05 <- Select_Base_caf_ipc05 %>% 
  dplyr::filter(!is.na(Month))

#============== 6
Select_Base_caf_ipc06 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_06, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_06)

# Suppression des NA
Select_Base_caf_ipc06 <- Select_Base_caf_ipc06 %>% 
  dplyr::filter(!is.na(Month))

#============= 7
Select_Base_caf_ipc07 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_07, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_07)

# Suppression des NA
Select_Base_caf_ipc07 <- Select_Base_caf_ipc07 %>% 
  dplyr::filter(!is.na(Month))

#============== 8
Select_Base_caf_ipc08 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_08, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_08)

# Suppression des NA
Select_Base_caf_ipc08 <- Select_Base_caf_ipc08 %>% 
  dplyr::filter(!is.na(Month))

#============== 9
Select_Base_caf_ipc09 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_09, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_09)

# Suppression des NA
Select_Base_caf_ipc09 <- Select_Base_caf_ipc09 %>% 
  dplyr::filter(!is.na(Month))

#============== 10
Select_Base_caf_ipc10 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_10, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_10)


# Suppression des NA
Select_Base_caf_ipc10 <- Select_Base_caf_ipc10 %>% 
  dplyr::filter(!is.na(Month))

#============== 11
Select_Base_caf_ipc11 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_11, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_11)

# Suppression des NA
Select_Base_caf_ipc11 <- Select_Base_caf_ipc11 %>% 
  dplyr::filter(!is.na(Month))


#============== 12
Select_Base_caf_ipc12 <- Select_Base_caf_ipc1 %>% 
  dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                exercise_label_12, exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  dplyr::rename("Month" = exercise_label_12)


# Suppression des NA
Select_Base_caf_ipc12 <- Select_Base_caf_ipc12 %>% 
  dplyr::filter(!is.na(Month))



#----------------------------------------------{ Concaténation des bases sélectionnées }-----------------------------------#

Select_Base_caf_ipc_Finale <- rbind(Select_Base_caf_ipc01, Select_Base_caf_ipc02, Select_Base_caf_ipc03, Select_Base_caf_ipc04,
                                    Select_Base_caf_ipc05, Select_Base_caf_ipc06, Select_Base_caf_ipc07, Select_Base_caf_ipc08,
                                    Select_Base_caf_ipc09, Select_Base_caf_ipc10, Select_Base_caf_ipc11, Select_Base_caf_ipc12)




# Harmonisations 
Select_Base_caf_ipc_Finale$adm1_name[Select_Base_caf_ipc_Finale$adm1_name == "Plateau Central"] <- "Plateau-Central"
Select_Base_caf_ipc_Finale$adm1_name[Select_Base_caf_ipc_Finale$adm1_name == "Segou"] <- "Ségou"

Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Dosso Commune"] <- "Dosso"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Diffa Commune"] <- "Diffa"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Tillaberi Commune"] <- "Tillaberi"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Ville de Dosso"] <- "Dosso"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Ville de Niamey"] <- "Niamey"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Ville De Maradi"] <- "Maradi"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Ville De Tahoua"] <- "Tahoua"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Ville de Tillaberi"] <- "Tillaberi"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Ville De Zinder"] <- "Zinder"
Select_Base_caf_ipc_Finale$adm2_name[Select_Base_caf_ipc_Finale$adm2_name == "Segou"] <- "Ségou"


Select_Base_caf_ipc_Finale_Admin2 <- Select_Base_caf_ipc_Finale %>% 
  dplyr::mutate(pop_phase35  = rowSums(Select_Base_caf_ipc_Finale[14:16],na.rm = T),
                share_phase35   = rowSums(Select_Base_caf_ipc_Finale[14:16],na.rm = T) / rowSums(Select_Base_caf_ipc_Finale[12:16],na.rm = T))


Select_Base_caf_ipc_Finale_Admin2 <-  Select_Base_caf_ipc_Finale_Admin2 %>% 
  dplyr::filter(adm0_name == "Burkina Faso" | adm0_name ==  "Mali" | adm0_name ==  "Niger")



# write_excel_csv2(Select_Base_caf_ipc_Finale_Admin2, "Select_Base_caf_ipc_Finale_27_10_2022.csv")


# Vérification du code
Control1 <- Select_Base_caf_ipc_Finale %>% 
  dplyr::filter(Month ==1)

Control2 <- Select_Base_caf_ipc_Finale %>% 
  dplyr::filter(Month ==2)

Control3 <- Select_Base_caf_ipc_Finale %>% 
  dplyr::filter(Month ==3)

Control4 <- Select_Base_caf_ipc_Finale %>%
  dplyr::filter(Month ==4)

Control5 <- Select_Base_caf_ipc_Finale %>%
  dplyr::filter(Month ==5)

Controle <- Select_Base_caf_ipc1 %>% 
  dplyr::filter(exercise_label =="Jan-May")


dim(Control1)
dim(Control2)
dim(Control3)
dim(Control4)
dim(Control5)
dim(Controle)


df_status(Select_Base_caf_ipc_Finale)


#-------------------------------------------------------------------- { Admin 1} -----------------------------------------------

# Select_Base_caf_ipc_Finale_Admin1 <- Select_Base_caf_ipc_Finale %>% 
#   dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, Month, exercise_year, chtype, 
#                 reference_label, phase1, phase2, phase3, phase4, phase5) %>%
#   dplyr::filter(adm0_name == "Burkina Faso" | adm0_name ==  "Mali" | adm0_name ==  "Niger") %>% 
#   dplyr::mutate(pop_phase35s  = rowSums(Select_Base_caf_ipc_Finale_Admin1[9:13], na.rm = T),
#                 share_phase35s   = rowSums(Select_Base_caf_ipc_Finale_Admin1[11:13], na.rm = T) / 
#                   rowSums(Select_Base_caf_ipc_Finale_Admin1[9:13],na.rm = T))
# 
# 
# 
# Select_Base_caf_ipc_Finale_Admin12 <-  Select_Base_caf_ipc_Finale_Admin1 %>% 
#   dplyr::group_by(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, Month, exercise_year, reference_label) %>% 
#   dplyr::summarise(pop_phase35  = sum(pop_phase35s, na.rm = T),
#                    share_phase35   = sum(share_phase35s, na.rm = T))
# 
# 







