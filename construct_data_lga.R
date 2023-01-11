library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)
library(raster)


#Can't found data for the admin 2 level about the shapefile and we need these things to go forward and have conflict intensity

bfa_shp1 <- read_sf("bfa_admbnda_adm1_igb_20200323.shp")
bfa_shp1 <- cbind(bfa_shp1, st_coordinates(st_centroid(bfa_shp1)))
bfa_area <- tibble(Area = st_area(bfa_shp1)/1000000, Region = bfa_shp1$ADM1_FR)

mli_shp1 <- read_sf("mli_admbnda_adm1_1m_gov_20211110b.shp")
mli_shp1 <- cbind(mli_shp1, st_coordinates(st_centroid(mli_shp1)))
mli_area <- tibble(Area = st_area(mli_shp1)/1000000, Region = mli_shp1$admin1Name)

ner_shp1 <- read_sf("NER_adm01_feb2018.shp")
ner_shp1 %<>% mutate(adm_01 = ifelse(adm_01=="Tillabéri","Tillaberi", adm_01))
ner_shp1 <- cbind(ner_shp1, st_coordinates(st_centroid(ner_shp1)))
ner_area <- tibble(Area = st_area(ner_shp1)/1000000, Region = ner_shp1$adm_01)

lga_area <- rbind(bfa_area, mli_area, ner_area)

acled_africa <- read_xlsx("Africa_1997-2022_Nov04.xlsx")

acled_lga <- acled_africa %>% filter(COUNTRY %in% c("Mali", "Niger", "Burkina Faso"), YEAR > 2014) %>% 
  dplyr::select(EVENT_DATE, YEAR, EVENT_TYPE, SUB_EVENT_TYPE, ACTOR1, COUNTRY, ADMIN1, ADMIN2, FATALITIES, EVENT_DATE) %>% 
  mutate(Date = ymd(EVENT_DATE))

acled_lga$Month <- format(as.Date(acled_lga$Date), "%m")

acled_lga_ <- acled_lga %>% mutate(FATALITIES_civil = ifelse(EVENT_TYPE == "Violence against civilians", FATALITIES, 0),
                                    EVENTS_civil  = ifelse(EVENT_TYPE == "Violence against civilians", 1, 0),
                                    EVENTS_battles = ifelse(EVENT_TYPE == "Battles", 1, 0),
                                    EVENTS_explo  = ifelse(EVENT_TYPE == "Explosions/Remote violence", 1, 0),
                                    EVENTS_kidnap = ifelse(SUB_EVENT_TYPE ==  "Abduction/forced disappearance", 1, 0),
                                    EVENTS_looting = ifelse(SUB_EVENT_TYPE == "Looting/property destruction", 1,0),
                                    ACTOR1_islamic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) &
                                      (grepl("Islam",ACTOR1) | grepl("JNIM",ACTOR1) | ACTOR1 == "Katiba Macina" | ACTOR1 == "MUJAO: Movement for Unity and Jihad in West Africa"), 1, 0),
                                    ACTOR1_ethnic = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                      (grepl("Ethnic",ACTOR1) | grepl("Communal",ACTOR1) | ACTOR1=="Dan Na Ambassagou"), 1, 0),
                                    ACTOR1_military = ifelse((EVENT_TYPE %in% c("Violence against civilians","Battles","Explosions/Remote violence")) & 
                                      (grepl("Military Forces",ACTOR1)), 1, 0)) %>% 
  group_by(COUNTRY, ADMIN1, ADMIN2, YEAR, Month) %>% summarise(fatal = sum(FATALITIES, na.rm = T), 
                                            max_fatal = max(FATALITIES),
                                            fatal_civil = sum(FATALITIES_civil),
                                            events_civil = sum(EVENTS_civil),
                                            events_battle = sum(EVENTS_battles),
                                            events_explo = sum(EVENTS_explo),
                                            events_kidnap = sum(EVENTS_kidnap),
                                            events_looting = sum(EVENTS_looting),
                                            events_islamic = sum(ACTOR1_islamic),
                                            events_ethnic = sum(ACTOR1_ethnic),
                                            events_military = sum(ACTOR1_military),
                                            .groups = "drop") %>% 
  complete(COUNTRY, ADMIN1, ADMIN2, YEAR, Month,
           fill = list(fatal = 0, max_fatal = 0, fatal_civil = 0, events_civil = 0, events_battle = 0,
                       events_explo = 0,events_kidnap = 0, events_looting = 0, events_islamic = 0,
                       events_ethnic = 0, events_military = 0))


acled_lga_ %<>% mutate(log_fatal = log(fatal+1))

#Calculate variable for average
year_past_fatal <- acled_lga %>% group_by(EVENT_DATE, COUNTRY, ADMIN1, ADMIN2) %>% summarise(fatal = sum(FATALITIES), .groups = "drop") %>% 
  mutate(date = ymd(EVENT_DATE)) %>% dplyr::select(-EVENT_DATE) %>% complete(date = full_seq(date, period = 1), nesting(COUNTRY, ADMIN1, ADMIN2),
                                                                      fill = list(fatal = 0)) %>% group_by(ADMIN1, ADMIN2) %>% 
  mutate(fatal_rolling365 = zoo::rollapplyr(fatal, width = 365, FUN = sum, partial = TRUE),
   fatal_rolling182 = zoo::rollapplyr(fatal, width = 182, FUN = sum, partial = TRUE))  %>% mutate(year = year(date))

year_past_fatal %<>% mutate(ADMIN1 = ifelse(ADMIN1=="Segou", "Ségou", ADMIN1),
                            ADMIN1 = ifelse(ADMIN1 == "Plateau Central", "Plateau-Central", ADMIN1))

#I don't know if is it correct to calculate year_past_fatal by month

year_past_fatal$Month <- format(as.Date(year_past_fatal$date), "%m")
acled_lga_ext <- left_join(acled_lga_, year_past_fata %>% select(date, ADMIN1, ADMIN2, fatal_rolling365), by = c("date", "ADMIN1", "ADMIN2"))

#conflict intensity to calculate after in ADMIN 2 level after getting the shapefile           
acled_lga_ext <- left_join(acled_lga_ext, lga_area, by = "Region") %>% mutate(Area = as.numeric(Area))
acled_lga_ext %<>% mutate(conflict_intensity = (events_civil + events_battle + events_explo)/Area)

#Harmonised framework
cadre <- read_excel("cadre_harmonise_caf_ipc.xlsx", sheet = 1)

cadre_lga <- cadre %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                           exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  filter(adm0_name %in% c("Burkina Faso", "Mali", "Niger"))

cadre_exercises <- cadre_lga %>% 
  mutate(reference_label_01 = case_when(reference_label == "Jan-May" ~ 1),
         reference_label_02 = case_when(reference_label == "Jan-May" ~ 2),
         reference_label_03 = case_when(reference_label == "Jan-May" ~ 3),
         reference_label_04 = case_when(reference_label == "Jan-May" ~ 4),
         reference_label_05 = case_when(reference_label == "Jan-May" ~ 5),
        
         reference_label_06 = case_when(reference_label == "Jun-Aug" ~ 6),
         reference_label_07 = case_when(reference_label == "Jun-Aug" ~ 7),
         reference_label_08 = case_when(reference_label == "Jun-Aug" ~ 8),
        
         reference_label_09 = case_when(reference_label == "Sep-Dec" ~ 9),
         reference_label_10 = case_when(reference_label == "Sep-Dec" ~ 10),
         reference_label_11 = case_when(reference_label == "Sep-Dec" ~ 11),
         reference_label_12 = case_when(reference_label == "Sep-Dec" ~ 12))

cadre1 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_01 ) %>% 
                              rename("Month" = reference_label_01) %>% filter(!is.na(Month))

cadre2 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_02 )%>%
                              rename("Month" = reference_label_02) %>% filter(!is.na(Month))

cadre3 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_03 )%>%
                              rename("Month" = reference_label_03) %>% filter(!is.na(Month))

cadre4 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_04 )%>%
                               rename("Month" = reference_label_04) %>% filter(!is.na(Month))

cadre5 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_05 )%>%
                              rename("Month" = reference_label_05) %>% filter(!is.na(Month))

cadre6 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_06 ) %>% 
                              rename("Month" = reference_label_06) %>% filter(!is.na(Month))
  
cadre7 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_07 ) %>% 
                                rename("Month" = reference_label_07) %>% filter(!is.na(Month))

cadre8 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_08 ) %>% 
                              rename("Month" = reference_label_08) %>% filter(!is.na(Month))

cadre9 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_09 ) %>% 
                              rename("Month" = reference_label_09) %>% filter(!is.na(Month))

cadre10 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_10 ) %>% 
                              rename("Month" = reference_label_10) %>% filter(!is.na(Month))

cadre11 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_11 ) %>% 
                              rename("Month" = reference_label_11) %>% filter(!is.na(Month))

cadre12 <- cadre_exercises %>% dplyr::select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, reference_label_12) %>%
                                rename("Month" = reference_label_12) %>% filter(!is.na(Month))

cadre_final <- rbind(cadre1, cadre2, cadre3, cadre4, cadre5, cadre6, cadre7, cadre8, cadre9, cadre10, cadre11, cadre12 )

cadre_final %<>% mutate(adm1_name = ifelse(adm1_name == "Plateau Central", "Plateau-Central", adm1_name),
                        adm1_name = ifelse(adm1_name == "Segou", "Ségou", adm1_name),
                        adm2_name = ifelse(adm2_name == "Diffa Commune", "Diffa", adm2_name),
                        adm2_name = ifelse(adm2_name == "Tillaberi Commune", "Tillaberi", adm2_name),
                        adm2_name = ifelse(adm2_name == "Ville de Dosso", "Dosso", adm2_name),
                        adm2_name = ifelse(adm2_name == "Dosso Commune", "Dosso", adm2_name),
                        adm2_name = ifelse(adm2_name == "Ville de Niamey", "Niamey", adm2_name),
                        adm2_name = ifelse(adm2_name == "Ville De Maradi", "Maradi", adm2_name),
                        adm2_name = ifelse(adm2_name == "Ville De Tahoua", "Tahoua", adm2_name),
                        adm2_name = ifelse(adm2_name == "Ville de Tillaberi", "Tillaberi", adm2_name),
                        adm2_name = ifelse(adm2_name == "Ville De Zinder", "Zinder", adm2_name),
                        adm2_name = ifelse(adm2_name == "Segou", "Ségou", adm2_name)
                        )
cadre_final %<>% mutate(chTot = phase1 + phase2 + phase3 + phase4 + phase5, share_phase35 = (phase3 + phase4 + phase5)/chTot, pop_phase35 = (phase3 + phase4 + phase5) )

inform_data <- read_excel("base_global_inform.xlsx")

#Vegetation health index

base_global_vhi <- read_excel("base_global_vhi.xlsx")

vhi <- base_vhi %>% dplyr::select(admin1Name_fr, Date, Moyen_VHI) %>% rename(Region = admin1Name_fr)

vhi$Month <- format(as.Date(vhi$Date), "%m")

vhi_month <- vhi %>% group_by(Region, Month)%>% 
  summarise(vhi = mean(Moyen_VHI), .groups = "drop") %>% complete(Region, Month)

#Some issue to calculate three month
vhi_monthly <- vhi_month %>% group_by(Month, Region) %>%
  mutate(three_month_mean = (lag(vhi) + lag(vhi,2) + lag(vhi,3))/3) %>% ungroup() %>% 
  complete(Month, Region)

vhi_monthly %<>% mutate(Region = case_when(Region == "Boucle Du Mouhoun" ~ "Boucle du Mouhoun",
                                             Region == "Centre-est" ~ "Centre-Est",
                                             Region == "Centre-nord" ~ "Centre-Nord",
                                             Region == "Centre-ouest" ~ "Centre-Ouest",
                                             Region == "Centre-sud" ~ "Centre-Sud",
                                             Region == "Hauts-bassins" ~ "Hauts-Bassins",
                                             Region == "Segou" ~ "Ségou",
                                             Region == "Plateau Central" ~ "Plateau-Central",
                                             TRUE ~ Region))
vhi_added <- vhi_monthly %>% filter(Region == "Gao") %>% mutate(Region="Menaka")
vhi_monthly <- rbind(vhi_monthly, vhi_added)

#----------------------------------------------------{ Base inform_sahel-2016_v203 }-----------------------------------------------#
base_inform_V203 <- readxl::read_excel("inform_sahel-2016_v203.xlsx", sheet = "Vulnerability")

Select_inform_V2016 <- base_inform_V203 %>% 
  dplyr::select(Years, COUNTRY, ISO3, `...4`, `Prevalence of Underweight in children 0-59 months of age`, 
                `Mortality rate, under-5`, `Gini Index`, `Human Development Index`, `Health Conditions Index`) %>% 
  rename(Admin1 = "COUNTRY", "Admin0Pcode" = ISO3, "Admin1Pcode" = `...4`, 
         "PRV.UW.CHLD" = `Prevalence of Underweight in children 0-59 months of age`, 
         "MORT.RATE.U5" = `Mortality rate, under-5`, "INC.GINI" = `Gini Index`, "HUM.DEV.IND" = `Human Development Index`, "HEAL.COND.IND" = `Health Conditions Index`) %>% 
  filter(Admin0Pcode == "BFA" | Admin0Pcode == "MLI" | Admin0Pcode == "NER") %>% 
  mutate("Admin0" = case_when(Admin0Pcode == "BFA" ~ "Burkina Faso",
                              Admin0Pcode == "MLI" ~ "Mali",
                              Admin0Pcode == "NER" ~ "Niger"))

#--------------------------------------------{ Base inform_sahel-2016_v330_final-as300916 }-----------------------------------------------#
base_inform_V330 <- readxl::read_excel("inform_sahel-2016_v330_final-as300916.xlsx", sheet = "Vulnerability")

Select_inform_V330 <- base_inform_V330 %>% 
  dplyr::select(Years, COUNTRY, ISO3, `ISO-ADMIN1`, `Prevalence of Underweight in children 0-59 months of age`, 
                `Mortality rate, under-5`, `Gini Index`, `Human Development Index`, `Health Conditions Index`) %>% 
  rename(Admin1 = "COUNTRY", "Admin0Pcode" = ISO3, "Admin1Pcode" = `ISO-ADMIN1`, 
         "PRV.UW.CHLD" = `Prevalence of Underweight in children 0-59 months of age`, 
         "MORT.RATE.U5" = `Mortality rate, under-5`, "INC.GINI" = `Gini Index`, "HUM.DEV.IND" = `Human Development Index`, "HEAL.COND.IND" = `Health Conditions Index`) %>% 
  filter(Admin0Pcode == "BFA" | Admin0Pcode == "MLI" | Admin0Pcode == "NER") %>% 
  mutate("Admin0" = case_when(Admin0Pcode == "BFA" ~ "Burkina Faso",
                              Admin0Pcode == "MLI" ~ "Mali",
                              Admin0Pcode == "NER" ~ "Niger"))

#----------------------------------------------------{ Base inform_sahel-2017_v102 }-----------------------------------------------#
base_inform_V102 <- readxl::read_excel("inform_sahel-2017_v102.xlsx", sheet = "Vulnerability")


Select_inform_2017 <- base_inform_V102 %>%
  dplyr::select(Years, COUNTRY, ISO3, `ISO-ADMIN1`, `Prevalence of Underweight in children 0-59 months of age`, 
                `Mortality rate, under-5`, `Gini Index`, `Human Development Index`, `Health Conditions Index`) %>% 
  rename(Admin1 = "COUNTRY", "Admin0Pcode" = ISO3, "Admin1Pcode" = `ISO-ADMIN1`, 
         "PRV.UW.CHLD" = `Prevalence of Underweight in children 0-59 months of age`, 
         "MORT.RATE.U5" = `Mortality rate, under-5`, "INC.GINI" = `Gini Index`, "HUM.DEV.IND" = `Human Development Index`, "HEAL.COND.IND" = `Health Conditions Index`) %>% 
  filter(Admin0Pcode == "BFA" | Admin0Pcode == "MLI" | Admin0Pcode == "NER") %>% 
  mutate("Admin0" = case_when(Admin0Pcode == "BFA" ~ "Burkina Faso",
                              Admin0Pcode == "MLI" ~ "Mali",
                              Admin0Pcode == "NER" ~ "Niger"))

head(Select_inform_2017)

#----------------------------------------------------{ Base inform_sahel-2018_v100 }-----------------------------------------------#
base_inform_V100 <- readxl::read_excel("inform_sahel-2018_v100.xlsx", sheet = "Vulnerability")

Select_inform_2018 <- base_inform_V100 %>%
  dplyr::select(Years, COUNTRY, ISO3, `ISO-ADMIN1`, `Prevalence of Underweight in children 0-59 months of age`, 
                `Mortality rate, under-5`, `Gini Index`, `Human Development Index`, `Health Conditions Index`) %>% 
  rename(Admin1 = "COUNTRY", "Admin0Pcode" = ISO3, "Admin1Pcode" = `ISO-ADMIN1`, 
         "PRV.UW.CHLD" = `Prevalence of Underweight in children 0-59 months of age`, 
         "MORT.RATE.U5" = `Mortality rate, under-5`, "INC.GINI" = `Gini Index`, "HUM.DEV.IND" = `Human Development Index`, "HEAL.COND.IND" = `Health Conditions Index`) %>% 
  filter(Admin0Pcode == "BFA" | Admin0Pcode == "MLI" | Admin0Pcode == "NER") %>% 
  mutate("Admin0" = case_when(Admin0Pcode == "BFA" ~ "Burkina Faso",
                              Admin0Pcode == "MLI" ~ "Mali",
                              Admin0Pcode == "NER" ~ "Niger"))

#----------------------------------------------------{ Base inform_risk_sahel_2021 }-----------------------------------------------#
base_inform_2021 <- readxl::read_excel("inform_risk_sahel_2021.xlsx", sheet = "Vulnerability")

Select_inform_2021 <- base_inform_2021 %>%
  dplyr::select(Years, COUNTRY, ISO3, `ISO-ADMIN1`, `Prevalence of Underweight in children 0-59 months of age`, 
                `Mortality rate, under-5`, `Gini Index`, `Human Development Index`, `Health Conditions Index`) %>% 
  rename(Admin1 = "COUNTRY", "Admin0Pcode" = ISO3, "Admin1Pcode" = `ISO-ADMIN1`, 
         "PRV.UW.CHLD" = `Prevalence of Underweight in children 0-59 months of age`, 
         "MORT.RATE.U5" = `Mortality rate, under-5`, "INC.GINI" = `Gini Index`, "HUM.DEV.IND" = `Human Development Index`, "HEAL.COND.IND" = `Health Conditions Index`) %>% 
  filter(Admin0Pcode == "BFA" | Admin0Pcode == "MLI" | Admin0Pcode == "NER") %>% 
  mutate("Admin0" = case_when(Admin0Pcode == "BFA" ~ "Burkina Faso",
                              Admin0Pcode == "MLI" ~ "Mali",
                              Admin0Pcode == "NER" ~ "Niger"))
#----------------------------------------------------{ Base Fusion des bases }-----------------------------------------------#

base_inform <- rbind(Select_inform_V330, Select_inform_2017, Select_inform_2018, Select_inform_2021) 

base_inform_global <- base_inform %>% select(Years, Admin1, PRV.UW.CHLD, MORT.RATE.U5, HUM.DEV.IND, HEAL.COND.IND, Admin0) %>% relocate(Years, Admin0, Admin1, PRV.UW.CHLD, MORT.RATE.U5, HUM.DEV.IND, HEAL.COND.IND)
































