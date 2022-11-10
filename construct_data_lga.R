library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

acled_africa <- read_xlsx("Africa_1997-2022_Nov04.xlsx")

acled_lga <- acled_africa %>% filter(COUNTRY %in% c("Mali", "Niger", "Burkina Faso"), YEAR > 2011) %>% 
  select(EVENT_DATE, YEAR, EVENT_TYPE, SUB_EVENT_TYPE, ACTOR1, COUNTRY, ADMIN1, ADMIN2, FATALITIES, EVENT_DATE) %>% 
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
                                            .groups = "drop")  

#Harmonised framework
cadre <- read_excel("cadre_harmonise_caf_ipc.xlsx", sheet = 1)

cadre_lga <- cadre %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                           exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5) %>% 
  filter(adm0_name %in% c("Burkina Faso", "Mali", "Niger"))

cadre_exercises <- cadre_lga %>% 
  mutate(exercise_label_01 = case_when(exercise_label == "Jan-May" ~ 1),
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
        exercise_label_12 = case_when(exercise_label == "Sep-Dec" ~ 12))

cadre1 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_01 ) %>% 
                              rename("Month" = exercise_label_01) %>% filter(!is.na(Month))

cadre2 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_02 )%>%
                              rename("Month" = exercise_label_02) %>% filter(!is.na(Month))

cadre3 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_03 )%>%
                              rename("Month" = exercise_label_03) %>% filter(!is.na(Month))

cadre4 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_04 )%>%
                               rename("Month" = exercise_label_04) %>% filter(!is.na(Month))

cadre5 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_05 )%>%
                              rename("Month" = exercise_label_05) %>% filter(!is.na(Month))

cadre6 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_06 ) %>% 
                              rename("Month" = exercise_label_06) %>% filter(!is.na(Month))
  
cadre7 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_07 ) %>% 
                                rename("Month" = exercise_label_07) %>% filter(!is.na(Month))

cadre8 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_08 ) %>% 
                              rename("Month" = exercise_label_08) %>% filter(!is.na(Month))

cadre9 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_09 ) %>% 
                              rename("Month" = exercise_label_09) %>% filter(!is.na(Month))

cadre10 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_10 ) %>% 
                              rename("Month" = exercise_label_10) %>% filter(!is.na(Month))

cadre11 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_11 ) %>% 
                              rename("Month" = exercise_label_11) %>% filter(!is.na(Month))

cadre12 <- cadre_exercises %>% select(adm0_name, adm0_pcod3, adm1_name, adm1_pcod2, adm2_name, adm2_pcod2, exercise_label, 
                             exercise_year, chtype, reference_label, phase1, phase2, phase3, phase4, phase5, exercise_label_12) %>%
                                rename("Month" = exercise_label_12) %>% filter(!is.na(Month))

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





















