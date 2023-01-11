library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

pdi_burkina <- read_excel("Base Finale Burkina_14_11_2022.xlsx", sheet = 2)
pdi_mali <- read_excel("Base Finale Mali 08-11-2022.xlsx", sheet = 2)
pdi_niger <- read_excel("Base Finale Niger_08_11_2022.xlsx", sheet = 2)

#Period for Burkina : 31/01/2019 to 31/10/2022  

names(pdi_burkina) <- c("Dates", "Admin0", "Admin1", "Admin2", "Total_PDIs")
pdi_burkina_adm2 <- pdi_burkina %>% select(Dates, Admin0, Admin1, Admin2, Total_PDIs)
pdi_burkina_adm2 %<>% mutate(Admin1 = ifelse(Admin1 == "Plateau Central", "Plateau-Central", Admin1))

#Period for Mali : 31/01/2014 to 31/08/2022 

pdi_mali_2 <- pdi_mali %>% select(`Survey Date`, `Admin 0`, `Admin 1`, `Admin 2`, `Total PDI`) %>% rename(Dates = `Survey Date`, Total_PDIs = `Total PDI`)
names(pdi_mali_2) <- c("Dates", "Admin0", "Admin1", "Admin2", "Total_PDIs")
pdi_mali_2 %<>% mutate(Admin1 = ifelse(Admin1 == "Segou", "Ségou", Admin1))

#Period for Niger : 31/03/2019 to 31/07/2022  

pdi_niger_adm2 <- pdi_niger %>% select(Dates, `Admin 0`, `Admin 1`, `Admin 2`, Total_PDIs)
names(pdi_niger_adm2) <- c("Dates", "Admin0", "Admin1", "Admin 2", "Total_PDIs")


