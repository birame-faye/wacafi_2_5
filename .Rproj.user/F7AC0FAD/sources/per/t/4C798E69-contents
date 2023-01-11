library(tidyverse)
library(readxl)

#Burkina Faso where corn is the most consumed cereal

wfp_bfa <- read_csv("WFP_2022Sep08_BurkinaFaso_FoodPricesData.csv")

wfp_bfa_  <- wfp_bfa %>% dplyr::select(Country, 'Admin 1', Commodity, 'Price Type', Year, Month, Price) %>%  
  rename(Region = 'Admin 1')

wfp_bfa_month <- wfp_bfa_ %>%  filter(Commodity %in% "Maize (white)", `Price Type`=="Retail")%>% group_by(Country, Region, Year, Month) %>% 
  summarise(mean_price = mean(Price, na.rm = T), .groups = "drop") %>% complete(Country, Region, Month, Year, fill = list(mean_price = 150))

#Mali where is the most consumed commodity RICE

wfp_mli <- read_csv("WFP_2022Sep08_Mali_FoodPricesData.csv")

wfp_mli_  <- wfp_mli %>% select(Country, 'Admin 1', Commodity, 'Price Type', Year, Month, Price) %>%  
  rename(Region = 'Admin 1')

wfp_mli_month <- wfp_mli_ %>% filter(Commodity %in% "Rice (imported)", `Price Type`=="Retail") %>% group_by(Country, Region, Year, Month) %>% 
  summarise(mean_price = mean(Price, na.rm = TRUE), .groups = "drop")%>% complete(Country, Region, Month, Year, fill = list(mean_price = 300))

#In Niger the most consumed commodity is 

wfp_ner <- read_csv("WFP_2022Sep08_Niger_FoodPricesData.csv")

wfp_ner_ <- wfp_ner %>% select(Country, 'Admin 1', Commodity, 'Price Type', Year, Month, Price) %>% 
  rename(Region = 'Admin 1')

wfp_ner_month <- wfp_ner_ %>% filter(Commodity %in% "Rice (imported)", 'Price Type' ==  "Retail") %>% group_by(Country, Region, Year, Month) %>% 
  summarise(mean_price = mean(Price, na.rm = TRUE), .groups = "drop")%>% complete(Country, Region, Month, Year, fill = list(mean_price = 250))

price_lga_month <-  rbind(wfp_bfa_month, wfp_mli_month, wfp_ner_month)

