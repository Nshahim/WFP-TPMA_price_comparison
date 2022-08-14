library(dplyr)
library(readxl)
`%notin%` <- Negate(`%in%`)

data <- read_excel("input/FI_DATA_Merged.xlsx", sheet = "FI_PRICES_DATA")

#Based on market ---------------------------
discrep_data_market <- data %>% 
  filter(Items %in% c("Imported vegetable oil", "bread", "high-quality rice", "low-quality rice"),
         Item_In_Stock_Shop == "Yes",
         Province %in% c("Uruzgan", "Kandahar", "Daykundi")) %>% 
  group_by(week, Province, TPMA_Location_ID, Items) %>% 
  mutate(PRICE_FI_STANDARDIZED = round(as.numeric(PRICE_FI_STANDARDIZED), 2),
    discrep = round(sum(PRICE_FI_STANDARDIZED)/length(PRICE_FI_STANDARDIZED), 2),
    var_perc = round((max(PRICE_FI_STANDARDIZED)-min(PRICE_FI_STANDARDIZED))/min(PRICE_FI_STANDARDIZED)*100,2)) %>%
  filter(PRICE_FI_STANDARDIZED != discrep) %>% 
  # mutate(PRICE_FI_STANDARDIZED = round(PRICE_FI_STANDARDIZED,2)) %>% 
  arrange(.by_group = T) %>% 
  select(week, Province, TPMA_Location_Name, TPMA_Location_ID, Items, UNIT_FI_STANDARDIZED, PRICE_FI_STANDARDIZED,var_perc, KEY)

#Per Item
discrep_data_market_bread <- discrep_data_market %>% 
  filter(Items %in% "bread")
discrep_data_market_oil <- discrep_data_market %>% 
  filter(Items %in% "Imported vegetable oil")
discrep_data_market_high_rice <- discrep_data_market %>% 
  filter(Items %in% "high-quality rice")
discrep_data_market_low_rice <- discrep_data_market %>% 
  filter(Items %in% "low-quality rice")


#Based on Location Type ---------------------------
discrep_data_location <- data %>% 
  filter(Items %in% c("Imported vegetable oil", "bread", "high-quality rice", "low-quality rice"),
         Item_In_Stock_Shop == "Yes",
         Province %in% c("Uruzgan", "Kandahar", "Daykundi")) %>% 
  group_by(week, Province, Location_Type, Items) %>% 
  mutate(PRICE_FI_STANDARDIZED = round(as.numeric(PRICE_FI_STANDARDIZED), 2),
         Avg_PRICE_FI_STANDARDIZED = mean(PRICE_FI_STANDARDIZED)) %>%
  # filter(Price_FI %notin% discrep) %>% 
  arrange(.by_group = T) %>% 
  select(week, Province, Location_Type, Items, UNIT_FI_STANDARDIZED, PRICE_FI_STANDARDIZED, Avg_PRICE_FI_STANDARDIZED, KEY) %>% 
  unique() %>% 
  ungroup()

#Per Item
discrep_data_location_bread <- discrep_data_location %>% 
  filter(Items %in% "bread")
discrep_data_location_oil <- discrep_data_location %>% 
  filter(Items %in% "Imported vegetable oil")
discrep_data_location_high_rice <- discrep_data_location %>% 
  filter(Items %in% "high-quality rice")
discrep_data_location_low_rice <- discrep_data_location %>% 
  filter(Items %in% "low-quality rice")

#Export list
data_market_output <- list(
  Bread = discrep_data_market_bread,
  "Cooking Oil" = discrep_data_market_oil,
  "Rice (high qual)" = discrep_data_market_high_rice,
  "Rice (low qual)" = discrep_data_market_low_rice
)

data_location_output <- list(
  Bread = discrep_data_location_bread,
  "Cooking Oil" = discrep_data_location_oil,
  "Rice (high qual)" = discrep_data_location_high_rice,
  "Rice (low qual)" = discrep_data_location_low_rice
)

#Export
write.xlsx(data_market_output, paste0("output/TPMA_Price_Comparison_per_market_",lubridate::today(), ".xlsx"))
write.xlsx(data_location_output, paste0("output/TPMA_Price_Comparison_per_location_",lubridate::today(), ".xlsx"))



#Test
