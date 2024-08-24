library(readxl)
library(dplyr)
library(ggplot2)

mtmg_xlsx = read_excel("./R/data/Map_The_Meal_Gap.xlsx")
zip_to_coords = read.csv("./R/data/zip_lat_long.csv")
zip_to_fip = read.csv("./R/data/ZIP-COUNTY-FIPS_2017-06.csv")
federal_codes = read.table("./R/data/FederalCodes_GA.txt", 
                           sep = "|", 
                           header = TRUE,
                           quote = "")

mtmg_county = read_excel("./R/data/Map_The_Meal_Gap.xlsx", sheet = "County")
glimpse(mtmg_county)

glimpse(federal_codes)

georgia_federal_data = federal_codes %>%
  filter(census_class_code %in% c("H1", "H6")) %>%
  arrange(county_name) %>%
  mutate(FIPS = 13000 + as.numeric(county_numeric))

combined_data = mtmg_county %>% 
  filter(Year == 2021) %>%
  left_join(georgia_federal_data, by = "FIPS")

ggplot(combined_data, aes(prim_long_dec, prim_lat_dec, color = `Median Income (5 Yr ACS)`, size = `Total Population (5 Year ACS)`)) + 
  geom_point() + 
  coord_fixed()