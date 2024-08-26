library(readxl)
library(dplyr)
library(ggplot2)

mtmg_xlsx = read_excel("./data/Map_The_Meal_Gap.xlsx")
federal_codes = read.table("./data/FederalCodes_GA.txt", 
                           sep = "|", 
                           header = TRUE,
                           quote = "")

mtmg_county = read_excel("./data/Map_The_Meal_Gap.xlsx", sheet = "County")
glimpse(mtmg_county)

mtmg_service_region = mtmg_county %>%
  filter((`Member 1 ID` == 324 | FIPS == 13105)) %>%
  select(!contains(c("Ratio", "Member", "Census", "Continuum"))) %>%
  group_by(`County, State`, Year)

ggplot(mtmg_service_region, aes(factor(Year), `Overall Food Insecurity Rate`, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.1) +
  facet_wrap(~ `County, State`)

education_data = read_excel("./data/ACSST1Y2022.S1501-2024-08-26T001853.xlsx", sheet = "Data")











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