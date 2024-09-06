library(readxl)
library(dplyr)
library(ggplot2)

names = c("Banks County, Georgia", 
          "Barrow County, Georgia", 
          "Clarke County, Georgia",
          "Elbert County, Georgia",
          "Franklin County, Georgia",
          "Habersham County, Georgia",
          "Hart County, Georgia",
          "Jackson County, Georgia",
          "Madison County, Georgia",
          "Oconee County, Georgia",
          "Oglethorpe County, Georgia",
          "Rabun County, Georgia",
          "Stephens County, Georgia",
          "Towns County, Georgia",
          "White County, Georgia")

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

georgia_federal_data = federal_codes %>%
  filter(census_class_code %in% c("H1", "H6")) %>%
  arrange(county_name) %>%
  mutate(FIPS = 13000 + as.numeric(county_numeric))

combined_data = mtmg_county %>% 
  filter(Year == 2021) %>%
  left_join(georgia_federal_data, by = "FIPS")

get_data = function()
{
  county_name = "Banks County, Georgia"
  
  age_data = NULL
  
  for (i in 2:12)
  {
    year_data = S0101_data %>% 
      filter(Year == 2010 + i)
    temp_data = year_data[county_name]
    temp_data = t(unlist(temp_data, use.names = FALSE))
    temp_data = data.frame(2010 + i, temp_data)
    colnames(temp_data) = c("Year", year_data$Age)
    
    age_data = age_data %>% rbind(temp_data)
  }
  
  education_data = NULL
  
  for (i in 2:12)
  {
    year_data = S1501_data %>% 
      filter(Year == 2010 + i)
    temp_data = year_data[county_name]
    temp_data = t(unlist(temp_data, use.names = FALSE))
    temp_data = data.frame(2010 + i, temp_data)
    colnames(temp_data) = c("Year", year_data$Education)
    
    education_data = education_data %>% rbind(temp_data)
  }
  
  poverty_data = NULL
  
  for (i in 2:12)
  {
    year_data = S1701_data %>% 
      filter(Year == 2010 + i)
    temp_data = year_data[county_name]
    temp_data = t(unlist(temp_data, use.names = FALSE))
    temp_data = data.frame(2010 + i, temp_data)
    colnames(temp_data) = c("Year", year_data$Poverty)
    
    poverty_data = poverty_data %>% rbind(temp_data)
  }
  
  snap_data = SNAP_data[c("Year", county_name)] %>%
    filter(Year %in% 2012:2022)
  colnames(snap_data) = c("Year", "Population participating in SNAP")
  
  data = full_join(age_data, education_data, by = "Year")
  data = full_join(data, poverty_data, by = "Year")
  data = full_join(data, snap_data, by = "Year")
  return(data)
}

pop_data = get_data()
correlation_matrix = cor(pop_data)
correlation = data.frame(Correlation = correlation_matrix["Population participating in SNAP",])
significant = correlation %>% filter(abs(Correlation) > 0.5)
positive_correlation = significant %>% filter(Correlation > 0)
negative_correlation = significant %>% filter(Correlation < 0)