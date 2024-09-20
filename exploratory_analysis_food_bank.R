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

get_data = function(county_name)
{
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
  data = data %>% select(c(1:29, 35))
  return(data)
}

for (i in 1:length(names))
{
  pop_data = get_data(names[i])
  write.csv(pop_data, paste("./data/Formatted_Data/", gsub(",", "", gsub(" ", "_", names[i])), ".csv", sep = ""), row.names = FALSE)
}

pop_data = get_data("Banks County, Georgia")
correlation_matrix = cor(pop_data)
correlation = data.frame(Correlation = correlation_matrix["Population participating in SNAP",2:(length(pop_data) - 1)])
significant = correlation %>% filter(abs(Correlation) > 0.5)
positive_correlation = significant %>% filter(Correlation > 0) %>% arrange(desc(Correlation))
negative_correlation = significant %>% filter(Correlation < 0) %>% arrange(Correlation)