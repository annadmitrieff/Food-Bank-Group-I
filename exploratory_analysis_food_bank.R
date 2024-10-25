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

# mtmg_xlsx = read_excel("./data/Map_The_Meal_Gap.xlsx")
# federal_codes = read.table("./data/FederalCodes_GA.txt",
#                            sep = "|",
#                            header = TRUE,
#                            quote = "")
# 
# mtmg_county = read_excel("./data/Map_The_Meal_Gap.xlsx", sheet = "County")
# glimpse(mtmg_county)
# 
# mtmg_service_region = mtmg_county %>%
#   filter((`Member 1 ID` == 324 | FIPS == 13105)) %>%
#   select(!contains(c("Ratio", "Member", "Census", "Continuum"))) %>%
#   group_by(`County, State`, Year)
# 
# georgia_federal_data = federal_codes %>%
#   filter(census_class_code %in% c("H1", "H6")) %>%
#   arrange(county_name) %>%
#   mutate(FIPS = 13000 + as.numeric(county_numeric))

# combined_data = mtmg_county %>%
#   filter(Year == 2021) %>%
#   left_join(georgia_federal_data, by = "FIPS")

format_analysis = function(data, county_name)
{
  data = transpose_data(data, "Label", "County, State")
  data = data %>% filter(`County, State` == county_name)
  data$Year = as.numeric(data$Year)
  data = data[-1]
}

sum_all_analysis = function(data)
{
  return_data = NULL
  for (i in 1:length(unique(data$Year)))
  {
    year_data = data %>% filter(Year == as.numeric(Year[1]) + i - 1)
    rownames(year_data) = NULL
    temp_data = year_data[, 1:3]
    temp_data[, 3] = rowSums(data.frame(lapply(year_data[,3:ncol(year_data)], as.numeric)))
    
    return_data = rbind(return_data, temp_data)
  }
  return_data = transpose_data(return_data, "Label", "County, State")
  return_data$Year = as.numeric(return_data$Year)
  return_data = return_data[-1]
}

get_all = function()
{ 
  sex_and_age = remove_duplicates(DP05_data$`SEX AND AGE`)
  sex_and_age = sex_and_age %>% filter(Label != "16 years and over")
  sex_and_age = sex_and_age %>% filter(Label != "Sex ratio (males per 100 females)")
  sex_and_age = sex_and_age %>% filter(Label != "Under 18 years")
  sex_and_age = sum_all_analysis(sex_and_age)
  
  race = remove_duplicates(DP05_data$`Race alone or in combination with one or more other races`)
  race$Label = gsub("Some Other Race", "Some other race", race$Label)
  race = race %>% filter(Label != "Total population")
  race = sum_all_analysis(race)
  
  education = S1701_data$`EDUCATIONAL ATTAINMENT`
  education = sum_all_analysis(education)
  
  poverty = remove_duplicates(S1701_data$`ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS`)
  poverty = rbind(poverty, S1701_poverty_data$FIRST %>% filter(Label == "Population for whom poverty status is determined"))
  poverty$Label = gsub("Population for whom poverty status is determined", "100 percent of poverty level", poverty$Label)
  poverty = poverty %>% filter(!Label %in% names(get_mismatched_names(poverty)))
  poverty = poverty %>% filter(!Label %in% c("Male", "Female"))
  poverty = sum_all_analysis(poverty)
  
  employment = remove_duplicates(S1701_data$`EMPLOYMENT STATUS`)
  employment = employment %>% filter(!Label %in% c("Male", "Female"))
  employment = sum_all_analysis(employment)
  
  marriage = sum_duplicates(DP02_data$`MARITAL STATUS`)
  marriage = sum_all_analysis(marriage)
  
  school = DP02_data$`SCHOOL ENROLLMENT`
  school = sum_all_analysis(school)
  
  veteran = DP02_data$`VETERAN STATUS`
  veteran = veteran %>% filter(Label == "Civilian veterans")
  veteran = sum_all_analysis(veteran)
  
  disability = remove_duplicates(DP02_data$`DISABILITY STATUS OF THE CIVILIAN NONINSTITUTIONALIZED POPULATION`)
  disability = disability %>% filter(disability[names[1]] != "(X)")
  disability = disability %>% filter(!Label %in% c("Under 18 years", "18 to 64 years", "65 years and over"))
  disability = sum_all_analysis(disability)
  
  income = S2001_data$FIRST
  income = remove_duplicates(income)
  income$Label = gsub("FULL-TIME YEAR-ROUND WORKERS WITH EARNINGS", "Full-time year-round workers with earnings", income$Label)
  income = income %>% filter(!Label %in% c("Mean earnings (dollars)",
                                           "Mean earnings (dollars) for full-time year-round workers with earnings",
                                           "Median earnings (dollars) for full-time year-round workers with earnings"))
  
  income = convert_percentages(income, income[income$Label == "Full-time year-round workers with earnings",])
  
  income = sum_all_analysis(income)
  
  mmg = MMG_data %>% filter(!(grepl("%", MMG_data$Label) & !grepl("#", MMG_data$Label)))
  mmg = sum_all_analysis(mmg)
  snap = sum_all_analysis(SNAP_data)
  
  temp_data = inner_join(sex_and_age, race, by = "Year")
  temp_data = inner_join(temp_data, education, by = "Year")
  temp_data = inner_join(temp_data, poverty, by = "Year")
  temp_data = inner_join(temp_data, employment, by = "Year")
  temp_data = inner_join(temp_data, marriage, by = "Year")
  temp_data = inner_join(temp_data, school, by = "Year")
  temp_data = inner_join(temp_data, veteran, by = "Year")
  temp_data = inner_join(temp_data, disability, by = "Year")
  temp_data = inner_join(temp_data, income, by = "Year")
  temp_data = inner_join(temp_data, mmg, by = "Year")
  temp_data = inner_join(temp_data, snap, by = "Year")
  
  combined_data = data.frame(apply(temp_data, 2, function(x) as.numeric(as.character(x))))
  names(combined_data) = names(temp_data)
  
  return(combined_data)
}

get_data = function(county_name)
{
  sex_and_age = remove_duplicates(DP05_data$`SEX AND AGE`)
  sex_and_age = sex_and_age %>% filter(Label != "16 years and over")
  sex_and_age = sex_and_age %>% filter(Label != "Sex ratio (males per 100 females)")
  sex_and_age = sex_and_age %>% filter(Label != "Under 18 years")
  sex_and_age = format_analysis(sex_and_age, county_name)
  
  race = remove_duplicates(DP05_data$`Race alone or in combination with one or more other races`)
  race$Label = gsub("Some Other Race", "Some other race", race$Label)
  race = race %>% filter(Label != "Total population")
  race = format_analysis(race, county_name)
  
  education = S1701_data$`EDUCATIONAL ATTAINMENT`
  education = format_analysis(education, county_name)
  
  poverty = remove_duplicates(S1701_data$`ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS`)
  poverty = rbind(poverty, S1701_poverty_data$FIRST %>% filter(Label == "Population for whom poverty status is determined"))
  poverty$Label = gsub("Population for whom poverty status is determined", "100 percent of poverty level", poverty$Label)
  poverty = poverty %>% filter(!Label %in% names(get_mismatched_names(poverty)))
  poverty = poverty %>% filter(!Label %in% c("Male", "Female"))
  poverty = format_analysis(poverty, county_name)
  
  employment = remove_duplicates(S1701_data$`EMPLOYMENT STATUS`)
  employment = employment %>% filter(!Label %in% c("Male", "Female"))
  employment = format_analysis(employment, county_name)
  
  marriage = sum_duplicates(DP02_data$`MARITAL STATUS`)
  marriage = format_analysis(marriage, county_name)
  
  school = DP02_data$`SCHOOL ENROLLMENT`
  school = format_analysis(school, county_name)
  
  veteran = DP02_data$`VETERAN STATUS`
  veteran = veteran %>% filter(Label == "Civilian veterans")
  veteran = format_analysis(veteran, county_name)
  
  disability = remove_duplicates(DP02_data$`DISABILITY STATUS OF THE CIVILIAN NONINSTITUTIONALIZED POPULATION`)
  disability = disability %>% filter(disability[county_name] != "(X)")
  disability = disability %>% filter(!Label %in% c("Under 18 years", "18 to 64 years", "65 years and over"))
  
  disability = format_analysis(disability, county_name)
  
  income = S2001_data$FIRST
  income = remove_duplicates(income)
  income$Label = gsub("FULL-TIME YEAR-ROUND WORKERS WITH EARNINGS", "Full-time year-round workers with earnings", income$Label)
  income = income %>% filter(!Label %in% c("Mean earnings (dollars)",
                                           "Mean earnings (dollars) for full-time year-round workers with earnings",
                                           "Median earnings (dollars) for full-time year-round workers with earnings"))
  
  income = convert_percentages(income, income[income$Label == "Full-time year-round workers with earnings",])
  
  income = format_analysis(income, county_name)
  
  mmg = format_analysis(MMG_data,county_name)
  snap = format_analysis(SNAP_data,county_name)
  
  temp_data = inner_join(sex_and_age, race, by = "Year")
  temp_data = inner_join(temp_data, education, by = "Year")
  temp_data = inner_join(temp_data, poverty, by = "Year")
  temp_data = inner_join(temp_data, employment, by = "Year")
  temp_data = inner_join(temp_data, marriage, by = "Year")
  temp_data = inner_join(temp_data, school, by = "Year")
  temp_data = inner_join(temp_data, veteran, by = "Year")
  temp_data = inner_join(temp_data, disability, by = "Year")
  temp_data = inner_join(temp_data, income, by = "Year")
  temp_data = inner_join(temp_data, mmg, by = "Year")
  temp_data = inner_join(temp_data, snap, by = "Year")
  
  combined_data = data.frame(apply(temp_data, 2, function(x) as.numeric(as.character(x))))
  names(combined_data) = names(temp_data)
  
  return(combined_data)
}

get_significant_factors = function(county_name, factor_col, exclude = c())
{
  county = get_data(county_name)
  county = exclude_percentages(county)
  
  county = county[,!colnames(county) %in% exclude]
  
  cor_matrix = cor(county)
  cor = data.frame(Label = rownames(cor_matrix), Correlation = cor_matrix[,factor_col])
  sig = cor %>% filter(abs(Correlation) > 0.5)
  pos = sig %>% filter(Correlation > 0) %>% arrange(desc(Correlation))
  pos = pos[-1,]
  pos = head(pos, n = 5)
  neg = sig %>% filter(Correlation < 0) %>% arrange(Correlation)
  neg = head(neg, n = 5)
  
  complete = rbind(pos, neg)
  rownames(complete) = NULL
  complete = complete %>% arrange(desc(Correlation))
  
  return(complete)
}

get_factors = function(county_name, factor_col, exclude = c())
{ 
  county = get_data(county_name)
  county = exclude_percentages(county)
  
  county = county[,!colnames(county) %in% exclude]
  
  cor_matrix = cor(county)
  cor = data.frame(Label = rownames(cor_matrix), Correlation = cor_matrix[,factor_col])
  cor = cor %>% arrange(desc(Correlation))
  cor = cor[-1,]
  rownames(cor) = NULL
  return(cor)
}