library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

source("~/GitHub/Food-Bank-Group-I/Data_Extraction.R", echo=TRUE)
source("~/GitHub/Food-Bank-Group-I/exploratory_analysis_food_bank.R", echo=TRUE)

if (FALSE)
{
  MMG_data = extract_from_mmg_excel()
  MMG_data = MMG_data %>% mutate(a = round(`% of individuals in households below 130% of poverty line that are food insecure` * `# of individuals that are food insecure`),
                                 b = round(`% of individuals in households above 130% of poverty line that are food insecure` * `# of individuals that are food insecure`),
                                 c = round(`% of children in households below 185% of poverty line that are food insecure` * `# of children that are food insecure`),
                                 d = round(`% of children in households above 185% of poverty line that are food insecure` * `# of children that are food insecure`))
  names(MMG_data) = c(names(MMG_data)[!names(MMG_data) %in% c("a", "b", "c", "d")],
                      "# of individuals in households below 130% of poverty line that are food insecure",
                      "# of individuals in households above 130% of poverty line that are food insecure",
                      "# of children in households below 185% of poverty line that are food insecure",
                      "# of children in households above 185% of poverty line that are food insecure")
  MMG_data = transpose_data(MMG_data, "County, State", "Label")

  SNAP_data = extract_from_SNAP_excel()

  DP02_data = extract_from_acs_csv("./data/DP02_csv", "Error|Percent")
  DP05_data = extract_from_acs_csv("./data/DP05_csv", "Error|Percent")
  S0101_data = extract_from_acs_csv("./data/S0101_csv", "Error|Percent|Male|Female")
  S1501_data = extract_from_acs_csv("./data/S1501_csv", "Error|Percent|Male|Female")
  S1701_data = extract_from_acs_csv("./data/S1701_csv", "Error|Percent|Male|Female|poverty", 2012)
  S1701_poverty_data = extract_from_acs_csv("./data/S1701_csv", "Error|Percent|Male|Female|Total", 2012)
  S2001_data = extract_from_acs_csv("./data/S2001_csv", "Error|Male|Female|Percent")
  
  S1701_data$`ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS` = rbind(S1701_data$`All Individuals below:`, S1701_data$`ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS`)
  S1701_data$`All Individuals below:` = NULL
}

cor_factor = "SNAP All Persons Non-Public Assistance Participation"
#cor_factor = "Calc: SNAP Total PA and Non-PA People"
#cor_factor = "# of individuals that are food insecure"

if (FALSE)
{
  cor_averages = data.frame(Label = NULL, Correlation = NULL)
  for (i in 1:length(names))
  {
    print(names[i])

    exclude = c("# of children in households above 185% of poverty line that are food insecure",
                "# of children in households below 185% of poverty line that are food insecure",
                "# of children that are food insecure",
                "# of individuals in households above 130% of poverty line that are food insecure",
                "# of individuals in households below 130% of poverty line that are food insecure",
                "100 percent of poverty level",
                "200 percent of poverty level",
                "185 percent of poverty level",
                "150 percent of poverty level",
                "50 percent of poverty level")

    temp_data = get_significant_factors(names[i], cor_factor, exclude)

    mismatched = !(temp_data$Label %in% cor_averages$Label)

    for (n in 1:length(mismatched))
    {
      if (!mismatched[n])
      {
        cor_averages[cor_averages$Label == temp_data$Label[n], 2] = as.numeric(cor_averages[cor_averages$Label == temp_data$Label[n], 2]) + round(temp_data[n, 2])
      }
      else
      {
        cor_averages = rbind(cor_averages, temp_data[n,])
        cor_averages[cor_averages$Label == temp_data$Label[n], 2] = round(cor_averages[cor_averages$Label == temp_data$Label[n], 2])
      }
    }
  }

  #cor_averages[, 2] = cor_averages[, 2] / length(names)
  
  county_cor = NULL
  
  for (i in 1:length(names))
  {
    exclude = c("# of children in households above 185% of poverty line that are food insecure",
                "# of children in households below 185% of poverty line that are food insecure",
                "# of children that are food insecure",
                "# of individuals in households above 130% of poverty line that are food insecure",
                "# of individuals in households below 130% of poverty line that are food insecure")
    
    temp_data = get_factors(names[i], cor_factor, exclude)
    temp_data = temp_data %>% arrange(Label)
    
    if (is.null(county_cor))
    {
      county_cor = temp_data
    }
    else
    { 
      county_cor = cbind(county_cor, temp_data[,2])
    }
  }
  names(county_cor) = c("Label", names)
}

county_data = list()
neg_prop_data = list()
pos_prop_data = list()

prop_data = NULL

for (i in 1:(length(names) + 1))
{
  print(names[i])
  if (!length(county_data) < 16)
  {
    temp_data = county_data[[i]]
  }
  if (i == 16)
  {
    temp_data = get_all()
  }
  else
  {
    temp_data = get_data(names[i])
  }
  county_data[[i]] = temp_data
  
  #temp_data = county_data[[i]]
  
  neg_data = data.frame(rep(names[i]),
                        temp_data$Year,
                        "% of population participating in SNAP",
                        "All",
                        temp_data$`Calc: SNAP Total PA and Non-PA People` / temp_data$`Total population`)
  neg_data = as.data.frame(mapply(c, neg_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "% of population food insecure",
                                             "All",
                                             temp_data$`# of individuals that are food insecure` / temp_data$`Total population`)))
  neg_data = as.data.frame(mapply(c, neg_data, 
                   data.frame(rep(names[i]), 
                              temp_data$Year,
                              "% of population over 65 years",
                              "Negative",
                              temp_data$`65 years and over` / temp_data$`Total population`)))
  neg_data = as.data.frame(mapply(c, neg_data, 
                   data.frame(rep(names[i]), 
                              temp_data$Year,
                              "% of population with a disability", 
                              "Negative",
                              temp_data$`With a disability` / temp_data$`Total population`)))
  neg_data = as.data.frame(mapply(c, neg_data, 
                   data.frame(rep(names[i]), 
                              temp_data$Year,
                              "% of population divorced", 
                              "Negative",
                              temp_data$`Divorced` / temp_data$`Total population`)))
  neg_data = as.data.frame(mapply(c, neg_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "% of population in high school", 
                                             "Negative",
                                             temp_data$`High school (grades 9-12)` / temp_data$`Total population`)))
  neg_data = as.data.frame(mapply(c, neg_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "% of population over 25 with only a high school diploma",
                                             "Negative",
                                             temp_data$`High school graduate (includes equivalency)` / temp_data$`Total population`)))
  names(neg_data) = c("County, State",
                      "Year",
                      "Label",
                      "Category",
                      "Percent")
  pos_data = data.frame(rep(names[i]), 
                        temp_data$Year,
                        "% of population unemployed", 
                        "Positive",
                        temp_data$`Unemployed` / temp_data$`Total population`)
  pos_data = as.data.frame(mapply(c, pos_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "% of population that are veterans", 
                                             "Positive",
                                             temp_data$`Civilian veterans` / temp_data$`Total population`)))
  pos_data = as.data.frame(mapply(c, pos_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "% of population in kindergarten", 
                                             "Positive",
                                             temp_data$`Kindergarten` / temp_data$`Total population`)))
  pos_data = as.data.frame(mapply(c, pos_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "% of population under 100 percent of poverty level", 
                                             "Positive",
                                             temp_data$`100 percent of poverty level` / temp_data$`Total population`)))
  pos_data = as.data.frame(mapply(c, pos_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "% of population in college or graduate school", 
                                             "Positive",
                                             temp_data$`College or graduate school` / temp_data$`Total population`)))
  pos_data = as.data.frame(mapply(c, pos_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "Median earnings in dollars", 
                                             "NA",
                                             temp_data$`Median earnings (dollars)`)))
  pos_data = as.data.frame(mapply(c, pos_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "Average meal cost in dollars", 
                                             "NA",
                                             temp_data$`Average meal cost`)))
  names(pos_data) = c("County, State",
                      "Year",
                      "Label",
                      "Category",
                      "Percent")
  
  race_data = data.frame(rep(names[i]), 
                         temp_data$Year,
                         "% of population that are Black or African American", 
                         "Race / Ethnicity",
                         temp_data$`Black or African American` / temp_data$`Total population`)
  race_data = as.data.frame(mapply(c, race_data, 
                                  data.frame(rep(names[i]), 
                                             temp_data$Year,
                                             "% of population that are Asian", 
                                             "Race / Ethnicity",
                                             temp_data$`Asian` / temp_data$`Total population`)))
  race_data = as.data.frame(mapply(c, race_data, 
                                   data.frame(rep(names[i]), 
                                              temp_data$Year,
                                              "% of population that are White", 
                                              "Race / Ethnicity",
                                              temp_data$`White` / temp_data$`Total population`)))
  race_data = as.data.frame(mapply(c, race_data, 
                                   data.frame(rep(names[i]), 
                                              temp_data$Year,
                                              "% of population that are Native Hawaiian and Other Pacific Islander", 
                                              "Race / Ethnicity",
                                              temp_data$`Native Hawaiian and Other Pacific Islander` / temp_data$`Total population`)))
  race_data = as.data.frame(mapply(c, race_data, 
                                   data.frame(rep(names[i]), 
                                              temp_data$Year,
                                              "% of population that are some other race", 
                                              "Race / Ethnicity",
                                              temp_data$`Some other race` / temp_data$`Total population`)))
  names(race_data) = c("County, State",
                       "Year",
                       "Label",
                       "Category",
                       "Percent")
  
  
  prop_data = rbind(prop_data, neg_data)
  prop_data = rbind(prop_data, pos_data)
  prop_data = rbind(prop_data, race_data)
}

prop_data$Percent = as.numeric(prop_data$Percent)
prop_data$`County, State` = replace_na(prop_data$`County, State`, "Service Region")

#prop_data = transpose_data(prop_data, "County, State", "Label")
names(county_data) = names