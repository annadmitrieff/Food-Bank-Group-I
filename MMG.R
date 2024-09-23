library(readxl)
library(dplyr)
library(stringr)

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

categories = c("County, State",
               "Year",
               "% of total population that are food insecure",
               "# of individuals that are food insecure",
               "% of individuals in households below 130% of poverty line that are food insecure",
               "% of individuals in households above 130% of poverty line that are food insecure",
               "% of children that are food insecure",
               "# of children that are food insecure",
               "% of children in households below 185% of poverty line that are food insecure",
               "% of children in households above 185% of poverty line that are food insecure",
               "Average meal cost",
               "Weighted annual food budget shortfall")

extract_from_mmg_excel = function()
{
  MMG_folder = list.files("./data/MMG_Data", pattern = "^MMG", full.names = TRUE)
  MMG_data = NULL
  
  temp_sheet = read_excel(MMG_folder[1], sheet = "County")
  temp_data = temp_sheet %>% filter(`State Name` == "GA" 
                                    & `County Code` %in% c("BANKS",
                                                           "BARROW",
                                                           "CLARKE", 
                                                           "ELBERT",
                                                           "FRANKLIN",
                                                           "HABERSHAM",
                                                           "HART",
                                                           "JACKSON",
                                                           "MADISON",
                                                           "OCONEE",
                                                           "OGLETHORPE",
                                                           "RABUN",
                                                           "STEPHENS",
                                                           "TOWNS",
                                                           "WHITE"))
  temp_data$Year = rep(2009)
  temp_data[11] = temp_data[11] + temp_data[12]
  temp_data[2] = names
  temp_data = temp_data[,c(2, 19, 4, 5, 10, 11, 15:18, 14, 13)]
  names(temp_data) = categories
  
  MMG_data = temp_data
  
  for (i in 0:7)
  {
    sheet_name = excel_sheets(MMG_folder[i + 2])
    sheet_name = grep("County", sheet_name, value = TRUE)
    temp_sheet = read_excel(MMG_folder[i + 2], sheet = sheet_name[1])
    temp_data = temp_sheet %>% filter(`County, State` %in% names)
    temp_data$Year = rep(2010 + i)
    temp_data[11] = as.numeric(unlist(temp_data[11], use.names = FALSE))
    temp_data[12] = as.numeric(unlist(temp_data[11], use.names = FALSE))
    temp_data[11] = temp_data[11] + temp_data[12]
    temp_data = temp_data[,c(3, 19, 4, 5, 10, 11, 13:18)]
    names(temp_data) = categories
    temp_data[3] = as.numeric(unlist(temp_data[3], use.names = FALSE))
    temp_data[5] = as.numeric(unlist(temp_data[5], use.names = FALSE))
    temp_data[7] = as.numeric(unlist(temp_data[7], use.names = FALSE))
    temp_data[9] = as.numeric(unlist(temp_data[9], use.names = FALSE))
    temp_data[10] = as.numeric(unlist(temp_data[10], use.names = FALSE))
    
    MMG_data = MMG_data %>% rbind(temp_data)
    
  }
  
  temp_sheet = read_excel(MMG_folder[10], sheet = "2018 County", skip = 1)
  temp_data = temp_sheet %>% filter(`County, State` %in% names)
  
  temp_data$Year = rep(2018)
  temp_data[11] = as.numeric(unlist(temp_data[11], use.names = FALSE))
  temp_data[12] = as.numeric(unlist(temp_data[11], use.names = FALSE))
  temp_data[11] = temp_data[11] + temp_data[12]
  temp_data[2] = names
  temp_data = temp_data[,c(3, 19, 4, 5, 10, 11, 13:18)]
  names(temp_data) = categories
  temp_data[3] = as.numeric(unlist(temp_data[3], use.names = FALSE))
  temp_data[5] = as.numeric(unlist(temp_data[5], use.names = FALSE))
  temp_data[7] = as.numeric(unlist(temp_data[7], use.names = FALSE))
  temp_data[9] = as.numeric(unlist(temp_data[9], use.names = FALSE))
  temp_data[10] = as.numeric(unlist(temp_data[10], use.names = FALSE))
  
  MMG_data = MMG_data %>% rbind(temp_data)
  
  # 2019 to 2022
  temp_sheet = read_excel(MMG_folder[11], sheet = "County")
  temp_data = temp_sheet %>% filter(`County, State` %in% names)
  
  temp_data[11] = as.numeric(unlist(temp_data[11], use.names = FALSE))
  temp_data[12] = as.numeric(unlist(temp_data[11], use.names = FALSE))
  temp_data[11] = temp_data[11] + temp_data[12]
  temp_data = temp_data[,c(3, 4, 5, 6, 11:17, 19)]
  temp_data = temp_data %>% arrange(Year)
  names(temp_data) = categories
  temp_data[3] = as.numeric(unlist(temp_data[3], use.names = FALSE))
  temp_data[5] = as.numeric(unlist(temp_data[5], use.names = FALSE))
  temp_data[7] = as.numeric(unlist(temp_data[7], use.names = FALSE))
  temp_data[9] = as.numeric(unlist(temp_data[9], use.names = FALSE))
  temp_data[10] = as.numeric(unlist(temp_data[10], use.names = FALSE))
  
  MMG_data = MMG_data %>% rbind(temp_data)
  
  return(MMG_data)
}

MMG_data = extract_from_mmg_excel()

format_mmg = function()
{
  combined_data = NULL
  for (i in 9:22)
  {
    temp_data = MMG_data %>% filter(Year == 2000 + i)
    temp_data = temp_data[, 3:12]
    temp_data = t(temp_data)
    rownames(temp_data) = NULL
    temp_data = data.frame(temp_data)
    temp_data = temp_data %>% mutate(Label = colnames(MMG_data)[3:12], Year = rep(2000 + i))
    colnames(temp_data) = c(names, "Label", "Year")
    temp_data = temp_data[c("Label", "Year", names)]
    
    combined_data = rbind(combined_data, temp_data)
  }
  
  return(combined_data)
}

MMG_data = format_mmg()