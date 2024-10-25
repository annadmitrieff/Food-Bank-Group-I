library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

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

extract_from_acs_csv = function(folder_path, exclude, start_year = 2010)
{
  folder = list.files(folder_path, pattern = "^ACS", full.names = TRUE)
  data = list()
  
  for (n in 1:length(folder))
  {
    temp_data = read.csv(folder[n])
    temp_data = temp_data[ , !grepl(exclude, names(temp_data))]
    temp_data$Year = start_year + n - 1
    temp_data[,1] = gsub("\u00A0","", temp_data[,1])
    temp_data = data.frame(lapply(temp_data, function(x) gsub("[,]", "", x)))
    names(temp_data) = c("Label", names, "Year")
    temp_data = temp_data[c("Label", "Year", names)]
    
    header_indices = which(temp_data[,3] == "")
    headers = temp_data[header_indices, 1]
    headers = gsub("\\s*\\([^\\)]+\\)","", headers)
    headers = gsub("\u00A0","", headers)
    
    header_indices = c(header_indices, nrow(temp_data) + 1)
    
    if (header_indices[1] != 1)
    {
      header_indices = c(1, header_indices)
      headers = c("FIRST", headers)
    }
    
    for (i in 1:length(headers))
    {
      start = header_indices[i] + 1
      end = header_indices[i + 1] - 1
      
      if (length(data) == 0 | !all(headers[i] %in% names(data)))
      {
        data[[headers[i]]] = data.frame(matrix(ncol = length(names) + 2, nrow = 0))
      }
      
      data[[headers[i]]] = rbind(data[[headers[i]]], temp_data[start:end,])
      
      row.names(data[[i]]) = NULL
    }
  }
  
  return(data)
}

transpose_data = function(data, id_col, new_id_col, group_col = "Year")
{
  transposed_data = NULL
  
  region = !names(data) %in% c(id_col, group_col)
  groups = unlist(unique(data[group_col]), use.names = FALSE)
  
  for (i in 1:length(groups))
  {
    temp_data = data %>% filter(data[group_col] == groups[i])
    temp_data = temp_data[region]
    temp_data = t(temp_data)
    temp_data = data.frame(temp_data)
    
    temp_data = temp_data %>% mutate(colnames(data)[region], rep(groups[i]))
    
    colnames(temp_data) = c(unlist(unique(data[id_col]), use.names = FALSE), new_id_col, group_col)
    rownames(temp_data) = NULL
    
    temp_data = temp_data[c(new_id_col, group_col, unlist(unique(data[id_col]), use.names = FALSE))]
    
    transposed_data = rbind(transposed_data, temp_data)
  }
  
  return(transposed_data)
}

remove_duplicates = function(data, id_col = "Label", group_col = "Year")
{
  groups = unlist(unique(data[group_col]), use.names = FALSE)
  return_data = NULL
  
  for (i in 1:length(groups))
  { 
    temp_data = data %>% filter(data[group_col] == groups[i])
    temp_data = temp_data[!duplicated(temp_data[id_col]),]
    return_data = rbind(return_data, temp_data)
  }
  
  return(return_data)
}

sum_duplicates = function(data, id_col = "Label", group_col = "Year")
{
  groups = unlist(unique(data[group_col]), use.names = FALSE)
  return_data = NULL
  
  for (i in 1:length(groups))
  { 
    temp_data = data %>% filter(data[group_col] == groups[i])
    
    first = duplicated(temp_data$Label, fromLast = TRUE)
    last = duplicated(temp_data$Label)
    unique = !first & !last
    
    temp_data[first, !colnames(temp_data) %in% c(id_col, group_col)] = data.frame(lapply(temp_data[first, !colnames(temp_data) %in% c(id_col, group_col)], function(x) as.numeric(x))) + data.frame(lapply(temp_data[last, !colnames(temp_data) %in% c(id_col, group_col)], function(x) as.numeric(x)))
    temp_data = temp_data[first | unique,]
    return_data = rbind(return_data, temp_data)
  }
  
  return(return_data)
}

extract_from_mmg_excel = function()
{
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
  
  temp_data = temp_data[,c(3, 4, 5, 6, 11:17, 19)]
  temp_data = temp_data %>% arrange(Year)
  names(temp_data) = categories
  temp_data[3] = as.numeric(unlist(temp_data[3], use.names = FALSE))
  temp_data[5] = as.numeric(unlist(temp_data[5], use.names = FALSE))
  temp_data[7] = as.numeric(unlist(temp_data[7], use.names = FALSE))
  temp_data[9] = as.numeric(unlist(temp_data[9], use.names = FALSE))
  temp_data[10] = as.numeric(unlist(temp_data[10], use.names = FALSE))
  temp_data[11] = as.numeric(unlist(temp_data[11], use.names = FALSE))
  temp_data[12] = as.numeric(unlist(temp_data[12], use.names = FALSE))
  
  MMG_data = MMG_data %>% rbind(temp_data)
  
  return(MMG_data)
}

extract_from_SNAP_excel = function()
{
  SNAP_folder = list.files("./data/SNAP_Participation", pattern = "^J", full.names = TRUE)
  SNAP_data = NULL
  
  for (i in 0:12)
  {
    temp_sheet = read_excel(SNAP_folder[i + 1], skip = 3)
    str = str_split_fixed(temp_sheet$`Substate/Region`, " ", n = 7)[,1:4]
    temp_sheet[c("FIPS", "State", "EBT", "County")] = str
    temp_data = temp_sheet[c("State",
                             "County",
                             "SNAP All Persons Public Assistance Participation",
                             "SNAP All Persons Non-Public Assistance Participation",
                             "Calc: SNAP Total PA and Non-PA People")]
    temp_data = temp_data %>% 
      filter(State == "GA" 
             & County %in% c("BANKS",
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
    
    pop = data.frame(Year = rep(2010 + i), t(temp_data[,3:5]))
    colnames(pop) = c("Year", names)
    
    SNAP_data = SNAP_data %>% rbind(pop)
  }
  
  SNAP_data = SNAP_data %>% mutate(Label = rep(c("SNAP All Persons Public Assistance Participation",
                                                 "SNAP All Persons Non-Public Assistance Participation",
                                                 "Calc: SNAP Total PA and Non-PA People"), 13))
  SNAP_data = SNAP_data[c("Label", "Year", names)]
  
  rownames(SNAP_data) = NULL
  
  return(SNAP_data)
}

get_mismatched_names = function(data, id_col = "Label", group_col = "Year")
{
  groups = unlist(unique(data[group_col]), use.names = FALSE)
  
  occurances = table(data["Label"])
  
  return(occurances[occurances < length(groups)])
}

exclude_percentages = function(data)
{
  cols = names(data)
  
  temp_data = data
  
  for (col in 1:length(cols))
  {
    if (all(temp_data[cols[col]] == 0, na.rm = TRUE))
    {
      next
    }
    else if (all(temp_data[cols[col]] <= 1, na.rm = TRUE))
    {
      temp_data[cols[col]] = NULL
    }
  }
  
  return(temp_data)
}

convert_percentages = function(data, total_pop)
{
  return_data = NULL
  for (i in 1:nrow(data))
  {
    if (any(grepl("%", data[i,])))
    {
      pop = total_pop %>% filter(Year == data[i, "Year"])
      pop = pop[,-c(1, 2)]
      pop = as.numeric(pop)
      
      temp_data = gsub("%", "", data[i,-c(1, 2)])
      temp_data = as.numeric(temp_data) / 100
      
      temp_data = round(temp_data * pop)
      
      temp_data = c(data[i, 1], data[i, 2], temp_data)
    }
    else
    {
      temp_data = data[i,]
    }
    
    return_data = rbind(return_data, temp_data)
  }
  
  return(return_data)
}
