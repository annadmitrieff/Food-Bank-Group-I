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
    
    header_indices = which(temp_data[,2] == "")
    headers = temp_data[header_indices, 1]
    headers = gsub("\\s*\\([^\\)]+\\)","", headers)
    headers = gsub("\u00A0","", headers)
    
    header_indices = c(header_indices, nrow(temp_data) + 1)
    
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

folder = list.files("./data/S0101_csv", pattern = "^ACS", full.names = TRUE)
data = list()

total_pop_data = NULL

for (n in 1:length(folder))
{
  temp_data = read.csv(folder[n])
  temp_data = temp_data[ , !grepl("Error|Percent|Male|Female", names(temp_data))]
  temp_data = temp_data[1,]
  temp_data = temp_data %>% mutate(Year = 2010 + n - 1)
  
  total_pop_data = rbind(total_pop_data, temp_data)
}

total_pop_data = data.frame(total_pop_data[c(1, 17)], lapply(total_pop_data[2:16], function(x) as.numeric(gsub("[,]", "", x))))


DP05_data = extract_from_acs_csv("./data/DP05_csv", "Error|Percent")
S0101_data = extract_from_acs_csv("./data/S0101_csv", "Error|Percent|Male|Female")
S1501_data = extract_from_acs_csv("./data/S1501_csv", "Error|Percent|Male|Female")
S1701_data = extract_from_acs_csv("./data/S1701_csv", "Error|Percent|Male|Female|poverty", 2012)
S1701_poverty_data = extract_from_acs_csv("./data/S1701_csv", "Error|Percent|Male|Female|Total", 2012)

names(total_pop_data) = c(names(S0101_data$AGE[c(1, 17)]), names(S0101_data$AGE[2:16]))

lambda = function(x)
{
  col = 0
  for (i in 2:16)
  {
    if(identical(x, age_data[,i]))
    {
      col = i
    }
  }
  
  for (i in 1:length(x))
  {
    if (grepl("%", x[i]))
    {
      a = gsub("[%]", "", x[i])
      b = total_pop_data[total_pop_data$Year == age_data$Year[i], col + 1]

      x[i] = as.numeric(a) * b / 100
      x[i] = round(as.numeric(x[i]))
      
      if (i == 1)
      {
        print(as.numeric(a))
        print(b)
        print(x[i])
      }
    }
    else
    {
      x[i] = as.numeric(x[i])
    }
  }
  
  return(x)
}

age_data = S0101_data$AGE
age_data = data.frame(age_data[,c(1, 17)], lapply(age_data[,2:16], lambda))
age_data = age_data %>% filter(Label %in% Label[13:18]) %>%
  group_by(Year) %>%
  summarise_at(2:16, function(x) sum(as.numeric(x)))

names(age_data) = names(c(S0101_data$AGE[1, 17], S0101_data$AGE[2:16]))


