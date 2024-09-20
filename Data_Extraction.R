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

DP05_data = extract_from_acs_csv("./data/DP05_csv", "Error|Percent")
S0101_data = extract_from_acs_csv("./data/S0101_csv", "Error|Percent|Male|Female")
S1501_data = extract_from_acs_csv("./data/S1501_csv", "Error|Percent|Male|Female")
S1701_data = extract_from_acs_csv("./data/S1701_csv", "Error|Percent|Male|Female|poverty", 2012)
S1701_poverty_data = extract_from_acs_csv("./data/S1701_csv", "Error|Percent|Male|Female|Total", 2012)

