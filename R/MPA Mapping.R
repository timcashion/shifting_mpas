

#I think I disregarded this file without making a note about it. 
#Probably should move this to not use again. Code for doing this is contained in OutputDataCleaning.R


library(tidyverse)
mpa_files <- list.files("./InputData/")
mpa_files <- mpa_files[grepl(mpa_files, pattern= "Anchovy Bay-MPA ")]
static_mpa_files <- mpa_files[grepl(mpa_files, pattern= "Static")]
shifting_mpa_files <- mpa_files[grepl(mpa_files, pattern= "Shifting")]

mpa_files

file <- mpa_files[1]
headers <- as.character(seq(0,20,1))


for (file in mpa_files) {
  year <- 2000
  data <- read_csv(paste("./InputData/", file, sep=""), col_names=headers, skip=1)
  data <- data %>% rename(row_num=`0`)
  #data$row_num <- seq(1,20,1)
  #data$year <- year
  data_melt <- gather(data, "col_num", "MPA", -year, -row_num)
}

