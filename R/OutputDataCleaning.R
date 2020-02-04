

library(tidyverse)
setwd("../ModelOutputs/")

setwd("C:/Users/t.cashion/Google Drive/Desktop/SESYNC/ModelOutputs")
####Clean Raw Data to Tidy Format####

#Need to add in 'scenario' after implementing in EwE to these loops to generate a 'scenario' column we can use and modify the data by


models <- list.files()
folders <- models
#Focus on average outputs first
files <- c("Ecospace_Annual_Average_Biomass.csv", "Ecospace_Annual_Average_Catch.csv") #Define files we need to make graphs
group_data= data.frame()


folder <- folders[1]
file <- files[1]

for (folder in folders) {
  for (file in files){
    df_name <- paste(folder, file, sep="/")
    df_name <- gsub(df_name, pattern=".csv", replacement="")
    df_name <- gsub(df_name, pattern="Ecospace_", replacement="")
    df_name <- gsub(df_name, pattern="SardineBank_", replacement="")
    df_type <- gsub(df_name, pattern=".*/Annual_", replacement="")
    df_RCP <- gsub(df_name, pattern="/.*", replacement="")
    # df_RCP <- gsub(df_name, pattern="Basemodel_", replacement="")
    df_RCP <- gsub(df_RCP, pattern="_.*", replacement="")
    df_design <- gsub(df_name, pattern="/.*", replacement="")
    df_design <- gsub(df_design, pattern=paste(df_RCP, "_",  sep=""), replacement="")
    df_design <- gsub(df_design, pattern="_\\d\\d", replacement="")
    df_design <- gsub(df_design, pattern="_\\d", replacement="")
    df_scenario <- gsub(df_name, pattern="/.*", replacement="")
    df_scenario <- gsub(df_scenario, pattern=paste(df_RCP, "_", df_design, "_",  sep=""), replacement="")
    data <- read_csv(print(paste(".", folder, file, sep="/")), skip=21)
    data <- data %>% gather(group, value, -Year)
    data$type <- df_type
    data$RCP <- df_RCP
    data$design <- df_design
    data$scenario <- df_scenario
    assign(df_name, data)
    group_data <- rbind(group_data, assign(df_name, data))
  }
}

write_csv(group_data, "../DataOutputs/AnnualAverageData_tidy.csv")
group_data <- read_csv("../DataOutputs/AnnualAverageData_tidy.csv")
####Average Biomass and Catch under different scenarios ####



library(tidyverse)
library(RColorBrewer)
# setwd("./ModelOutputs/")

####Clean Raw Data to Tidy Format####
models <- list.files()
folders <- models
#folders <- folders[1]

files <- c("Ecospace_Average_Biomass.csv", "Ecospace_Average_Catch.csv") #Define files we need to make graphs

group_data= data.frame()
headers = as.character(seq(1,20,1))
years <- seq(0,100, 1)


folder <- folders[1]
file <- files[1]
year <- years[1]
start <- Sys.time()
for (folder in folders) {
  files <- list.files(paste("./", folder, "/csv/", sep=""))
  
  for (file in files){
    df_temp <- tibble()
    df_name <- paste(folder, file, sep="/")
    df_name <- gsub(df_name, pattern=".csv", replacement="")
    # df_name <- gsub(df_name, pattern="Ecospace_", replacement="")
    df_name <- gsub(df_name, pattern="SardineBank_", replacement="")
    df_type <- gsub(df_name, pattern=".*/EcospaceMap", replacement="")
    df_type <- gsub(df_type, pattern="-.*", replacement="")
    df_RCP <- gsub(df_name, pattern="/.*", replacement="")
    # df_RCP <- gsub(df_name, pattern="Basemodel_", replacement="")
    df_RCP <- gsub(df_RCP, pattern="_.*", replacement="")
    df_design <- gsub(df_name, pattern="/.*", replacement="")
    df_design <- gsub(df_design, pattern=paste(df_RCP, "_",  sep=""), replacement="")
    df_design <- gsub(df_design, pattern="_\\d\\d", replacement="")
    df_design <- gsub(df_design, pattern="_\\d", replacement="")
    df_scenario <- gsub(df_name, pattern="/.*", replacement="")
    df_scenario <- gsub(df_scenario, pattern=paste(df_RCP, "_", df_design, "_",  sep=""), replacement="")
    df_fg <- df_name
    df_fg <- gsub(df_fg, pattern = ".*EcospaceMapBiomass-", replacement = "")
    df_fg <- gsub(df_fg, pattern = ".*EcospaceMapCatch-", replacement = "")
    df_fg <- gsub(df_fg, pattern = ".*EcospaceMapEffort-", replacement = "")
    
    data <- read_csv(print(paste(".", folder, "csv", file, sep="/")), col_names=headers)
    for (year in years) {
      skip_rows <- 21 + (year*22)
      end_row <- 19 + skip_rows
      data_temp <- data[skip_rows:end_row, ]
      data_temp$year <- year
      data_temp$row_num <- seq(1,20, 1)
      data_melt <- gather(data_temp, "col_num", "value", -year, -row_num)
      df_temp <- bind_rows(df_temp, data_melt)
    }
    
    # for (year in years) {
    #   skip_rows <- 21 + (year*23)
    #   end_row <- 20
    #   
    #   data <- read_csv(print(paste(".", folder, "csv", file, sep="/")), skip=skip_rows, n_max=end_row, skip_empty_rows = FALSE, col_names=headers)
    #   data$year <- year
    #   data$row_num <- seq(1,20, 1)
    #   data_melt <- gather(data, "col_num", "value", -year, -row_num)
    #   df_temp <- bind_rows(df_temp, data_melt)
    # }
    df_temp <- df_temp %>%
      mutate_all(as.numeric)
    data <- df_temp
    data$type <- df_type
    data$RCP <- df_RCP
    data$design <- df_design
    data$scenario <- df_scenario
    data$fg <- df_fg
    #assign(df_name, data)
    group_data <- bind_rows(group_data, data)
  }
}
group_data <- group_data %>% 
  mutate(col_num=as.numeric(col_num))

#write_csv(group_data, "../DataOutputs/SpatialData_tidy.csv")

write_csv(group_data %>% filter(scenario=="5"), "../DataOutputs/SpatialData_tidy_5.csv")
write_csv(group_data %>% filter(scenario=="10"), "../DataOutputs/SpatialData_tidy_10.csv")
write_csv(group_data %>% filter(scenario=="15"), "../DataOutputs/SpatialData_tidy_15.csv")

# group_data <- read_csv("../DataOutputs/SpatialData_tidy.csv")

end <- Sys.time() #This above loop takes about 7 minutes to run. 
end-start

####Read in MPA boundary files:####
#Adjust code to account for different MPA (%) scenarios 
setwd("../shiftingmpa-models/")
scenarios <- c("MPA Scenario 15 Percent", "MPA Scenario 10 Percent", "MPA Scenario 5 Percent")

for (scenario in scenarios){
  mpas <- tibble()
  input_files <- list.files(paste("./InputData/", scenario,"/", sep=""))
  input_files <- input_files[grepl(input_files, pattern="Anchovy Bay-MPA")]
  for (file in input_files){
    design_name <- gsub(file, pattern="Anchovy Bay-MPA - ", replacement="")
    design_name <- gsub(design_name, pattern=".csv", replacement="")
    mpa <- read_csv(paste("./InputData/", scenario, "/", file, sep=""))
    colnames(mpa)[1] <- "row_num"
    rows <- unique(mpa$row_num)
    cols <- seq(2,21,1)
    for (row in rows) {
      for (col in cols){
        cell <- mpa[row,col]
        cell <- as.numeric(cell)
        n <- 1
        cell_coords <- list(c(row+1, col),
                            c(row-1, col),
                            c(row+1, col+1),
                            c(row+1, col-1),
                            c(row-1, col+1),
                            c(row-1, col-1),
                            c(row, col+1),
                            c(row, col-1)
        )
        for (n in 1:8){
          cell_coord <- cell_coords[[n]]
          x <- min(cell_coord[1],20)
          x <- max(x, 1)
          y <- min(cell_coord[2], 21)
          y <- max(y, 2)
          
          adj_cell <- mpa[x, y]
          
          if(nrow(adj_cell)>0){
            adj_cell <- as.numeric(adj_cell)
            if (cell==1 & adj_cell==0){
              mpa[x, y] <- 2
            }
          }
        }
      }
    }
    mpa_melt <- gather(mpa, "col_num", "mpa", -row_num)
    mpa_melt$design <- paste("MPA_", design_name, sep="")
    mpa_melt$col_num <- as.numeric(mpa_melt$col_num)
    mpa_melt$mpa <- as.factor(as.character(mpa_melt$mpa))
    year_frame <- tibble(year=seq(0,100,1), design=paste("MPA_", design_name, sep=""))
    mpa_frame <- left_join(year_frame, mpa_melt)
    mpa_frame$scenario <- scenario
    mpas <- bind_rows(mpas, mpa_frame)
  }
  mpas$design <- gsub(mpas$design, pattern="-5", replacement="")
  temporal_files <- list.files(paste("./InputData/", scenario,"/", sep=""))
  temporal_files <- temporal_files[grepl(temporal_files, pattern="Temporal")]
  temporal_mpas <- tibble()
  for (i in temporal_files) {
    name <- gsub(i, pattern="AnchovyBay-MPA - Temporal - ", replacement="")
    name <- gsub(name, pattern=".csv", replacement="")
    name <- gsub(name, pattern= " ", replacement="_")
    temporal_mpa <- temporal_mpa <- read_csv(paste("./InputData/", scenario, "/", i , sep=""))
    
    temporal_mpa$mpa <- paste("MPA_", name, " - ", temporal_mpa$mpa, sep="")
    temporal_mpa$Date <- gsub(temporal_mpa$Date, pattern="-01", replacement="")
    temporal_mpa$Date <- gsub(temporal_mpa$Date, pattern="Jan-", replacement="")
    temporal_mpa$year <- as.numeric(as.character(str_sub(temporal_mpa$Date, start=3, end=4)))
    temporal_mpa$scenario <- scenario
    temporal_mpa <- temporal_mpa %>% 
      select(mpa, scenario, year, m1) %>% 
      rename(design=mpa) %>% 
      mutate(year = as.numeric(year))
    temporal_mpas <- bind_rows(temporal_mpas, temporal_mpa)
    
  }
  mpas <- left_join(mpas, temporal_mpas)
  
  mpas$m1[grepl(mpas$design, pattern="Static")] <- 0
  mpas <- mpas %>% fill(m1)
  mpas$mpa <- as.numeric(mpas$mpa)
  mpas$mpa[which(grepl(mpas$design, pattern="Shifting") & mpas$mpa==1 & mpas$m1==1)] <- 1 
  mpas$mpa[which(grepl(mpas$design, pattern="Shifting") & mpas$mpa==1 & mpas$m1==0)] <- 0 
  mpas$mpa[which(grepl(mpas$design, pattern="Shifting") & mpas$mpa==2 & mpas$m1==1)] <- 2 
  mpas$mpa[which(grepl(mpas$design, pattern="Shifting") & mpas$mpa==2 & mpas$m1==0)] <- 0 
  
  mpas <- mpas %>% select(-m1)

  mpas$design <- gsub(mpas$design, pattern=" -..", replacement="")
  # x <- unique(mpas)
  # sum(as.numeric(x$mpa))
  mpas <- mpas %>% 
    mutate(mpa= as.numeric(mpa)) %>% 
    group_by(year, scenario, design, row_num, col_num) %>% 
    summarize(mpa = sum(mpa))
  scenario_df_name <- gsub(scenario, pattern=" ", replacement="_")
  assign(scenario_df_name, mpas)
}

mpas <- MPA_Scenario_5_Percent %>% 
  bind_rows(MPA_Scenario_10_Percent) %>% 
  bind_rows(MPA_Scenario_15_Percent)

write_csv(mpas, "../DataOutputs/MPAData_tidy.csv")
mpas <- read_csv("../DataOutputs/MPAData_tidy.csv")


#Estimate fisheries revenues including price elasticity 
# setwd("../ModelOutputs/")

prices <- read_csv("./InputData/Sardine Bank-Off-vessel price.csv")
price_clean <- prices %>% 
  select(-X1) %>% 
  gather(key="Fleet", value="Price", -`Group name`) %>% 
  filter(is.na(Price)==F) %>% 
  rename(group = `Group name`)

#Assumed elasticites based on average of values covered in Asche et al. 2005 study:
#Seals is assumed to be average of all values of included groups
#Mackerel ad is actually horse mackerel + Fat fish (pelagics)
#Anchovy is just Fat fish (pelagics)
# Row Labels	Average of Own-Price elasticity
# Anchovy	-1.6
# Cod	-2.752857143
# Mackerel ad	-1.44
# Seals	#DIV/0!
# Shrimp	-0.734444444
# Whiting	-12.37
# Grand Total	-2.623809524




elasticities <- tribble(~group, ~elasticity,
                        "Seals", 2.623809524,
                        "Cod", 2.752857143,
                        "Whiting", 12.37,
                        "Mackerel ad", 1.44,
                        "Anchovy", 1.6,
                        "Shrimp", 0.734444444)

price_clean <- price_clean %>% 
  left_join(elasticities)

price_output <- price_clean %>% 
  mutate(Fleet = gsub(Fleet, pattern=" \\(EUR\\/biomass\\)", replacement="")) %>% 
  mutate(fg = group) %>% 
  mutate(group = paste(Fleet, group, sep=" | "))
write_csv(price_output, "../DataOutputs/PriceData_tidy.csv")


##Distance from port##
#Currently only works for 1 port but could be re-done to account for multiple and take the minimum distance (assuming all fishers can exit from and return to any port)
ports <- read_csv("./InputData/Anchovy Bay-Ports (All fleets)_OnePort.csv")
colnames(ports)[1] <- "row_num"

ports_tidy <- ports %>% 
  gather(key="col_num", value="port", -row_num) %>% 
  mutate(col_num = as.numeric(col_num))
ports_tidy$port <- ports_tidy$port^2
port_col <- ports_tidy$col_num[which(ports_tidy$port==1)]
port_row <- ports_tidy$row_num[which(ports_tidy$port==1)]
ports_tidy$distance <- 0
ports_tidy$distance <- sqrt((ports_tidy$row_num - port_row)^2 + (ports_tidy$col_num - port_col)^2)
write_csv(ports_tidy, "../DataOutputs/PortData_tidy.csv")

