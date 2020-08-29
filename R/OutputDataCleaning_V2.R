

library(tidyverse)
library(RColorBrewer)
setwd("./ModelOutputs/")

####Clean Raw Data to Tidy Format####
models <- list.files()
folders <- models
#Focus on average outputs first
files <- c("Ecospace_Annual_Average_Biomass.csv", "Ecospace_Annual_Average_Catch.csv") #Define files we need to make graphs
group_data= data.frame()


# folder <- folders[1]
# file <- files[1]

for (folder in folders) {
  for (file in files){
    df_name <- paste(folder, file, sep="/")
    df_name <- gsub(df_name, pattern=".csv", replacement="")
    df_name <- gsub(df_name, pattern="Ecospace_", replacement="")
    df_name <- gsub(df_name, pattern="SardineBank_", replacement="")
    df_type <- gsub(df_name, pattern=".*/Annual_", replacement="")
    df_RCP <- gsub(df_name, pattern="/.*", replacement="")
    df_RCP <- gsub(df_name, pattern="Basemodel_", replacement="")
    df_RCP <- gsub(df_RCP, pattern="_.*", replacement="")
    df_Scenario <- gsub(df_name, pattern="/.*", replacement="")
    df_Scenario <- gsub(df_Scenario, pattern=paste(".*_",df_RCP, "_",  sep=""), replacement="")
    data <- read_csv(print(paste(".", folder, file, sep="/")), skip=21)
    data <- data %>% gather(group, value, -Year)
    data$type <- df_type
    data$RCP <- df_RCP
    data$scenario <- df_Scenario
    assign(df_name, data)
    group_data <- rbind(group_data, assign(df_name, data))
  }
}

write_csv(group_data, "../DataOutputs/AnnualAverageData_tidy.csv")
group_data <- read_csv("../DataOutputs/AnnualAverageData_tidy.csv")
####Average Biomass and Catch under different scenarios ####



library(tidyverse)
library(RColorBrewer)
setwd("./ModelOutputs/")

####Clean Raw Data to Tidy Format####
models <- list.files()
folders <- models
#folders <- folders[1]

files <- c("Ecospace_Average_Biomass.csv", "Ecospace_Average_Catch.csv") #Define files we need to make graphs

group_data= data.frame()
headers = as.character(seq(1,20,1))
years <- seq(0,100, 1)


# folder <- folders[1]
# file <- files[1]
# year <- years[1]
start <- Sys.time()
for (folder in folders) {
  files <- list.files(paste("./", folder, "/csv/", sep=""))
  
  for (file in files){
    df_temp <- data_frame()
    df_name <- paste(folder, file, sep="/")
    df_name <- gsub(df_name, pattern=".csv", replacement="")
    # df_name <- gsub(df_name, pattern="Ecospace_", replacement="")
    df_name <- gsub(df_name, pattern="SardineBank_", replacement="")
    df_type <- gsub(df_name, pattern=".*/EcospaceMap", replacement="")
    df_type <- gsub(df_type, pattern="-.*", replacement="")
    df_RCP <- gsub(df_name, pattern="/.*", replacement="")
    df_RCP <- gsub(df_name, pattern="Basemodel_", replacement="")
    df_RCP <- gsub(df_RCP, pattern="_.*", replacement="")
    df_Scenario <- gsub(df_name, pattern="/.*", replacement="")
    df_Scenario <- gsub(df_Scenario, pattern=paste(".*_",df_RCP, "_",  sep=""), replacement="")
    df_fg <- df_name
    df_fg <- gsub(df_fg, pattern = ".*EcospaceMapBiomass-", replacement = "")
    df_fg <- gsub(df_fg, pattern = ".*EcospaceMapCatch-", replacement = "")
    df_fg <- gsub(df_fg, pattern = ".*EcospaceMapEffort-", replacement = "")
    for (year in years) {
      skip_rows <- 21 + (year*23)
      end_row <- 20
      data <- read_csv(print(paste(".", folder, "csv", file, sep="/")), skip=skip_rows, n_max=end_row, skip_empty_rows = FALSE, col_names=headers)
      data$year <- year
      data$row_num <- seq(1,20, 1)
      data_melt <- gather(data, "col_num", "value", -year, -row_num)
      df_temp <- bind_rows(df_temp, data_melt)
    }
    
    data <- df_temp
    data$type <- df_type
    data$RCP <- df_RCP
    data$scenario <- df_Scenario
    data$fg <- df_fg
    #assign(df_name, data)
    group_data <- bind_rows(group_data, data)
  }
}
group_data <- group_data %>% 
  mutate(col_num=as.numeric(col_num))

write_csv(group_data, "../DataOutputs/SpatialData_tidy.csv")

group_data <- read_csv("../DataOutputs/SpatialData_tidy.csv")

end <- Sys.time() #This above loop takes about 7 minutes to run. 
end-start

####Read in MPA boundary files:####
#Adjust code to account for different MPA (%) scenarios 

scenarios <- c("MPA Scenario 15 Percent", "MPA Scenario 10 Percent", "MPA Scenario 5 Percent")
scenario <- scenarios[1]
file <- input_files[1]

for (scenario in scenarios){
  mpas <- tibble()
  input_files <- list.files(paste("../InputData/", scenario,"/", sep=""))
  input_files <- input_files[grepl(input_files, pattern="Anchovy Bay-MPA")]
  for (file in input_files){
    design_name <- gsub(file, pattern="Anchovy Bay-MPA - ", replacement="")
    design_name <- gsub(design_name, pattern=".csv", replacement="")
    mpa <- read_csv(paste("../InputData/", scenario, "/", file, sep=""))
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
  mpas$design <- gsub(mpas$design, pattern="-4", replacement="")
  
  temporal_mpa <- read_csv(paste("../InputData/", scenario, "/AnchovyBay-MPA - Temporal - Square Shifting.csv" , sep=""))
  # temporal_mpa <- read_csv("../InputData/AnchovyBay-MPA - Temporal - Square Shifting.csv")
  temporal_mpa$mpa <- paste("MPA_Square_Shifting - ", temporal_mpa$mpa, sep="")
  temporal_mpa$Date <- gsub(temporal_mpa$Date, pattern="-01", replacement="")
  temporal_mpa$Date <- gsub(temporal_mpa$Date, pattern="", replacement="")
  temporal_mpa$year <- substr(temporal_mpa$Date, 3,4)
  temporal_mpa$scenario <- scenario
  temporal_mpa <- temporal_mpa %>% 
    select(mpa, scenario, year, m1) %>% 
    rename(design=mpa) %>% 
    mutate(year = as.numeric(year))
  
  temp <- left_join(mpas, temporal_mpa)
  temp$m1[grepl(temp$design, pattern="Static")] <- 0
  temp <- temp %>% fill(m1)
  temp$mpa <- as.numeric(temp$mpa)
  temp$mpa[which(grepl(temp$design, pattern="MPA_Square_Shifting") & temp$mpa==1 & temp$m1==1)] <- 1 
  temp$mpa[which(grepl(temp$design, pattern="MPA_Square_Shifting") & temp$mpa==1 & temp$m1==0)] <- 0 
  temp$mpa[which(grepl(temp$design, pattern="MPA_Square_Shifting") & temp$mpa==2 & temp$m1==1)] <- 2 
  temp$mpa[which(grepl(temp$design, pattern="MPA_Square_Shifting") & temp$mpa==2 & temp$m1==0)] <- 0 
  
  temp <- temp %>% select(-m1)
  
  mpas <- temp
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
setwd("./ModelOutputs/")
prices <- read_csv("../InputData/Sardine Bank-Off-vessel price.csv")
price_clean <- prices %>% 
  select(-X1) %>% 
  gather(key="Fleet", value="Price", -`Group name`) %>% 
  filter(is.na(Price)==F) %>% 
  rename(group = `Group name`)

#Assumed elasticites based on average of values covered in ASche et al. 2005 study:
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
ports <- read_csv("../InputData/Anchovy Bay-Ports (All fleets)_OnePort.csv")
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




#### East China Sea Code####

?as.matrix.dist
####Adjust for Price Elasticity Manually (October 15, 2018) ####
#Updated with upper and lower bounds equivalent to the aggregate fishery in the regions 95% coefficient of variation value spread
rm(list=ls())
library(tidyverse)

fleet_data <- read.csv("FleetData_tidy.csv")
ecs_price_elast <- read.csv("../PriceElasticities_ECS_SCS.csv") %>%
  filter(Ecosystem=="ECS") %>%
  select(-c(Ecosystem,Elasticity.Applied)) %>%
  droplevels() %>%
  rename(group_name=FunctionalGroup) %>%
  mutate(group_name=as.character(group_name))

ecs_info <- read.csv("ecs_fc_fleet_forR.csv")
ecs_info <- drop_na(ecs_info)

ecs_groups <- as.character(ecs_info$FunctionalGroup)
ecs_fleets <- as.character(unique(ecs_info$Fleet))
ecs_fleets <- ecs_fleets[0:(length(ecs_fleets)-1)]
ecs_fleet_df <- data.frame(fleet=seq(1,12,1), fleet_name=ecs_fleets)
ecs_group_df <- data.frame(group=as.character(seq(1,38,1)), group_name=ecs_groups)
ecs_group_df <- left_join(ecs_group_df, ecs_price_elast)
ecs_group_df <- ecs_group_df %>% 
  mutate(group=as.numeric(as.character(group)))
ecs_group_df$Elasticity[is.na(ecs_group_df$Elasticity)] <- 0
ecs_group_df$Elasticity[which(ecs_group_df$group_name=="Hairtails (A)")] <- 0.58

catch_2014 <- fleet_data %>%
  filter(year==2014) %>% 
  filter(RCP=="RCP2.6") %>%
  filter(type=="catch") %>%
  filter(scenario=="StatusQuo") %>%
  group_by(group) %>%
  summarize(catch_2014=sum(value)) %>%
  ungroup()

#How can I modify this to be a non-linear relationship? Log both catch amounts before division? 
#Set upper and lower bounds based on discussion with Ocean Asia team. 

lb <- -35 
ub <- +35

fleet_data_catch <- fleet_data %>% filter(type=="catch") %>%
  group_by(year, scenario, RCP, group) %>%
  summarize(value=sum(value)) %>%
  ungroup() %>%
  left_join(catch_2014) %>%
  left_join(ecs_group_df) %>%
  mutate(percent_change = 100* ((value/catch_2014)-1) )  


#Put in simple upper and lower bounds on the estimates 
fleet_data_catch$percent_change[which(fleet_data_catch$percent_change <lb)] <- lb
fleet_data_catch$percent_change[which(fleet_data_catch$percent_change >ub)] <- ub

#Most rows originally fell outside these imposed ranges. I didn't find this unlikely:
nrow(fleet_data_catch[fleet_data_catch$percent_change ==ub,])
nrow(fleet_data_catch[fleet_data_catch$percent_change ==lb,])
nrow(fleet_data_catch[fleet_data_catch$percent_change >lb & fleet_data_catch$percent_change <ub,])

#Calculate how price elasticity effects change in value: 
fleet_data_catch <- fleet_data_catch %>%
  mutate(value_change = percent_change*-Elasticity) %>%
  select(-c(value))


fleet_data_value <- fleet_data %>% filter(type=="value") %>%
  left_join(fleet_data_catch)

fleet_data_value$new_value <- 0
fleet_data_value$new_value <- fleet_data_value$value * ((100+fleet_data_value$value_change)/100)
fleet_data_value$new_value[is.nan(fleet_data_value$new_value)==T] <- 0
fleet_data_value <- fleet_data_value %>% 
  select(-c(catch_2014, group_name, Elasticity, percent_change, value_change, value)) %>%
  rename(value=new_value)
fleet_data_adjusted <- rbind(fleet_data %>% filter(type %in% c("catch","biomass")), fleet_data_value)

write.csv(fleet_data_adjusted, "FleetData_tidy_ElasticityAdjusted_October152018.csv", row.names=F)

group_data <- read.csv("GroupData_tidy.csv")
group_data$group <- as.character(group_data$group)
group_data$group[which(group_data$group=="Haritails (A)")] <-"Hairtails (A)"


group_data <- group_data %>% 
  rename(group_name=group) %>%
  mutate(group_name= as.character(group_name)) %>%
  left_join(ecs_group_df)

catch_2014 <- group_data %>%
  filter(year==2014) %>% 
  filter(RCP=="RCP2.6") %>%
  filter(type=="catch") %>%
  filter(scenario=="StatusQuo") %>%
  group_by(group) %>%
  summarize(catch_2014=sum(value)) %>%
  ungroup()

lb <- -35 
ub <- +35

group_data_catch <- group_data %>% filter(type=="catch") %>%
  group_by(year, scenario, RCP, group) %>%
  summarize(value=sum(value)) %>%
  ungroup() %>%
  left_join(catch_2014) %>%
  left_join(ecs_group_df) %>%
  mutate(percent_change = 100* ((value/catch_2014)-1) ) #Using arc elasticity for calculation

#Put in simple upper and lower bounds on the estimates 
group_data_catch$percent_change[which(group_data_catch$percent_change <lb)] <- lb
group_data_catch$percent_change[which(group_data_catch$percent_change >ub)] <- ub

#Most rows originally fell outside these imposed ranges. I didn't find this unlikely:
nrow(group_data_catch[group_data_catch$percent_change ==ub,])
nrow(group_data_catch[group_data_catch$percent_change ==lb,])
nrow(group_data_catch[group_data_catch$percent_change >lb & group_data_catch$percent_change <ub,])

#Calculate how price elasticity effects change in value: 
group_data_catch <- group_data_catch %>%
  mutate(value_change = percent_change*-Elasticity) %>%
  select(-c(value))


group_data_value <- group_data %>% filter(type=="value") %>%
  left_join(group_data_catch)
group_data_value$new_value <- 0
group_data_value$new_value <- group_data_value$value * ((100+group_data_value$value_change)/100)
group_data_value$new_value[is.nan(group_data_value$new_value)==T] <- 0
group_data_value <- group_data_value %>% 
  select(-group) %>%
  rename(group=group_name) %>% 
  select(-c(catch_2014, Elasticity, percent_change, value_change, value)) %>%
  rename(value=new_value)
group_data <- group_data %>%
  select(-group) %>%
  rename(group=group_name) %>% 
  select(-Elasticity)
group_data_adjusted <- rbind(group_data %>% filter(type %in% c("catch","biomass")), group_data_value)
test <- group_data_adjusted %>% filter(is.na(value))
write.csv(group_data_adjusted, "GroupData_tidy_ElasticityAdjusted_October152018.csv", row.names=F)

rm(list=ls())


####Read in cleaned data ####
rm(list=ls())
library(tidyverse)
library(ggplot2)
setwd("C:/Users/t.cashion/Google Drive/Desktop/Current_Internal/East China Sea/ECS Model Development")
setwd("~/Desktop/Google Drive/Desktop/Current_Internal/East China Sea/ECS Model Development")
group_data <- read.csv("GroupData_tidy_ElasticityAdjusted_October152018.csv")
fleet_data <- read.csv("FleetData_tidy_ElasticityAdjusted_October152018.csv")

####Sample Tables: ####


colnames(group_data)
unique(group_data$type)

summary_table1 <- group_data %>% 
  group_by(RCP, scenario, year, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2000 & year <=2009)

summary_table2 <- group_data %>% 
  group_by(RCP, scenario, year, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2090 & year <2099)


####Absolute Value Figures - 12 bars per plot ####
summary_table1 <- group_data %>% 
  group_by(RCP, scenario, year, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2000 & year <=2009) %>% 
  group_by(RCP, scenario, type) %>% 
  summarize(average_2000s = mean(value_aggregate),
            stdev_2000s = sd(value_aggregate)) %>%
  mutate(se_2000s = stdev_2000s/sqrt(10))

summary_table2 <- group_data %>% 
  group_by(RCP, scenario, year, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2090 & year <2099) %>% 
  group_by(RCP, scenario, type) %>% 
  summarize(average_2090s = mean(value_aggregate),
            stdev_2090s = sd(value_aggregate)) %>%
  mutate(se_2090s = stdev_2090s/sqrt(9))


melted1<- summary_table1 %>% gather(key= variable, value=value, -RCP, -type, -scenario)
melted2<- summary_table2 %>% gather(key= variable, value=value, -RCP, -type, -scenario)
melted <- rbind(melted1, melted2)
melted$year <- gsub(melted$variable, pattern=".*_", replacement="")
melted$variable <- gsub(melted$variable, pattern="_.*", replacement="")
melted$aggregate <- paste(melted$RCP, melted$scenario, melted$year, sep="/")
melted$label <- paste(melted$scenario, melted$year, sep=" ")

spread <- melted %>%
  filter(variable != "se")  %>%
  spread(key=variable, value=value)

summary_table_biomass <- spread %>% filter(type=="biomass")
summary_table_catch <- spread %>% filter(type=="catch")
summary_table_value <- spread %>% filter(type=="value")


tab1 <- summary_table_biomass %>% 
  ungroup() %>%
  select(-c(aggregate, label, stdev)) %>%
  spread(year, average) %>%
  mutate(diff=100*((`2090s`-`2000s`)/`2000s`))
tab2 <- summary_table_catch %>% 
  ungroup() %>%
  select(-c(aggregate, label, stdev)) %>%
  spread(year, average) %>%
  mutate(diff=100*((`2090s`-`2000s`)/`2000s`))
tab3 <- summary_table_value %>% 
  ungroup() %>%
  select(-c(aggregate, label, stdev)) %>%
  spread(year, average) %>%
  mutate(diff=100*((`2090s`-`2000s`)/`2000s`))


colnames(tab1)
#scenarios <- c()#Selected list of scenarios of interest
scenarios <- as.character(unique(melted$scenario))

output_table_biomass <- summary_table_biomass %>% 
  filter(year=="2090s") %>%
  mutate(value = paste(round(average, 1), " (", round(stdev,2), ")", sep="")) %>%
  select(-c( year,aggregate, label, average, stdev)) %>%
  spread(scenario, value=value) %>% 
  select(RCP, type, scenarios) %>%
  {.}

output_table_catch <- summary_table_catch %>% 
  filter(year=="2090s") %>%
  mutate(value = paste(round(average, 1), " (", round(stdev,2), ")", sep="")) %>%
  select(-c( year,aggregate, label, average, stdev)) %>%
  spread(scenario, value=value) %>% 
  select(RCP, type, scenarios) %>%
  {.}

output_table_value <- summary_table_value %>% 
  filter(year=="2090s") %>%
  mutate(value = paste(round(average, 1), " (", round(stdev,2), ")", sep="")) %>%
  select(-c(year,aggregate, label, average, stdev)) %>%
  spread(scenario, value=value) %>% 
  select(RCP,type, scenarios) %>% 
  {.}

df <- rbind(output_table_biomass, output_table_catch, output_table_value)
df <- df %>% ungroup() %>% select(RCP, type, StatusQuo, Increase50, Decrease2, Decrease25, Decrease50, NoFeed50)
write.csv(df, "Output Tables for ECS Report.csv",row.names=F)

summary_table1 <- group_data %>%  
  filter(RCP=="RCP8.5") %>%
  filter(scenario=="StatusQuo") %>%
  group_by(group, year, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2000 & year <=2009) %>% 
  group_by(group, type) %>% 
  summarize(average_2000s = mean(value_aggregate),
            stdev_2000s = sd(value_aggregate)) %>%
  mutate(se_2000s = stdev_2000s/sqrt(10))

summary_table2 <- group_data %>% 
  filter(RCP=="RCP8.5") %>%
  filter(scenario=="StatusQuo") %>%
  group_by(group, year, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2090 & year <=2099) %>% 
  group_by(group, type) %>% 
  summarize(average_2090s = mean(value_aggregate),
            stdev_2090s = sd(value_aggregate)) %>%
  mutate(se_2090s = stdev_2090s/sqrt(9))

summary_table <- left_join(summary_table1, summary_table2)
summary_table <- summary_table %>% 
  filter(type=="biomass") %>%
  mutate(DecreasePercent = (average_2000s-average_2090s)/average_2000s)
declined_groups <- summary_table %>%
  filter(DecreasePercent >=0.99) %>%
  select(group) %>%
  rename(ECS=group)
write.csv(declined_groups, "DeclinedSpeciesList_99PercentBiomass_RCP8.5_StatusQuo.csv", row.names=F)

summary_table <- left_join(summary_table1, summary_table2) %>% ungroup()

n = 10

top_n_catch1 <- summary_table %>% 
  filter(type=="catch") %>% 
  select(group, average_2000s) %>%
  top_n(n=n) %>%
  arrange(desc(average_2000s))
others_n_catch <- summary_table %>% 
  filter(type=="catch") %>% 
  select(group, average_2000s) %>%
  top_n(n=(n-nrow(summary_table)/3)) %>%
  summarize(average_2000s=sum(average_2000s)) %>%
  mutate(group="Others") %>%
  select(group, average_2000s)
top_n_catch1 <- rbind(top_n_catch1, others_n_catch)
write.csv(top_n_catch1, "Top_N_Catch_2000s.csv", row.names=F)

top_n_catch2 <- summary_table %>% 
  filter(type=="catch") %>% 
  select(group, average_2090s) %>%
  top_n(n=n) %>%
  arrange(desc(average_2090s))
others_n_catch <- summary_table %>% 
  filter(type=="catch") %>% 
  select(group, average_2090s) %>%
  top_n(n=(n-nrow(summary_table)/3)) %>%
  summarize(average_2090s=sum(average_2090s)) %>%
  mutate(group="Others") %>%
  select(group, average_2090s)
top_n_catch2 <- rbind(top_n_catch2, others_n_catch)
write.csv(top_n_catch2, "Top_N_Catch_2090s.csv", row.names=F)



top_n_value1 <- summary_table %>% 
  filter(type=="value") %>% 
  select(group, average_2000s) %>%
  top_n(n=n) %>%
  arrange(desc(average_2000s))
others_n_value <- summary_table %>% 
  filter(type=="value") %>% 
  select(group, average_2000s) %>%
  top_n(n=(n-nrow(summary_table)/3)) %>%
  summarize(average_2000s=sum(average_2000s)) %>%
  mutate(group="Others") %>%
  select(group, average_2000s)
top_n_value1 <- rbind(top_n_value1, others_n_value)
write.csv(top_n_value1, "Top_N_Value_2000s.csv", row.names=F)


top_n_value2 <- summary_table %>% 
  filter(type=="value") %>% 
  select(group, average_2090s) %>%
  top_n(n=n) %>%
  arrange(desc(average_2090s))
others_n_value <- summary_table %>% 
  filter(type=="value") %>% 
  select(group, average_2090s) %>%
  top_n(n=(n-nrow(summary_table)/3)) %>%
  summarize(average_2090s=sum(average_2090s)) %>%
  mutate(group="Others") %>%
  select(group, average_2090s)
top_n_value2 <- rbind(top_n_value2, others_n_value)
write.csv(top_n_value2, "Top_N_Value_2090s.csv", row.names=F)


colours = brewer.pal(6, "RdBu")
colours = c("#EF8A62", "#B2182B",  "#FDDBC7", "#D1E5F0", "#67A9CF", "#2166AC") #Re-ordered so colours make more sense 

summary_table_biomass <- summary_table_biomass %>%
  ungroup() %>%
  mutate(scenario = as.character(scenario)) %>%
  mutate(scenario = factor(scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))) %>%
  arrange(RCP, scenario)

summary_table_catch <- summary_table_catch %>%
  ungroup() %>%
  mutate(scenario = as.character(scenario)) %>%
  mutate(scenario = factor(scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))) %>%
  arrange(RCP, scenario)

summary_table_value <- summary_table_value %>%
  ungroup() %>%
  mutate(scenario = as.character(scenario)) %>%
  mutate(scenario = factor(scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))) %>%
  arrange(RCP, scenario)

# summary_table_biomass$scenario <- factor(summary_table_biomass$scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))
# summary_table_biomass$scenario <- factor(summary_table_biomass$scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))





#RCPs:
rcps <- c("RCP2.6", "RCP8.5")
rcp_num <- length(rcps)
scenario_num <- 6
start_num <- 1 
sep <- 1

x.seq <- c()
breaks <- c()
for (i in 1:rcp_num) {
  y <- i-1
  x <- seq(start_num+(scenario_num*y)+y, y+scenario_num*i)
  break_x <- mean(x)
  breaks <- append(breaks, break_x)
  x.seq <- append(x.seq, x)
}

data=transform(summary_table_biomass[which(summary_table_biomass$year=="2090s"),], x=x.seq)
#Biomass Plot
p <- ggplot(data=transform(summary_table_biomass[which(summary_table_biomass$year=="2090s"),], x=x.seq), aes(x=x, average, fill=scenario)) +
  geom_col() +
  theme_classic() +
  ylab(expression(paste("Biomass (tonnes/km"^"2",")", sep=""))) + 
  scale_y_continuous(breaks=seq(0,150,30), limits=c(0,150), expand = c(0, 0)) +
  scale_x_discrete(breaks=NULL) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=12, hjust = 0.5),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        axis.title = element_text(size=12)) +
  xlab("") + 
  labs(fill="Fishing Scenario") +
  scale_fill_manual(values=colours) +
  geom_hline(yintercept = summary_table_biomass$average[which(summary_table_biomass$aggregate=="RCP2.6/StatusQuo/2000s")], linetype="dashed", colour="black") +
  geom_errorbar(aes(x=x,ymin=average-stdev, ymax=average+stdev), width=.2,
                position=position_dodge(.9)) + 
  scale_x_continuous(labels=rcps, breaks=breaks) +
  NULL
print(p)
ggsave("Absolute Change in Biomass from 2000s to 2090s ECS_AxisSpaced.png", width=12, height = 9, units="cm", dpi=300)

p <- ggplot(data=transform(summary_table_catch[which(summary_table_catch$year=="2090s"),], x=x.seq), aes(x=x, average, fill=scenario)) +
  geom_col() +
  theme_classic() +
  ylab(expression(paste("Catch (tonnes/km"^"2",")", sep=""))) + 
  scale_y_continuous(breaks=seq(0,20,5), limits=c(0,20), expand = c(0, 0)) +
  scale_x_discrete(breaks=NULL) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=12, hjust = 0.5),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        axis.title = element_text(size=12)) +
  xlab("") + 
  labs(fill="Fishing Scenario") +
  scale_fill_manual(values=colours) +
  geom_hline(yintercept = summary_table_catch$average[which(summary_table_catch$aggregate=="RCP2.6/StatusQuo/2000s")], linetype="dashed", colour="black") +
  geom_errorbar(aes(x=x,ymin=average-stdev, ymax=average+stdev), width=.2,
                position=position_dodge(.9)) + 
  scale_x_continuous(labels=rcps, breaks=breaks) +
  NULL
print(p)

ggsave("Absolute Change in Catch from 2000s to 2090s ECS_AxisSpaced.png", width=12, height = 9, units="cm", dpi=300)

p <- ggplot(data=transform(summary_table_value[which(summary_table_value$year=="2090s"),], x=x.seq), aes(x=x, average, fill=scenario)) +
  geom_col() +
  theme_classic() +
  ylab(expression(paste("Value ($/km"^"2",")", sep=""))) + 
  scale_y_continuous(breaks=seq(0,25000,5000), limits=c(0,25000), expand = c(0, 0)) +
  scale_x_discrete(labels=c("RCP2.6", "RCP8.5"), breaks=c("RCP2.6/Increase50/2090s","RCP8.5/Increase50/2090s")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=12, hjust = 0.55),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        axis.title = element_text(size=12)) +
  xlab("") + 
  labs(fill="Fishing Scenario") +
  scale_fill_manual(values=colours) +
  geom_hline(yintercept = summary_table_value$average[which(summary_table_value$aggregate=="RCP2.6/StatusQuo/2000s")], linetype="dashed", colour="black") +
  geom_errorbar(aes(x=x,ymin=average-stdev, ymax=average+stdev), width=.2,
                position=position_dodge(.9)) + 
  scale_x_continuous(labels=c("RCP2.6", "RCP8.5"), breaks=breaks) +
  NULL
print(p)
ggsave("Absolute Change in Value from 2000s to 2090s ECS_AxisSpaced.png", width=12, height = 9, units="cm", dpi=300)






####Percent Change Figures - 12 bars per plot ####
summary_table1 <- group_data %>% 
  group_by(RCP, scenario, year, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2000 & year <=2009) %>% 
  group_by(RCP, scenario, type) %>% 
  summarize(average_2000s = mean(value_aggregate),
            stdev_2000s = sd(value_aggregate)) %>%
  mutate(se_2000s = stdev_2000s/sqrt(10))

summary_table2 <- group_data %>% 
  group_by(RCP, scenario, year, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2090 & year <2099) %>% 
  group_by(RCP, scenario, type) %>% 
  summarize(average_2090s = mean(value_aggregate),
            stdev_2090s = sd(value_aggregate)) %>%
  mutate(se_2090s = stdev_2090s/sqrt(9))


summary_table <- left_join(summary_table1, summary_table2)
summary_table <- summary_table %>%
  mutate(diff= ((average_2090s/average_2000s)-1)*100,
         error = sqrt((se_2000s^2)+(se_2090s^2)))

summary_table$aggregate <- paste(summary_table$RCP, summary_table$scenario, sep="/")

summary_table_biomass <- summary_table %>% filter(type=="biomass")
summary_table_catch <- summary_table %>% filter(type=="catch")
summary_table_value <- summary_table %>% filter(type=="value")


p <- ggplot(data=summary_table_biomass, aes(aggregate, diff, fill=RCP)) +
  geom_col() +
  theme_classic() +
  ylab("Percent Change") + 
  #ggtitle("Biomass") +
  scale_y_continuous(breaks=seq(0,60,10), limits=c(0,60), expand = c(0,0)) +
  scale_x_discrete(labels=summary_table_biomass$scenario) +
  xlab("Scenario") + 
  scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_errorbar(aes(x=aggregate,ymin=diff-error, ymax=diff+error), width=.2,
                position=position_dodge(.9)) + 
  NULL
print(p)
ggsave("Average Change in Biomass from 2000s to 2090s ECS.png", width=12, height = 9, units="cm", dpi=300)

p <- ggplot(data=summary_table_catch, aes(aggregate, diff, fill=RCP)) +
  geom_col() +
  theme_classic() +
  ylab("Percent Change") + 
  #ggtitle("Catch") +
  scale_y_continuous(breaks=seq(-20,280,20), limits=c(-20,280)) +
  scale_x_discrete(labels=summary_table_biomass$scenario) +
  xlab("Scenario") + 
  scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_errorbar(aes(x=aggregate,ymin=diff-error, ymax=diff+error), width=.2,
                position=position_dodge(.9)) + 
  NULL
print(p)
ggsave("Average Change in Catch from 2000s to 2090s ECS.png", width=12, height = 9, units="cm", dpi=300)

p <- ggplot(data=summary_table_value, aes(aggregate, diff, fill=RCP)) +
  geom_col() +
  theme_classic() +
  ylab("Percent Change") + 
  #ggtitle("Value") +
  scale_y_continuous(breaks=seq(-800,1200,200), limits=c(-800,1200)) +
  scale_x_discrete(labels=summary_table_biomass$scenario) +
  xlab("Scenario") + 
  scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_errorbar(aes(x=aggregate,ymin=diff-error, ymax=diff+error), width=.2,
                position=position_dodge(.9)) + 
  NULL
print(p)
ggsave("Average Change in Value from 2000s to 2090s ECS.png", width=12, height = 9, units="cm", dpi=300)





#### Attempt at doing it by species group ####

summary_table1 <- group_data %>% 
  group_by(RCP, scenario, year, group, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2000 & year <=2009) %>% 
  group_by(RCP, scenario, group, type) %>% 
  summarize(average_2000s = mean(value_aggregate))


summary_table2 <- group_data %>% 
  group_by(RCP, scenario, year, group, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2090 & year <2099) %>% 
  group_by(RCP, scenario, group, type) %>% 
  summarize(average_2090s = mean(value_aggregate)) 

summary_table <- left_join(summary_table1, summary_table2)
summary_table <- summary_table %>%
  mutate(diff= ((average_2090s/average_2000s)-1)*100) #%>%


summary_table$aggregate <- paste(summary_table$RCP, summary_table$scenario, summary_table$group, sep="/")

summary_table_biomass <- summary_table %>% filter(type=="biomass") %>% filter(RCP=="RCP8.5") #%>% filter(diff<100)
summary_table_biomass <- summary_table_biomass[order(summary_table_biomass$scenario, summary_table_biomass$diff),]

summary_table_biomass <- summary_table %>% filter(type=="biomass")
summary_table_catch <- summary_table %>% filter(type=="catch")
summary_table_value <- summary_table %>% filter(type=="value")

write.csv(summary_table, "FunctionalGroupResponsesAllScenarios_ECS.csv", row.names=F)




p <- ggplot(data=transform(summary_table_biomass, x=x.seq), aes(x=x, average_2090s, fill=scenario)) +
  geom_col() +
  theme_classic() +
  ylab(expression(paste("Biomass (tonnes/km"^"2",")", sep=""))) + 
  scale_y_continuous(breaks=seq(0,140,30), limits=c(0,140), expand = c(0, 0)) +
  scale_x_discrete(breaks=NULL) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=12, hjust = 0.5),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        axis.title = element_text(size=12)) +
  xlab("") + 
  labs(fill="Fishing Scenario") +
  scale_fill_manual(values=colours) +
  geom_hline(yintercept = summary_table_biomass$average_2000s[which(summary_table_biomass$aggregate=="RCP2.6/StatusQuo/2000s")], linetype="dashed", colour="black") +
  # geom_errorbar(aes(x=x,ymin=average_2090s-stdev, ymax=average_2090s+stdev), width=.2,
  #               position=position_dodge(.9)) + 
  scale_x_continuous(labels=rcps, breaks=breaks) +
  NULL
print(p)
ggsave("Absolute Change in Biomass from 2000s to 2090s ECS_AxisSpaced.png", width=12, height = 9, units="cm", dpi=300)



####Decrease50RCP2.6 compared to StatusQuoRCP8.5####
summary_table1 <- group_data %>% 
  group_by(RCP, scenario, year, group, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2000 & year <=2009) %>% 
  group_by(RCP, scenario, group, type) %>% 
  summarize(average_2000s = mean(value_aggregate))


summary_table2 <- group_data %>% 
  group_by(RCP, scenario, year, group, type) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2090 & year <2099) %>% 
  group_by(RCP, scenario, group, type) %>% 
  summarize(average_2090s = mean(value_aggregate)) 

summary_table <- left_join(summary_table1, summary_table2)
summary_table <- summary_table %>%
  mutate(diff= ((average_2090s/average_2000s)-1)*100) #%>%


summary_table$aggregate <- paste(summary_table$RCP, summary_table$scenario, summary_table$group, sep="/")

summary_table_biomass <- summary_table %>% filter(type=="biomass") %>% filter(RCP=="RCP8.5") #%>% filter(diff<100)
summary_table_biomass <- summary_table_biomass[order(summary_table_biomass$scenario, summary_table_biomass$diff),]

summary_table_biomass <- summary_table %>% filter(type=="biomass")
summary_table_catch <- summary_table %>% filter(type=="catch")
summary_table_value <- summary_table %>% filter(type=="value")


comparison_table <- summary_table %>% 
  filter((RCP=="RCP2.6" & scenario=="Decrease50") | (RCP=="RCP8.5" & scenario=="StatusQuo")) %>%
  select(-c(average_2000s, diff)) #%>%
#spread(average_2090s, -type)

comparison_table <- comparison_table %>% select(-c(aggregate)) 
comparison_table2 <- comparison_table %>%
  unite(temp, RCP, scenario, type) %>%
  spread(temp, average_2090s)
colnames(comparison_table2)

comparison_table2 <- comparison_table2 %>% 
  mutate("Biomass (% Change)" = ((RCP2.6_Decrease50_biomass/RCP8.5_StatusQuo_biomass)-1)*100,
         "Catch (% Change)" = ((RCP2.6_Decrease50_catch/RCP8.5_StatusQuo_catch)-1)*100,
         "Value (% Change)" = ((RCP2.6_Decrease50_value/RCP8.5_StatusQuo_value)-1)*100)
write.csv(comparison_table2, "Projected Changes in ECS under RCP2.6_Decrease50 relative to RCP8.5_StatusQuo.csv", row.names=F)

table1 <- read.csv("Projected Changes in ECS under RCP2.6_Decrease50 relative to RCP8.5_StatusQuo.csv")
important_species <- c("Crabs", "Threadfin bream", "Snappers", "Pelagic sharks and rays",
                       "Large croakers", "Small croakers", "Hairtails", "Large pelagic fishes",
                       "Groupers", "Small pelagic fishes", "Jellyfish", "Cephalopods")
table1_filtered <- table1 %>% filter(group %in% important_species)
table1_filtered <- table1_filtered[,-c(2:7)]
write.csv(table1_filtered, "Projected Changes in ECS under RCP2.6_Decrease50 relative to RCP8.5_StatusQuo_LimitedResults.csv", row.names=F)


p <- ggplot(data=summary_table_biomass, aes(aggregate, diff, fill=scenario)) +
  geom_col() +
  theme_classic() +
  ylab("Average change from 2000s to 2090s (%)") + 
  ggtitle("Biomass") 

#+
#scale_y_continuous(breaks=seq(-20,60,10))
print(p)

p <- ggplot(data=summary_table_catch, aes(aggregate, diff, fill=RCP)) +
  geom_col() +
  theme_classic() +
  ylab("Average change from 2000s to 2090s (%)") + 
  ggtitle("Catch") +
  # scale_y_continuous(breaks=seq(-20,320,20)) +
  NULL
print(p)

p <- ggplot(data=summary_table_value, aes(aggregate, diff, fill=RCP)) +
  geom_col() +
  theme_classic() +
  ylab("Average change from 2000s to 2090s (%)") + 
  ggtitle("Value") +
  # scale_y_continuous(breaks=seq(-20,240,20)) +
  NULL
print(p)










colnames(fleet_data)




#Sample Graphs:
colnames(group_data)

#Compare biomass over multiple RCP scenarios:
graph_data <- group_data %>%  
  filter(year >=2005) %>% 
  filter(year <=2098) %>%  
  filter(type == "biomass") %>% 
  filter(scenario =="StatusQuo")
graph_data$aggregate <- paste(graph_data$group, graph_data$RCP, sep="/")
p <- ggplot(data=graph_data, aes(x=year, y=value, colour=RCP, group=aggregate)) + 
  geom_line() +
  theme_classic() +
  xlab("Year") + 
  ylab("Catch (t/ km^2)") +
  scale_x_continuous(limits=c(2015,2100)) + 
  guides(colour=guide_legend(title="Scenario"))
print(p)


#Groups of Interest
graph_data <- group_data %>%  
  filter(year >=2005) %>% 
  filter(year <=2098) %>%  
  filter(scenario != "NoFeed Scenario") %>% 
  filter(type == "biomass") %>% 
  filter(group =="Marine mammals")
graph_data$aggregate <- paste(graph_data$scenario, graph_data$RCP, sep="/")
p <- ggplot(data=graph_data, aes(x=year, y=value, colour=aggregate, group=aggregate)) + 
  geom_line() +
  theme_classic() +
  xlab("Year") + 
  ylab("Biomass (t/km^2)") +
  ggtitle("Marine mammals biomass") +
  scale_x_continuous(limits=c(2015,2100)) + 
  guides(colour=guide_legend(title="Scenario"))
print(p)


#Important Fisheries
graph_data <- group_data %>%  
  filter(year >=2005) %>% 
  filter(year <=2098) %>%  
  filter(scenario != "NoFeed Scenario") %>% 
  filter(type == "biomass") %>% 
  filter(group =="Marine mammals")
graph_data$aggregate <- paste(graph_data$scenario, graph_data$RCP, sep="/")
p <- ggplot(data=graph_data, aes(x=year, y=value, colour=aggregate, group=aggregate)) + 
  geom_line() +
  theme_classic() +
  xlab("Year") + 
  ylab("Biomass (t/km^2)") +
  ggtitle("Marine mammals biomass") +
  scale_x_continuous(limits=c(2015,2100)) + 
  guides(colour=guide_legend(title="Scenario"))
print(p)

#Compare Catch and Value at aggregate level under different scenarios:
colnames(fleet_data)
#Catch
graph_data <- fleet_data %>%  
  filter(year >=2015) %>% 
  filter(type == "catch") %>%
  group_by(year, RCP, scenario) %>%
  summarize(catch=sum(value))


graph_data$aggregate <- paste(graph_data$scenario, graph_data$RCP, sep="/")
p <- ggplot(data=graph_data, aes(x=year, y=catch, colour=aggregate, group=aggregate)) + 
  geom_line() +
  theme_classic() +
  xlab("Year") + 
  ylab("Catch (t/ km^2)") +
  scale_x_continuous(limits=c(2015,2100)) + 
  guides(colour=guide_legend(title="Scenario")) + #Change legend Title
  #scale_y_continuous(limits=c(0,16), breaks=seq(0,15,5)) +
  NULL
print(p)

#Value
graph_data <- fleet_data %>%  
  filter(year >=2005) %>% 
  filter(type == "value") %>%
  group_by(year, RCP, scenario) %>%
  summarize(value=sum(value))


graph_data$aggregate <- paste(graph_data$scenario, graph_data$RCP, sep="/")
p <- ggplot(data=graph_data, aes(x=year, y=value, colour=aggregate, group=aggregate)) + 
  geom_line() +
  theme_classic() +
  xlab("Year") + 
  ylab("Value (USD)") +
  scale_x_continuous(limits=c(2015,2100)) + 
  guides(colour=guide_legend(title="Scenario")) +
  #scale_y_continuous(limits=c(0,16), breaks=seq(0,15,5)) + #Change legend Title
  NULL
print(p)








#Compare Catch and Value at aggregate level under different scenarios:
colnames(fleet_data)
#Catch
graph_data <- fleet_data %>%
  filter(RCP == "RCP2.6") %>%
  filter(year >=2015) %>% 
  filter(type == "catch") %>%
  group_by(year, RCP, scenario) %>%
  summarize(catch=sum(value))


graph_data$aggregate <- paste(graph_data$scenario, graph_data$RCP, sep="/")
p <- ggplot(data=graph_data, aes(x=year, y=catch, colour=aggregate, group=aggregate)) + 
  geom_line() +
  theme_classic() +
  xlab("Year") + 
  ylab("Catch (t/ km^2)") +
  scale_x_continuous(limits=c(2015,2100)) + 
  guides(colour=guide_legend(title="Scenario")) #Change legend Title
#scale_y_continuous(limits=c(0,16), breaks=seq(0,15,5))
print(p)

#Value
graph_data <- fleet_data %>%
  filter(RCP == "RCP2.6") %>%
  filter(year >=2005) %>% 
  filter(type == "value") %>%
  group_by(year, RCP, scenario) %>%
  summarize(value=sum(value))


graph_data$aggregate <- paste(graph_data$scenario, graph_data$RCP, sep="/")
p <- ggplot(data=graph_data, aes(x=year, y=value, colour=aggregate, group=aggregate)) + 
  geom_line() +
  theme_classic() +
  xlab("Year") + 
  ylab("Value (USD)") +
  scale_x_continuous(limits=c(2015,2100)) + 
  guides(colour=guide_legend(title="Scenario"))
#scale_y_continuous(limits=c(0,16), breaks=seq(0,15,5))#Change legend Title
print(p)






for (n in 1:length(unique(summary_table1$fleet))) {
  print(n)
  assign(paste("plot", n, sep="_"),)}



###Fleet level Tables and Facet Wrap Plots ###
####Clean Raw Data to Tidy Format####

ecs_info <- read.csv("ecs_fc_fleet_forR.csv")
ecs_info <- drop_na(ecs_info)

ecs_groups <- as.character(ecs_info$FunctionalGroup)
ecs_fleets <- as.character(unique(ecs_info$Fleet))
ecs_fleets <- ecs_fleets[0:(length(ecs_fleets)-1)]
ecs_fleet_df <- data.frame(fleet=seq(1,12,1), fleet_name=ecs_fleets)

summary_table1 <- fleet_data %>% 
  group_by(RCP, scenario, year, type, fleet) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2000 & year <=2009) %>% 
  group_by(RCP, scenario, type, fleet) %>% 
  summarize(average_2000s = mean(value_aggregate),
            stdev_2000s = sd(value_aggregate)) %>%
  mutate(se_2000s = stdev_2000s/sqrt(10))

summary_table2 <- fleet_data %>% 
  group_by(RCP, scenario, year, type, fleet) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2090 & year <2099) %>% 
  group_by(RCP, scenario, type, fleet) %>% 
  summarize(average_2090s = mean(value_aggregate),
            stdev_2090s = sd(value_aggregate)) %>%
  mutate(se_2090s = stdev_2090s/sqrt(9))


melted1<- summary_table1 %>% gather(key= variable, value=value, -RCP, -type, -scenario, -fleet)
melted2<- summary_table2 %>% gather(key= variable, value=value, -RCP, -type, -scenario, -fleet)
melted <- rbind(melted1, melted2)
melted$year <- gsub(melted$variable, pattern=".*_", replacement="")
melted$variable <- gsub(melted$variable, pattern="_.*", replacement="")
melted$aggregate <- paste(melted$RCP,  melted$scenario, melted$year, sep="/")
melted$label <- paste(melted$scenario, melted$year, melted$fleet, sep=" ")

melted <- left_join(melted, ecs_fleet_df)

spread <- melted %>%
  filter(variable != "se")  %>%
  spread(key=variable, value=value)
summary_table_catch <- spread %>% filter(type=="catch")
summary_table_value <- spread %>% filter(type=="value")




data <- summary_table_catch[which(summary_table_catch$year=="2090s"),]
data <- data %>%
  ungroup() %>%
  mutate(scenario = as.character(scenario)) %>%
  mutate(scenario = factor(scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))) %>%
  arrange(RCP, scenario)
data <- data %>% 
  arrange(fleet, RCP, scenario)
data=transform(data, x=x.seq)
baseline <- summary_table_catch[which(summary_table_catch$aggregate=="RCP2.6/StatusQuo/2000s"),]
baseline <- baseline %>% mutate(baseline= average) %>%  ungroup() %>%  
  select(c(fleet_name, baseline))

data <- left_join(data, baseline)


colours = brewer.pal(6, "RdBu")

colours = c("#EF8A62", "#B2182B",  "#FDDBC7", "#D1E5F0", "#67A9CF", "#2166AC") #Re-ordered so colours make more sense 

p <- ggplot(data=data, aes(x=x, y=average, fill=scenario)) +
  geom_col() +
  theme_classic() +
  ylab(expression(paste("Catch (tonnes/km"^"2",")", sep=""))) + 
  scale_y_continuous(expand = c(0, 0)) + #breaks=seq(0,30,5), limits=c(0,30), 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=10, hjust = 0.5),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        axis.title = element_text(size=12)) +
  xlab("") + 
  labs(fill="Fishing Scenario") +
  scale_fill_manual(values=colours) +
  geom_hline(aes(yintercept = data$baseline), linetype="dashed", colour="black") +
  geom_errorbar(aes(x=x,ymin=average-stdev, ymax=average+stdev), width=.2,
                position=position_dodge(.9)) + 
  scale_x_continuous(labels=rcps, breaks=breaks) +
  facet_wrap(~fleet_name, ncol=3, scales="free_y") +
  NULL
print(p)
ggsave("Absolute Change in Fleet Level Catch from 2000s to 2090s ECS.png", width=24, height = 18, units="cm", dpi=300)


data <- summary_table_value[which(summary_table_catch$year=="2090s"),]
data <- data %>%
  ungroup() %>%
  mutate(scenario = as.character(scenario)) %>%
  mutate(scenario = factor(scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))) %>%
  arrange(RCP, scenario)
data <- data %>% 
  arrange(fleet, RCP, scenario)
data=transform(data, x=x.seq)
baseline <- summary_table_value[which(summary_table_value$aggregate=="RCP2.6/StatusQuo/2000s"),]
baseline <- baseline %>% mutate(baseline= average) %>%  ungroup() %>%  
  select(c(fleet_name, baseline))

data <- left_join(data, baseline)
p <- ggplot(data=data, aes(x=x, average, fill=scenario)) +
  geom_col() +
  theme_classic() +
  ylab(expression(paste("Value ($/km"^"2",")", sep=""))) + 
  scale_y_continuous(expand = c(0, 0)) + #breaks=seq(0,30,5), limits=c(0,30), 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=10, hjust = 0.5),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        axis.title = element_text(size=12)) +
  xlab("") + 
  labs(fill="Fishing Scenario") +
  scale_fill_manual(values=colours) +
  geom_hline(aes(yintercept = data$baseline), linetype="dashed", colour="black") +
  geom_errorbar(aes(x=x,ymin=average-stdev, ymax=average+stdev), width=.2,
                position=position_dodge(.9)) + 
  scale_x_continuous(labels=rcps, breaks=breaks) +
  facet_wrap(~fleet_name, ncol=3, scales="free_y") +
  NULL
print(p)
ggsave("Absolute Change in Fleet Level Value from 2000s to 2090s ECS.png", width=24, height = 18, units="cm", dpi=300)






#rm(list=ls())
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

#setwd("C:/Users/t.cashion/Google Drive/Desktop/Current_Internal/East China Sea/ECS Model Development")
setwd("~/Desktop/Google Drive/Desktop/Current_Internal/East China Sea/ECS Model Development")
group_data <- read.csv("GroupData_tidy_ElasticityAdjusted_October152018.csv")
fleet_data <- read.csv("FleetData_tidy_ElasticityAdjusted_October152018.csv")
#RCPs:
rcps <- c("RCP2.6", "RCP8.5")
rcp_num <- length(rcps)
scenario_num <- 6
start_num <- 1 
sep <- 1

x.seq <- c()
breaks <- c()
for (i in 1:rcp_num) {
  y <- i-1
  x <- seq(start_num+(scenario_num*y)+y, y+scenario_num*i)
  break_x <- mean(x)
  breaks <- append(breaks, break_x)
  x.seq <- append(x.seq, x)
}


ecs_info <- read.csv("ecs_fc_fleet_forR.csv")
ecs_info <- drop_na(ecs_info)

ecs_groups <- as.character(ecs_info$FunctionalGroup)
ecs_fleets <- as.character(unique(ecs_info$Fleet))
ecs_fleets <- ecs_fleets[0:(length(ecs_fleets)-1)]
ecs_fleet_df <- data.frame(fleet=seq(1,12,1), fleet_name=ecs_fleets)
ecs_group_df <- data.frame(group=as.factor(seq(1,38,1)), group_name=ecs_groups)




summary_table1 <- group_data %>% 
  group_by(RCP, scenario, year, type, group) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2000 & year <=2009) %>% 
  group_by(RCP, scenario, type, group) %>% 
  summarize(average_2000s = mean(value_aggregate),
            stdev_2000s = sd(value_aggregate)) %>%
  mutate(se_2000s = stdev_2000s/sqrt(10))

summary_table2 <- group_data %>% 
  group_by(RCP, scenario, year, type, group) %>% 
  summarize(value_aggregate = sum(value)) %>% 
  filter(year >=2090 & year <2099) %>% 
  group_by(RCP, scenario, type, group) %>% 
  summarize(average_2090s = mean(value_aggregate),
            stdev_2090s = sd(value_aggregate)) %>%
  mutate(se_2090s = stdev_2090s/sqrt(9))


melted1<- summary_table1 %>% gather(key= variable, value=value, -RCP, -type, -scenario, -group)
melted2<- summary_table2 %>% gather(key= variable, value=value, -RCP, -type, -scenario, -group)
melted <- rbind(melted1, melted2)
melted$year <- gsub(melted$variable, pattern=".*_", replacement="")
melted$variable <- gsub(melted$variable, pattern="_.*", replacement="")
melted$aggregate <- paste(melted$RCP,  melted$scenario, melted$year, sep="/")
melted$label <- paste(melted$RCP, melted$scenario, sep=" ")

#melted <- left_join(melted, ecs_group_df)

spread <- melted %>%
  filter(variable != "se")  %>%
  spread(key=variable, value=value)
summary_table_biomass <- spread %>% filter(type=="biomass")
summary_table_catch <- spread %>% filter(type=="catch")
summary_table_value <- spread %>% filter(type=="value")


data <- summary_table_biomass[which(summary_table_biomass$year=="2090s"),]
data <- data[order(data$group),]
data=transform(data, x=x.seq)
baseline <- summary_table_biomass[which(summary_table_biomass$aggregate=="RCP2.6/StatusQuo/2000s"),]
baseline <- baseline %>% mutate(baseline= average) %>%  ungroup() %>%  
  select(c(group, baseline))

data <- left_join(data, baseline)

aggregates <- unique(data$aggregate)
num <- aggregates[1]

df <- data %>% filter(aggregate==num)
df_full <- df[0,]
n= 5 #Number of top to take. 
for (num in aggregates) {
  df <- data %>% filter(aggregate==num)
  df_top <- top_n(x=df, n=n, wt=average)
  df_bottom <- top_n(x=df, n=-(length(unique(data$group))-n), wt=average)
  others <- sum(df_bottom$average)
  df_other <- df_bottom[1,]
  df_other$group <- "Others"
  df_other$average <- others
  df_partial <- rbind(df_top, df_other)
  df_full <- rbind(df_full, df_partial)
}


colours = brewer.pal(11,"RdBu")
df_full <- df_full %>%
  ungroup() %>%
  mutate(scenario = as.character(scenario)) %>%
  mutate(scenario = factor(scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))) %>%
  arrange(RCP, scenario)

p <- ggplot(data=df_full, aes(x=x, y=average, fill=group)) +
  geom_col() +
  theme_classic() +
  ylab(expression(paste("Biomass (tonnes/km"^"2",")", sep=""))) + 
  scale_y_continuous(expand = c(0, 0)) + #breaks=seq(0,30,5), limits=c(0,30), 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=7, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        axis.title = element_text(size=12)) +
  xlab("") + 
  labs(fill="Functional group") +
  scale_fill_manual(values=colours) +
  #  scale_x_continuous(labels=rcps, breaks=breaks) +
  scale_x_continuous(labels=unique(df_full$label), breaks=x.seq) +
  NULL
print(p)
ggsave("Absolute Change in Group Level Biomass from 2000s to 2090s ECS_AllGroups.png", width=12, height = 9, units="cm", dpi=300)



data <- summary_table_biomass[which(summary_table_biomass$year=="2090s"),]
data <- data[order(data$group),]
data=transform(data, x=x.seq)
baseline <- summary_table_biomass[which(summary_table_biomass$aggregate=="RCP2.6/StatusQuo/2000s"),]
baseline <- baseline %>% mutate(baseline= average) %>%  ungroup() %>%  
  select(c(group, baseline))

data <- left_join(data, baseline)

aggregates <- unique(data$aggregate)
num <- aggregates[1]

excluded_groups <- c("Phytoplankton", "Zooplankton", "Benthic Producer", "Detritus")
data <- data %>% filter(!(group %in% excluded_groups))
df <- data %>% filter(aggregate==num)
df_full <- df[0,]
n= 5 #Number of top to take. 
for (num in aggregates) {
  df <- data %>% filter(aggregate==num)
  df_top <- top_n(x=df, n=n, wt=average)
  df_bottom <- top_n(x=df, n=-(length(unique(data$group))-n), wt=average)
  others <- sum(df_bottom$average)
  df_other <- df_bottom[1,]
  df_other$group <- "Others"
  df_other$average <- others
  df_partial <- rbind(df_top, df_other)
  df_full <- rbind(df_full, df_partial)
}


colours = brewer.pal(11,"RdBu")

df_full <- df_full %>%
  ungroup() %>%
  mutate(scenario = as.character(scenario)) %>%
  mutate(scenario = factor(scenario, levels=c("StatusQuo", "Increase50", "Decrease2", "Decrease25", "Decrease50", "NoFeed50"))) %>%
  arrange(RCP, scenario)

p <- ggplot(data=df_full, aes(x=x, y=average, fill=group)) +
  geom_col() +
  theme_classic() +
  ylab(expression(paste("Biomass (tonnes/km"^"2",")", sep=""))) + 
  scale_y_continuous(expand = c(0, 0)) + #breaks=seq(0,30,5), limits=c(0,30), 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=7, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        axis.title = element_text(size=12)) +
  xlab("") + 
  labs(fill="Functional group") +
  scale_fill_manual(values=colours) +
  #scale_x_continuous(labels=c("RCP2.6", "RCP2.6", "RCP8.5"), breaks=c(2.5,7.5,12.5)) +
  scale_x_continuous(labels=unique(df_full$label), breaks=x.seq) +
  NULL
print(p)
ggsave("Absolute Change in Group Level Biomass from 2000s to 2090s ECS_RestrictedGroups.png", width=12, height = 9, units="cm", dpi=300)






#Final line plots on decreasing biomass of key groups####
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

#setwd("C:/Users/t.cashion/Google Drive/Desktop/Current_Internal/East China Sea/ECS Model Development")
setwd("~/Desktop/Google Drive/Desktop/Current_Internal/East China Sea/ECS Model Development")
group_data <- read.csv("GroupData_tidy_ElasticityAdjusted_October152018.csv")
fleet_data <- read.csv("FleetData_tidy_ElasticityAdjusted_October152018.csv")

ecs_info <- read.csv("ecs_fc_fleet_forR.csv")
ecs_info <- drop_na(ecs_info)

ecs_groups <- as.character(ecs_info$FunctionalGroup)
ecs_fleets <- as.character(unique(ecs_info$Fleet))
ecs_fleets <- ecs_fleets[0:(length(ecs_fleets)-1)]
ecs_fleet_df <- data.frame(fleet=seq(1,12,1), fleet_name=ecs_fleets)
ecs_group_df <- data.frame(group=as.character(seq(1,38,1)), group_name=ecs_groups)

group_data <- group_data %>%
  left_join(ecs_group_df)

ecs_group_df
fishery_concern <- c("Haritails (A)", "Large croakers", "Small croakers", "Small pelagic fishes", "Cephalopods")
unimportant_groups <- c("Detritus", "Phytoplankton", "Benthic Producer", "Zooplankton", "Polychaetes")
non_fishes <- c("Detritus", "Phytoplankton", "Benthic Producer", "Zooplankton", "Polychaetes",
                "Echinoderms", "Other invertebrates", "Benthic crustaceans", "Molluscs", "Shrimps",
                "Crabs", "Cephalopods", "Marine mammals", "Seabirds", "Marine turtles", "Jellyfish")


plot_data <- group_data %>%
  filter(group %in% fishery_concern) %>%
  filter(type=="biomass")

plot_data <- plot_data %>%
  mutate(group = replace(group, group == "Haritails (A)", "Adult hairtails"))


fishery_labels <- c(
  `Adult hairtails` = "Adult hairtails",
  `Large croakers` = "Large croakers",
  `Small croakers` = "Small croakers",
  `Small pelagic fishes` = "Small pelagic fishes",
  `Cephalopods` = "Cephalopods",
  `RCP2.6` = "",
  `RCP8.5` = "")
fishery_biomass_plot <- ggplot(data=plot_data, aes(x=year, y=value, group=scenario, color=scenario)) +
  geom_line() +
  theme_classic() +
  facet_wrap(facets= ~group+RCP, ncol=2, scales="free_y", labeller = as_labeller(fishery_labels)) +
  ylab(expression(paste("Biomass (tonnes/km"^"2",")", sep=""))) + 
  xlab("Year") +
  guides(colour=guide_legend(title="Scenario")) + 
  theme(strip.background=element_blank()) + 
  ggtitle(label="RCP 2.6                                                                   RCP 8.5") + 
  theme(plot.title = element_text(hjust = 0.5, size=12)) + 
  NULL
print(fishery_biomass_plot)
ggsave("FisheriesOfConcernBiomassPlot_ECS.png", width=12, height = 9, units="cm", dpi=300)



scenarios <- c("StatusQuo", "Increase50", "Decrease50")
plot_data <- group_data %>%
  filter(!(group %in% non_fishes)) %>%
  filter(type=="biomass") %>%
  filter(scenario %in% scenarios) %>%
  mutate(aggregate=paste(RCP, scenario, sep="/"))
plot_data <- plot_data %>%
  mutate(group = as.character(group)) %>%
  mutate(group = replace(group, group == "Demersal fishes1", "Small demersal fishes")) %>% 
  mutate(group = replace(group, group == "Demersal fishes2", "Medium demersal fishes")) %>% 
  mutate(group = replace(group, group == "Demersal fishes3", "Large demersal fishes"))

fishes_biomass_plot <- ggplot(data=plot_data, aes(x=year, y=value, group=aggregate, color=aggregate)) +
  geom_line() +
  geom_line(data=plot_data[which(plot_data$year<2005),], aes(x=year, y=value, group=aggregate), color="black") +
  theme_classic() +
  facet_wrap(facets= ~group, ncol=4, scales="free_y") +
  ylab(expression(paste("Biomass (tonnes/km"^"2",")", sep=""))) + 
  xlab("Year") +
  guides(colour=guide_legend(title="Scenario")) + 
  theme(strip.background=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, size=12)) + 
  NULL
print(fishes_biomass_plot)


