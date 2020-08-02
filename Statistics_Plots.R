# Master's thesis
# Study of casual relationships between greenhouse gas emission levels based on the EU ETS database
# Piotr Gretszel
# AGH UST, Faculty of Management, Informational Technologies and Econometrics

# Attaching libraries
set.seed(1920)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(psych)
library(ggpubr)
library(xlsx)

# Reading data
setwd("C:/Users/piotr/Documents/Studia/Praca Magisterska/Projekt/Kod")
Data <- read.csv2("Data.csv", na.strings = "", stringsAsFactors = FALSE)
numerics <- c(2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
strings <- c(1, 5, 13)

for (i in 1:ncol(Data)) {
  if (i %in% numerics) {
    Data[,i] <- as.numeric(Data[,i])
  }  else {
    Data[,i] <- as.character(Data[,i])
  }
}
remove(i, numerics, strings)

# Data statistics
table1 <- t(describe(Data %>% select(Allowance_Allocation, Total_Verified_Emissions, GDP_growth, Ratio, FDI, Labor),
                     quant = c(.25, .75), IQR = TRUE))
write.xlsx(as.data.frame(table1), file = "Tables/Summary.xlsx", showNA = FALSE)
remove(table1)

# Statistics for countries
countries <- unique(Data$Compliance_Country)
for (country in countries) {
  tempData <- Data[Data$Compliance_Country %in% country,]
  table1 <- t(describe(tempData %>% select(Allowance_Allocation, Total_Verified_Emissions, GDP_growth, Ratio, FDI, Labor),
                       quant = c(.25, .75), IQR = TRUE, na.rm = TRUE))
  write.xlsx(as.data.frame(table1), file = paste0("Tables/Countries/", country, ".xlsx"), showNA = FALSE)
}
remove(table1, tempData, country)

# Statistics for groups of countries
group_of_countries <- list(
  western_europe_countries = c("AT", "BE", "ES", "FR", "GB", "IE", "NL", "PT"),
  high_renewable_energy_countries = c("SE", "FI", "LT", "DK", "AT", "PT", "EE"),
  low_renewable_energy_countries = c("GB", "NL", "BE", "IE", "PL", "SK", "HU"),
  high_air_pollution_countries = c("CZ", "LT", "HU", "LV", "PL", "SK", "AT"),
  low_air_pollution_countries = c("SE", "FI", "IE", "ES", "PT", "DK", "EE"),
  high_hdi_countries = c("IE", "DE", "SE", "NL", "DK", "FI", "GB", "BE"),
  medium_hdi_countries = c("AT", "SI", "ES", "CZ", "FR", "IT", "EE"),
  low_hdi_countries = c("GR", "PL", "LT", "SK", "LV", "PT", "HU")
)
for (i in 1:length(group_of_countries)) {
  tempData <- Data[Data$Compliance_Country %in% group_of_countries[[i]],]
  table1 <- t(describe(tempData %>% select(Allowance_Allocation, Total_Verified_Emissions, GDP_growth, Ratio, FDI, Labor),
                       quant = c(.25, .75), IQR = TRUE, na.rm = TRUE))
  write.xlsx(as.data.frame(table1), file = paste0("Tables/Groups/", names(group_of_countries)[i], ".xlsx"), showNA = FALSE)
}
remove(table1, tempData, i)

# Statistics for years
years <- unique(Data$Year)
for (year in years) {
  tempData <- Data[Data$Year %in% year,]
  table1 <- t(describe(tempData %>% select(Allowance_Allocation, Total_Verified_Emissions, GDP_growth, Ratio, FDI, Labor),
                       quant = c(.25, .75), IQR = TRUE, na.rm = TRUE))
  write.xlsx(as.data.frame(table1), file = paste0("Tables/Years/", year, ".xlsx"), showNA = FALSE)
}
remove(tempData, year, table1)

# Time series plots
Data_ts <- Data %>% group_by(Year) %>% summarise(
  Allowance_Allocation = sum(Allowance_Allocation, na.rm = TRUE),
  Total_Verified_Emissions = sum(Total_Verified_Emissions, na.rm = TRUE),
  GDP_growth = mean(GDP_growth, na.rm = TRUE),
  FDI = mean(FDI, na.rm = TRUE),
  Labor = mean(Labor, na.rm = TRUE)
)

Data_ts_comb <- Data[Data$IsCombustionSector == 1,] %>% group_by(Year) %>% summarise(
  Allowance_Allocation = sum(Allowance_Allocation, na.rm = TRUE),
  Total_Verified_Emissions = sum(Total_Verified_Emissions, na.rm = TRUE),
  GDP_growth = mean(GDP_growth, na.rm = TRUE),
  FDI = mean(FDI, na.rm = TRUE),
  Labor = mean(Labor, na.rm = TRUE)
)

Data_ts_other <- Data[Data$IsCombustionSector == 0,] %>% group_by(Year) %>% summarise(
  Allowance_Allocation = sum(Allowance_Allocation, na.rm = TRUE),
  Total_Verified_Emissions = sum(Total_Verified_Emissions, na.rm = TRUE),
  GDP_growth = mean(GDP_growth, na.rm = TRUE),
  FDI = mean(FDI, na.rm = TRUE),
  Labor = mean(Labor, na.rm = TRUE)
)

ggsave("Allowances_all.png", plot =
         Data_ts %>% ggplot(aes(x = Year, y = Allowance_Allocation)) +
         geom_line(col = "red") +
         geom_point(col = "red") +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("Allowances"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")

ggsave("Allowances_combustion.png", plot =
         Data_ts_comb %>% ggplot(aes(x = Year, y = Allowance_Allocation)) +
         geom_line(col = "red") +
         geom_point(col = "red") +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("Allowances"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")

ggsave("Allowances_other.png", plot =
         Data_ts_other %>% ggplot(aes(x = Year, y = Allowance_Allocation)) +
         geom_line(col = "red") + 
         geom_point(col = "red") +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("Allowances"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")

ggsave("Emission_all.png", plot =
         Data_ts %>% ggplot(aes(x = Year, y = Total_Verified_Emissions)) + 
         geom_line(col = "red") +
         geom_point(col = "red") +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("Emission"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")

ggsave("Emission_combustion.png", plot =
         Data_ts_comb %>% ggplot(aes(x = Year, y = Total_Verified_Emissions)) +
         geom_line(col = "red") +
         geom_point(col = "red") +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("Emission"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")

ggsave("Emission_other.png", plot =
         Data_ts_other %>% ggplot(aes(x = Year, y = Total_Verified_Emissions)) + 
         geom_line(col = "red") +
         geom_point(col = "red") +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("Emission"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")

ggsave("GDP_growth.png", plot =
         Data_ts %>% ggplot(aes(x = Year, y = GDP_growth)) + 
         geom_line(col = "red") +
         geom_point(col = "red") +
         geom_hline(yintercept = 0, lty = 2) +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("GDP growth"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")

ggsave("FDI.png", plot =
         Data_ts %>% ggplot(aes(x = Year, y = FDI)) + 
         geom_line(col = "red") +
         geom_point(col = "red") +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("FDI"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")

ggsave("Labor.png", plot =
         Data_ts %>% ggplot(aes(x = Year, y = Labor)) + 
         geom_line(col = "red") +
         geom_point(col = "red") +
         scale_x_continuous(labels = seq(from = 2005, to = 2018, by = 1), breaks = seq(from = 2005, to = 2018, by = 1)) +
         scale_y_continuous(labels = comma) +
         xlab("Year") +
         ylab("Labor"),
       device = "png", path = "Plots_time_series",
       width = 200, height = 164, units = "mm")
remove(Data_ts, Data_ts_comb, Data_ts_other)

# Bar charts of time series for individual countries
for (country in countries) {
  ggsave(paste0(country, "_GDP_growth.png"), plot =
           Data[Data$Compliance_Country %in% country,] %>%
           ggplot(aes(x = Year, y = GDP_growth, fill = PhaseName)) +
           geom_bar(stat = "summary", fun = mean) +
           theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                 axis.title.y = element_blank()) +
           scale_y_continuous(labels = comma),
         device = "png", path = "Plots_countries",
         width = 200, height = 164, units = "mm")
  
  ggsave(paste0(country, "_FDI.png"), plot =
           Data[Data$Compliance_Country %in% country,] %>%
           ggplot(aes(x = Year, y = FDI, fill = PhaseName)) +
           geom_bar(stat = "summary", fun = mean) +
           theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                 axis.title.y = element_blank()) +
           scale_y_continuous(labels = comma),
         device = "png", path = "Plots_countries",
         width = 200, height = 164, units = "mm")
  
  ggsave(paste0(country, "_labor.png"), plot =
           Data[Data$Compliance_Country %in% country,] %>%
           ggplot(aes(x = Year, y = Labor, fill = PhaseName)) +
           geom_bar(stat = "summary", fun = mean) +
           theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                 axis.title.y = element_blank()) +
           scale_y_continuous(labels = comma),
         device = "png", path = "Plots_countries",
         width = 200, height = 164, units = "mm")
  
  tempAll <- Data[Data$Compliance_Country %in% country,]
  tempCombustion <- Data[Data$Compliance_Country %in% country & Data$IsCombustionSector == 1,]
  tempOther <- Data[Data$Compliance_Country %in% country & Data$IsCombustionSector == 0,]

  if (nrow(tempAll) > 0) {
    ggsave(paste0(country, "_all_allowances.png"), plot = 
             tempAll %>%
             drop_na(Allowance_Allocation) %>%
             ggplot(aes(x = Year, y = Allowance_Allocation, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
    
    ggsave(paste0(country, "_all_emission.png"), plot = 
             tempAll %>%
             drop_na(Total_Verified_Emissions) %>%
             ggplot(aes(x = Year, y = Total_Verified_Emissions, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
    
    tempAll <- tempAll %>% 
      group_by(Year) %>% 
      summarise(
        Ratio = sum(Allowance_Allocation, na.rm = TRUE)/sum(Total_Verified_Emissions, na.rm = TRUE),
        PhaseName = PhaseName
      )
    
    ggsave(paste0(country, "_all_ratio.png"), plot = 
             tempAll %>%
             drop_na(Ratio) %>%
             ggplot(aes(x = Year, y = Ratio, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma),
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
  }
  
  if (nrow(tempCombustion) > 0) {
    ggsave(paste0(country, "_combustion_allowances.png"), plot = 
             tempCombustion %>%
             drop_na(Allowance_Allocation) %>%
             ggplot(aes(x = Year, y = Allowance_Allocation, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
    
    ggsave(paste0(country, "_combustion_emission.png"), plot = 
             tempCombustion %>%
             drop_na(Total_Verified_Emissions) %>%
             ggplot(aes(x = Year, y = Total_Verified_Emissions, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
    
    tempCombustion <- tempCombustion %>% 
      group_by(Year) %>% 
      summarise(
        Ratio = sum(Allowance_Allocation, na.rm = TRUE)/sum(Total_Verified_Emissions, na.rm = TRUE),
        PhaseName = PhaseName
      )
    
    ggsave(paste0(country, "_combustion_ratio.png"), plot = 
             tempCombustion %>%
             drop_na(Ratio) %>%
             ggplot(aes(x = Year, y = Ratio, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma),
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
  }
  
  if (nrow(tempOther) > 0) {
    ggsave(paste0(country, "_other_allowances.png"), plot = 
             tempOther %>%
             drop_na(Allowance_Allocation) %>%
             ggplot(aes(x = Year, y = Allowance_Allocation, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
    
    ggsave(paste0(country, "_other_emission.png"), plot = 
             tempOther %>%
             drop_na(Total_Verified_Emissions) %>%
             ggplot(aes(x = Year, y = Total_Verified_Emissions, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
    
    tempOther <- tempOther %>%
      group_by(Year) %>%
      summarise(
        Ratio = sum(Allowance_Allocation, na.rm = TRUE)/sum(Total_Verified_Emissions, na.rm = TRUE),
        PhaseName = PhaseName
      )
    
    ggsave(paste0(country, "_other_ratio.png"), plot = 
             tempOther %>%
             drop_na(Ratio) %>%
             ggplot(aes(x = Year, y = Ratio, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma),
           device = "png", path = "Plots_countries", 
           width = 164, height = 200, units = "mm")
  }
}
remove(tempCombustion, tempOther, country)

# Bar charts of time series for groups of countries
for (i in 1:length(group_of_countries)) {
  ggsave(paste0(names(group_of_countries)[i], "_GDP_growth.png"), plot =
           Data[Data$Compliance_Country %in% group_of_countries[[i]],] %>%
           ggplot(aes(x = Year, y = GDP_growth, fill = PhaseName)) +
           geom_bar(stat = "summary", fun = mean) +
           theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                 axis.title.y = element_blank()) +
           scale_y_continuous(labels = comma),
         device = "png", path = "Plots_groups",
         width = 200, height = 164, units = "mm")
  
  ggsave(paste0(names(group_of_countries)[i], "_FDI.png"), plot =
           Data[Data$Compliance_Country %in% group_of_countries[[i]],] %>%
           ggplot(aes(x = Year, y = FDI, fill = PhaseName)) +
           geom_bar(stat = "summary", fun = mean) +
           theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                 axis.title.y = element_blank()) +
           scale_y_continuous(labels = comma),
         device = "png", path = "Plots_groups",
         width = 200, height = 164, units = "mm")
  
  ggsave(paste0(names(group_of_countries)[i], "_labor.png"), plot =
           Data[Data$Compliance_Country %in% group_of_countries[[i]],] %>%
           ggplot(aes(x = Year, y = Labor, fill = PhaseName)) +
           geom_bar(stat = "summary", fun = mean) +
           theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                 axis.title.y = element_blank()) +
           scale_y_continuous(labels = comma),
         device = "png", path = "Plots_groups",
         width = 200, height = 164, units = "mm")
  
  tempAll <- Data[Data$Compliance_Country %in% group_of_countries[[i]],]
  tempCombustion <- Data[Data$Compliance_Country %in% group_of_countries[[i]] & Data$IsCombustionSector == 1,]
  tempOther <- Data[Data$Compliance_Country %in% group_of_countries[[i]] & Data$IsCombustionSector == 0,]
  
  if (nrow(tempAll) > 0) {
    ggsave(paste0(names(group_of_countries)[i], "_all_alowances.png"), plot = 
             tempAll %>%
             drop_na(Allowance_Allocation) %>%
             ggplot(aes(x = Year, y = Allowance_Allocation, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
    
    ggsave(paste0(names(group_of_countries)[i], "_all_emission.png"), plot = 
             tempAll %>%
             drop_na(Total_Verified_Emissions) %>%
             ggplot(aes(x = Year, y = Total_Verified_Emissions, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
    
    tempAll <- tempAll %>% 
      group_by(Year) %>% 
      summarise(
        Ratio = sum(Allowance_Allocation, na.rm = TRUE)/sum(Total_Verified_Emissions, na.rm = TRUE),
        PhaseName = PhaseName
      )
    
    ggsave(paste0(names(group_of_countries)[i], "_all_ratio.png"), plot = 
             tempAll %>%
             drop_na(Ratio) %>%
             ggplot(aes(x = Year, y = Ratio, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma),
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
  }
  
  if (nrow(tempCombustion) > 0) {
    ggsave(paste0(names(group_of_countries)[i], "_combustion_alowances.png"), plot = 
             tempCombustion %>%
             drop_na(Allowance_Allocation) %>%
             ggplot(aes(x = Year, y = Allowance_Allocation, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
    
    ggsave(paste0(names(group_of_countries)[i], "_combustion_emission.png"), plot = 
             tempCombustion %>%
             drop_na(Total_Verified_Emissions) %>%
             ggplot(aes(x = Year, y = Total_Verified_Emissions, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
    
    tempCombustion <- tempCombustion %>% 
      group_by(Year) %>% 
      summarise(
        Ratio = sum(Allowance_Allocation, na.rm = TRUE)/sum(Total_Verified_Emissions, na.rm = TRUE),
        PhaseName = PhaseName
      )
    
    ggsave(paste0(names(group_of_countries)[i], "_combustion_ratio.png"), plot = 
             tempCombustion %>%
             drop_na(Ratio) %>%
             ggplot(aes(x = Year, y = Ratio, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma),
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
  }
  
  if (nrow(tempOther) > 0) {
    ggsave(paste0(names(group_of_countries)[i], "_other_alowances.png"), plot = 
             tempOther %>%
             drop_na(Allowance_Allocation) %>%
             ggplot(aes(x = Year, y = Allowance_Allocation, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
    
    ggsave(paste0(names(group_of_countries)[i], "_other_emission.png"), plot = 
             tempOther %>%
             drop_na(Total_Verified_Emissions) %>%
             ggplot(aes(x = Year, y = Total_Verified_Emissions, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
    
    tempOther <- tempOther %>%
      group_by(Year) %>%
      summarise(
        Ratio = sum(Allowance_Allocation, na.rm = TRUE)/sum(Total_Verified_Emissions, na.rm = TRUE),
        PhaseName = PhaseName
      )
    
    ggsave(paste0(names(group_of_countries)[i], "_other_ratio.png"), plot = 
             tempOther %>%
             drop_na(Ratio) %>%
             ggplot(aes(x = Year, y = Ratio, fill = PhaseName)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma),
           device = "png", path = "Plots_groups", 
           width = 164, height = 200, units = "mm")
  }
}
remove(tempCombustion, tempOther, i)

# Number of all installations for individual countries
table <- Data %>%
  group_by(Compliance_Country) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Installations_countries_all.xlsx", showNA = FALSE, row.names = FALSE)

table <- Data[Data$IsCombustionSector == 1,] %>%
  group_by(Compliance_Country) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Installations_countries_combustion.xlsx", showNA = FALSE, row.names = FALSE)

table <- Data[Data$IsCombustionSector == 0,] %>%
  group_by(Compliance_Country) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Installations_countries_other.xlsx", showNA = FALSE, row.names = FALSE)

# Number of all installations for individual years
table <- Data %>%
  group_by(Year) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Installations_years_all.xlsx", showNA = FALSE, row.names = FALSE)

table <- Data[Data$IsCombustionSector == 1,] %>%
  group_by(Year) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Installations_years_combustion.xlsx", showNA = FALSE, row.names = FALSE)

table <- Data[Data$IsCombustionSector == 0,] %>%
  group_by(Year) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Installations_years_other.xlsx", showNA = FALSE, row.names = FALSE)

# Number of all installations for individual countries in specific year
table <- Data %>%
  group_by(Compliance_Country, Year) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Count_year_all.xlsx", showNA = FALSE, row.names = FALSE)

table <- Data[Data$IsCombustionSector == 1,] %>%
  group_by(Compliance_Country, Year) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Count_year_combustion.xlsx", showNA = FALSE, row.names = FALSE)

table <- Data[Data$IsCombustionSector == 0,] %>%
  group_by(Compliance_Country, Year) %>%
  summarise(
    Installations_With_Allowance = sum(!is.na(Allowance_Allocation)),
    Installations_With_Emission = sum(!is.na(Total_Verified_Emissions))
  )
write.xlsx(as.data.frame(table), file ="Tables/Couny_year_other.xlsx", showNA = FALSE, row.names = FALSE)
remove(table)

# Bar charts of the relationship between the GDP quantile and the volume of emissions
for (year in years) {
  tempCombustion <- Data[Data$Year %in% year & Data$IsCombustionSector == 1,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, GDP_quantile)
  tempCombustion <- tempCombustion[complete.cases(tempCombustion),]
  tempOther <- Data[Data$Year %in% year & Data$IsCombustionSector == 0,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, GDP_quantile)
  tempOther <- tempOther[complete.cases(tempOther),]
  
  if (nrow(tempCombustion) > 0) {
    ggsave(paste0(year, "_combustion_alowances.png"), plot = 
             tempCombustion %>%
             ggplot(aes(x = GDP_quantile, y = Allowance_Allocation)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_GDP", 
           width = 164, height = 200, units = "mm")
    
    ggsave(paste0(year, "_combustion_emission.png"), plot = 
             tempCombustion %>%
             ggplot(aes(x = GDP_quantile, y = Total_Verified_Emissions)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_GDP", 
           width = 164, height = 200, units = "mm")
    
    tempCombustion <- tempCombustion %>%
      group_by(GDP_quantile) %>% 
      summarise(
        Ratio = sum(Allowance_Allocation, na.rm = TRUE)/sum(Total_Verified_Emissions, na.rm = TRUE)
      )
    
    ggsave(paste0(year, "_combustion_ratio.png"), plot = 
             tempCombustion %>%
             ggplot(aes(x = GDP_quantile, y = Ratio)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma),
           device = "png", path = "Plots_GDP", 
           width = 164, height = 200, units = "mm")
  }
  
  if (nrow(tempOther) > 0) {
    ggsave(paste0(year, "_other_alowances.png"), plot = 
             tempOther %>%
             ggplot(aes(x = GDP_quantile, y = Allowance_Allocation)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_GDP", 
           width = 164, height = 200, units = "mm")
    
    ggsave(paste0(year, "_other_emission.png"), plot = 
             tempOther %>%
             ggplot(aes(x = GDP_quantile, y = Total_Verified_Emissions)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma), 
           device = "png", path = "Plots_GDP", 
           width = 164, height = 200, units = "mm")
    
    tempOther <- tempOther %>%
      group_by(GDP_quantile) %>%
      summarise(
        Ratio = sum(Allowance_Allocation, na.rm = TRUE)/sum(Total_Verified_Emissions, na.rm = TRUE)
      )
    
    ggsave(paste0(year, "_other_ratio.png"), plot = 
             tempOther %>%
             ggplot(aes(x = GDP_quantile, y = Ratio)) +
             geom_bar(stat = "identity") +
             theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                   axis.title.y = element_blank()) +
             scale_y_continuous(labels = comma),
           device = "png", path = "Plots_GDP", 
           width = 164, height = 200, units = "mm")
  }
}
remove(tempCombustion, tempOther, year)

# Boxplots for individual countries
for (country in countries) {
  tempCombustion <- Data[Data$Compliance_Country %in% country & Data$IsCombustionSector == 1,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio)
  tempCombustion <- tempCombustion[complete.cases(tempCombustion),]
  tempOther <- Data[Data$Compliance_Country %in% country & Data$IsCombustionSector == 0,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio)
  tempOther <- tempOther[complete.cases(tempOther),]
  tempAll <- Data[Data$Compliance_Country %in% country,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio, GDP_growth, FDI, Labor)
  tempAll <- tempAll[complete.cases(tempAll),]

  if (nrow(tempCombustion) > 0) {
    p1 <- ggplot(tempCombustion, aes(x = PhaseName, y = Allowance_Allocation)) +
      geom_boxplot(outlier.colour = "red") +
      theme(axis.title.x = element_blank()) +
      scale_y_continuous(limits = quantile(tempCombustion$Allowance_Allocation, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      ylab("Allowances")
    
    p2 <- ggplot(tempCombustion, aes(x = PhaseName, y = Total_Verified_Emissions)) +
      geom_boxplot(outlier.colour = "red") +
      theme(axis.title.x = element_blank()) +
      scale_y_continuous(limits = quantile(tempCombustion$Total_Verified_Emissions, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      ylab("Emission")
    
    p3 <- ggplot(tempCombustion, aes(x = PhaseName, y = Ratio)) +
      geom_boxplot(outlier.colour = "red") +
      theme(axis.title.x = element_blank()) +
      scale_y_continuous(limits = quantile(tempCombustion$Ratio, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      ylab("Allowances/Emission")
    
    ggsave(paste0(country, "_combustion.png"), plot = ggarrange(p1, p2, p3, nrow = 3),
           device = "png", path = "Boxplots_countries", 
           width = 180, height = 300, units = "mm")
  }
  
  if (nrow(tempOther) > 0) {
    p1 <- ggplot(tempOther, aes(x = PhaseName, y = Allowance_Allocation)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempOther$Allowance_Allocation, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances")
    
    p2 <- ggplot(tempOther, aes(x = PhaseName, y = Total_Verified_Emissions)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempOther$Total_Verified_Emissions, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Emission")
    
    p3 <- ggplot(tempOther, aes(x = PhaseName, y = Ratio)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempOther$Ratio, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances/Emission")
    
    ggsave(paste0(country, "_other.png"), plot = ggarrange(p1, p2, p3, nrow = 3),
           device = "png", path = "Boxplots_countries", 
           width = 180, height = 300, units = "mm")
  }
  
  if (nrow(tempAll) > 0) {
    p1 <- ggplot(tempAll, aes(x = PhaseName, y = Allowance_Allocation)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$Allowance_Allocation, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances")
    
    p2 <- ggplot(tempAll, aes(x = PhaseName, y = Total_Verified_Emissions)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$Total_Verified_Emissions, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Emission")
    
    p3 <- ggplot(tempAll, aes(x = PhaseName, y = Ratio)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$Ratio, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances/Emission")
    
    ggsave(paste0(country, "_all.png"), plot = ggarrange(p1, p2, p3, nrow = 3),
           device = "png", path = "Boxplots_countries", 
           width = 180, height = 300, units = "mm")
    
    p4 <- tempAll %>%
      ggplot(aes(x = PhaseName, y = GDP_growth)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$GDP_growth, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("GDP growth")
    
    p5 <- tempAll %>%
      ggplot(aes(x = PhaseName, y = FDI)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$FDI, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
        ylab("FDI")
    
    p6 <- tempAll %>%
      ggplot(aes(x = PhaseName, y = Labor)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$Labor, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Labor")
    
    ggsave(paste0(country, "_economic.png"), plot = ggarrange(p4, p5, p6, nrow = 3),
           device = "png", path = "Boxplots_countries",
           width = 160, height = 300, units = "mm")
  }
}
remove(p1, p2, p3, p4, p5, p6, tempCombustion, tempOther, tempAll, country)

# Boxplots for group of countries
for (i in 1:length(group_of_countries)) {
  tempCombustion <- Data[Data$Compliance_Country %in% group_of_countries[[i]] & Data$IsCombustionSector == 1,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio)
  tempCombustion <- tempCombustion[complete.cases(tempCombustion),]
  tempOther <- Data[Data$Compliance_Country %in% group_of_countries[[i]] & Data$IsCombustionSector == 0,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio)
  tempOther <- tempOther[complete.cases(tempOther),]
  tempAll <- Data[Data$Compliance_Country %in% group_of_countries[[i]],] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio, GDP_growth, FDI, Labor)
  tempAll <- tempAll[complete.cases(tempAll),]
  
  if (nrow(tempCombustion) > 0) {
    p1 <- ggplot(tempCombustion, aes(x = PhaseName, y = Allowance_Allocation)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempCombustion$Allowance_Allocation, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances")
    
    p2 <- ggplot(tempCombustion, aes(x = PhaseName, y = Total_Verified_Emissions)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempCombustion$Total_Verified_Emissions, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Emission")
    
    p3 <- ggplot(tempCombustion, aes(x = PhaseName, y = Ratio)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempCombustion$Ratio, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances/Emission")
    
    ggsave(paste0(names(group_of_countries)[i], "_combustion.png"), plot = ggarrange(p1, p2, p3, nrow = 3),
           device = "png", path = "Boxplots_groups", 
           width = 180, height = 300, units = "mm")
  }
  
  if (nrow(tempOther) > 0) {
    p1 <- ggplot(tempOther, aes(x = PhaseName, y = Allowance_Allocation)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempOther$Allowance_Allocation, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances")
    
    p2 <- ggplot(tempOther, aes(x = PhaseName, y = Total_Verified_Emissions)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempOther$Total_Verified_Emissions, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Emission")
    
    p3 <- ggplot(tempOther, aes(x = PhaseName, y = Ratio)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempOther$Ratio, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances/Emission")
    
    ggsave(paste0(names(group_of_countries)[i], "_other.png"), plot = ggarrange(p1, p2, p3, nrow = 3),
           device = "png", path = "Boxplots_groups", 
           width = 180, height = 300, units = "mm")
  }
  
  if (nrow(tempAll) > 0) {
    p1 <- ggplot(tempAll, aes(x = PhaseName, y = Allowance_Allocation)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$Allowance_Allocation, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances")
    
    p2 <- ggplot(tempAll, aes(x = PhaseName, y = Total_Verified_Emissions)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$Total_Verified_Emissions, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Emission")
    
    p3 <- ggplot(tempAll, aes(x = PhaseName, y = Ratio)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$Ratio, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Allowances/Emission")
    
    ggsave(paste0(names(group_of_countries)[i], "_all.png"), plot = ggarrange(p1, p2, p3, nrow = 3),
           device = "png", path = "Boxplots_groups", 
           width = 180, height = 300, units = "mm")
    
    p4 <- tempAll %>%
      ggplot(aes(x = PhaseName, y = GDP_growth)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$GDP_growth, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("GDP growth")
    
    p5 <- tempAll %>%
      ggplot(aes(x = PhaseName, y = FDI)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$FDI, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("FDI")
    
    p6 <- tempAll %>%
      ggplot(aes(x = PhaseName, y = Labor)) +
      geom_boxplot(outlier.colour = "red") +
      scale_y_continuous(limits = quantile(tempAll$Labor, c(0.1, 0.9), na.rm = TRUE), labels = comma) +
      theme(axis.title.x = element_blank()) +
      ylab("Labor")
    
    ggsave(paste0(names(group_of_countries)[i], "_economic.png"), plot = ggarrange(p4, p5, p6, nrow = 3),
           device = "png", path = "Boxplots_groups",
           width = 160, height = 300, units = "mm")
  }
}
remove(p1, p2, p3, p4, p5, p6, tempCombustion, tempOther, tempAll, i)

# Kruskal-Willis tests for individual countries
tableCombustion <- matrix(data = NA, nrow = length(countries), ncol = 4)
tableOther <- matrix(data = NA, nrow = length(countries), ncol = 4)
tableAll <- matrix(data = NA, nrow = length(countries), ncol = 7)
tableCombustion[,1] <- countries
tableOther[,1] <- countries
tableAll[,1] <- countries

for (i in 1:length(countries)) {
  tempCombustion <- Data[Data$Compliance_Country %in% countries[i] & Data$IsCombustionSector == 1,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio)
  tempCombustion <- tempCombustion[complete.cases(tempCombustion),]
  tempOther <- Data[Data$Compliance_Country %in% countries[i] & Data$IsCombustionSector == 0,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio)
  tempOther <- tempOther[complete.cases(tempOther),]
  tempAll <- Data[Data$Compliance_Country %in% countries[i],] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio, GDP_growth, FDI, Labor)
  tempAll <- tempAll[complete.cases(tempAll),]
  
  if (length(unique(tempCombustion$PhaseName)) > 1) {
    tableCombustion[i, 2] <- round(kruskal.test(tempCombustion$Allowance_Allocation ~ tempCombustion$PhaseName, 
                                                data = tempCombustion)[[3]], digits = 4)
    tableCombustion[i, 3] <- round(kruskal.test(tempCombustion$Total_Verified_Emission ~ tempCombustion$PhaseName,
                                                data = tempCombustion)[[3]], digits = 4)
    tableCombustion[i, 4] <- round(kruskal.test(tempCombustion$Ratio ~ tempCombustion$PhaseName, 
                                                data = tempCombustion)[[3]], digits = 4)
  } else {
    tableCombustion[i, 2] <- NA
    tableCombustion[i, 3] <- NA
    tableCombustion[i, 4] <- NA
  }
  
  if (length(unique(tempOther$PhaseName)) > 1) {
    tableOther[i, 2] <- round(kruskal.test(tempOther$Allowance_Allocation ~ tempOther$PhaseName, 
                                                data = tempOther)[[3]], digits = 4)
    tableOther[i, 3] <- round(kruskal.test(tempOther$Total_Verified_Emission ~ tempOther$PhaseName, 
                                                data = tempOther)[[3]], digits = 4)
    tableOther[i, 4] <- round(kruskal.test(tempOther$Ratio ~ tempOther$PhaseName,
                                                data = tempOther)[[3]], digits = 4)
  } else {
    tableOther[i, 2] <- NA
    tableOther[i, 3] <- NA
    tableOther[i, 4] <- NA
  }
  
  if (length(unique(tempAll$PhaseName)) > 1) {
    tableAll[i, 2] <- round(kruskal.test(tempAll$Allowance_Allocation ~ tempAll$PhaseName, 
                                                data = tempAll)[[3]], digits = 4)
    tableAll[i, 3] <- round(kruskal.test(tempAll$Total_Verified_Emission ~ tempAll$PhaseName, 
                                                data = tempAll)[[3]], digits = 4)
    tableAll[i, 4] <- round(kruskal.test(tempAll$Ratio ~ tempAll$PhaseName,
                                                data = tempAll)[[3]], digits = 4)
    tableAll[i, 5] <- round(kruskal.test(tempAll$GDP_growth ~ tempAll$PhaseName,
                                                data = tempAll)[[3]], digits = 4)
    tableAll[i, 6] <- round(kruskal.test(tempAll$FDI ~ tempAll$PhaseName,
                                                data = tempAll)[[3]], digits = 4)
    tableAll[i, 7] <- round(kruskal.test(tempAll$Labor ~ tempAll$PhaseName,
                                                data = tempAll)[[3]], digits = 4)
  } else {
    tableAll[i, 2] <- NA
    tableAll[i, 3] <- NA
    tableAll[i, 4] <- NA
    tableAll[i, 5] <- NA
    tableAll[i, 6] <- NA
    tableAll[i, 7] <- NA
  }
}
tableCombustion <- as.data.frame(tableCombustion)
tableOther <- as.data.frame(tableOther)
tableAll <- as.data.frame(tableAll)
colnames(tableCombustion) <- c("Country", "Allowances", "Emission", "Allowances/Emission")
colnames(tableOther) <- c("Country", "Allowances", "Emission", "Allowances/Emission")
colnames(tableAll) <- c("Country", "Allowances", "Emission", "Allowances/Emission", "GDP growth", "FDI", "Labor")
write.xlsx(as.data.frame(tableCombustion), file = "Tables/Kruskal_countries_combustion.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(as.data.frame(tableOther), file = "Tables/Kruskal_countries_other.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(as.data.frame(tableAll), file = "Tables/Kruskal_countries_all.xlsx", showNA = FALSE, row.names = FALSE)
remove(tableAll, tableCombustion, tableOther, tempAll, tempCombustion, tempOther, i, countries, years)

# Kruskal-Willis tests for groups of countries
tableCombustion <- matrix(data = NA, nrow = length(group_of_countries), ncol = 4)
tableOther <- matrix(data = NA, nrow = length(group_of_countries), ncol = 4)
tableAll <- matrix(data = NA, nrow = length(group_of_countries), ncol = 7)
tableCombustion[,1] <- names(group_of_countries)
tableOther[,1] <- names(group_of_countries)
tableAll[,1] <- names(group_of_countries)

for (i in 1:length(group_of_countries)) {
  tempCombustion <- Data[Data$Compliance_Country %in% group_of_countries[[i]] & Data$IsCombustionSector == 1,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio)
  tempCombustion <- tempCombustion[complete.cases(tempCombustion),]
  tempOther <- Data[Data$Compliance_Country %in% group_of_countries[[i]] & Data$IsCombustionSector == 0,] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio)
  tempOther <- tempOther[complete.cases(tempOther),]
  tempAll <- Data[Data$Compliance_Country %in% group_of_countries[[i]],] %>%
    select(Allowance_Allocation, Total_Verified_Emissions, PhaseName, Ratio, GDP_growth, FDI, Labor)
  tempAll <- tempAll[complete.cases(tempAll),]
  
  if (length(unique(tempCombustion$PhaseName)) > 1) {
    tableCombustion[i, 2] <- round(kruskal.test(tempCombustion$Allowance_Allocation ~ tempCombustion$PhaseName, 
                                                data = tempCombustion)[[3]], digits = 4)
    tableCombustion[i, 3] <- round(kruskal.test(tempCombustion$Total_Verified_Emission ~ tempCombustion$PhaseName,
                                                data = tempCombustion)[[3]], digits = 4)
    tableCombustion[i, 4] <- round(kruskal.test(tempCombustion$Ratio ~ tempCombustion$PhaseName, 
                                                data = tempCombustion)[[3]], digits = 4)
  } else {
    tableCombustion[i, 2] <- NA
    tableCombustion[i, 3] <- NA
    tableCombustion[i, 4] <- NA
  }
  
  if (length(unique(tempOther$PhaseName)) > 1) {
    tableOther[i, 2] <- round(kruskal.test(tempOther$Allowance_Allocation ~ tempOther$PhaseName, 
                                           data = tempOther)[[3]], digits = 4)
    tableOther[i, 3] <- round(kruskal.test(tempOther$Total_Verified_Emission ~ tempOther$PhaseName, 
                                           data = tempOther)[[3]], digits = 4)
    tableOther[i, 4] <- round(kruskal.test(tempOther$Ratio ~ tempOther$PhaseName,
                                           data = tempOther)[[3]], digits = 4)
  } else {
    tableOther[i, 2] <- NA
    tableOther[i, 3] <- NA
    tableOther[i, 4] <- NA
  }
  
  if (length(unique(tempAll$PhaseName)) > 1) {
    tableAll[i, 2] <- round(kruskal.test(tempAll$Allowance_Allocation ~ tempAll$PhaseName, 
                                         data = tempAll)[[3]], digits = 4)
    tableAll[i, 3] <- round(kruskal.test(tempAll$Total_Verified_Emission ~ tempAll$PhaseName, 
                                         data = tempAll)[[3]], digits = 4)
    tableAll[i, 4] <- round(kruskal.test(tempAll$Ratio ~ tempAll$PhaseName,
                                         data = tempAll)[[3]], digits = 4)
    tableAll[i, 5] <- round(kruskal.test(tempAll$GDP_growth ~ tempAll$PhaseName,
                                         data = tempAll)[[3]], digits = 4)
    tableAll[i, 6] <- round(kruskal.test(tempAll$FDI ~ tempAll$PhaseName,
                                         data = tempAll)[[3]], digits = 4)
    tableAll[i, 7] <- round(kruskal.test(tempAll$Labor ~ tempAll$PhaseName,
                                         data = tempAll)[[3]], digits = 4)
  } else {
    tableAll[i, 2] <- NA
    tableAll[i, 3] <- NA
    tableAll[i, 4] <- NA
    tableAll[i, 5] <- NA
    tableAll[i, 6] <- NA
    tableAll[i, 7] <- NA
  }
}
tableCombustion <- as.data.frame(tableCombustion)
tableOther <- as.data.frame(tableOther)
tableAll <- as.data.frame(tableAll)
colnames(tableCombustion) <- c("Group of countries", "Allowances", "Emission", "Allowances/Emission")
colnames(tableOther) <- c("Group of countries", "Allowances", "Emission", "Allowances/Emission")
colnames(tableAll) <- c("Group of countries", "Allowances", "Emission", "Allowances/Emission", "GDP growth", "FDI", "Labor")
write.xlsx(as.data.frame(tableCombustion), file = "Tables/Kruskal_groups_combustion.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(as.data.frame(tableOther), file = "Tables/Kruskal_groups_other.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(as.data.frame(tableAll), file = "Tables/Kruskal_groups_all.xlsx", showNA = FALSE, row.names = FALSE)
remove(tableAll, tableCombustion, tableOther, tempAll, tempCombustion, tempOther, i, group_of_countries)