# Master's thesis
# Study of casual relationships between greenhouse gas emission levels based on the EU ETS database
# Piotr Gretszel
# AGH UST, Faculty of Management, Informational Technologies and Econometrics

# Attaching libraries
set.seed(1920)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)

# Reading data
setwd("C:/Users/piotr/Documents/Studia/Praca Magisterska/Projekt/Kod")
Data <- read_excel("Data.xlsx", col_types = c(rep("text", 2), rep("numeric", 29)))

# Choosing variables
countries_to_drop <- c("CY", "HR", "IS", "LI", "MT", "LU", "BG", "NO", "RO")
'%ni%' <- Negate('%in%')
Data <- Data[which(Data$Compliance_Country %ni% countries_to_drop),]

# Conversion and gathering data
tempYears <- list()
for (i in 4:length(Data)) {
  tempYears[[i - 3]] <- gather(Data[,i], key = "Year", value = "value")
  tempYears[[i - 3]]$Year <- substr(colnames(Data[,i]), 1, 4)
  colnames(tempYears[[i - 3]]) <- c("Year", str_sub(colnames(Data[,i]), 6))
}

Data <- Data[,1:3]
years <- seq(from = 2005, to = 2018, by = 1)
tempData <- matrix(NA, nrow = 0, ncol = (3 + ncol(Data)))

for (i in seq(from = 1, to = length(tempYears), by = 2)) {
  tempMatrix <- matrix(NA, nrow = nrow(Data), ncol = (3 + ncol(Data)))
  for (j in 0:1) {
    if (j == 0) {
      tempMatrix[,1] <- as.matrix(tempYears[[i + j]][,1])
      tempMatrix[,2] <- as.matrix(tempYears[[i + j]][,2])
    } else {
      tempMatrix[,3] <- as.matrix(tempYears[[i + j]][,2])
    }
    tempMatrix[,4:(3 + ncol(Data))] <- as.matrix(Data)
  }
  tempData <- rbind(tempData, tempMatrix)
}
tempData <- as.data.frame(tempData)
colnames(tempData) <- c("Year", "Allowance_Allocation", "Total_Verified_Emissions", colnames(Data))
Data <- tempData %>% mutate(Year = as.numeric(Year),
                            Allowance_Allocation = as.numeric(Allowance_Allocation),
                            Total_Verified_Emissions = as.numeric(Total_Verified_Emissions),
                            Main_Activity_Code = as.numeric(Main_Activity_Code))

# Attaching GDP data
GDP <- read_excel("GDP_growth.xlsx")
GDP <- GDP[which(GDP$`Country Code` %ni% countries_to_drop),]
GDP <- GDP %>% 
  select(-`Country Name`, -`Indicator Name`) %>%
  gather("Year", "GDP_growth", -`Country Code`)
colnames(GDP) <- c("Compliance_Country", colnames(GDP)[2:ncol(GDP)])
Data <- merge(x = Data, y = GDP, by = c("Compliance_Country", "Year"), all.y = TRUE)

# GDP quantile
Data <- Data %>% mutate(GDP_quantile = NA)
for (year in years) {
  Data$GDP_quantile[Data$Year %in% year] <- dplyr::ntile(Data$GDP_growth[Data$Year %in% year], 10)
}

# Attaching FDI data
FDI <- read_excel("FDI.xlsx")
FDI <- FDI[which(FDI$`Country Code` %ni% countries_to_drop),]
FDI <- FDI %>%
  gather("Year", "FDI", -`Country Code`)
colnames(FDI) <- c("Compliance_Country", colnames(FDI)[2:ncol(FDI)])
Data <- merge(x = Data, y = FDI, by = c("Compliance_Country", "Year"), all.y = TRUE)

# Attaching labor data
Labor <- read_excel("Labor.xlsx")
Labor <- Labor[which(Labor$`Country Code` %ni% countries_to_drop),]
Labor <- Labor %>%
  gather("Year", "Labor", -`Country Code`)
colnames(Labor) <- c("Compliance_Country", colnames(Labor)[2:ncol(Labor)])
Data <- merge(x = Data, y = Labor, by = c("Compliance_Country", "Year"), all.y = TRUE)

# Additional variables
combustion_codes <- c(1, 20)
Data <- Data %>% mutate(
  Phase2 = ifelse(Year %in% c(2008:2012), 1, 0),
  Phase3 = ifelse(Year %in% c(2013:2018), 1, 0),
  PhaseName = ifelse(Year %in% c(2008:2012), "Phase2", ifelse(Year %in% c(2013:2018), "Phase3", "Phase1")),
  Y2008 = ifelse(Year == 2008, 1, 0),
  Y2009 = ifelse(Year == 2009, 1, 0),
  IsCombustionSector = ifelse(Main_Activity_Code %in% combustion_codes, 1, 0),
  Emission = Total_Verified_Emissions/1000000,
  Allow = Allowance_Allocation/1000000,
  Ratio = ifelse(!is.finite(Allowance_Allocation/Total_Verified_Emissions), NA, 
                  ifelse(!is.na(Allowance_Allocation/Total_Verified_Emissions) & (Allowance_Allocation/Total_Verified_Emissions) < 0,
                         NA, Allowance_Allocation/Total_Verified_Emissions)),
  Phase2_Allow = Phase2 * Allow,
  Phase3_Allow = Phase3 * Allow,
  Combustion_Allow = IsCombustionSector * Allow,
  Y2008_Allow = Y2008 * Allow,
  Y2009_Allow = Y2009 * Allow
)

# Writing to csv
write.csv2(Data, file = "Data.csv", row.names = FALSE, na = "")

# Cleaning up the environment
remove(GDP, FDI, Labor, tempData, tempMatrix, tempYears, i, j, year, years, combustion_codes, countries_to_drop, '%ni%')