# Master's thesis
# Study of casual relationships between greenhouse gas emission levels based on the EU ETS database
# Piotr Gretszel
# AGH UST, Faculty of Management, Informational Technologies and Econometrics

# Reading libraries
set.seed(1920)
library(plm)
library(tseries)
library(lmtest)
library(panelvar)
library(dplyr)
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

# Data preprocessing
countries <- unique(Data$Compliance_Country)
years <- unique(Data$Year)

Data.all <- Data %>%
  group_by(Compliance_Country, Year) %>%
  summarise(
    Emission = mean(Emission, na.rm = TRUE),
    Allow = mean(Allow, na.rm = TRUE),
    Phase2_Allow = mean(Phase2_Allow, na.rm = TRUE),
    Phase3_Allow = mean(Phase3_Allow, na.rm = TRUE),
    Y2009 = mean(Y2009, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pdata.frame(index = c("Compliance_Country", "Year"))

Data.combustion <- Data[which(Data$IsCombustionSector == 1),] %>%
  group_by(Compliance_Country, Year) %>%
  summarise(
    Emission = mean(Emission, na.rm = TRUE),
    Allow = mean(Allow, na.rm = TRUE),
    Phase2_Allow = mean(Phase2_Allow, na.rm = TRUE),
    Phase3_Allow = mean(Phase3_Allow, na.rm = TRUE),
    Y2009 = mean(Y2009, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pdata.frame(index = c("Compliance_Country", "Year"))

Data.other <- Data[which(Data$IsCombustionSector == 0),] %>%
  group_by(Compliance_Country, Year) %>%
  summarise(
    Emission = mean(Emission, na.rm = TRUE),
    Allow = mean(Allow, na.rm = TRUE),
    Phase2_Allow = mean(Phase2_Allow, na.rm = TRUE),
    Phase3_Allow = mean(Phase3_Allow, na.rm = TRUE),
    Y2009 = mean(Y2009, na.rm = TRUE)
  ) %>%
  ungroup %>%
  pdata.frame(index = c("Compliance_Country", "Year"))

# Pooled regression
formula <- Emission ~ lag(Emission, 1) + Allow + Phase2_Allow + Phase3_Allow + Y2009
results <- matrix(data = NA, nrow = 1000, ncol = 5)
effects <- matrix(data = NA, nrow = 1000, ncol = 4)

model_pooled_all <- plm(formula, data = Data.all, model = "pooling")
j <- 1
k <- j + length(rownames(summary(model_pooled_all)$coefficients)) - 1
m <- 1
n <- 1
results[1:k, 3] <- rownames(summary(model_pooled_all)$coefficients)
results[1:k, 4] <- summary(model_pooled_all)$coefficients[,1]
results[1:k, 5] <- summary(model_pooled_all)$coefficients[,4]
j <- j + length(rownames(summary(model_pooled_all)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_pooled_all)$r.squared[1]
results[j, 5] <- summary(model_pooled_all)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_pooled_all)$fstatistic$statistic
results[j, 5] <- summary(model_pooled_all)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "L-M test (two-ways)"
results[j, 4] <- plmtest(model_pooled_all, c("twoways"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_all, c("twoways"), type = ("bp"))$p.value
j <- j + 1
results[j, 3] <- "L-M test (time)"
results[j, 4] <- plmtest(model_pooled_all, c("time"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_all, c("time"), type = ("bp"))$p.value
j <- j + 1
results[j, 3] <- "L-M test (individual)"
results[j, 4] <- plmtest(model_pooled_all, c("individual"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_all, c("individual"), type = ("bp"))$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

model_pooled_combustion <- plm(formula, data = Data.combustion, model = "pooling")
k <- j + length(rownames(summary(model_pooled_combustion)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_pooled_combustion)$coefficients) 
results[j:k, 4] <- summary(model_pooled_combustion)$coefficients[,1]
results[j:k, 5] <- summary(model_pooled_combustion)$coefficients[,4]
j <- j + length(rownames(summary(model_pooled_combustion)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_pooled_combustion)$r.squared[1]
results[j, 5] <- summary(model_pooled_combustion)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_pooled_combustion)$fstatistic$statistic
results[j, 5] <- summary(model_pooled_combustion)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "L-M test (two-ways)"
results[j, 4] <- plmtest(model_pooled_combustion, c("twoways"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_combustion, c("twoways"), type = ("bp"))$p.value
j <- j + 1
results[j, 3] <- "L-M test (time)"
results[j, 4] <- plmtest(model_pooled_combustion, c("time"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_combustion, c("time"), type = ("bp"))$p.value
j <- j + 1
results[j, 3] <- "L-M test (individual)"
results[j, 4] <- plmtest(model_pooled_combustion, c("individual"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_combustion, c("individual"), type = ("bp"))$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

model_pooled_other <- plm(formula, data = Data.other, model = "pooling")
k <- j + length(rownames(summary(model_pooled_other)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_pooled_other)$coefficients) 
results[j:k, 4] <- summary(model_pooled_other)$coefficients[,1]
results[j:k, 5] <- summary(model_pooled_other)$coefficients[,4]
j <- j + length(rownames(summary(model_pooled_other)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_pooled_other)$r.squared[1]
results[j, 5] <- summary(model_pooled_other)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_pooled_other)$fstatistic$statistic
results[j, 5] <- summary(model_pooled_other)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "L-M test (two-ways)"
results[j, 4] <- plmtest(model_pooled_other, c("twoways"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_other, c("twoways"), type = ("bp"))$p.value
j <- j + 1
results[j, 3] <- "L-M test (time)"
results[j, 4] <- plmtest(model_pooled_other, c("time"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_other, c("time"), type = ("bp"))$p.value
j <- j + 1
results[j, 3] <- "L-M test (individual)"
results[j, 4] <- plmtest(model_pooled_other, c("individual"), type = ("bp"))$statistic
results[j, 5] <- plmtest(model_pooled_other, c("individual"), type = ("bp"))$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "Pooled"
j <- j + 1
m <- j
n <- j

# One-way fixed individual effects model
model_fe_all_ind <- plm(formula, data = Data.all, model = "within", effect = "individual")
k <- j + length(rownames(summary(model_fe_all_ind)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_all_ind)$coefficients)
results[j:k, 4] <- summary(model_fe_all_ind)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_all_ind)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_all_ind)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_all_ind)$r.squared[1]
results[j, 5] <- summary(model_fe_all_ind)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_all_ind)$fstatistic$statistic
results[j, 5] <- summary(model_fe_all_ind)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (individual)"
results[j, 4] <- pFtest(model_fe_all_ind, model_pooled_all)$statistic
results[j, 5] <- pFtest(model_fe_all_ind, model_pooled_all)$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

a <- 1
b <- a + length(names(fixef(model_fe_all_ind))) - 1
effects[a:b, 1] <- "FE (individual)"
effects[a:b, 2] <- "All"
effects[a:b, 3] <- names(fixef(model_fe_all_ind))
effects[a:b, 4] <- fixef(model_fe_all_ind)
a <- a + length(names(fixef(model_fe_all_ind)))

model_fe_combustion_ind <- plm(formula, data = Data.combustion, model = "within", effect = "individual")
k <- j + length(rownames(summary(model_fe_combustion_ind)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_combustion_ind)$coefficients)
results[j:k, 4] <- summary(model_fe_combustion_ind)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_combustion_ind)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_combustion_ind)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_combustion_ind)$r.squared[1]
results[j, 5] <- summary(model_fe_combustion_ind)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_combustion_ind)$fstatistic$statistic
results[j, 5] <- summary(model_fe_combustion_ind)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (individual)"
results[j, 4] <- pFtest(model_fe_combustion_ind, model_pooled_combustion)$statistic
results[j, 5] <- pFtest(model_fe_combustion_ind, model_pooled_combustion)$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

b <- a + length(names(fixef(model_fe_combustion_ind))) - 1
effects[a:b, 1] <- "FE (individual)"
effects[a:b, 2] <- "Combustion"
effects[a:b, 3] <- names(fixef(model_fe_combustion_ind))
effects[a:b, 4] <- fixef(model_fe_combustion_ind)
a <- a + length(names(fixef(model_fe_combustion_ind)))

model_fe_other_ind <- plm(formula, data = Data.other, model = "within", effect = "individual")
k <- j + length(rownames(summary(model_fe_other_ind)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_other_ind)$coefficients)
results[j:k, 4] <- summary(model_fe_other_ind)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_other_ind)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_other_ind)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_other_ind)$r.squared[1]
results[j, 5] <- summary(model_fe_other_ind)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_other_ind)$fstatistic$statistic
results[j, 5] <- summary(model_fe_other_ind)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (individual)"
results[j, 4] <- pFtest(model_fe_other_ind, model_pooled_other)$statistic
results[j, 5] <- pFtest(model_fe_other_ind, model_pooled_other)$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "FE (individual)"
j <- j + 1
m <- j
n <- j

b <- a + length(names(fixef(model_fe_other_ind))) - 1
effects[a:b, 1] <- "FE (individual)"
effects[a:b, 2] <- "Other"
effects[a:b, 3] <- names(fixef(model_fe_other_ind))
effects[a:b, 4] <- fixef(model_fe_other_ind)
a <- a + length(names(fixef(model_fe_other_ind)))

# One-way fixed time effects model
model_fe_all_time <- plm(formula, data = Data.all, model = "within", effect = "time")
k <- j + length(rownames(summary(model_fe_all_time)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_all_time)$coefficients)
results[j:k, 4] <- summary(model_fe_all_time)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_all_time)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_all_time)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_all_time)$r.squared[1]
results[j, 5] <- summary(model_fe_all_time)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_all_time)$fstatistic$statistic
results[j, 5] <- summary(model_fe_all_time)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (time)"
results[j, 4] <- pFtest(model_fe_all_time, model_pooled_all)$statistic
results[j, 5] <- pFtest(model_fe_all_time, model_pooled_all)$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

b <- a + length(names(fixef(model_fe_all_time))) - 1
effects[a:b, 1] <- "FE (time)"
effects[a:b, 2] <- "All"
effects[a:b, 3] <- names(fixef(model_fe_all_time))
effects[a:b, 4] <- fixef(model_fe_all_time)
a <- a + length(names(fixef(model_fe_all_time)))

model_fe_combustion_time <- plm(formula, data = Data.combustion, model = "within", effect = "time")
k <- j + length(rownames(summary(model_fe_combustion_time)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_combustion_time)$coefficients)
results[j:k, 4] <- summary(model_fe_combustion_time)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_combustion_time)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_combustion_time)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_combustion_time)$r.squared[1]
results[j, 5] <- summary(model_fe_combustion_time)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_combustion_time)$fstatistic$statistic
results[j, 5] <- summary(model_fe_combustion_time)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (time)"
results[j, 4] <- pFtest(model_fe_combustion_time, model_pooled_combustion)$statistic
results[j, 5] <- pFtest(model_fe_combustion_time, model_pooled_combustion)$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

b <- a + length(names(fixef(model_fe_combustion_time))) - 1
effects[a:b, 1] <- "FE (time)"
effects[a:b, 2] <- "Combustion"
effects[a:b, 3] <- names(fixef(model_fe_combustion_time))
effects[a:b, 4] <- fixef(model_fe_combustion_time)
a <- a + length(names(fixef(model_fe_combustion_time)))

model_fe_other_time <- plm(formula, data = Data.other, model = "within", effect = "time")
k <- j + length(rownames(summary(model_fe_other_time)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_other_time)$coefficients)
results[j:k, 4] <- summary(model_fe_other_time)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_other_time)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_other_time)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_other_time)$r.squared[1]
results[j, 5] <- summary(model_fe_other_time)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_other_time)$fstatistic$statistic
results[j, 5] <- summary(model_fe_other_time)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (time)"
results[j, 4] <- pFtest(model_fe_other_time, model_pooled_other)$statistic
results[j, 5] <- pFtest(model_fe_other_time, model_pooled_other)$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "FE (time)"
j <- j + 1
m <- j
n <- j

b <- a + length(names(fixef(model_fe_other_time))) - 1
effects[a:b, 1] <- "FE (time)"
effects[a:b, 2] <- "Other"
effects[a:b, 3] <- names(fixef(model_fe_other_time))
effects[a:b, 4] <- fixef(model_fe_other_time)
a <- a + length(names(fixef(model_fe_other_time)))

# Two-ways fixed effects model
model_fe_all_tw <- plm(formula, data = Data.all, model = "within", effect = "twoways")
k <- j + length(rownames(summary(model_fe_all_tw)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_all_tw)$coefficients)
results[j:k, 4] <- summary(model_fe_all_tw)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_all_tw)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_all_tw)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_all_tw)$r.squared[1]
results[j, 5] <- summary(model_fe_all_tw)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_all_tw)$fstatistic$statistic
results[j, 5] <- summary(model_fe_all_tw)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (two-ways)"
results[j, 4] <- pFtest(model_fe_all_tw, model_pooled_all)$statistic
results[j, 5] <- pFtest(model_fe_all_tw, model_pooled_all)$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

b <- a + length(names(fixef(model_fe_all_tw, effect = "individual"))) - 1
effects[a:b, 1] <- "Two-ways FE (individual)"
effects[a:b, 2] <- "All"
effects[a:b, 3] <- names(fixef(model_fe_all_tw, effect = "individual"))
effects[a:b, 4] <- fixef(model_fe_all_tw, effect = "individual")
a <- a + length(names(fixef(model_fe_all_tw, effect = "individual")))
b <- a + length(names(fixef(model_fe_all_tw, effect = "time"))) - 1
effects[a:b, 1] <- "Two-ways FE (time)"
effects[a:b, 2] <- "All"
effects[a:b, 3] <- names(fixef(model_fe_all_tw, effect = "time"))
effects[a:b, 4] <- fixef(model_fe_all_tw, effect = "time")
a <- a + length(names(fixef(model_fe_all_tw, effect = "time")))

model_fe_combustion_tw <- plm(formula, data = Data.combustion, model = "within", effect = "twoways")
k <- j + length(rownames(summary(model_fe_combustion_tw)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_combustion_tw)$coefficients)
results[j:k, 4] <- summary(model_fe_combustion_tw)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_combustion_tw)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_combustion_tw)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_combustion_tw)$r.squared[1]
results[j, 5] <- summary(model_fe_combustion_tw)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_combustion_tw)$fstatistic$statistic
results[j, 5] <- summary(model_fe_combustion_tw)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (two-ways)"
results[j, 4] <- pFtest(model_fe_combustion_tw, model_pooled_all)$statistic
results[j, 5] <- pFtest(model_fe_combustion_tw, model_pooled_all)$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

b <- a + length(names(fixef(model_fe_combustion_tw, effect = "individual"))) - 1
effects[a:b, 1] <- "Two-ways FE (individual)"
effects[a:b, 2] <- "Combustion"
effects[a:b, 3] <- names(fixef(model_fe_combustion_tw, effect = "individual"))
effects[a:b, 4] <- fixef(model_fe_combustion_tw, effect = "individual")
a <- a + length(names(fixef(model_fe_combustion_tw, effect = "individual")))
b <- a + length(names(fixef(model_fe_combustion_tw, effect = "time"))) - 1
effects[a:b, 1] <- "Two-ways FE (time)"
effects[a:b, 2] <- "Combustion"
effects[a:b, 3] <- names(fixef(model_fe_combustion_tw, effect = "time"))
effects[a:b, 4] <- fixef(model_fe_combustion_tw, effect = "time")
a <- a + length(names(fixef(model_fe_combustion_tw, effect = "time")))

model_fe_other_tw <- plm(formula, data = Data.other, model = "within", effect = "twoways")
k <- j + length(rownames(summary(model_fe_other_tw)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_fe_other_tw)$coefficients)
results[j:k, 4] <- summary(model_fe_other_tw)$coefficients[,1]
results[j:k, 5] <- summary(model_fe_other_tw)$coefficients[,4]
j <- j + length(rownames(summary(model_fe_other_tw)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_fe_other_tw)$r.squared[1]
results[j, 5] <- summary(model_fe_other_tw)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_fe_other_tw)$fstatistic$statistic
results[j, 5] <- summary(model_fe_other_tw)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "F test (two-ways)"
results[j, 4] <- pFtest(model_fe_other_tw, model_pooled_all)$statistic
results[j, 5] <- pFtest(model_fe_other_tw, model_pooled_all)$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "FE (two-ways)"
j <- j + 1
m <- j
n <- j

b <- a + length(names(fixef(model_fe_other_tw, effect = "individual"))) - 1
effects[a:b, 1] <- "Two-ways FE (individual)"
effects[a:b, 2] <- "Other"
effects[a:b, 3] <- names(fixef(model_fe_other_tw, effect = "individual"))
effects[a:b, 4] <- fixef(model_fe_other_tw, effect = "individual")
a <- a + length(names(fixef(model_fe_other_tw, effect = "individual")))
b <- a + length(names(fixef(model_fe_other_tw, effect = "time"))) - 1
effects[a:b, 1] <- "Two-ways FE (time)"
effects[a:b, 2] <- "Other"
effects[a:b, 3] <- names(fixef(model_fe_other_tw, effect = "time"))
effects[a:b, 4] <- fixef(model_fe_other_tw, effect = "time")
a <- a + length(names(fixef(model_fe_other_tw, effect = "time")))

# Random effects model
model_re_all <- plm(formula, data = Data.all, model = "random")
k <- j + length(rownames(summary(model_re_all)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_re_all)$coefficients)
results[j:k, 4] <- summary(model_re_all)$coefficients[,1]
results[j:k, 5] <- summary(model_re_all)$coefficients[,4]
j <- j + length(rownames(summary(model_re_all)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_re_all)$r.squared[1]
results[j, 5] <- summary(model_re_all)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_re_all)$fstatistic$statistic
results[j, 5] <- summary(model_re_all)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE time)"
results[j, 4] <- phtest(model_fe_all_time, model_re_all)$statistic
results[j, 5] <- phtest(model_fe_all_time, model_re_all)$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE individual)"
results[j, 4] <- phtest(model_fe_all_ind, model_re_all)$statistic
results[j, 5] <- phtest(model_fe_all_ind, model_re_all)$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE two-ways)"
results[j, 4] <- phtest(model_fe_all_tw, model_re_all)$statistic
results[j, 5] <- phtest(model_fe_all_tw, model_re_all)$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

model_re_combustion <- plm(formula, data = Data.combustion, model = "random")
k <- j + length(rownames(summary(model_re_combustion)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_re_combustion)$coefficients)
results[j:k, 4] <- summary(model_re_combustion)$coefficients[,1]
results[j:k, 5] <- summary(model_re_combustion)$coefficients[,4]
j <- j + length(rownames(summary(model_re_combustion)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_re_combustion)$r.squared[1]
results[j, 5] <- summary(model_re_combustion)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_re_combustion)$fstatistic$statistic
results[j, 5] <- summary(model_re_combustion)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE time)"
results[j, 4] <- phtest(model_fe_combustion_time, model_re_combustion)$statistic
results[j, 5] <- phtest(model_fe_combustion_time, model_re_combustion)$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE individual)"
results[j, 4] <- phtest(model_fe_combustion_ind, model_re_combustion)$statistic
results[j, 5] <- phtest(model_fe_combustion_ind, model_re_combustion)$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE two-ways)"
results[j, 4] <- phtest(model_fe_combustion_tw, model_re_combustion)$statistic
results[j, 5] <- phtest(model_fe_combustion_tw, model_re_combustion)$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

model_re_other <- plm(formula, data = Data.other, model = "random")
k <- j + length(rownames(summary(model_re_other)$coefficients)) - 1
results[j:k, 3] <- rownames(summary(model_re_other)$coefficients)
results[j:k, 4] <- summary(model_re_other)$coefficients[,1]
results[j:k, 5] <- summary(model_re_other)$coefficients[,4]
j <- j + length(rownames(summary(model_re_other)$coefficients))
results[j, 3] <- "R^2"
results[j, 4] <- summary(model_re_other)$r.squared[1]
results[j, 5] <- summary(model_re_other)$r.squared[2]
j <- j + 1
results[j, 3] <- "F test"
results[j, 4] <- summary(model_re_other)$fstatistic$statistic
results[j, 5] <- summary(model_re_other)$fstatistic$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE time)"
results[j, 4] <- phtest(model_fe_other_time, model_re_other)$statistic
results[j, 5] <- phtest(model_fe_other_time, model_re_other)$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE individual)"
results[j, 4] <- phtest(model_fe_other_ind, model_re_other)$statistic
results[j, 5] <- phtest(model_fe_other_ind, model_re_other)$p.value
j <- j + 1
results[j, 3] <- "Hausman test (FE two-ways)"
results[j, 4] <- phtest(model_fe_other_tw, model_re_other)$statistic
results[j, 5] <- phtest(model_fe_other_tw, model_re_other)$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "RE"
j <- j + 1
m <- j
n <- j

# Testing of serial correlation between groups
results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_pooled_all, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_pooled_all, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_pooled_all, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_pooled_all, test = c("cd"))$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_pooled_combustion, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_pooled_combustion, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_pooled_combustion, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_pooled_combustion, test = c("cd"))$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_pooled_other, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_pooled_other, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_pooled_other, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_pooled_other, test = c("cd"))$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "Pooled"
j <- j + 1
m <- j
n <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_all_time, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_all_time, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_all_time, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_all_time, test = c("cd"))$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_combustion_time, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_combustion_time, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_combustion_time, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_combustion_time, test = c("cd"))$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_other_time, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_other_time, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_other_time, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_other_time, test = c("cd"))$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "FE (time)"
j <- j + 1
m <- j
n <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_all_ind, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_all_ind, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_all_ind, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_all_ind, test = c("cd"))$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_combustion_ind, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_combustion_ind, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_combustion_ind, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_combustion_ind, test = c("cd"))$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_other_ind, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_other_ind, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_other_ind, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_other_ind, test = c("cd"))$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "FE (individual)"
j <- j + 1
m <- j
n <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_all_tw, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_all_tw, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_all_tw, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_all_tw, test = c("cd"))$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_combustion_tw, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_combustion_tw, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_combustion_tw, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_combustion_tw, test = c("cd"))$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_fe_other_tw, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_fe_other_tw, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_fe_other_tw, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_fe_other_tw, test = c("cd"))$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "FE (two-ways)"
j <- j + 1
m <- j
n <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_re_all, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_re_all, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_re_all, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_re_all, test = c("cd"))$p.value
results[m:j, 2] <- "All"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_re_combustion, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_re_combustion, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_re_combustion, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_re_combustion, test = c("cd"))$p.value
results[m:j, 2] <- "Combustion"
j <- j + 1
m <- j

results[j, 3] <- "BP LM test"
results[j, 4] <- pcdtest(model_re_other, test = c("lm"))$statistic
results[j, 5] <- pcdtest(model_re_other, test = c("lm"))$p.value
j <- j + 1
results[j, 3] <- "Pesaran CD test"
results[j, 4] <- pcdtest(model_re_other, test = c("cd"))$statistic
results[j, 5] <- pcdtest(model_re_other, test = c("cd"))$p.value
results[m:j, 2] <- "Other"
results[n:j, 1] <- "RE"
j <- j + 1
m <- j
n <- j

# Testing of serial correlation in time
results[j, 2] <- "All"
results[j, 4] <- pbgtest(model_pooled_all)$statistic
results[j, 5] <- pbgtest(model_pooled_all)$p.value
j <- j + 1

results[j, 2] <- "Combustion"
results[j, 4] <- pbgtest(model_pooled_combustion)$statistic
results[j, 5] <- pbgtest(model_pooled_combustion)$p.value
j <- j + 1

results[j, 2] <- "Other"
results[j, 4] <- pbgtest(model_pooled_other)$statistic
results[j, 5] <- pbgtest(model_pooled_other)$p.value
results[m:j, 1] <- "Pooled"
j <- j + 1
m <- j

results[j, 2] <- "All"
results[j, 4] <- pbgtest(model_fe_all_time)$statistic
results[j, 5] <- pbgtest(model_fe_all_time)$p.value
j <- j + 1

results[j, 2] <- "Combustion"
results[j, 4] <- pbgtest(model_fe_combustion_time)$statistic
results[j, 5] <- pbgtest(model_fe_combustion_time)$p.value
j <- j + 1

results[j, 2] <- "Other"
results[j, 4] <- pbgtest(model_fe_other_time)$statistic
results[j, 5] <- pbgtest(model_fe_other_time)$p.value
results[m:j, 1] <- "FE (time)"
j <- j + 1
m <- j

results[j, 2] <- "All"
results[j, 4] <- pbgtest(model_fe_all_ind)$statistic
results[j, 5] <- pbgtest(model_fe_all_ind)$p.value
j <- j + 1

results[j, 2] <- "Combustion"
results[j, 4] <- pbgtest(model_fe_combustion_ind)$statistic
results[j, 5] <- pbgtest(model_fe_combustion_ind)$p.value
j <- j + 1

results[j, 2] <- "Other"
results[j, 4] <- pbgtest(model_fe_other_ind)$statistic
results[j, 5] <- pbgtest(model_fe_other_ind)$p.value
results[j:m, 1] <- "FE (individual)"
j <- j + 1
m <- j

results[j, 2] <- "All"
results[j, 4] <- pbgtest(model_fe_all_tw)$statistic
results[j, 5] <- pbgtest(model_fe_all_tw)$p.value
j <- j + 1

results[j, 2] <- "Combustion"
results[j, 4] <- pbgtest(model_fe_combustion_tw)$statistic
results[j, 5] <- pbgtest(model_fe_combustion_tw)$p.value
j <- j + 1

results[j, 2] <- "Other"
results[j, 4] <- pbgtest(model_fe_other_tw)$statistic
results[j, 5] <- pbgtest(model_fe_other_tw)$p.value
results[m:j, 1] <- "FE (two-ways)"
j <- j + 1
m <- j

results[j, 2] <- "All"
results[j, 4] <- pbgtest(model_re_all)$statistic
results[j, 5] <- pbgtest(model_re_all)$p.value
j <- j + 1

results[j, 2] <- "Combustion"
results[j, 4] <- pbgtest(model_re_combustion)$statistic
results[j, 5] <- pbgtest(model_re_combustion)$p.value
j <- j + 1

results[j, 2] <- "Other"
results[j, 4] <- pbgtest(model_re_other)$statistic
results[j, 5] <- pbgtest(model_re_other)$p.value
results[m:j, 1] <- "RE"
results[n:j, 3] <- "B-G/Wooldrige test"
j <- j + 1
m <- j
n <- j

# Testing existance of unit root
results[j, 4] <- adf.test(Data.all$Emission)$statistic
results[j, 5] <- adf.test(Data.all$Emission)$p.value
results[j, 2] <- "All"
j <- j + 1

results[j, 4] <- adf.test(Data.combustion$Emission)$statistic
results[j, 5] <- adf.test(Data.combustion$Emission)$p.value
results[j, 2] <- "Combustion"
j <- j + 1

results[j, 4] <- adf.test(Data.other$Emission)$statistic
results[j, 5] <- adf.test(Data.other$Emission)$p.value
results[j, 2] <- "Other"
results[m:j, 3] <- "ADF test (Emission)"
results[n:j, 1] <- "-"
j <- j + 1
m <- j
n <- j

# Testing heteroskedasticity
results[j, 2] <- "All"
results[j, 4] <- bptest(formula, data = Data.all, studentize = FALSE)$statistic
results[j, 5] <- bptest(formula, data = Data.all, studentize = FALSE)$p.value
j <- j + 1

results[j, 2] <- "Combustion"
results[j, 4] <- bptest(formula, data = Data.combustion, studentize = FALSE)$statistic
results[j, 5] <- bptest(formula, data = Data.combustion, studentize = FALSE)$p.value
j <- j + 1

results[j, 2] <- "Other"
results[j, 4] <- bptest(formula, data = Data.other, studentize = FALSE)$statistic
results[j, 5] <- bptest(formula, data = Data.other, studentize = FALSE)$p.value
results[j, 2] <- "All"
results[m:j, 3] <- "BP test"
j <- j + 1
m <- j

results[j, 2] <- "All"
results[j, 4] <- bptest(formula, data = Data.all, studentize = TRUE)$statistic
results[j, 5] <- bptest(formula, data = Data.all, studentize = TRUE)$p.value
j <- j + 1

results[j, 2] <- "Combustion"
results[j, 4] <- bptest(formula, data = Data.combustion, studentize = TRUE)$statistic
results[j, 5] <- bptest(formula, data = Data.combustion, studentize = TRUE)$p.value
j <- j + 1

results[j, 2] <- "Other"
results[j, 4] <- bptest(formula, data = Data.other, studentize = TRUE)$statistic
results[j, 5] <- bptest(formula, data = Data.other, studentize = TRUE)$p.value
results[n:j, 1] <- "-"
results[m:j, 3] <- "BP test (stud)"
j <- j + 1
m <- j
n <- j

# Robust coefficients
k <- j + length(coeftest(model_pooled_all, vcovHC(model_pooled_all, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_pooled_all, vcovHC(model_pooled_all, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_pooled_all, vcovHC(model_pooled_all, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_pooled_all, vcovHC(model_pooled_all, method = "arellano"))[,4]
j <- j + length(coeftest(model_pooled_all, vcovHC(model_pooled_all, method = "arellano"))[,1])

k <- j + length(coeftest(model_pooled_all, vcovHC(model_pooled_all, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_pooled_all, vcovHC(model_pooled_all, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_pooled_all, vcovHC(model_pooled_all, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_pooled_all, vcovHC(model_pooled_all, type = "HC3"))[,4]
results[m:k, 2] <- "All"
j <- j + length(coeftest(model_pooled_all, vcovHC(model_pooled_all, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, method = "arellano"))[,4]
j <- j + length(coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, method = "arellano"))[,1])

k <- j + length(coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, type = "HC3"))[,4]
results[m:k, 2] <- "Combustion"
j <- j + length(coeftest(model_pooled_combustion, vcovHC(model_pooled_combustion, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_pooled_other, vcovHC(model_pooled_other, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_pooled_other, vcovHC(model_pooled_other, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_pooled_other, vcovHC(model_pooled_other, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_pooled_other, vcovHC(model_pooled_other, method = "arellano"))[,4]
j <- j + length(coeftest(model_pooled_other, vcovHC(model_pooled_other, method = "arellano"))[,1])

k <- j + length(coeftest(model_pooled_other, vcovHC(model_pooled_other, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_pooled_other, vcovHC(model_pooled_other, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_pooled_other, vcovHC(model_pooled_other, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_pooled_other, vcovHC(model_pooled_other, type = "HC3"))[,4]
results[m:k, 2] <- "Other"
results[n:k, 1] <- "Pooled"
j <- j + length(coeftest(model_pooled_other, vcovHC(model_pooled_other, type = "HC3"))[,1])
m <- j
n <- j

k <- j + length(coeftest(model_fe_all_time, vcovHC(model_fe_all_time, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_all_time, vcovHC(model_pooled_all, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_all_time, vcovHC(model_fe_all_time, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_all_time, vcovHC(model_fe_all_time, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_all_time, vcovHC(model_fe_all_time, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_all_time, vcovHC(model_fe_all_time, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_all_time, vcovHC(model_pooled_all, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_all_time, vcovHC(model_fe_all_time, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_all_time, vcovHC(model_fe_all_time, type = "HC3"))[,4]
results[m:k, 2] <- "All"
j <- j + length(coeftest(model_fe_all_time, vcovHC(model_fe_all_time, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, type = "HC3"))[,4]
results[m:k, 2] <- "Combustion"
j <- j + length(coeftest(model_fe_combustion_time, vcovHC(model_fe_combustion_time, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_fe_other_time, vcovHC(model_fe_other_time, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_other_time, vcovHC(model_fe_other_time, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_other_time, vcovHC(model_fe_other_time, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_other_time, vcovHC(model_fe_other_time, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_other_time, vcovHC(model_fe_other_time, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_other_time, vcovHC(model_fe_other_time, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_other_time, vcovHC(model_pooled_other, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_other_time, vcovHC(model_pooled_other, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_other_time, vcovHC(model_fe_other_time, type = "HC3"))[,4]
results[m:k, 2] <- "Other"
results[n:k, 1] <- "FE (time)"
j <- j + length(coeftest(model_fe_other_time, vcovHC(model_fe_other_time, type = "HC3"))[,1])
m <- j
n <- j

k <- j + length(coeftest(model_fe_all_ind, vcovHC(model_fe_all_ind, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_all_ind, vcovHC(model_pooled_all, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_all_ind, vcovHC(model_fe_all_ind, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_all_ind, vcovHC(model_fe_all_ind, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_all_ind, vcovHC(model_fe_all_ind, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_all_ind, vcovHC(model_fe_all_ind, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_all_ind, vcovHC(model_pooled_all, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_all_ind, vcovHC(model_fe_all_ind, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_all_ind, vcovHC(model_fe_all_ind, type = "HC3"))[,4]
results[m:k, 2] <- "All"
j <- j + length(coeftest(model_fe_all_ind, vcovHC(model_fe_all_ind, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, type = "HC3"))[,4]
results[m:k, 2] <- "Combustion"
j <- j + length(coeftest(model_fe_combustion_ind, vcovHC(model_fe_combustion_ind, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_fe_other_ind, vcovHC(model_fe_other_ind, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_other_ind, vcovHC(model_fe_other_ind, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_other_ind, vcovHC(model_fe_other_ind, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_other_ind, vcovHC(model_fe_other_ind, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_other_ind, vcovHC(model_fe_other_ind, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_other_ind, vcovHC(model_fe_other_ind, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_other_ind, vcovHC(model_pooled_other, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_other_ind, vcovHC(model_pooled_other, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_other_ind, vcovHC(model_fe_other_ind, type = "HC3"))[,4]
results[m:k, 2] <- "Other"
results[n:k, 1] <- "FE (individual)"
j <- j + length(coeftest(model_fe_other_ind, vcovHC(model_fe_other_ind, type = "HC3"))[,1])
m <- j
n <- j

k <- j + length(coeftest(model_fe_all_tw, vcovHC(model_fe_all_tw, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_all_tw, vcovHC(model_pooled_all, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_all_tw, vcovHC(model_fe_all_tw, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_all_tw, vcovHC(model_fe_all_tw, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_all_tw, vcovHC(model_fe_all_tw, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_all_tw, vcovHC(model_fe_all_tw, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_all_tw, vcovHC(model_pooled_all, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_all_tw, vcovHC(model_fe_all_tw, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_all_tw, vcovHC(model_fe_all_tw, type = "HC3"))[,4]
results[m:k, 2] <- "All"
j <- j + length(coeftest(model_fe_all_tw, vcovHC(model_fe_all_tw, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, type = "HC3"))[,4]
results[m:k, 2] <- "Combustion"
j <- j + length(coeftest(model_fe_combustion_tw, vcovHC(model_fe_combustion_tw, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_fe_other_tw, vcovHC(model_fe_other_tw, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_other_tw, vcovHC(model_fe_other_tw, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_fe_other_tw, vcovHC(model_fe_other_tw, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_fe_other_tw, vcovHC(model_fe_other_tw, method = "arellano"))[,4]
j <- j + length(coeftest(model_fe_other_tw, vcovHC(model_fe_other_tw, method = "arellano"))[,1])

k <- j + length(coeftest(model_fe_other_tw, vcovHC(model_fe_other_tw, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_fe_other_tw, vcovHC(model_pooled_other, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_fe_other_tw, vcovHC(model_pooled_other, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_fe_other_tw, vcovHC(model_fe_other_tw, type = "HC3"))[,4]
results[m:k, 2] <- "Other"
results[n:k, 1] <- "FE (two-ways)"
j <- j + length(coeftest(model_fe_other_tw, vcovHC(model_fe_other_tw, type = "HC3"))[,1])
m <- j
n <- j

k <- j + length(coeftest(model_re_all, vcovHC(model_re_all, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_re_all, vcovHC(model_pooled_all, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_re_all, vcovHC(model_re_all, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_re_all, vcovHC(model_re_all, method = "arellano"))[,4]
j <- j + length(coeftest(model_re_all, vcovHC(model_re_all, method = "arellano"))[,1])

k <- j + length(coeftest(model_re_all, vcovHC(model_re_all, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_re_all, vcovHC(model_pooled_all, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_re_all, vcovHC(model_re_all, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_re_all, vcovHC(model_re_all, type = "HC3"))[,4]
results[m:k, 2] <- "All"
j <- j + length(coeftest(model_re_all, vcovHC(model_re_all, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_re_combustion, vcovHC(model_re_combustion, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_re_combustion, vcovHC(model_re_combustion, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_re_combustion, vcovHC(model_re_combustion, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_re_combustion, vcovHC(model_re_combustion, method = "arellano"))[,4]
j <- j + length(coeftest(model_re_combustion, vcovHC(model_re_combustion, method = "arellano"))[,1])

k <- j + length(coeftest(model_re_combustion, vcovHC(model_re_combustion, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_re_combustion, vcovHC(model_re_combustion, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_re_combustion, vcovHC(model_re_combustion, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_re_combustion, vcovHC(model_re_combustion, type = "HC3"))[,4]
results[m:k, 2] <- "Combustion"
j <- j + length(coeftest(model_re_combustion, vcovHC(model_re_combustion, type = "HC3"))[,1])
m <- j

k <- j + length(coeftest(model_re_other, vcovHC(model_re_other, method = "arellano"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_re_other, vcovHC(model_re_other, method = "arellano"))), " [Arellano]")
results[j:k, 4] <- coeftest(model_re_other, vcovHC(model_re_other, method = "arellano"))[,1]
results[j:k, 5] <- coeftest(model_re_other, vcovHC(model_re_other, method = "arellano"))[,4]
j <- j + length(coeftest(model_re_other, vcovHC(model_re_other, method = "arellano"))[,1])

k <- j + length(coeftest(model_re_other, vcovHC(model_re_other, type = "HC3"))[,1]) - 1
results[j:k, 3] <- paste0(rownames(coeftest(model_re_other, vcovHC(model_re_other, type = "HC3"))), " [HC3]")
results[j:k, 4] <- coeftest(model_re_other, vcovHC(model_re_other, type = "HC3"))[,1]
results[j:k, 5] <- coeftest(model_re_other, vcovHC(model_re_other, type = "HC3"))[,4]
results[m:k, 2] <- "Other"
results[n:k, 1] <- "RE"
j <- j + length(coeftest(model_re_other, vcovHC(model_re_other, type = "HC3"))[,1])
m <- j
n <- j

# Dynamic panel var models (All phases, dependent - Emission)
model_pvar_all <- pvargmm(
  dependent_vars = c("Emission"),
  lags = 1,
  exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
  transformation = "fd",
  data = Data.all,
  panel_identifier = c("Compliance_Country", "Year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 3,
  max_instr_predet_vars = 1,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
)
k <- j + length(summary(model_pvar_all)$results$Emission@coef.names) - 1
results[j:k, 3] <- summary(model_pvar_all)$results$Emission@coef.names
results[j:k, 4] <- summary(model_pvar_all)$results$Emission@coef
results[j:k, 5] <- summary(model_pvar_all)$results$Emission@pvalues
j <- j + length(summary(model_pvar_all)$results$Emission@coef.names)
results[j, 3] <- "No. of instruments/Observations"
results[j, 4] <- summary(model_pvar_all)$max_instr_dependent_vars
results[j, 5] <- summary(model_pvar_all)$nof_observations
j <- j + 1
results[j, 3] <- "Hansen test"
results[j, 4] <- hansen_j_test(model_pvar_all)$statistic
results[j, 5] <- hansen_j_test(model_pvar_all)$p.value
results[m:j, 2] <- "All"
results[m:j, 1] <- "PVAR (Emission)"
j <- j + 1
m <- j

model_pvar_combustion <- pvargmm(
  dependent_vars = c("Emission"),
  lags = 1,
  exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
  transformation = "fd",
  data = Data.combustion,
  panel_identifier = c("Compliance_Country", "Year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 2,
  max_instr_predet_vars = 1,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
)
k <- j + length(summary(model_pvar_combustion)$results$Emission@coef.names) - 1
results[j:k, 3] <- summary(model_pvar_combustion)$results$Emission@coef.names
results[j:k, 4] <- summary(model_pvar_combustion)$results$Emission@coef
results[j:k, 5] <- summary(model_pvar_combustion)$results$Emission@pvalues
j <- j + length(summary(model_pvar_combustion)$results$Emission@coef.names)
results[j, 3] <- "No. of instruments/Observations"
results[j, 4] <- summary(model_pvar_combustion)$max_instr_dependent_vars
results[j, 5] <- summary(model_pvar_combustion)$nof_observations
j <- j + 1
results[j, 3] <- "Hansen test"
results[j, 4] <- hansen_j_test(model_pvar_combustion)$statistic
results[j, 5] <- hansen_j_test(model_pvar_combustion)$p.value
results[m:j, 2] <- "Combustion"
results[m:j, 1] <- "PVAR (Emission)"
j <- j + 1
m <- j

model_pvar_other <- pvargmm(
  dependent_vars = c("Emission"),
  lags = 1,
  exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
  transformation = "fd",
  data = Data.other,
  panel_identifier = c("Compliance_Country", "Year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 3,
  max_instr_predet_vars = 1,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
)
k <- j + length(summary(model_pvar_other)$results$Emission@coef.names) - 1
results[j:k, 3] <- summary(model_pvar_other)$results$Emission@coef.names
results[j:k, 4] <- summary(model_pvar_other)$results$Emission@coef
results[j:k, 5] <- summary(model_pvar_other)$results$Emission@pvalues
j <- j + length(summary(model_pvar_other)$results$Emission@coef.names)
results[j, 3] <- "No. of instruments/Observations"
results[j, 4] <- summary(model_pvar_other)$max_instr_dependent_vars
results[j, 5] <- summary(model_pvar_other)$nof_observations
j <- j + 1
results[j, 3] <- "Hansen test"
results[j, 4] <- hansen_j_test(model_pvar_other)$statistic
results[j, 5] <- hansen_j_test(model_pvar_other)$p.value
results[m:j, 2] <- "Other"
results[m:j, 1] <- "PVAR (Emission)"

# Writing results to a file
results <- results[complete.cases(results),]
results <- as.data.frame(results)
colnames(results) <- c("Model", "Sector", "Variable", "Coefficient", "P-val/Adj")
results <- results %>% mutate(Coefficient = as.numeric(Coefficient), `P-val/Adj` = as.numeric(`P-val/Adj`))
write.xlsx(results, file = "Results/Results_Emission_all_data_sectors_countries_years.xlsx", showNA = FALSE, row.names = FALSE)

effects <- effects[complete.cases(effects),]
effects <- as.data.frame(effects)
colnames(effects) <- c("Model", "Sector", "Variable", "Effect")
effects <- effects %>% mutate(Effect = as.numeric(Effect))
write.xlsx(effects, file = "Results/Effects_Emission_all_data_sectors_countries_years.xlsx", showNA = FALSE, row.names = FALSE)

# Cleaning up the environment
rm(list = ls())