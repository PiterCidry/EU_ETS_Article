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
Data <- read.csv2("Data.csv", na.strings = "", stringsAsFactors = FALSE) %>% select(Compliance_Country, Year, Allowance_Allocation,
                                                                                    Total_Verified_Emissions, Permit_Monitoring_Plan_ID,
                                                                                    GDP_growth, Phase2, Phase3, Y2009,
                                                                                    IsCombustionSector, Emission, Allow, Phase2_Allow,
                                                                                    Phase3_Allow, Combustion_Allow) %>%
  distinct(Permit_Monitoring_Plan_ID, Year, .keep_all = TRUE)

# Search for significant dependency in individual countries
countries <- unique(Data$Compliance_Country)
formula <- Emission ~ lag(Emission, 1) + Allow + Phase2_Allow + Phase3_Allow + Y2009
models_pooled_all <- list()
models_fe_time_all <- list()
models_fe_ind_all <- list()
models_fe_tw_all <- list()
models_re_all <- list()
models_pvar_all <- list()
models_pooled_combustion <- list()
models_fe_time_combustion <- list()
models_fe_ind_combustion <- list()
models_fe_tw_combustion <- list()
models_re_combustion <- list()
models_pvar_combustion <- list()
models_pooled_other <- list()
models_fe_time_other <- list()
models_fe_ind_other <- list()
models_fe_tw_other <- list()
models_re_other <- list()
models_pvar_other <- list()

# Models for individual countries index for specific installation
for (i in 1:length(countries)) {
  tryCatch(
    {
      print(paste0("(All) Outer loop: ", i, "/", length(countries)))
      # Data selection
      DataAll <- Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)),] %>%
        pdata.frame(index = c("Permit_Monitoring_Plan_ID", "Year"))
      # Pooled regression
      models_pooled_all[[i]] <- plm(formula, data = DataAll, model = "pooling")
      # One-way fixed time effects model
      models_fe_time_all[[i]] <- plm(formula, data = DataAll, model = "within", effect = "time")
      # One-way fixed individual effects model
      models_fe_ind_all[[i]] <- plm(formula, data = DataAll, model = "within", effect = "individual")
      # Two-ways fixed effects model
      models_fe_tw_all[[i]] <- plm(formula, data = DataAll, model = "within", effect = "twoways")
      # Random effects model
      models_re_all[[i]] <- plm(formula, data = DataAll, model = "random")
      
      tryCatch(
        {
          for (j in seq(from = 2, to = floor(length(unique(DataAll$Permit_Monitoring_Plan_ID))^(1/3)), by = 2)) {
            print(paste0("Inner loop: ", j, "/", floor(length(unique(DataAll$Permit_Monitoring_Plan_ID))^(1/3))))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("Emission"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
              transformation = "fd",
              data = DataAll,
              panel_identifier = c("Permit_Monitoring_Plan_ID", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = TRUE
            )
            models_pvar_all[[i]] <- temp_pvar
            # Hansen test
            hansen_pval <- hansen_j_test(temp_pvar)$p.value
            if (hansen_pval > 0.1 & hansen_pval < 0.99) {
              models_pvar_all[[i]] <- temp_pvar
              break
            }
          }
        },
        error = function(cond) {
          message("ERROR!")
          message(cond)
        },
        finally = {
          next
        }
      )
    },
    error = function(cond) {
      message("ERROR!")
      message(cond)
    },
    finally = {
      next
    }
  )
}

for (i in 1:length(countries)) {
  tryCatch(
    {
      print(paste0("(Combustion) Outer loop: ", i, "/", length(countries)))
      # Data selection
      DataCombustion <- Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)
                                   & Data$IsCombustionSector == 1),] %>%
        pdata.frame(index = c("Permit_Monitoring_Plan_ID", "Year"))
      # Pooled regression
      models_pooled_combustion[[i]] <- plm(formula, data = DataCombustion, model = "pooling")
      # One-way fixed time effects model
      models_fe_time_combustion[[i]] <- plm(formula, data = DataCombustion, model = "within", effect = "time")
      # One-way fixed individual effects model
      models_fe_ind_combustion[[i]] <- plm(formula, data = DataCombustion, model = "within", effect = "individual")
      # Two-ways fixed effects model
      models_fe_tw_combustion[[i]] <- plm(formula, data = DataCombustion, model = "within", effect = "twoways")
      # Random effects model
      models_re_combustion[[i]] <- plm(formula, data = DataCombustion, model = "random")
      
      tryCatch(
        {
          for (j in seq(from = 2, to = floor(length(unique(DataCombustion$Permit_Monitoring_Plan_ID))^(1/3)), by = 2)) {
            print(paste0("Inner loop: ", j, "/", floor(length(unique(DataCombustion$Permit_Monitoring_Plan_ID))^(1/3))))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("Emission"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
              transformation = "fd",
              data = DataCombustion,
              panel_identifier = c("Permit_Monitoring_Plan_ID", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = TRUE
            )
            models_pvar_combustion[[i]] <- temp_pvar
            # Hansen test
            hansen_pval <- hansen_j_test(temp_pvar)$p.value
            if (hansen_pval > 0.1 & hansen_pval < 0.99) {
              models_pvar_combustion[[i]] <- temp_pvar
              break
            }
          }
        },
        error = function(cond) {
          message("ERROR!")
          message(cond)
        },
        finally = {
          next
        }
      )
    },
    error = function(cond) {
      message("ERROR!")
      message(cond)
    },
    finally = {
      next
    }
  )
}

for (i in 1:length(countries)) {
  tryCatch(
    {
      print(paste0("(Other) Outer loop: ", i, "/", length(countries)))
      # Data selection
      DataOther <- Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)),] %>%
        pdata.frame(index = c("Permit_Monitoring_Plan_ID", "Year"))
      # Pooled regression
      models_pooled_other[[i]] <- plm(formula, data = DataOther, model = "pooling")
      # One-way fixed time effects model
      models_fe_time_other[[i]] <- plm(formula, data = DataOther, model = "within", effect = "time")
      # One-way fixed individual effects model
      models_fe_ind_other[[i]] <- plm(formula, data = DataOther, model = "within", effect = "individual")
      # Two-ways fixed effects model
      models_fe_tw_other[[i]] <- plm(formula, data = DataOther, model = "within", effect = "twoways")
      # Random effects model
      models_re_other[[i]] <- plm(formula, data = DataOther, model = "random")
      
      tryCatch(
        {
          for (j in seq(from = 2, to = floor(length(unique(DataOther$Permit_Monitoring_Plan_ID))^(1/3)), by = 2)) {
            print(paste0("Inner loop: ", j, "/", floor(length(unique(DataOther$Permit_Monitoring_Plan_ID))^(1/3))))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("Emission"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
              transformation = "fd",
              data = DataOther,
              panel_identifier = c("Permit_Monitoring_Plan_ID", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = TRUE
            )
            models_pvar_other[[i]] <- temp_pvar
            # Hansen test
            hansen_pval <- hansen_j_test(temp_pvar)$p.value
            if (hansen_pval > 0.1 & hansen_pval < 0.99) {
              models_pvar_other[[i]] <- temp_pvar
              break
            }
          }
        },
        error = function(cond) {
          message("ERROR!")
          message(cond)
        },
        finally = {
          next
        }
      )
    },
    error = function(cond) {
      message("ERROR!")
      message(cond)
    },
    finally = {
      next
    }
  )
}
remove(temp_pvar, DataAll, DataCombustion, DataOther, hansen_pval, i, j)

# Writing results to a data frame
results <- matrix(NA, nrow = 10000, ncol = 6)
effects <- matrix(NA, nrow = 10000, ncol = 5)

j <- 1
l <- j
m <- j
n <- 1
o <- n
for (i in 1:length(countries)) {
  # Pooled model
  k <- j + length(summary(models_pooled_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_pooled_all[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_pooled_all[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_pooled_all[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_pooled_all[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_pooled_all[[i]])$r.squared[1]
  results[j, 6] <- summary(models_pooled_all[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_pooled_all[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_pooled_all[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (time)"
  results[j, 5] <- plmtest(models_pooled_all[[i]], c("time"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_all[[i]], c("time"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (individual)"
  results[j, 5] <- plmtest(models_pooled_all[[i]], c("individual"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_all[[i]], c("individual"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (two-ways)"
  results[j, 5] <- plmtest(models_pooled_all[[i]], c("twoways"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_all[[i]], c("twoways"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_pooled_all[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_pooled_all[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_pooled_all[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_pooled_all[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_pooled_all[[i]])$statistic
  results[j, 6] <- pbgtest(models_pooled_all[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_pooled_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_pooled_all[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_pooled_all[[i]], vcovHC(models_pooled_all[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_pooled_all[[i]], vcovHC(models_pooled_all[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_pooled_all[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_pooled_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_pooled_all[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_pooled_all[[i]], models_pooled_all[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_pooled_all[[i]], models_pooled_all[[i]]$vcov)[,4]
  results[l:k, 3] <- "Pooled"
  j <- j + length(summary(models_pooled_all[[i]])[["coefficients"]][,1])
  l <- j
  # RE model
  k <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_re_all[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_re_all[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_re_all[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_re_all[[i]])$r.squared[1]
  results[j, 6] <- summary(models_re_all[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_re_all[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_re_all[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_re_all[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_re_all[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_re_all[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_re_all[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_re_all[[i]])$statistic
  results[j, 6] <- pbgtest(models_re_all[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_re_all[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_re_all[[i]], vcovHC(models_re_all[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_re_all[[i]], vcovHC(models_re_all[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_re_all[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_re_all[[i]], models_re_all[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_re_all[[i]], models_re_all[[i]]$vcov)[,4]
  results[l:k, 3] <- "RE"
  j <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1])
  l <- j
  # One way fixed time effects model
  results[j, 4] <- "Hausman test (time)"
  results[j, 5] <- phtest(models_fe_time_all[[i]], models_re_all[[i]])$statistic
  results[j, 6] <- phtest(models_fe_time_all[[i]], models_re_all[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_time_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_time_all[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_time_all[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_time_all[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_time_all[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_time_all[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_time_all[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_time_all[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_time_all[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (time effects)"
  results[j, 5] <- pFtest(models_fe_time_all[[i]], models_pooled_all[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_time_all[[i]], models_pooled_all[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_time_all[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_time_all[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_time_all[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_time_all[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_time_all[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_time_all[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_time_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_time_all[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_time_all[[i]], vcovHC(models_fe_time_all[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_time_all[[i]], vcovHC(models_fe_time_all[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_time_all[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_time_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_time_all[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_time_all[[i]], models_fe_time_all[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_time_all[[i]], models_fe_time_all[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (time)"
  j <- j + length(summary(models_fe_time_all[[i]])[["coefficients"]][,1])
  l <- j
  # One way fixed individual effects model
  results[j, 4] <- "Hausman test (individual)"
  results[j, 5] <- phtest(models_fe_ind_all[[i]], models_re_all[[i]])$statistic
  results[j, 6] <- phtest(models_fe_ind_all[[i]], models_re_all[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_ind_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_ind_all[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_ind_all[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_ind_all[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_ind_all[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_ind_all[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_ind_all[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_ind_all[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_ind_all[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (individual effects)"
  results[j, 5] <- pFtest(models_fe_ind_all[[i]], models_pooled_all[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_ind_all[[i]], models_pooled_all[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_ind_all[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_ind_all[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_ind_all[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_ind_all[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_ind_all[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_ind_all[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_ind_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_ind_all[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_ind_all[[i]], vcovHC(models_fe_ind_all[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_ind_all[[i]], vcovHC(models_fe_ind_all[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_ind_all[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_ind_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_ind_all[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_ind_all[[i]], models_fe_ind_all[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_ind_all[[i]], models_fe_ind_all[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (individual)"
  j <- j + length(summary(models_fe_ind_all[[i]])[["coefficients"]][,1])
  l <- j
  # Two-ways fixed effects model
  results[j, 4] <- "Hausman test (two-ways)"
  results[j, 5] <- phtest(models_fe_tw_all[[i]], models_re_all[[i]])$statistic
  results[j, 6] <- phtest(models_fe_tw_all[[i]], models_re_all[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_tw_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_tw_all[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_tw_all[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_tw_all[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_tw_all[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_tw_all[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_tw_all[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_tw_all[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_tw_all[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (two-ways effects)"
  results[j, 5] <- pFtest(models_fe_tw_all[[i]], models_pooled_all[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_tw_all[[i]], models_pooled_all[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_tw_all[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_tw_all[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_tw_all[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_tw_all[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_tw_all[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_tw_all[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_tw_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_tw_all[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_tw_all[[i]], vcovHC(models_fe_tw_all[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_tw_all[[i]], vcovHC(models_fe_tw_all[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_tw_all[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_tw_all[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_tw_all[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_tw_all[[i]], models_fe_tw_all[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_tw_all[[i]], models_fe_tw_all[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (two-ways)"
  j <- j + length(summary(models_fe_tw_all[[i]])[["coefficients"]][,1])
  l <- j
  # Testing heteroskedasticity
  results[j, 4] <- "BP test"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow)),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow)),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow)),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow)),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (Emission)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)), "Emission"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)), "Emission"])$p.value
  results[l:j, 3] <- "-"
  j <- j + 1
  l <- j
  # PVAR
  k <- j + length(summary(models_pvar_all[[i]])$results$Emission@coef.names) - 1
  results[j:k, 4] <- summary(models_pvar_all[[i]])$results$Emission@coef.names
  results[j:k, 5] <- summary(models_pvar_all[[i]])$results$Emission@coef
  results[j:k, 6] <- summary(models_pvar_all[[i]])$results$Emission@pvalues
  j <- j + length(summary(models_pvar_all[[i]])$results$Emission@coef.names)
  results[j, 4] <- "No. of instruments/observations"
  results[j, 5] <- summary(models_pvar_all[[i]])$max_instr_dependent_vars
  results[j, 6] <- summary(models_pvar_all[[i]])$nof_observations
  j <- j + 1
  results[j, 4] <- "Hansen test"
  results[j, 5] <- hansen_j_test(models_pvar_all[[i]])$statistic
  results[j, 6] <- hansen_j_test(models_pvar_all[[i]])$p.value
  results[l:j, 3] <- "PVAR (Emission)"
  results[m:j, 2] <- "All"
  results[m:j, 1] <- countries[i]
  j <- j + 1
  l <- j
  m <- j
  # Time effects
  p <- n + length(fixef(models_fe_time_all[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_all[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_all[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_all[[i]]))
  p <- n + length(fixef(models_fe_tw_all[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_all[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_all[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  effects[o:p, 2] <- "All"
  effects[o:p, 1] <- countries[i]
  n <- n + length(fixef(models_fe_tw_all[[i]], effect = "time"))
  o <- n
  
  # Pooled model
  k <- j + length(summary(models_pooled_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_pooled_combustion[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_pooled_combustion[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_pooled_combustion[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_pooled_combustion[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_pooled_combustion[[i]])$r.squared[1]
  results[j, 6] <- summary(models_pooled_combustion[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_pooled_combustion[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_pooled_combustion[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (time)"
  results[j, 5] <- plmtest(models_pooled_combustion[[i]], c("time"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_combustion[[i]], c("time"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (individual)"
  results[j, 5] <- plmtest(models_pooled_combustion[[i]], c("individual"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_combustion[[i]], c("individual"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (two-ways)"
  results[j, 5] <- plmtest(models_pooled_combustion[[i]], c("twoways"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_combustion[[i]], c("twoways"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_pooled_combustion[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_pooled_combustion[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_pooled_combustion[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_pooled_combustion[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_pooled_combustion[[i]])$statistic
  results[j, 6] <- pbgtest(models_pooled_combustion[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_pooled_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_pooled_combustion[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_pooled_combustion[[i]], vcovHC(models_pooled_combustion[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_pooled_combustion[[i]], vcovHC(models_pooled_combustion[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_pooled_combustion[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_pooled_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_pooled_combustion[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_pooled_combustion[[i]], models_pooled_combustion[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_pooled_combustion[[i]], models_pooled_combustion[[i]]$vcov)[,4]
  results[l:k, 3] <- "Pooled"
  j <- j + length(summary(models_pooled_combustion[[i]])[["coefficients"]][,1])
  l <- j
  # RE model
  k <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_re_combustion[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_re_combustion[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_re_combustion[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_re_combustion[[i]])$r.squared[1]
  results[j, 6] <- summary(models_re_combustion[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_re_combustion[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_re_combustion[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_re_combustion[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_re_combustion[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_re_combustion[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_re_combustion[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_re_combustion[[i]])$statistic
  results[j, 6] <- pbgtest(models_re_combustion[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_re_combustion[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_re_combustion[[i]], vcovHC(models_re_combustion[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_re_combustion[[i]], vcovHC(models_re_combustion[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_re_combustion[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_re_combustion[[i]], models_re_combustion[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_re_combustion[[i]], models_re_combustion[[i]]$vcov)[,4]
  results[l:k, 3] <- "RE"
  j <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1])
  l <- j
  # One way fixed time effects model
  results[j, 4] <- "Hausman test (time)"
  results[j, 5] <- phtest(models_fe_time_combustion[[i]], models_re_combustion[[i]])$statistic
  results[j, 6] <- phtest(models_fe_time_combustion[[i]], models_re_combustion[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_time_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_time_combustion[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_time_combustion[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_time_combustion[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_time_combustion[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_time_combustion[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_time_combustion[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_time_combustion[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_time_combustion[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (time effects)"
  results[j, 5] <- pFtest(models_fe_time_combustion[[i]], models_pooled_combustion[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_time_combustion[[i]], models_pooled_combustion[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_time_combustion[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_time_combustion[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_time_combustion[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_time_combustion[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_time_combustion[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_time_combustion[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_time_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_time_combustion[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_time_combustion[[i]], vcovHC(models_fe_time_combustion[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_time_combustion[[i]], vcovHC(models_fe_time_combustion[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_time_combustion[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_time_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_time_combustion[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_time_combustion[[i]], models_fe_time_combustion[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_time_combustion[[i]], models_fe_time_combustion[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (time)"
  j <- j + length(summary(models_fe_time_combustion[[i]])[["coefficients"]][,1])
  l <- j
  # One way fixed individual effects model
  results[j, 4] <- "Hausman test (individual)"
  results[j, 5] <- phtest(models_fe_ind_combustion[[i]], models_re_combustion[[i]])$statistic
  results[j, 6] <- phtest(models_fe_ind_combustion[[i]], models_re_combustion[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_ind_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_ind_combustion[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_ind_combustion[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_ind_combustion[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_ind_combustion[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_ind_combustion[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_ind_combustion[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_ind_combustion[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_ind_combustion[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (individual effects)"
  results[j, 5] <- pFtest(models_fe_ind_combustion[[i]], models_pooled_combustion[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_ind_combustion[[i]], models_pooled_combustion[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_ind_combustion[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_ind_combustion[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_ind_combustion[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_ind_combustion[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_ind_combustion[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_ind_combustion[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_ind_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_ind_combustion[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_ind_combustion[[i]], vcovHC(models_fe_ind_combustion[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_ind_combustion[[i]], vcovHC(models_fe_ind_combustion[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_ind_combustion[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_ind_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_ind_combustion[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_ind_combustion[[i]], models_fe_ind_combustion[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_ind_combustion[[i]], models_fe_ind_combustion[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (individual)"
  j <- j + length(summary(models_fe_ind_combustion[[i]])[["coefficients"]][,1])
  l <- j
  # Two-ways fixed effects model
  results[j, 4] <- "Hausman test (two-ways)"
  results[j, 5] <- phtest(models_fe_tw_combustion[[i]], models_re_combustion[[i]])$statistic
  results[j, 6] <- phtest(models_fe_tw_combustion[[i]], models_re_combustion[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_tw_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_tw_combustion[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_tw_combustion[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_tw_combustion[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_tw_combustion[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_tw_combustion[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_tw_combustion[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_tw_combustion[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_tw_combustion[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (two-ways effects)"
  results[j, 5] <- pFtest(models_fe_tw_combustion[[i]], models_pooled_combustion[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_tw_combustion[[i]], models_pooled_combustion[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_tw_combustion[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_tw_combustion[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_tw_combustion[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_tw_combustion[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_tw_combustion[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_tw_combustion[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_tw_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_tw_combustion[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_tw_combustion[[i]], vcovHC(models_fe_tw_combustion[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_tw_combustion[[i]], vcovHC(models_fe_tw_combustion[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_tw_combustion[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_tw_combustion[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_tw_combustion[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_tw_combustion[[i]], models_fe_tw_combustion[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_tw_combustion[[i]], models_fe_tw_combustion[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (two-ways)"
  j <- j + length(summary(models_fe_tw_combustion[[i]])[["coefficients"]][,1])
  l <- j
  # Testing heteroskedasticity
  results[j, 4] <- "BP test"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (Emission)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)
                                       & Data$IsCombustionSector == 1), "Emission"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)
                                       & Data$IsCombustionSector == 1), "Emission"])$p.value
  results[l:j, 3] <- "-"
  j <- j + 1
  l <- j
  # PVAR
  k <- j + length(summary(models_pvar_combustion[[i]])$results$Emission@coef.names) - 1
  results[j:k, 4] <- summary(models_pvar_combustion[[i]])$results$Emission@coef.names
  results[j:k, 5] <- summary(models_pvar_combustion[[i]])$results$Emission@coef
  results[j:k, 6] <- summary(models_pvar_combustion[[i]])$results$Emission@pvalues
  j <- j + length(summary(models_pvar_combustion[[i]])$results$Emission@coef.names)
  results[j, 4] <- "No. of instruments/observations"
  results[j, 5] <- summary(models_pvar_combustion[[i]])$max_instr_dependent_vars
  results[j, 6] <- summary(models_pvar_combustion[[i]])$nof_observations
  j <- j + 1
  results[j, 4] <- "Hansen test"
  results[j, 5] <- hansen_j_test(models_pvar_combustion[[i]])$statistic
  results[j, 6] <- hansen_j_test(models_pvar_combustion[[i]])$p.value
  results[l:j, 3] <- "PVAR (Emission)"
  results[m:j, 2] <- "Combustion"
  results[m:j, 1] <- countries[i]
  j <- j + 1
  l <- j
  m <- j
  # Time effects
  p <- n + length(fixef(models_fe_time_combustion[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_combustion[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_combustion[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_combustion[[i]]))
  p <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_combustion[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_combustion[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  effects[o:p, 2] <- "Combustion"
  effects[o:p, 1] <- countries[i]
  n <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "time"))
  o <- n
  
  # Pooled model
  k <- j + length(summary(models_pooled_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_pooled_other[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_pooled_other[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_pooled_other[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_pooled_other[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_pooled_other[[i]])$r.squared[1]
  results[j, 6] <- summary(models_pooled_other[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_pooled_other[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_pooled_other[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (time)"
  results[j, 5] <- plmtest(models_pooled_other[[i]], c("time"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_other[[i]], c("time"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (individual)"
  results[j, 5] <- plmtest(models_pooled_other[[i]], c("individual"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_other[[i]], c("individual"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "L-M test (two-ways)"
  results[j, 5] <- plmtest(models_pooled_other[[i]], c("twoways"), type = ("bp"))$statistic
  results[j, 6] <- plmtest(models_pooled_other[[i]], c("twoways"), type = ("bp"))$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_pooled_other[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_pooled_other[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_pooled_other[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_pooled_other[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_pooled_other[[i]])$statistic
  results[j, 6] <- pbgtest(models_pooled_other[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_pooled_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_pooled_other[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_pooled_other[[i]], vcovHC(models_pooled_other[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_pooled_other[[i]], vcovHC(models_pooled_other[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_pooled_other[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_pooled_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_pooled_other[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_pooled_other[[i]], models_pooled_other[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_pooled_other[[i]], models_pooled_other[[i]]$vcov)[,4]
  results[l:k, 3] <- "Pooled"
  j <- j + length(summary(models_pooled_other[[i]])[["coefficients"]][,1])
  l <- j
  # RE model
  k <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_re_other[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_re_other[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_re_other[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_re_other[[i]])$r.squared[1]
  results[j, 6] <- summary(models_re_other[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_re_other[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_re_other[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_re_other[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_re_other[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_re_other[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_re_other[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_re_other[[i]])$statistic
  results[j, 6] <- pbgtest(models_re_other[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_re_other[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_re_other[[i]], vcovHC(models_re_other[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_re_other[[i]], vcovHC(models_re_other[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_re_other[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_re_other[[i]], models_re_other[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_re_other[[i]], models_re_other[[i]]$vcov)[,4]
  results[l:k, 3] <- "RE"
  j <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1])
  l <- j
  # One way fixed time effects model
  results[j, 4] <- "Hausman test (time)"
  results[j, 5] <- phtest(models_fe_time_other[[i]], models_re_other[[i]])$statistic
  results[j, 6] <- phtest(models_fe_time_other[[i]], models_re_other[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_time_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_time_other[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_time_other[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_time_other[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_time_other[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_time_other[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_time_other[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_time_other[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_time_other[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (time effects)"
  results[j, 5] <- pFtest(models_fe_time_other[[i]], models_pooled_other[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_time_other[[i]], models_pooled_other[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_time_other[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_time_other[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_time_other[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_time_other[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_time_other[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_time_other[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_time_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_time_other[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_time_other[[i]], vcovHC(models_fe_time_other[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_time_other[[i]], vcovHC(models_fe_time_other[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_time_other[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_time_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_time_other[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_time_other[[i]], models_fe_time_other[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_time_other[[i]], models_fe_time_other[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (time)"
  j <- j + length(summary(models_fe_time_other[[i]])[["coefficients"]][,1])
  l <- j
  # One way fixed individual effects model
  results[j, 4] <- "Hausman test (individual)"
  results[j, 5] <- phtest(models_fe_ind_other[[i]], models_re_other[[i]])$statistic
  results[j, 6] <- phtest(models_fe_ind_other[[i]], models_re_other[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_ind_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_ind_other[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_ind_other[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_ind_other[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_ind_other[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_ind_other[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_ind_other[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_ind_other[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_ind_other[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (individual effects)"
  results[j, 5] <- pFtest(models_fe_ind_other[[i]], models_pooled_other[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_ind_other[[i]], models_pooled_other[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_ind_other[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_ind_other[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_ind_other[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_ind_other[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_ind_other[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_ind_other[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_ind_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_ind_other[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_ind_other[[i]], vcovHC(models_fe_ind_other[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_ind_other[[i]], vcovHC(models_fe_ind_other[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_ind_other[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_ind_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_ind_other[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_ind_other[[i]], models_fe_ind_other[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_ind_other[[i]], models_fe_ind_other[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (individual)"
  j <- j + length(summary(models_fe_ind_other[[i]])[["coefficients"]][,1])
  l <- j
  # Two-ways fixed effects model
  results[j, 4] <- "Hausman test (two-ways)"
  results[j, 5] <- phtest(models_fe_tw_other[[i]], models_re_other[[i]])$statistic
  results[j, 6] <- phtest(models_fe_tw_other[[i]], models_re_other[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_tw_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- rownames(summary(models_fe_tw_other[[i]])[["coefficients"]])
  results[j:k, 5] <- summary(models_fe_tw_other[[i]])[["coefficients"]][,1]
  results[j:k, 6] <- summary(models_fe_tw_other[[i]])[["coefficients"]][,4]
  j <- j + length(summary(models_fe_tw_other[[i]])[["coefficients"]][,1])
  results[j, 4] <- "R^2"
  results[j, 5] <- summary(models_fe_tw_other[[i]])$r.squared[1]
  results[j, 6] <- summary(models_fe_tw_other[[i]])$r.squared[2]
  j <- j + 1
  results[j, 4] <- "F test"
  results[j, 5] <- summary(models_fe_tw_other[[i]])$fstatistic$statistic
  results[j, 6] <- summary(models_fe_tw_other[[i]])$fstatistic$p.value
  j <- j + 1
  results[j, 4] <- "F test (two-ways effects)"
  results[j, 5] <- pFtest(models_fe_tw_other[[i]], models_pooled_other[[i]])$statistic
  results[j, 6] <- pFtest(models_fe_tw_other[[i]], models_pooled_other[[i]])$p.value
  j <- j + 1
  results[j, 4] <- "BP LM test"
  results[j, 5] <- pcdtest(models_fe_tw_other[[i]], test = c("lm"))$statistic
  results[j, 6] <- pcdtest(models_fe_tw_other[[i]], test = c("lm"))$p.value
  j <- j + 1
  results[j, 4] <- "Pesaran CD test"
  results[j, 5] <- pcdtest(models_fe_tw_other[[i]], test = c("cd"))$statistic
  results[j, 6] <- pcdtest(models_fe_tw_other[[i]], test = c("cd"))$p.value
  j <- j + 1
  results[j, 4] <- "B-G/Wooldrige test"
  results[j, 5] <- pbgtest(models_fe_tw_other[[i]])$statistic
  results[j, 6] <- pbgtest(models_fe_tw_other[[i]])$p.value
  j <- j + 1
  k <- j + length(summary(models_fe_tw_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_tw_other[[i]])[["coefficients"]]), " [Arellano]")
  results[j:k, 5] <- coeftest(models_fe_tw_other[[i]], vcovHC(models_fe_tw_other[[i]], method = "arellano"))[,1]
  results[j:k, 6] <- coeftest(models_fe_tw_other[[i]], vcovHC(models_fe_tw_other[[i]], method = "arellano"))[,4]
  j <- j + length(summary(models_fe_tw_other[[i]])[["coefficients"]][,1])
  k <- j + length(summary(models_fe_tw_other[[i]])[["coefficients"]][,1]) - 1
  results[j:k, 4] <- paste0(rownames(summary(models_fe_tw_other[[i]])[["coefficients"]]), " [HC3]")
  results[j:k, 5] <- coeftest(models_fe_tw_other[[i]], models_fe_tw_other[[i]]$vcov)[,1]
  results[j:k, 6] <- coeftest(models_fe_tw_other[[i]], models_fe_tw_other[[i]]$vcov)[,4]
  results[l:k, 3] <- "FE (two-ways)"
  j <- j + length(summary(models_fe_tw_other[[i]])[["coefficients"]][,1])
  l <- j
  # Testing heteroskedasticity
  results[j, 4] <- "BP test"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (Emission)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)
                                       & Data$IsCombustionSector == 0), "Emission"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% countries[i] & !is.na(Data$Emission) & !is.na(Data$Allow)
                                       & Data$IsCombustionSector == 0), "Emission"])$p.value
  results[l:j, 3] <- "-"
  j <- j + 1
  l <- j
  # PVAR
  k <- j + length(summary(models_pvar_other[[i]])$results$Emission@coef.names) - 1
  results[j:k, 4] <- summary(models_pvar_other[[i]])$results$Emission@coef.names
  results[j:k, 5] <- summary(models_pvar_other[[i]])$results$Emission@coef
  results[j:k, 6] <- summary(models_pvar_other[[i]])$results$Emission@pvalues
  j <- j + length(summary(models_pvar_other[[i]])$results$Emission@coef.names)
  results[j, 4] <- "No. of instruments/observations"
  results[j, 5] <- summary(models_pvar_other[[i]])$max_instr_dependent_vars
  results[j, 6] <- summary(models_pvar_other[[i]])$nof_observations
  j <- j + 1
  results[j, 4] <- "Hansen test"
  results[j, 5] <- hansen_j_test(models_pvar_other[[i]])$statistic
  results[j, 6] <- hansen_j_test(models_pvar_other[[i]])$p.value
  results[l:j, 3] <- "PVAR (Emission)"
  results[m:j, 2] <- "Other"
  results[m:j, 1] <- countries[i]
  j <- j + 1
  l <- j
  m <- j
  # Time effects
  p <- n + length(fixef(models_fe_time_other[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_other[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_other[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_other[[i]]))
  p <- n + length(fixef(models_fe_tw_other[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_other[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_other[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  effects[o:p, 2] <- "Other"
  effects[o:p, 1] <- countries[i]
  n <- n + length(fixef(models_fe_tw_other[[i]], effect = "time"))
  o <- n
}
remove(i, j, k, l, m, n, o, p, countries)

# Writing results to a file
results <- results[complete.cases(results),]
results <- as.data.frame(results)
colnames(results) <- c("Country", "Sector", "Model", "Variable", "Coefficient", "P-val/Adj")
results <- results %>% mutate(Coefficient = as.numeric(Coefficient), `P-val/Adj` = as.numeric(`P-val/Adj`))

effects <- effects[complete.cases(effects),]
effects <- as.data.frame(effects)
colnames(effects) <- c("Country", "Sector", "Model", "Year", "Effect")
effects <- effects %>% mutate(Year = as.numeric(Year), Effect = as.numeric(Effect))

write.xlsx(results, file = "Results/Results_Emission_countries_sectors_installations_years.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(effects, file = "Results/Effects_Emission_countries_sectors_installations_years.xlsx", showNA = FALSE, row.names = FALSE)

# Cleaning up the environment
rm(list = ls())