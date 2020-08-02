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
                                                                                    Phase3_Allow, Combustion_Allow, FDI, Labor)

# Search for significant dependency in groups of countries
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

# Models for groups of countries index for specific country
for (i in 1:length(group_of_countries)) {
  tryCatch(
    {
      print(paste0("(All) Outer loop: ", i, "/", length(group_of_countries)))
      # Data selection
      DataAll <- Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow)),]  %>%
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
          for (j in seq(from = 2, to = length(group_of_countries[[i]]))) {
            print(paste0("Inner loop: ", j, "/", length(group_of_countries[[i]])))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("Emission"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
              transformation = "fd",
              data = DataAll,
              panel_identifier = c("Compliance_Country", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = FALSE
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
  
for (i in 1:length(group_of_countries)) {
  tryCatch(
    {
      print(paste0("(Combustion) Outer loop: ", i, "/", length(group_of_countries)))
      # Data selection
      DataCombustion <- Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow)
                                   & Data$IsCombustionSector == 1),]  %>%
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
          for (j in seq(from = 2, to = length(group_of_countries[[i]]))) {
            print(paste0("Inner loop: ", j, "/", length(group_of_countries[[i]])))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("Emission"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
              transformation = "fd",
              data = DataCombustion,
              panel_identifier = c("Compliance_Country", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = FALSE
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

for (i in 1:length(group_of_countries)) {
  tryCatch(
    {
      print(paste0("(Other) Outer loop: ", i, "/", length(group_of_countries)))
      # Data selection
      DataOther <- Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow)
                              & Data$IsCombustionSector == 0),]  %>%
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
          for (j in seq(from = 2, to = length(group_of_countries[[i]]))) {
            print(paste0("Inner loop: ", j, "/", length(group_of_countries[[i]])))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("Emission"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "Y2009"),
              transformation = "fd",
              data = DataOther,
              panel_identifier = c("Compliance_Country", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = FALSE
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
for (i in 1:length(group_of_countries)) {
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
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow)),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow)),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow)),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow)),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (Emission)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow)), "Emission"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow)), "Emission"])$p.value
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
  results[m:j, 1] <- names(group_of_countries)[i]
  results[m:j, 2] <- "All"
  j <- j + 1
  l <- j
  m <- j
  # Effects
  p <- n + length(fixef(models_fe_time_all[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_all[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_all[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_all[[i]]))
  p <- n + length(fixef(models_fe_ind_all[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_ind_all[[i]]))
  effects[n:p, 5] <- fixef(models_fe_ind_all[[i]])
  effects[n:p, 3] <- "FE (ind)"
  n <- n + length(fixef(models_fe_ind_all[[i]]))
  p <- n + length(fixef(models_fe_tw_all[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_all[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_all[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  n <- n + length(fixef(models_fe_tw_all[[i]], effect = "time"))
  p <- n + length(fixef(models_fe_tw_all[[i]], effect = "individual")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_all[[i]], effect = "individual"))
  effects[n:p, 5] <- fixef(models_fe_tw_all[[i]], effect = "individual")
  effects[n:p, 3] <- "Two-ways FE (individual)"
  effects[o:p, 1] <- names(group_of_countries)[i]
  effects[o:p, 2] <- "All"
  n <- n + length(fixef(models_fe_tw_all[[i]], effect = "individual"))
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
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (Emission)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow) &
                                         Data$IsCombustionSector == 1), "Emission"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow) &
                                         Data$IsCombustionSector == 1), "Emission"])$p.value
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
  results[m:j, 1] <- names(group_of_countries)[i]
  results[m:j, 2] <- "Combustion"
  j <- j + 1
  l <- j
  m <- j
  # Effects
  p <- n + length(fixef(models_fe_time_combustion[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_combustion[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_combustion[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_combustion[[i]]))
  p <- n + length(fixef(models_fe_ind_combustion[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_ind_combustion[[i]]))
  effects[n:p, 5] <- fixef(models_fe_ind_combustion[[i]])
  effects[n:p, 3] <- "FE (ind)"
  n <- n + length(fixef(models_fe_ind_combustion[[i]]))
  p <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_combustion[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_combustion[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  n <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "time"))
  p <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "individual")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_combustion[[i]], effect = "individual"))
  effects[n:p, 5] <- fixef(models_fe_tw_combustion[[i]], effect = "individual")
  effects[n:p, 3] <- "Two-ways FE (individual)"
  effects[o:p, 1] <- names(group_of_countries)[i]
  effects[o:p, 2] <- "Combustion"
  n <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "individual"))
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
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (Emission)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow) &
                                         Data$IsCombustionSector == 0), "Emission"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$Emission) & !is.na(Data$Allow) &
                                         Data$IsCombustionSector == 0), "Emission"])$p.value
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
  results[m:j, 1] <- names(group_of_countries)[i]
  results[m:j, 2] <- "Other"
  j <- j + 1
  l <- j
  m <- j
  # Effects
  p <- n + length(fixef(models_fe_time_other[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_other[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_other[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_other[[i]]))
  p <- n + length(fixef(models_fe_ind_other[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_ind_other[[i]]))
  effects[n:p, 5] <- fixef(models_fe_ind_other[[i]])
  effects[n:p, 3] <- "FE (ind)"
  n <- n + length(fixef(models_fe_ind_other[[i]]))
  p <- n + length(fixef(models_fe_tw_other[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_other[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_other[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  n <- n + length(fixef(models_fe_tw_other[[i]], effect = "time"))
  p <- n + length(fixef(models_fe_tw_other[[i]], effect = "individual")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_other[[i]], effect = "individual"))
  effects[n:p, 5] <- fixef(models_fe_tw_other[[i]], effect = "individual")
  effects[n:p, 3] <- "Two-ways FE (individual)"
  effects[o:p, 1] <- names(group_of_countries)[i]
  effects[o:p, 2] <- "Other"
  n <- n + length(fixef(models_fe_tw_other[[i]], effect = "individual"))
  o <- n
}
remove(i, j, k, l, m, n, o)

# Writing results to a file
results <- results[complete.cases(results),]
results <- as.data.frame(results)
colnames(results) <- c("Group of countries", "Sector", "Model", "Variable", "Coefficient", "P-val/Adj")
results <- results %>% mutate(Coefficient = as.numeric(Coefficient), `P-val/Adj` = as.numeric(`P-val/Adj`))

effects <- effects[complete.cases(effects),]
effects <- as.data.frame(effects)
colnames(effects) <- c("Group of countries", "Sector", "Model", "Variable", "Effect")
effects <- effects %>% mutate(Effect = as.numeric(Effect))

write.xlsx(results, file = "Results/Results_Emission_groups_sectors_countries_years.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(effects, file = "Results/Effects_Emission_groups_sectors_countries_years.xlsx", showNA = FALSE, row.names = FALSE)

# Search for significant dependency in groups of countries
formula <- GDP_growth ~ lag(GDP_growth, 1) + Allow + Phase2_Allow + Phase3_Allow + FDI + Labor + Y2009
models_pooled_all <- list()
models_fe_time_all <- list()
models_fe_ind_all <- list()
models_fe_tw_all <- list()
# models_re_all <- list()
models_pvar_all <- list()
models_pooled_combustion <- list()
models_fe_time_combustion <- list()
models_fe_ind_combustion <- list()
models_fe_tw_combustion <- list()
# models_re_combustion <- list()
models_pvar_combustion <- list()
models_pooled_other <- list()
models_fe_time_other <- list()
models_fe_ind_other <- list()
models_fe_tw_other <- list()
# models_re_other <- list()
models_pvar_other <- list()

# Models for groups of countries index for specific country
for (i in 1:length(group_of_countries)) {
  tryCatch(
    {
      print(paste0("(All) Outer loop: ", i, "/", length(group_of_countries)))
      # Data selection
      DataAll <- Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow)),]  %>%
        group_by(Compliance_Country, Year) %>%
        summarise(
          GDP_growth = mean(GDP_growth, na.rm = TRUE),
          Allow = mean(Allow, na.rm = TRUE),
          Phase2_Allow = mean(Phase2_Allow, na.rm = TRUE),
          Phase3_Allow = mean(Phase3_Allow, na.rm = TRUE),
          FDI = mean(FDI, na.rm = TRUE),
          Labor = mean(Labor, na.rm = TRUE),
          Y2009 = mean(Y2009, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        pdata.frame(index = c("Compliance_Country", "Year"))
      # Pooled regression
      models_pooled_all[[i]] <- plm(formula, data = DataAll, model = "pooling")
      # One-way fixed time effects model
      models_fe_time_all[[i]] <- plm(formula, data = DataAll, model = "within", effect = "time")
      # One-way fixed individual effects model
      models_fe_ind_all[[i]] <- plm(formula, data = DataAll, model = "within", effect = "individual")
      # Two-ways fixed effects model
      models_fe_tw_all[[i]] <- plm(formula, data = DataAll, model = "within", effect = "twoways")
      # Random effects model
      # models_re_all[[i]] <- plm(formula, data = DataAll, model = "random")
      
      tryCatch(
        {
          for (j in seq(from = 2, to = length(group_of_countries[[i]]))) {
            print(paste0("Inner loop: ", j, "/", length(group_of_countries[[i]])))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("GDP_growth"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "FDI", "Labor", "Y2009"),
              transformation = "fd",
              data = DataAll,
              panel_identifier = c("Compliance_Country", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = FALSE
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

for (i in 1:length(group_of_countries)) {
  tryCatch(
    {
      print(paste0("(Combustion) Outer loop: ", i, "/", length(group_of_countries)))
      # Data selection
      DataCombustion <- Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow)
                            & Data$IsCombustionSector == 1),]  %>%
        group_by(Compliance_Country, Year) %>%
        summarise(
          GDP_growth = mean(GDP_growth, na.rm = TRUE),
          Allow = mean(Allow, na.rm = TRUE),
          Phase2_Allow = mean(Phase2_Allow, na.rm = TRUE),
          Phase3_Allow = mean(Phase3_Allow, na.rm = TRUE),
          FDI = mean(FDI, na.rm = TRUE),
          Labor = mean(Labor, na.rm = TRUE),
          Y2009 = mean(Y2009, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        pdata.frame(index = c("Compliance_Country", "Year"))
      # Pooled regression
      models_pooled_combustion[[i]] <- plm(formula, data = DataCombustion, model = "pooling")
      # One-way fixed time effects model
      models_fe_time_combustion[[i]] <- plm(formula, data = DataCombustion, model = "within", effect = "time")
      # One-way fixed individual effects model
      models_fe_ind_combustion[[i]] <- plm(formula, data = DataCombustion, model = "within", effect = "individual")
      # Two-ways fixed effects model
      models_fe_tw_combustion[[i]] <- plm(formula, data = DataCombustion, model = "within", effect = "twoways")
      # Random effects model
      # models_re_combustion[[i]] <- plm(formula, data = DataCombustion, model = "random")
      
      tryCatch(
        {
          for (j in seq(from = 2, to = length(group_of_countries[[i]]))) {
            print(paste0("Inner loop: ", j, "/", length(group_of_countries[[i]])))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("GDP_growth"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "FDI", "Labor", "Y2009"),
              transformation = "fd",
              data = DataCombustion,
              panel_identifier = c("Compliance_Country", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = FALSE
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

for (i in 1:length(group_of_countries)) {
  tryCatch(
    {
      print(paste0("(Other) Outer loop: ", i, "/", length(group_of_countries)))
      # Data selection
      DataOther <- Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow)
                                   & Data$IsCombustionSector == 0),]  %>%
        group_by(Compliance_Country, Year) %>%
        summarise(
          GDP_growth = mean(GDP_growth, na.rm = TRUE),
          Allow = mean(Allow, na.rm = TRUE),
          Phase2_Allow = mean(Phase2_Allow, na.rm = TRUE),
          Phase3_Allow = mean(Phase3_Allow, na.rm = TRUE),
          FDI = mean(FDI, na.rm = TRUE),
          Labor = mean(Labor, na.rm = TRUE),
          Y2009 = mean(Y2009, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        pdata.frame(index = c("Compliance_Country", "Year"))
      # Pooled regression
      models_pooled_other[[i]] <- plm(formula, data = DataOther, model = "pooling")
      # One-way fixed time effects model
      models_fe_time_other[[i]] <- plm(formula, data = DataOther, model = "within", effect = "time")
      # One-way fixed individual effects model
      models_fe_ind_other[[i]] <- plm(formula, data = DataOther, model = "within", effect = "individual")
      # Two-ways fixed effects model
      models_fe_tw_other[[i]] <- plm(formula, data = DataOther, model = "within", effect = "twoways")
      # Random effects model
      # models_re_other[[i]] <- plm(formula, data = DataOther, model = "random")
      
      tryCatch(
        {
          for (j in seq(from = 2, to = length(group_of_countries[[i]]))) {
            print(paste0("Inner loop: ", j, "/", length(group_of_countries[[i]])))
            # Dynamic panel var models
            temp_pvar <- pvargmm(
              dependent_vars = c("GDP_growth"),
              lags = 1,
              exog_vars =  c("Allow", "Phase2_Allow", "Phase3_Allow", "FDI", "Labor", "Y2009"),
              transformation = "fd",
              data = DataOther,
              panel_identifier = c("Compliance_Country", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,
              max_instr_dependent_vars = j,
              max_instr_predet_vars = 1,
              min_instr_dependent_vars = 2L,
              min_instr_predet_vars = 1L,
              collapse = FALSE
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
for (i in 1:length(group_of_countries)) {
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
  # k <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- rownames(summary(models_re_all[[i]])[["coefficients"]])
  # results[j:k, 5] <- summary(models_re_all[[i]])[["coefficients"]][,1]
  # results[j:k, 6] <- summary(models_re_all[[i]])[["coefficients"]][,4]
  # j <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1])
  # results[j, 4] <- "R^2"
  # results[j, 5] <- summary(models_re_all[[i]])$r.squared[1]
  # results[j, 6] <- summary(models_re_all[[i]])$r.squared[2]
  # j <- j + 1
  # results[j, 4] <- "F test"
  # results[j, 5] <- summary(models_re_all[[i]])$fstatistic$statistic
  # results[j, 6] <- summary(models_re_all[[i]])$fstatistic$p.value
  # j <- j + 1
  # results[j, 4] <- "BP LM test"
  # results[j, 5] <- pcdtest(models_re_all[[i]], test = c("lm"))$statistic
  # results[j, 6] <- pcdtest(models_re_all[[i]], test = c("lm"))$p.value
  # j <- j + 1
  # results[j, 4] <- "Pesaran CD test"
  # results[j, 5] <- pcdtest(models_re_all[[i]], test = c("cd"))$statistic
  # results[j, 6] <- pcdtest(models_re_all[[i]], test = c("cd"))$p.value
  # j <- j + 1
  # results[j, 4] <- "B-G/Wooldrige test"
  # results[j, 5] <- pbgtest(models_re_all[[i]])$statistic
  # results[j, 6] <- pbgtest(models_re_all[[i]])$p.value
  # j <- j + 1
  # k <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- paste0(rownames(summary(models_re_all[[i]])[["coefficients"]]), " [Arellano]")
  # results[j:k, 5] <- coeftest(models_re_all[[i]], vcovHC(models_re_all[[i]], method = "arellano"))[,1]
  # results[j:k, 6] <- coeftest(models_re_all[[i]], vcovHC(models_re_all[[i]], method = "arellano"))[,4]
  # j <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1])
  # k <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- paste0(rownames(summary(models_re_all[[i]])[["coefficients"]]), " [HC3]")
  # results[j:k, 5] <- coeftest(models_re_all[[i]], models_re_all[[i]]$vcov)[,1]
  # results[j:k, 6] <- coeftest(models_re_all[[i]], models_re_all[[i]]$vcov)[,4]
  # results[l:k, 3] <- "RE"
  # j <- j + length(summary(models_re_all[[i]])[["coefficients"]][,1])
  # l <- j
  # One way fixed time effects model
  # results[j, 4] <- "Hausman test (time)"
  # results[j, 5] <- phtest(models_fe_time_all[[i]], models_re_all[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_time_all[[i]], models_re_all[[i]])$p.value
  # j <- j + 1
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
  # results[j, 4] <- "Hausman test (individual)"
  # results[j, 5] <- phtest(models_fe_ind_all[[i]], models_re_all[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_ind_all[[i]], models_re_all[[i]])$p.value
  # j <- j + 1
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
  # results[j, 4] <- "Hausman test (two-ways)"
  # results[j, 5] <- phtest(models_fe_tw_all[[i]], models_re_all[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_tw_all[[i]], models_re_all[[i]])$p.value
  # j <- j + 1
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
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow)),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow)),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow)),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow)),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (GDP_growth)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow)), "GDP_growth"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow)), "GDP_growth"])$p.value
  results[l:j, 3] <- "-"
  j <- j + 1
  l <- j
  # PVAR
  k <- j + length(summary(models_pvar_all[[i]])$results$GDP_growth@coef.names) - 1
  results[j:k, 4] <- summary(models_pvar_all[[i]])$results$GDP_growth@coef.names
  results[j:k, 5] <- summary(models_pvar_all[[i]])$results$GDP_growth@coef
  results[j:k, 6] <- summary(models_pvar_all[[i]])$results$GDP_growth@pvalues
  j <- j + length(summary(models_pvar_all[[i]])$results$GDP_growth@coef.names)
  results[j, 4] <- "No. of instruments/observations"
  results[j, 5] <- summary(models_pvar_all[[i]])$max_instr_dependent_vars
  results[j, 6] <- summary(models_pvar_all[[i]])$nof_observations
  j <- j + 1
  results[j, 4] <- "Hansen test"
  results[j, 5] <- hansen_j_test(models_pvar_all[[i]])$statistic
  results[j, 6] <- hansen_j_test(models_pvar_all[[i]])$p.value
  results[l:j, 3] <- "PVAR (GDP_growth)"
  results[m:j, 1] <- names(group_of_countries)[i]
  results[m:j, 2] <- "All"
  j <- j + 1
  l <- j
  m <- j
  # Effects
  p <- n + length(fixef(models_fe_time_all[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_all[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_all[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_all[[i]]))
  p <- n + length(fixef(models_fe_ind_all[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_ind_all[[i]]))
  effects[n:p, 5] <- fixef(models_fe_ind_all[[i]])
  effects[n:p, 3] <- "FE (ind)"
  n <- n + length(fixef(models_fe_ind_all[[i]]))
  p <- n + length(fixef(models_fe_tw_all[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_all[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_all[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  n <- n + length(fixef(models_fe_tw_all[[i]], effect = "time"))
  p <- n + length(fixef(models_fe_tw_all[[i]], effect = "individual")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_all[[i]], effect = "individual"))
  effects[n:p, 5] <- fixef(models_fe_tw_all[[i]], effect = "individual")
  effects[n:p, 3] <- "Two-ways FE (individual)"
  effects[o:p, 1] <- names(group_of_countries)[i]
  effects[o:p, 2] <- "All"
  n <- n + length(fixef(models_fe_tw_all[[i]], effect = "individual"))
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
  # k <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- rownames(summary(models_re_combustion[[i]])[["coefficients"]])
  # results[j:k, 5] <- summary(models_re_combustion[[i]])[["coefficients"]][,1]
  # results[j:k, 6] <- summary(models_re_combustion[[i]])[["coefficients"]][,4]
  # j <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1])
  # results[j, 4] <- "R^2"
  # results[j, 5] <- summary(models_re_combustion[[i]])$r.squared[1]
  # results[j, 6] <- summary(models_re_combustion[[i]])$r.squared[2]
  # j <- j + 1
  # results[j, 4] <- "F test"
  # results[j, 5] <- summary(models_re_combustion[[i]])$fstatistic$statistic
  # results[j, 6] <- summary(models_re_combustion[[i]])$fstatistic$p.value
  # j <- j + 1
  # results[j, 4] <- "BP LM test"
  # results[j, 5] <- pcdtest(models_re_combustion[[i]], test = c("lm"))$statistic
  # results[j, 6] <- pcdtest(models_re_combustion[[i]], test = c("lm"))$p.value
  # j <- j + 1
  # results[j, 4] <- "Pesaran CD test"
  # results[j, 5] <- pcdtest(models_re_combustion[[i]], test = c("cd"))$statistic
  # results[j, 6] <- pcdtest(models_re_combustion[[i]], test = c("cd"))$p.value
  # j <- j + 1
  # results[j, 4] <- "B-G/Wooldrige test"
  # results[j, 5] <- pbgtest(models_re_combustion[[i]])$statistic
  # results[j, 6] <- pbgtest(models_re_combustion[[i]])$p.value
  # j <- j + 1
  # k <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- paste0(rownames(summary(models_re_combustion[[i]])[["coefficients"]]), " [Arellano]")
  # results[j:k, 5] <- coeftest(models_re_combustion[[i]], vcovHC(models_re_combustion[[i]], method = "arellano"))[,1]
  # results[j:k, 6] <- coeftest(models_re_combustion[[i]], vcovHC(models_re_combustion[[i]], method = "arellano"))[,4]
  # j <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1])
  # k <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- paste0(rownames(summary(models_re_combustion[[i]])[["coefficients"]]), " [HC3]")
  # results[j:k, 5] <- coeftest(models_re_combustion[[i]], models_re_combustion[[i]]$vcov)[,1]
  # results[j:k, 6] <- coeftest(models_re_combustion[[i]], models_re_combustion[[i]]$vcov)[,4]
  # results[l:k, 3] <- "RE"
  # j <- j + length(summary(models_re_combustion[[i]])[["coefficients"]][,1])
  # l <- j
  # One way fixed time effects model
  # results[j, 4] <- "Hausman test (time)"
  # results[j, 5] <- phtest(models_fe_time_combustion[[i]], models_re_combustion[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_time_combustion[[i]], models_re_combustion[[i]])$p.value
  # j <- j + 1
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
  # results[j, 4] <- "Hausman test (individual)"
  # results[j, 5] <- phtest(models_fe_ind_combustion[[i]], models_re_combustion[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_ind_combustion[[i]], models_re_combustion[[i]])$p.value
  # j <- j + 1
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
  # results[j, 4] <- "Hausman test (two-ways)"
  # results[j, 5] <- phtest(models_fe_tw_combustion[[i]], models_re_combustion[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_tw_combustion[[i]], models_re_combustion[[i]])$p.value
  # j <- j + 1
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
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 1),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (GDP_growth)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow) &
                                         Data$IsCombustionSector == 1), "GDP_growth"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow) &
                                         Data$IsCombustionSector == 1), "GDP_growth"])$p.value
  results[l:j, 3] <- "-"
  j <- j + 1
  l <- j
  # PVAR
  k <- j + length(summary(models_pvar_combustion[[i]])$results$GDP_growth@coef.names) - 1
  results[j:k, 4] <- summary(models_pvar_combustion[[i]])$results$GDP_growth@coef.names
  results[j:k, 5] <- summary(models_pvar_combustion[[i]])$results$GDP_growth@coef
  results[j:k, 6] <- summary(models_pvar_combustion[[i]])$results$GDP_growth@pvalues
  j <- j + length(summary(models_pvar_combustion[[i]])$results$GDP_growth@coef.names)
  results[j, 4] <- "No. of instruments/observations"
  results[j, 5] <- summary(models_pvar_combustion[[i]])$max_instr_dependent_vars
  results[j, 6] <- summary(models_pvar_combustion[[i]])$nof_observations
  j <- j + 1
  results[j, 4] <- "Hansen test"
  results[j, 5] <- hansen_j_test(models_pvar_combustion[[i]])$statistic
  results[j, 6] <- hansen_j_test(models_pvar_combustion[[i]])$p.value
  results[l:j, 3] <- "PVAR (GDP_growth)"
  results[m:j, 1] <- names(group_of_countries)[i]
  results[m:j, 2] <- "Combustion"
  j <- j + 1
  l <- j
  m <- j
  # Effects
  p <- n + length(fixef(models_fe_time_combustion[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_combustion[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_combustion[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_combustion[[i]]))
  p <- n + length(fixef(models_fe_ind_combustion[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_ind_combustion[[i]]))
  effects[n:p, 5] <- fixef(models_fe_ind_combustion[[i]])
  effects[n:p, 3] <- "FE (ind)"
  n <- n + length(fixef(models_fe_ind_combustion[[i]]))
  p <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_combustion[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_combustion[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  n <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "time"))
  p <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "individual")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_combustion[[i]], effect = "individual"))
  effects[n:p, 5] <- fixef(models_fe_tw_combustion[[i]], effect = "individual")
  effects[n:p, 3] <- "Two-ways FE (individual)"
  effects[o:p, 1] <- names(group_of_countries)[i]
  effects[o:p, 2] <- "Combustion"
  n <- n + length(fixef(models_fe_tw_combustion[[i]], effect = "individual"))
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
  # k <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- rownames(summary(models_re_other[[i]])[["coefficients"]])
  # results[j:k, 5] <- summary(models_re_other[[i]])[["coefficients"]][,1]
  # results[j:k, 6] <- summary(models_re_other[[i]])[["coefficients"]][,4]
  # j <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1])
  # results[j, 4] <- "R^2"
  # results[j, 5] <- summary(models_re_other[[i]])$r.squared[1]
  # results[j, 6] <- summary(models_re_other[[i]])$r.squared[2]
  # j <- j + 1
  # results[j, 4] <- "F test"
  # results[j, 5] <- summary(models_re_other[[i]])$fstatistic$statistic
  # results[j, 6] <- summary(models_re_other[[i]])$fstatistic$p.value
  # j <- j + 1
  # results[j, 4] <- "BP LM test"
  # results[j, 5] <- pcdtest(models_re_other[[i]], test = c("lm"))$statistic
  # results[j, 6] <- pcdtest(models_re_other[[i]], test = c("lm"))$p.value
  # j <- j + 1
  # results[j, 4] <- "Pesaran CD test"
  # results[j, 5] <- pcdtest(models_re_other[[i]], test = c("cd"))$statistic
  # results[j, 6] <- pcdtest(models_re_other[[i]], test = c("cd"))$p.value
  # j <- j + 1
  # results[j, 4] <- "B-G/Wooldrige test"
  # results[j, 5] <- pbgtest(models_re_other[[i]])$statistic
  # results[j, 6] <- pbgtest(models_re_other[[i]])$p.value
  # j <- j + 1
  # k <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- paste0(rownames(summary(models_re_other[[i]])[["coefficients"]]), " [Arellano]")
  # results[j:k, 5] <- coeftest(models_re_other[[i]], vcovHC(models_re_other[[i]], method = "arellano"))[,1]
  # results[j:k, 6] <- coeftest(models_re_other[[i]], vcovHC(models_re_other[[i]], method = "arellano"))[,4]
  # j <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1])
  # k <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1]) - 1
  # results[j:k, 4] <- paste0(rownames(summary(models_re_other[[i]])[["coefficients"]]), " [HC3]")
  # results[j:k, 5] <- coeftest(models_re_other[[i]], models_re_other[[i]]$vcov)[,1]
  # results[j:k, 6] <- coeftest(models_re_other[[i]], models_re_other[[i]]$vcov)[,4]
  # results[l:k, 3] <- "RE"
  # j <- j + length(summary(models_re_other[[i]])[["coefficients"]][,1])
  # l <- j
  # One way fixed time effects model
  # results[j, 4] <- "Hausman test (time)"
  # results[j, 5] <- phtest(models_fe_time_other[[i]], models_re_other[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_time_other[[i]], models_re_other[[i]])$p.value
  # j <- j + 1
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
  # results[j, 4] <- "Hausman test (individual)"
  # results[j, 5] <- phtest(models_fe_ind_other[[i]], models_re_other[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_ind_other[[i]], models_re_other[[i]])$p.value
  # j <- j + 1
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
  # results[j, 4] <- "Hausman test (two-ways)"
  # results[j, 5] <- phtest(models_fe_tw_other[[i]], models_re_other[[i]])$statistic
  # results[j, 6] <- phtest(models_fe_tw_other[[i]], models_re_other[[i]])$p.value
  # j <- j + 1
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
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = FALSE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = FALSE)$p.value
  j <- j + 1
  results[j, 4] <- "BP test (stud)"
  results[j, 5] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = TRUE)$statistic
  results[j, 6] <- bptest(formula, data = Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & 
                                                       !is.na(Data$Allow) & Data$IsCombustionSector == 0),], studentize = TRUE)$p.value
  j <- j + 1
  results[j, 4] <- "ADF test (GDP_growth)"
  results[j, 5] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow) &
                                         Data$IsCombustionSector == 0), "GDP_growth"])$statistic
  results[j, 6] <- adf.test(Data[which(Data$Compliance_Country %in% group_of_countries[[i]] & !is.na(Data$GDP_growth) & !is.na(Data$Allow) &
                                         Data$IsCombustionSector == 0), "GDP_growth"])$p.value
  results[l:j, 3] <- "-"
  j <- j + 1
  l <- j
  # PVAR
  k <- j + length(summary(models_pvar_other[[i]])$results$GDP_growth@coef.names) - 1
  results[j:k, 4] <- summary(models_pvar_other[[i]])$results$GDP_growth@coef.names
  results[j:k, 5] <- summary(models_pvar_other[[i]])$results$GDP_growth@coef
  results[j:k, 6] <- summary(models_pvar_other[[i]])$results$GDP_growth@pvalues
  j <- j + length(summary(models_pvar_other[[i]])$results$GDP_growth@coef.names)
  results[j, 4] <- "No. of instruments/observations"
  results[j, 5] <- summary(models_pvar_other[[i]])$max_instr_dependent_vars
  results[j, 6] <- summary(models_pvar_other[[i]])$nof_observations
  j <- j + 1
  results[j, 4] <- "Hansen test"
  results[j, 5] <- hansen_j_test(models_pvar_other[[i]])$statistic
  results[j, 6] <- hansen_j_test(models_pvar_other[[i]])$p.value
  results[l:j, 3] <- "PVAR (GDP_growth)"
  results[m:j, 1] <- names(group_of_countries)[i]
  results[m:j, 2] <- "Other"
  j <- j + 1
  l <- j
  m <- j
  # Effects
  p <- n + length(fixef(models_fe_time_other[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_time_other[[i]]))
  effects[n:p, 5] <- fixef(models_fe_time_other[[i]])
  effects[n:p, 3] <- "FE (time)"
  n <- n + length(fixef(models_fe_time_other[[i]]))
  p <- n + length(fixef(models_fe_ind_other[[i]])) - 1
  effects[n:p, 4] <- names(fixef(models_fe_ind_other[[i]]))
  effects[n:p, 5] <- fixef(models_fe_ind_other[[i]])
  effects[n:p, 3] <- "FE (ind)"
  n <- n + length(fixef(models_fe_ind_other[[i]]))
  p <- n + length(fixef(models_fe_tw_other[[i]], effect = "time")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_other[[i]], effect = "time"))
  effects[n:p, 5] <- fixef(models_fe_tw_other[[i]], effect = "time")
  effects[n:p, 3] <- "Two-ways FE (time)"
  n <- n + length(fixef(models_fe_tw_other[[i]], effect = "time"))
  p <- n + length(fixef(models_fe_tw_other[[i]], effect = "individual")) - 1
  effects[n:p, 4] <- names(fixef(models_fe_tw_other[[i]], effect = "individual"))
  effects[n:p, 5] <- fixef(models_fe_tw_other[[i]], effect = "individual")
  effects[n:p, 3] <- "Two-ways FE (individual)"
  effects[o:p, 1] <- names(group_of_countries)[i]
  effects[o:p, 2] <- "Other"
  n <- n + length(fixef(models_fe_tw_other[[i]], effect = "individual"))
  o <- n
}
remove(i, j, k, l, m, n, o, group_of_countries)

# Writing results to a file
results <- results[complete.cases(results),]
results <- as.data.frame(results)
colnames(results) <- c("Group of countries", "Sector", "Model", "Variable", "Coefficient", "P-val/Adj")
results <- results %>% mutate(Coefficient = as.numeric(Coefficient), `P-val/Adj` = as.numeric(`P-val/Adj`))

effects <- effects[complete.cases(effects),]
effects <- as.data.frame(effects)
colnames(effects) <- c("Group of countries", "Sector", "Model", "Variable", "Effect")
effects <- effects %>% mutate(Effect = as.numeric(Effect))

write.xlsx(results, file = "Results/Results_GDP_groups_sectors_countries_years.xlsx", showNA = FALSE, row.names = FALSE)
write.xlsx(effects, file = "Results/Effects_GDP_groups_sectors_countries_years.xlsx", showNA = FALSE, row.names = FALSE)

# Cleaning up the environment
rm(list = ls())