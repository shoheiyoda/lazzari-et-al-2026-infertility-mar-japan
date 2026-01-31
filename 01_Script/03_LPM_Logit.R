#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 03_LPM_Logit.R
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list = ls())

source("01_Script/00_SetParameters.R")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Set variable labels ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
coef_map_Tab2_3 <- c(
  "EduW_VS_JC_dum"      = "Wife's Education: VS/JC",
  "EduW_JHS_HS_dum"     = "Wife's Education: JHS/HS",
  "EduH_VS_JC_dum"      = "Husband's Education: VS/JC",
  "EduH_JHS_HS_dum"     = "Husband's Education: JHS/HS",
  "YearSvy_cnt2002"     = "Survey Year (-2002)",
  "CAgeCMarW_24_dum"    = "Wife's Age at First Marriage: 24-",
  "CAgeCMarW_30_34_dum" = "Wife's Age at First Marriage: 30-34",
  "CAgeCMarW_35_39_dum" = "Wife's Age at First Marriage: 35-39",
  "CAgeCMarW_40_44_dum" = "Wife's Age at First Marriage: 40-44",
  "CAgeCMarW_45_49_dum" = "Wife's Age at First Marriage: 45-49",
  "EduW_VS_JC_dum:YearSvy_cnt2002"  = "Survey Year x Wife's Education: VS/JC",
  "EduW_JHS_HS_dum:YearSvy_cnt2002" = "Survey Year x Wife's Education: JHS/HS",
  "(Intercept)" = "(Intercept)"
)

coef_map_Tab4 <- c(
  "EduW_VS_JC_dum"      = "Wife's Education: VS/JC",
  "EduW_JHS_HS_dum"     = "Wife's Education: JHS/HS",
  "EduH_VS_JC_dum"      = "Husband's Education: VS/JC",
  "EduH_JHS_HS_dum"     = "Husband's Education: JHS/HS",
  "as.factor(YearSvy)2021" = "Survey Year : 2021",
  "CAgeCMarW_24_dum"    = "Wife's Age at First Marriage: 24-",
  "CAgeCMarW_30_34_dum" = "Wife's Age at First Marriage: 30-34",
  "CAgeCMarW_35_39_dum" = "Wife's Age at First Marriage: 35-39",
  "CAgeCMarW_40_44_dum" = "Wife's Age at First Marriage: 40-44",
  "CAgeCMarW_45_49_dum" = "Wife's Age at First Marriage: 45-49",
  "(Intercept)" = "(Intercept)"
)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table 2: Infertility concerns ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_InfWorry <- 
  readRDS(here(tblDir, "tbl_Tab1_Tab2_4.rds")) |> 
  select(YearSvy_cnt2002, CAgeCMarW5y, Edu3GrpW, Edu3GrpH, InfWorry_bi, ends_with("_dum"), n) |> 
  drop_na() |> 
  uncount(n)

## Estimate models: LPM and Logit ----
LPM1_InfWorry   <-  lm(InfWorry_bi ~ EduW_JHS_HS_dum + EduW_VS_JC_dum + YearSvy_cnt2002, data = DF_InfWorry)
Logit1_InfWorry <- glm(InfWorry_bi ~ EduW_JHS_HS_dum + EduW_VS_JC_dum + YearSvy_cnt2002, data = DF_InfWorry, family = "binomial")

LPM2_InfWorry   <-  update(LPM1_InfWorry,   . ~ . + CAgeCMarW_24_dum + CAgeCMarW_30_34_dum + CAgeCMarW_35_39_dum + CAgeCMarW_40_44_dum + CAgeCMarW_45_49_dum)
Logit2_InfWorry <-  update(Logit1_InfWorry, . ~ . + CAgeCMarW_24_dum + CAgeCMarW_30_34_dum + CAgeCMarW_35_39_dum + CAgeCMarW_40_44_dum + CAgeCMarW_45_49_dum)

LPM3_InfWorry   <- update(LPM2_InfWorry,   . ~ . + EduH_JHS_HS_dum + EduH_VS_JC_dum)
Logit3_InfWorry <- update(Logit2_InfWorry, . ~ . + EduH_JHS_HS_dum + EduH_VS_JC_dum)

LPM4_InfWorry   <- update(LPM3_InfWorry,   . ~ . + EduW_JHS_HS_dum:YearSvy_cnt2002 + EduW_VS_JC_dum:YearSvy_cnt2002)
Logit4_InfWorry <- update(Logit3_InfWorry, . ~ . + EduW_JHS_HS_dum:YearSvy_cnt2002 + EduW_VS_JC_dum:YearSvy_cnt2002)

ListModel_InfWorry_LPM <- 
  list("Model1" = LPM1_InfWorry,
       "Model2" = LPM2_InfWorry,
       "Model3" = LPM3_InfWorry,
       "Model4" = LPM4_InfWorry)

ListModel_InfWorry_Logit <-
  list("Model1" = Logit1_InfWorry,
       "Model2" = Logit2_InfWorry,
       "Model3" = Logit3_InfWorry,
       "Model4" = Logit4_InfWorry)

## Regression table ----
### LPM ----
tab2save_InfWorry_LPM <- 
  modelsummary(ListModel_InfWorry_LPM, coef_map = coef_map_Tab2_3,
               title = "Table 2. Liner probability models predicting infertility concerns",
               stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
               gof_omit = "AIC|BIC|Log.Lik.|RMSE",
               output = "gt")

tab2save_InfWorry_LPM |> gtsave(filename = here(OutDir, "Table2.docx"))

### Logit ----
tab2save_InfWorry_Logit <-
  modelsummary(ListModel_InfWorry_Logit, coef_map = coef_map_Tab2_3,
             title = "Table A-2. Logit models predicting infertility concerns",
             stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
             gof_omit = "AIC|BIC|Log.Lik.|RMSE",
             output = "gt")

tab2save_InfWorry_Logit |> gtsave(filename = here(OutDir, "TableA-2.docx"))

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table 3: MAR use ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_InfTreat <- 
  readRDS(here(tblDir, "tbl_Tab1_Tab2_4.rds")) |> 
  select(YearSvy_cnt2002, CAgeCMarW5y, Edu3GrpW, Edu3GrpH, InfTreat_bi, ends_with("_dum"), n) |> 
  drop_na() |> 
  uncount(n)

## Estimate models: LPM and Logit ----
LPM1_InfTreat   <-  lm(InfTreat_bi ~ EduW_JHS_HS_dum + EduW_VS_JC_dum + YearSvy_cnt2002, data = DF_InfTreat)
Logit1_InfTreat <- glm(InfTreat_bi ~ EduW_JHS_HS_dum + EduW_VS_JC_dum + YearSvy_cnt2002, data = DF_InfTreat, family = "binomial")

LPM2_InfTreat   <-  update(LPM1_InfTreat,   . ~ . + CAgeCMarW_24_dum + CAgeCMarW_30_34_dum + CAgeCMarW_35_39_dum + CAgeCMarW_40_44_dum + CAgeCMarW_45_49_dum)
Logit2_InfTreat <-  update(Logit1_InfTreat, . ~ . + CAgeCMarW_24_dum + CAgeCMarW_30_34_dum + CAgeCMarW_35_39_dum + CAgeCMarW_40_44_dum + CAgeCMarW_45_49_dum)

LPM3_InfTreat   <- update(LPM2_InfTreat,   . ~ . + EduH_JHS_HS_dum + EduH_VS_JC_dum)
Logit3_InfTreat <- update(Logit2_InfTreat, . ~ . + EduH_JHS_HS_dum + EduH_VS_JC_dum)

LPM4_InfTreat   <- update(LPM3_InfTreat,   . ~ . + EduW_JHS_HS_dum:YearSvy_cnt2002 + EduW_VS_JC_dum:YearSvy_cnt2002)
Logit4_InfTreat <- update(Logit3_InfTreat, . ~ . + EduW_JHS_HS_dum:YearSvy_cnt2002 + EduW_VS_JC_dum:YearSvy_cnt2002)


ListModel_InfTreat_LPM <- 
  list("Model1" = LPM1_InfTreat,
       "Model2" = LPM2_InfTreat,
       "Model3" = LPM3_InfTreat,
       "Model4" = LPM4_InfTreat)

ListModel_InfTreat_Logit <-
  list("Model1" = Logit1_InfTreat,
       "Model2" = Logit2_InfTreat,
       "Model3" = Logit3_InfTreat,
       "Model4" = Logit4_InfTreat)
  
## Regression table ----
### LPM ----
tab2save_InfTreat_LPM <- 
  modelsummary(ListModel_InfTreat_LPM, coef_map = coef_map_Tab2_3,
               title = "Table 3. Liner probability models predicting MAR use",
               stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
               gof_omit = "AIC|BIC|Log.Lik.|RMSE",
               output = "gt")

tab2save_InfTreat_LPM |> gtsave(filename = here(OutDir, "Table3.docx"))

### Logit ----
tab2save_InfTreat_Logit <-
  modelsummary(ListModel_InfTreat_Logit, coef_map = coef_map_Tab2_3,
               title = "Table A-2. Logit models predicting infertility concerns",
               stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
               gof_omit = "AIC|BIC|Log.Lik.|RMSE",
               output = "gt") 
  
tab2save_InfTreat_Logit |> gtsave(filename = here(OutDir, "TableA-3.docx"))

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table 4: ART outcomes ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_InfTreatRslt <-
  readRDS(here(tblDir, "tbl_Tab1_Tab2_4.rds")) |> 
  select(YearSvy, CAgeCMarW5y, Edu3GrpW, Edu3GrpH, InfTreatRslt_bi, ends_with("_dum"), n) |> 
  drop_na() |> 
  uncount(n)

# note: ART outcomes are measured in the 15h (2015) and 16th (2021) JNFS only.

## Estimate models: LPM and Logit ----
LPM1_InfTreatRslt   <-  lm(InfTreatRslt_bi ~ EduW_JHS_HS_dum + EduW_VS_JC_dum + as.factor(YearSvy), data = DF_InfTreatRslt)
Logit1_InfTreatRslt <- glm(InfTreatRslt_bi ~ EduW_JHS_HS_dum + EduW_VS_JC_dum + as.factor(YearSvy), data = DF_InfTreatRslt, family = "binomial")

LPM2_InfTreatRslt   <- update(LPM1_InfTreatRslt,   . ~ . + CAgeCMarW_24_dum + CAgeCMarW_30_34_dum + CAgeCMarW_35_39_dum + CAgeCMarW_40_44_dum + CAgeCMarW_45_49_dum)
Logit2_InfTreatRslt <- update(Logit1_InfTreatRslt, . ~ . + CAgeCMarW_24_dum + CAgeCMarW_30_34_dum + CAgeCMarW_35_39_dum + CAgeCMarW_40_44_dum + CAgeCMarW_45_49_dum)

LPM3_InfTreatRslt   <- update(LPM2_InfTreatRslt,   . ~ . + EduH_JHS_HS_dum + EduH_VS_JC_dum)
Logit3_InfTreatRslt <- update(Logit2_InfTreatRslt, . ~ . + EduH_JHS_HS_dum + EduH_VS_JC_dum)

ListModel_InfTreatRslt_LPM <- 
  list("Model1" = LPM1_InfTreatRslt,
       "Model2" = LPM2_InfTreatRslt,
       "Model3" = LPM3_InfTreatRslt)

ListModel_InfTreatRslt_Logit <- 
  list("Model1" = Logit1_InfTreatRslt,
       "Model2" = Logit2_InfTreatRslt,
       "Model3" = Logit3_InfTreatRslt)

## Regression table ----
### LPM ----
tab2save_InfTreatRslt_LPM <- 
  modelsummary(ListModel_InfTreatRslt_LPM, coef_map = coef_map_Tab4,
               title = "Table 4. Linear probability models predicting MAR success rates (among those who have used MAR treatment)",
               stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
               gof_omit = "AIC|BIC|Log.Lik.|RMSE",
               output = "gt")

tab2save_InfTreatRslt_LPM |> gtsave(filename = here(OutDir, "Table4.docx"))

### Logit ----
tab2save_InfTreatRslt_Logit <- 
  modelsummary(ListModel_InfTreatRslt_Logit, coef_map = coef_map_Tab4,
               title = "Table A-4. Linear probability models predicting MAR success rates (among those who have used MAR treatment)",
               stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
               gof_omit = "AIC|BIC|Log.Lik.|RMSE",
               output = "gt")

tab2save_InfTreatRslt_Logit |> gtsave(filename = here(OutDir, "TableA-4.docx"))