#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 01_SelectVarsCases
#
#  Important note:
#  - Running this script requires access to the JNFS microdata. The resulting tibbles are already saved in `00_Data`.
#  - The JNFS microdata used in this study were harmonized across survey waves by the authors. 
#    As a result, this script cannot be applied directly to the raw JNFS microdata obtained through a secondary-use application. 
#    Questions regarding the data harmonization procedures can be directed to the authors.
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list = ls())

source("01_Script/00_SetParameters.R")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Read data ----
DF_raw <- 
  read_dta(paste0(JNFS_dir, "Nfs2021V3CAll.dta")) |> 
  select(SurveyRound, YearSvy, CAgeSvyW, 
         StrMarStat,
         NofChildEvb, NofChildIdl, NofChildExp,
         CAgeCMarW, CAgeCMarW5y, 
         SchoolGrd5GrpW, SchoolGrd5GrpH, 
         InfWorTre, InfTreatRslt,
         InfTreatInvNat01,
         InfTreatInvNat02,
         InfTreatInvNat03,
         InfTreatInvNat04,
         InfTreatTimingNat01,
         InfTreatTimingNat02,
         InfTreatTimingNat03,
         InfTreatTimingNat04,
         
         DiffRsnOkane_bi,
         DiffRsnIe_bi,
         DiffRsnShigoto_bi,
         DiffRsnNobinobi_bi,
         DiffRsnJibun_bi,
         DiffRsnKorei_bi,
         DiffRsnFutan_bi,
         DiffRsnKenko_bi,
         DiffRsnDekinai_bi,
         DiffRsnKyoryoku_bi,
         DiffRsnOtto_bi,
         DiffRsnTeinen_bi,
         DiffRsnOtherAll_bi,
         
         starts_with("UnacvRsn"),
         
         RDurMonthCMarNat01
  )  

# Set variables ----
DF <- 
  DF_raw |> 
  mutate(YearSvy_2002_dum = if_else(YearSvy == 2002, 1, 0),
         YearSvy_2005_dum = if_else(YearSvy == 2005, 1, 0),
         YearSvy_2010_dum = if_else(YearSvy == 2010, 1, 0),
         YearSvy_2015_dum = if_else(YearSvy == 2015, 1, 0),
         YearSvy_2021_dum = if_else(YearSvy == 2021, 1, 0),
         
         YearSvy_cnt2002 = YearSvy - 2002,

         CAgeCMarW_24_dum    = if_else(CAgeCMarW5y <= 2, 1, 0),
         CAgeCMarW_25_29_dum = if_else(CAgeCMarW5y == 3, 1, 0),
         CAgeCMarW_30_34_dum = if_else(CAgeCMarW5y == 4, 1, 0),
         CAgeCMarW_35_39_dum = if_else(CAgeCMarW5y == 5, 1, 0),
         CAgeCMarW_40_44_dum = if_else(CAgeCMarW5y == 6, 1, 0),
         CAgeCMarW_45_49_dum = if_else(CAgeCMarW5y == 7, 1, 0),

         CAgeCMarW5y = case_when(CAgeCMarW5y <= 2 ~ "<25",
                                 CAgeCMarW5y == 3 ~ "25-29",
                                 CAgeCMarW5y == 4 ~ "30-34",
                                 CAgeCMarW5y == 5 ~ "35-39",
                                 CAgeCMarW5y == 6 ~ "40-44",
                                 CAgeCMarW5y == 7 ~ "45-49"),

         InfWorry_bi = case_match(InfWorTre,
                                  1      ~ 0L,
                                  c(2:9) ~ 1L,
                                  .default = NA_integer_),
         
         InfTreat_bi = case_match(InfWorTre,
                                  c(2,6)     ~ 0L,
                                  c(3,4,7,8) ~ 1L,
                                  .default = NA_integer_),
                  
         InfTreatRslt_bi = case_match(InfTreatRslt,
                                      0 ~ 0L,
                                      1 ~ 1L,
                                      .default = NA_integer_),
         
         
         InfWorryTreatRslt = case_when(InfWorry_bi == 0 ~ "Never worried",
                                       InfWorry_bi == 1 & InfTreat_bi == 0 ~ "Worried, not treated",
                                       InfWorry_bi == 1 & InfTreat_bi == 1 & InfTreatRslt_bi == 0 ~ "Worried, treated, not conceived",
                                       InfWorry_bi == 1 & InfTreat_bi == 1 & InfTreatRslt_bi == 1 ~ "Worried, treated, conceived") |> 
                             fct_relevel("Never worried",
                                        "Worried, not treated",
                                        "Worried, treated, not conceived",
                                        "Worried, treated, conceived"),
         
         Edu3GrpW = case_when(SchoolGrd5GrpW %in% c(1,2) ~ "JHS/HS",
                              SchoolGrd5GrpW %in% c(3,4) ~ "VS/JC", 
                              SchoolGrd5GrpW == 5        ~ "Univ") |> 
                    fct_relevel("JHS/HS", "VS/JC", "Univ"),
         
         EduW_JHS_HS_dum = if_else(SchoolGrd5GrpW %in% c(1,2), 1, 0),
         EduW_VS_JC_dum  = if_else(SchoolGrd5GrpW %in% c(3,4), 1, 0),
         EduW_Univ_dum   = if_else(SchoolGrd5GrpW %in% c(5),   1, 0),

         Edu3GrpH = case_match(SchoolGrd5GrpH,
                               c(1,2) ~ "JHS/HS",
                               c(3,4) ~ "VS/JC", 
                               5      ~ "Univ") |> 
                    fct_relevel("JHS/HS", "VS/JC", "Univ"),

         EduH_JHS_HS_dum = if_else(SchoolGrd5GrpH %in% c(1,2), 1, 0),
         EduH_VS_JC_dum  = if_else(SchoolGrd5GrpH %in% c(3,4), 1, 0),
         EduH_Univ_dum   = if_else(SchoolGrd5GrpH %in% c(5),   1, 0),
                  
         across(starts_with("UnacvRsn"), ~ ifelse(. == 9, NA_integer_, .),
                .names = "{.col}_bi"),
         
         across(starts_with("DiffRsn"), ~ ifelse(. == 9, NA_integer_, .))
         )

# Save data ----
## Figure 1 ----
DF |> 
  filter(CAgeSvyW %in% c(40:49)) |> 
  filter(NofChildIdl %in% c(0:8) & NofChildEvb %in% c(0:8)) |> 
  drop_na(Edu3GrpW) |> 
  count(YearSvy, NofChildIdl, NofChildEvb) |> 
  saveRDS(file = here(OutDir, "tbl_Fig1.rds"))

## Table 1-4 ----
DF |> 
  filter(CAgeSvyW %in% c(40:49)) |> 
  select(YearSvy, YearSvy_cnt2002, CAgeCMarW5y, CAgeCMarW, Edu3GrpW, Edu3GrpH, ends_with("_dum"),
         InfWorry_bi, InfTreat_bi, InfTreatRslt_bi) |> 
  count(across(everything())) |> 
  saveRDS(file = here(OutDir, "tbl_Tab1_Tab2_4.rds"))

## Figure 2 ----
DF |> 
  filter(YearSvy >= 2015) |> 
  filter(CAgeSvyW %in% c(40:49)) |> 
  filter(NofChildIdl %in% c(0:8) & NofChildEvb %in% c(0:8)) |> 
  select(YearSvy, CAgeCMarW5y, Edu3GrpW, Edu3GrpH,
         NofChildIdl, NofChildEvb, 
         starts_with(c("InfTreatInvNat", "InfTreatTimingNat"))
  ) |> 
  count(across(everything())) |> 
  saveRDS(file = here(OutDir, "tbl_Fig2.rds"))

## Figure 3 ----
DF |> 
  filter(YearSvy >= 2002) |> 
  filter(CAgeSvyW %in% c(40:49)) |> 
  filter(NofChildIdl %in% c(0:8) & NofChildEvb %in% c(0:8)) |> 
  select(YearSvy, CAgeCMarW5y, Edu3GrpW, Edu3GrpH, starts_with("DiffRsn")) |> 
  count(across(everything())) |> 
  saveRDS(file = here(OutDir, "tbl_Fig3.rds"))

## Duration from marriage to first childbirth ----
DF |> 
  filter(YearSvy >= 2002) |> 
  filter(CAgeSvyW %in% c(40:49)) |> 
  filter(NofChildIdl %in% c(0:8) & NofChildEvb %in% c(0:8)) |> 
  drop_na(CAgeCMarW, Edu3GrpW, Edu3GrpH, InfWorry_bi, RDurMonthCMarNat01) |> 
  saveRDS(file = here(OutDir, "df_Dur_Mar2FstBth.rds"))

