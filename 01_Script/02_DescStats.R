#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 02_DescStats.R
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list = ls())

source("01_Script/00_SetParameters.R")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Figure1: Average ideal and achieved number of children ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Number of observations for Figure 1 ----
NofObs_Fig1 <-
  readRDS(here(tblDir, "tbl_Fig1.rds")) |> 
  reframe(n = sum(n))

## Mean ideal and achieved number of children by survey year ----
tbl_MeanNofChildIdl <-
  readRDS(here(tblDir, "tbl_Fig1.rds")) |>
  group_by(YearSvy, NofChildIdl) |> 
  reframe(n = sum(n)) |> 
  group_by(YearSvy) |> 
  reframe(Mean_Ideal = weighted.mean(NofChildIdl, w = n))

tbl_MeanNofChildEvb <-
  readRDS(here(tblDir, "tbl_Fig1.rds")) |>
  group_by(YearSvy, NofChildEvb) |> 
  reframe(n = sum(n)) |> 
  group_by(YearSvy) |> 
  reframe(Mean_Achieved = weighted.mean(NofChildEvb, w = n))

tbl_MeanNofChildIdlEvb <-
  tbl_MeanNofChildIdl |> 
  left_join(tbl_MeanNofChildEvb, by = "YearSvy") |> 
  pivot_longer(cols = starts_with("Mean_"),
               names_to  = "IdlEvb",
               values_to = "Mean",
               names_prefix = "Mean_")

## Figure 1 ----
Fig_MeanNofChildIdlEvb <-
  tbl_MeanNofChildIdlEvb |> 
  mutate(IdlEvb = if_else(IdlEvb == "Ideal", "Ideal family size", "Achieved family size")) |> 
  ggplot(aes(x = YearSvy,
             y = Mean,
             color = IdlEvb)
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.4) +
  geom_text(
    aes(label = sprintf("%.2f", Mean)),
    vjust = -0.8,
    size = 3.0,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Ideal family size"    = "#EC6857",
               "Achieved family size" = "#244C7C")
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, 10)) +
  scale_y_continuous(
    limits = c(1.7, 2.75),
    breaks = seq(1.75, 2.75, 0.25),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    x = "Survey year",
    y = "Ideal/Achieved family size",
    color = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    # grid similar to your screenshot
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    
    # axes
    axis.text  = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    
    # legend at the bottom
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.key = element_blank(),
    
    # spacing
    plot.margin = margin(10, 15, 10, 15)
  )

print(Fig_MeanNofChildIdlEvb)

ggsave(filename = here(OutDir, "Figure1.pdf"),
       plot = Fig_MeanNofChildIdlEvb,
       width  = 9,
       height = 6)


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table1 - Panel a: Wife's age at first marriage ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_CAgeCMarW <-
  readRDS(here(tblDir, "tbl_Tab1_Tab2_4.rds")) |> 
  select(YearSvy, CAgeCMarW, Edu3GrpW, Edu3GrpH, InfWorry_bi, n) |> 
  drop_na() |> 
  select(YearSvy, CAgeCMarW, Edu3GrpW, n)

## Mean age at marriage ----
# overall
MeanAgeMarW <-
  DF_CAgeCMarW |> 
  group_by(YearSvy) |> 
  reframe(MeanAgeMarW = weighted.mean(CAgeCMarW + 0.5, w = n))

# by education
MeanAgeMarW_byEduW <-
  DF_CAgeCMarW |> 
  group_by(YearSvy, Edu3GrpW) |> 
  reframe(MeanAgeMarW = weighted.mean(CAgeCMarW + 0.5, w = n))

## ANOVA ----
anov_AgeCMarW <-
  DF_CAgeCMarW |> 
  uncount(weights = n) |> 
  group_by(YearSvy) |> 
  group_map(~ {
    model <- aov(CAgeCMarW ~ Edu3GrpW, data = .x)
    tidy(model) |> 
      filter(term == "Edu3GrpW") |> 
      mutate(YearSvy = .y$YearSvy)
  }) |> 
  bind_rows() |> 
  select(YearSvy, statistic, df, p.value)

table_MeanAgeMarW <-
  MeanAgeMarW |> 
  mutate(Edu3GrpW = as.factor("Total")) |> 
  relocate(YearSvy, Edu3GrpW, MeanAgeMarW) |> 
  bind_rows(MeanAgeMarW_byEduW) |> 
  pivot_wider(
    names_from  = Edu3GrpW,
    values_from = MeanAgeMarW
  ) |> 
  left_join(anov_AgeCMarW, by = "YearSvy")

## Table1: Panel a
table_MeanAgeMarW |> 
  mutate(p.value = round(p.value, 5)) |>
  setNames(c("Year", "Total", "JHS/HS", "VS/JC", "Univ", "F", "df", "p-value")) |>
  tt() |>
  format_tt(j = 2:6, sprintf = "%.1f", num_fmt = "significant_cell") |>
  format_tt(j = 7,   digits = 1) |>
  format_tt(j = "p-value",  sprintf = "%.3f") |> 
  style_tt(j = 2:6, align = "d") |>
  style_tt(
    i = c(0,5),
    line = "b",
    line_width = 0.1
  ) |>
  style_tt(
    i = 0,
    line = "t",
    line_width = 0.1
  ) |>
  save_tt(here(OutDir, "Table1a.docx"),
          overwrite = TRUE)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table1 - Panel b: Infertility concerns ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_InfWorry <- 
  readRDS(here(tblDir, "tbl_Tab1_Tab2_4.rds")) |> 
  select(YearSvy, YearSvy_cnt2002, CAgeCMarW5y, Edu3GrpW, Edu3GrpH, InfWorry_bi, ends_with("_dum"), n) |> 
  drop_na()

## % of married couples who had ever worried about infertility: overall and by education ----
# overall
Prop_InfWorry <-
  DF_InfWorry |> 
  group_by(YearSvy, InfWorry_bi) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  ungroup() 

# by education
Prop_InfWorry_byEduW <-
  DF_InfWorry |> 
  group_by(YearSvy, Edu3GrpW, InfWorry_bi) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy, Edu3GrpW) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  ungroup() 

## Table1: Panel b ----
chisq_InfWorry <-
  Prop_InfWorry_byEduW |>
  group_by(YearSvy) |>
  reframe(
    chi_sq_test = list(chisq.test(matrix(Obs, ncol = 2, byrow = TRUE)))
  ) |>
  mutate(
    Chisq   = map_dbl(chi_sq_test, ~ .x$statistic),
    df      = map_dbl(chi_sq_test, ~ .x$parameter),
    p_value = map_dbl(chi_sq_test, ~ .x$p.value)
  ) |>
  select(YearSvy, Chisq, df, p_value)

table_InfWorry <-
  Prop_InfWorry |> 
  mutate(Edu3GrpW = as.factor("Total")) |> 
  relocate(YearSvy, Edu3GrpW, InfWorry_bi, Obs, Prop) |> 
  bind_rows(Prop_InfWorry_byEduW) |> 
  filter(InfWorry_bi == 1) |>
  mutate(Pct = 100 * Prop) |>
  select(YearSvy, Edu3GrpW, Pct) |>
  pivot_wider(
    names_from  = Edu3GrpW,
    values_from = Pct
  ) |>
  left_join(chisq_InfWorry, by = "YearSvy")

table_InfWorry |>
  mutate(p_value = round(p_value, 5)) |>
  setNames(c("Year", "Total", "JHS/HS", "VS/JC", "Univ", "χ²", "df", "p-value")) |>
  tt() |>
  format_tt(j = 2:6, sprintf = "%.1f", num_fmt = "significant_cell") |>
  format_tt(j = 7,   digits = 1) |>
  format_tt(j = "p-value",  sprintf = "%.3f") |> 
  style_tt(j = 2:6, align = "d") |>
  style_tt(
    i = c(0,5),
    line = "b",
    line_width = 0.1
  ) |>
  style_tt(
    i = 0,
    line = "t",
    line_width = 0.1
  ) |>
  save_tt(here(OutDir, "Table1b.docx"),
          overwrite = TRUE)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table1 - Panel c: MAR use ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_InfTreat <- 
  readRDS(here(tblDir, "tbl_Tab1_Tab2_4.rds")) |> 
  select(YearSvy, YearSvy_cnt2002, CAgeCMarW5y, Edu3GrpW, Edu3GrpH, InfTreat_bi, ends_with("_dum"), n) |> 
  drop_na()

## % of married couples infertility treatment after having worried about infertility: overall and by education ----
# overall
Prop_InfTreat <-
  DF_InfTreat |> 
  group_by(YearSvy, InfTreat_bi) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  ungroup()

# by education
Prop_InfTreat_byEduW <-
  DF_InfTreat |> 
  group_by(YearSvy, Edu3GrpW, InfTreat_bi) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy, Edu3GrpW) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  ungroup()
  
## Table1: Panel c ----
chisq_InfTreat <-
  Prop_InfTreat_byEduW |>
  group_by(YearSvy) |>
  reframe(
    chi_sq_test = list(chisq.test(matrix(Obs, ncol = 2, byrow = TRUE)))
  ) |>
  mutate(
    Chisq   = map_dbl(chi_sq_test, ~ .x$statistic),
    df      = map_dbl(chi_sq_test, ~ .x$parameter),
    p_value = map_dbl(chi_sq_test, ~ .x$p.value)
  ) |>
  select(YearSvy, Chisq, df, p_value)

table_InfTreat <-
  Prop_InfTreat |> 
  mutate(Edu3GrpW = as.factor("Total")) |> 
  relocate(YearSvy, Edu3GrpW, InfTreat_bi, Obs, Prop) |> 
  bind_rows(Prop_InfTreat_byEduW) |>
  filter(InfTreat_bi == 1) |>
  mutate(Pct = 100 * Prop) |>
  select(YearSvy, Edu3GrpW, Pct) |>
  pivot_wider(
    names_from  = Edu3GrpW,
    values_from = Pct
  ) |>
  left_join(chisq_InfTreat, by = "YearSvy")

table_InfTreat |>
  mutate(p_value = round(p_value, 5)) |>
  setNames(c("Year", "Total", "JHS/HS", "VS/JC", "Univ", "χ²", "df", "p-value")) |>
  tt() |>
  format_tt(j = 2:6, sprintf = "%.1f", num_fmt = "significant_cell") |>
  format_tt(j = 7,   digits = 1) |>
  format_tt(j = "p-value",  sprintf = "%.3f") |> 
  style_tt(j = 2:6, align = "d") |>
  style_tt(
    i = c(0,5),
    line = "b",
    line_width = 0.1
  ) |>
  style_tt(
    i = 0,
    line = "t",
    line_width = 0.1
  ) |>
  save_tt(here(OutDir, "Table1c.docx"),
          overwrite = TRUE)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table1 - Panel d: MAR outcomes ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_InfTreatRslt <-
  readRDS(here(tblDir, "tbl_Tab1_Tab2_4.rds")) |> 
  select(YearSvy, CAgeCMarW5y, Edu3GrpW, Edu3GrpH, InfTreatRslt_bi, ends_with("_dum"), n) |> 
  drop_na()

# note: MAR outcomes are measured in the 15h (2015) and 16th (2021) JNFS only.

## # % of married couples having a MAR-conceived child after having used treatment: overall and by education ----
# overall
Prop_InfTreatRslt <-
  DF_InfTreatRslt |> 
  group_by(YearSvy, InfTreatRslt_bi) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  ungroup()

# by education
Prop_InfTreatRslt_byEduW <-
  DF_InfTreatRslt |> 
  group_by(YearSvy, Edu3GrpW, InfTreatRslt_bi) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy, Edu3GrpW) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  ungroup()


## Table1: Panel d ----
chisq_InfTreatRslt <-
  Prop_InfTreatRslt_byEduW |>
  group_by(YearSvy) |>
  reframe(
    chi_sq_test = list(chisq.test(matrix(Obs, ncol = 2, byrow = TRUE)))
  ) |>
  mutate(
    Chisq   = map_dbl(chi_sq_test, ~ .x$statistic),
    df      = map_dbl(chi_sq_test, ~ .x$parameter),
    p_value = map_dbl(chi_sq_test, ~ .x$p.value)
  ) |>
  select(YearSvy, Chisq, df, p_value)

table_InfTreatRslt <-
  Prop_InfTreatRslt |> 
  mutate(Edu3GrpW = as.factor("Total")) |> 
  relocate(YearSvy, Edu3GrpW, InfTreatRslt_bi, Obs, Prop) |> 
  bind_rows(Prop_InfTreatRslt_byEduW) |>
  filter(InfTreatRslt_bi == 1) |>
  mutate(Pct = 100 * Prop) |>
  select(YearSvy, Edu3GrpW, Pct) |>
  pivot_wider(
    names_from  = Edu3GrpW,
    values_from = Pct
  ) |>
  left_join(chisq_InfTreatRslt, by = "YearSvy")

table_InfTreatRslt |>
  mutate(p_value = round(p_value, 5)) |>
  setNames(c("Year", "Total", "JHS/HS", "VS/JC", "Univ", "χ²", "df", "p-value")) |>
  tt() |>
  format_tt(j = 2:6, sprintf = "%.1f", num_fmt = "significant_cell") |>
  format_tt(j = 7,   digits = 1) |>
  format_tt(j = "p-value",  sprintf = "%.3f") |> 
  style_tt(j = 2:6, align = "d") |>
  style_tt(
    i = c(0,5),
    line = "b",
    line_width = 0.1
  ) |>
  style_tt(
    i = 0,
    line = "t",
    line_width = 0.1
  ) |>
  save_tt(here(OutDir, "Table1d.docx"),
          overwrite = TRUE)


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Duration from marriage to first childbirth ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_DurMonthCMarNat01 <-
  readRDS(here(tblDir, "df_Dur_Mar2FstBth.rds"))

DurCMarNat01_byEdu <-
  DF_DurMonthCMarNat01 |> 
  mutate(RDurYearCMarNat01 = RDurMonthCMarNat01 / 12) |> 
  group_by(YearSvy, Edu3GrpW) |> 
  reframe(
    n = n(),
    mean_month = mean(RDurMonthCMarNat01),
    sd_month   = sd(RDurMonthCMarNat01),
    se_month   = sd_month / sqrt(n),
    
    mean_year  = mean(RDurYearCMarNat01),
    sd_year    = sd(RDurYearCMarNat01),
    se_year    = sd_year / sqrt(n)
  )

DurMonthCMarNat01_byEdu <-
  DurCMarNat01_byEdu |> 
  select(YearSvy, Edu3GrpW, mean_month) |> 
  pivot_wider(
    names_from  = Edu3GrpW,
    values_from = mean_month
  ) 
  
DurYearCMarNat01_byEdu <-
  DurCMarNat01_byEdu |> 
  select(YearSvy, Edu3GrpW, mean_year) |> 
  pivot_wider(
    names_from  = Edu3GrpW,
    values_from = mean_year
  ) 

DurMonthCMarNat01_byEdu |> 
  setNames(c("Year", "JHS/HS", "VS/JC", "Univ")) |>
  tt() |>
  format_tt(j = 2:4, sprintf = "%.1f", num_fmt = "significant_cell") |>
  style_tt(j = 2:4, align = "d") |>
  style_tt(
    i = c(0,5),
    line = "b",
    line_width = 0.1
  ) |>
  style_tt(
    i = 0,
    line = "t",
    line_width = 0.1
  ) |>
  save_tt(here(OutDir, "DurMonthMar2FstBth.docx"),
          overwrite = TRUE)

DurYearCMarNat01_byEdu |> 
  setNames(c("Year", "JHS/HS", "VS/JC", "Univ")) |>
  tt() |>
  format_tt(j = 2:4, sprintf = "%.2f", num_fmt = "significant_cell") |>
  style_tt(j = 2:4, align = "d") |>
  style_tt(
    i = c(0,5),
    line = "b",
    line_width = 0.1
  ) |>
  style_tt(
    i = 0,
    line = "t",
    line_width = 0.1
  ) |>
  save_tt(here(OutDir, "DurYearMar2FstBth.docx"),
          overwrite = TRUE)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Figure 2: Mean ideal and achieved family size with and without MAR ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_GapIE_ART <-
  readRDS(here(tblDir, "tbl_Fig2.rds")) |> 
  drop_na(YearSvy, CAgeCMarW5y, Edu3GrpW, Edu3GrpH) |> 
  mutate(ART_Bth1 = case_when(InfTreatInvNat01 == 1 | InfTreatTimingNat01 == 1 ~ 1L, .default = 0L),
         ART_Bth2 = case_when(InfTreatInvNat02 == 1 | InfTreatTimingNat02 == 1 ~ 1L, .default = 0L),
         ART_Bth3 = case_when(InfTreatInvNat03 == 1 | InfTreatTimingNat03 == 1 ~ 1L, .default = 0L),
         ART_Bth4 = case_when(InfTreatInvNat04 == 1 | InfTreatTimingNat04 == 1 ~ 1L, .default = 0L),
         
         NofBthART = ART_Bth1 + ART_Bth2 + ART_Bth3 + ART_Bth4,
         
         NofChildEvb_woART = NofChildEvb - NofBthART,
         
         DifNofChildIdlEvb_wART  = NofChildIdl - NofChildEvb,
         DifNofChildIdlEvb_woART = NofChildIdl - NofChildEvb_woART,
         
         DifNofChildIdlEvb3Grp_wART = case_when(NofChildIdl <  NofChildEvb ~ "Ideal < Achieved",
                                                NofChildIdl == NofChildEvb ~ "Ideal = Achieved",
                                                NofChildIdl >  NofChildEvb ~ "Ideal > Achieved",
                                                .default = NA_character_) |> 
                                      fct_relevel("Ideal < Achieved",
                                                  "Ideal = Achieved",
                                                  "Ideal > Achieved"),
         
         DifNofChildIdlEvb3Grp_woART = case_when(NofChildIdl <  NofChildEvb_woART ~ "Ideal < Achieved",
                                                 NofChildIdl == NofChildEvb_woART ~ "Ideal = Achieved",
                                                 NofChildIdl >  NofChildEvb_woART ~ "Ideal > Achieved",
                                                 .default = NA_character_) |> 
                                       fct_relevel("Ideal < Achieved",
                                                   "Ideal = Achieved",
                                                   "Ideal > Achieved")
  ) |> 
  select(YearSvy, Edu3GrpW, 
         NofChildIdl, NofChildEvb, NofChildEvb_woART,
         DifNofChildIdlEvb3Grp_wART, DifNofChildIdlEvb3Grp_woART, n)

## Number of observations for Figure 2 ----
NofObs_Fig2 <-
  DF_GapIE_ART |> 
  uncount(weight = n) |> 
  nrow()

tbl_MeanNofChildIdlEvb_noART_total <-
  DF_GapIE_ART |>
  group_by(YearSvy) |> 
  reframe(
    Mean_Ideal          = weighted.mean(NofChildIdl,       w = n),
    Mean_Achieved       = weighted.mean(NofChildEvb,       w = n),
    Mean_Achieved_woART = weighted.mean(NofChildEvb_woART, w = n)
  ) |>
  pivot_longer(starts_with("Mean_"),
               names_to  = "Type",
               values_to = "Mean",
               names_prefix = "Mean_") |>
  mutate(
    Type = ifelse(Type == "Achieved_woART", "Achieved\nwithout MAR", Type) |>
           fct_relevel("Ideal", "Achieved", "Achieved\nwithout MAR"),
    
    Edu3GrpW = "Total"
  ) |> 
  relocate(YearSvy, Edu3GrpW, Type, Mean)

tbl_MeanNofChildIdlEvb_noART_byEduW <-
  DF_GapIE_ART |> 
  group_by(YearSvy, Edu3GrpW) |> 
  reframe(
    Mean_Ideal          = weighted.mean(NofChildIdl,       w = n),
    Mean_Achieved       = weighted.mean(NofChildEvb,       w = n),
    Mean_Achieved_woART = weighted.mean(NofChildEvb_woART, w = n)
  ) |> 
  pivot_longer(starts_with("Mean_"),
               names_to  = "Type",
               values_to = "Mean",
               names_prefix = "Mean_") |> 
  mutate(Type = ifelse(Type == "Achieved_woART", "Achieved\nwithout MAR", Type) |> 
                fct_relevel("Ideal", "Achieved", "Achieved\nwithout MAR"))

tbl_MeanNofChildIdlEvb_noART <-
  tbl_MeanNofChildIdlEvb_noART_total |> 
  bind_rows(tbl_MeanNofChildIdlEvb_noART_byEduW) |> 
  mutate(Edu3GrpW = fct_relevel(Edu3GrpW, "Total", "JHS/HS", "VS/JC", "Univ"))

## Figure 2 ----
Edu_lab <- c("Junior High/\nSenior High",
             "Junior College/\nVocational School",
             "University")
names(Edu_lab) <- c("JHS/HS", "VS/JC", "Univ")
  
Fig_MeanNofChildIdlEvb_noART_byEduW <-
  tbl_MeanNofChildIdlEvb_noART |> 
  ggplot(aes(x = Type,
             y = Mean)
  ) +
  facet_grid(YearSvy ~ Edu3GrpW,
             labeller = labeller(Edu3GrpW = Edu_lab)) +
  geom_col(fill = "#EC6857") +
  geom_text(aes(label = sprintf("%.2f", Mean)), 
            vjust = -0.5, 
            size = 6) +
  scale_fill_manual(values = c("black", "gray50", "white")) +
  ylim(0, 3) +
  labs(x = NULL,
       y = "Average Ideal/Achieved Number of Children",
       caption = "Note: The statistics were calculated based on the sample of married women aged 40-49, with N = 5,954") +
  theme_few() +
  theme(legend.position = "none",
        plot.title   = element_text(size = rel(1.5), hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        strip.text   = element_text(size = rel(1.5)),
        strip.text.y.right = element_blank(),
        axis.text.x  = element_text(size = rel(1.3)),
        axis.text.y  = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(Fig_MeanNofChildIdlEvb_noART_byEduW)

ggsave(filename = here(OutDir, "Figure2.pdf"),
       plot = Fig_MeanNofChildIdlEvb_noART_byEduW,
       width = 12,
       height = 6)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Figure 3: Self-reported reasons for not achieving the ideal number of children (total population) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
DF_DifRsn <-
  readRDS(here(tblDir, "tbl_Fig3.rds")) |> 
  drop_na()

label_order <- c(
  "It costs too much\nto raise and educate children",
  "Our house is\ntoo small",
  "It would interfere\nwith my job",
  "The environment is not conducive\nto a child's growth",
  "I want to enjoy my own\nand my husband's life",
  "I do not want to have a child\nat an older age",
  "I want to avoid the psychological\nand physical burden of child rearing",
  "Health reasons",
  "Because I cannot conceive\neven though I want children",
  "I cannot get my husband's cooperation\nin housework and childcare",
  "My husband does not want a child",
  "I want my youngest child to reach adulthood\nbefore my husband retires",
  "Others")

# Assign thematic category
category_map <- c(
  "It costs too much\nto raise and educate children"                       = "Economic and Practical Constraints",
  "Our house is\ntoo small"                                                = "Economic and Practical Constraints",
  "I want my youngest child to reach adulthood\nbefore my husband retires" = "Economic and Practical Constraints",
  "The environment is not conducive\nto a child's growth"                  = "Economic and Practical Constraints",
  
  "It would interfere\nwith my job"                                         = "Work–Life Balance and Lifestyle Preferences",
  "I want to enjoy my own\nand my husband's life"                           = "Work–Life Balance and Lifestyle Preferences",
  "I want to avoid the psychological\nand physical burden of child rearing" = "Work–Life Balance and Lifestyle Preferences",
  "I do not want to have a child\nat an older age"                          = "Work–Life Balance and Lifestyle Preferences",
  
  "Health reasons"                                                    = "Health and Relationship Barriers",
  "Because I cannot conceive\neven though I want children"            = "Health and Relationship Barriers",
  "I cannot get my husband's cooperation\nin housework and childcare" = "Health and Relationship Barriers",
  "My husband does not want a child"                                  = "Health and Relationship Barriers"
)

# Set category order manually
category_order <- c("Economic and Practical Constraints", 
                    "Work–Life Balance and Lifestyle Preferences", 
                    "Health and Relationship Barriers")

DF_DiffRsn_temp <-
  DF_DifRsn |>
  pivot_longer(cols = starts_with("DiffRsn"),
               names_to  = "DiffRsn",
               names_prefix = "DiffRsn",
               values_to = "response") |>
  mutate(DiffRsn = str_remove(DiffRsn, "_bi")) |> 
  mutate(DiffRsn = case_match(DiffRsn,
                             "Okane"     ~ label_order[1],
                             "Ie"        ~ label_order[2],
                             "Shigoto"   ~ label_order[3],
                             "Nobinobi"  ~ label_order[4],
                             "Jibun"     ~ label_order[5],
                             "Korei"     ~ label_order[6],
                             "Futan"     ~ label_order[7],
                             "Kenko"     ~ label_order[8],
                             "Dekinai"   ~ label_order[9],
                             "Kyoryoku"  ~ label_order[10],
                             "Otto"      ~ label_order[11],
                             "Teinen"    ~ label_order[12],
                             "OtherAll"  ~ label_order[13]) |> 
           fct_relevel(label_order)
           )

## Number of observations for Figure 3 ----
NofObs_Fig3 <-
  DF_DifRsn |> 
  nrow()

## Figure 3 ----
tbl_DiffRsn_temp <-
  DF_DiffRsn_temp |> 
  group_by(YearSvy, DiffRsn, response) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy, DiffRsn) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  filter(response == 1) |> 
  mutate(Category = recode(DiffRsn, !!!category_map)) |> 
  select(-c(response, Obs))

# Order reason_label within each category by 2021 values
ordering <- 
  tbl_DiffRsn_temp |> 
  filter(YearSvy == 2021) |> 
  arrange(Category, desc(Prop)) |> 
  pull(DiffRsn)

tbl_DiffRsn <-
  tbl_DiffRsn_temp |> 
  mutate(DiffRsn = factor(DiffRsn, levels = ordering))

# Split into three subsets
econ_tbl   <- tbl_DiffRsn %>% filter(Category == "Economic and Practical Constraints")
work_tbl   <- tbl_DiffRsn %>% filter(Category == "Work–Life Balance and Lifestyle Preferences")
health_tbl <- tbl_DiffRsn %>% filter(Category == "Health and Relationship Barriers")

# Helper function to create subplots
plot_category <- function(sub_df, cat_title) {
  sub_df |> 
    ggplot(aes(x = YearSvy,
               y = Prop * 100)) +
    geom_line(group = 1, color = "#244C7C") +
    geom_point(color = "#244C7C") +
    facet_wrap(~ DiffRsn, scales = "fixed", ncol = 4) +
    scale_y_continuous(limits = c(0, 55), expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Survey Year", 
      y = "Percentage", 
      title = cat_title
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(hjust = 1),
      panel.grid.minor = element_blank()
    )
}

# Create one plot per category
p1 <- plot_category(econ_tbl,   "Economic and Practical Constraints")
p2 <- plot_category(work_tbl,   "Work–Life Balance and Lifestyle Preferences")
p3 <- plot_category(health_tbl, "Health and Relationship Barriers")

Fig_DiffRsn <- 
  p1 / p2 / p3 + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "")

print(Fig_DiffRsn)

ggsave(filename = here(OutDir, "Figure3.pdf"),
       plot = Fig_DiffRsn,
       width  = 12,
       height = 10)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Figure A-1: Self-reported reasons for not achieving the ideal number of children, by education ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
tbl_DiffRsn_byEduW_temp <-
  DF_DiffRsn_temp |> 
  group_by(YearSvy, Edu3GrpW, DiffRsn, response) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy, Edu3GrpW, DiffRsn) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  filter(response == 1) |> 
  mutate(Category = recode(DiffRsn, !!!category_map)) |> 
  select(-c(response, Obs))

tbl_DiffRsn_byEduW <-
  tbl_DiffRsn_byEduW_temp |> 
  mutate(DiffRsn = factor(DiffRsn, levels = ordering))

# Split into three subsets
econ_tbl_byEduW   <- tbl_DiffRsn_byEduW |> filter(Category == "Economic and Practical Constraints")
work_tbl_byEduW   <- tbl_DiffRsn_byEduW |> filter(Category == "Work–Life Balance and Lifestyle Preferences")
health_tbl_byEduW <- tbl_DiffRsn_byEduW |> filter(Category == "Health and Relationship Barriers")

# Helper function to create subplots
plot_category_EduW <- function(sub_df, cat_title) {
  sub_df |> 
    ggplot(aes(x = YearSvy,
               y = Prop * 100,
               color = Edu3GrpW)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ DiffRsn, scales = "fixed", ncol = 4) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Survey Year", 
      y = "Percentage", 
      color = "Wife's Education",
      title = cat_title
    ) +
    theme_light(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      strip.text = element_text(size =  9, face = "bold"),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(hjust = 1),
      panel.grid.minor = element_blank()
    )
}

# Create one plot per category
p1 <- plot_category_EduW(econ_tbl_byEduW,   "Economic and Practical Constraints")
p2 <- plot_category_EduW(work_tbl_byEduW,   "Work–Life Balance and Lifestyle Preferences")
p3 <- plot_category_EduW(health_tbl_byEduW, "Health and Relationship Barriers")

Fig_DiffRsn_byEduW <- 
  p1 / p2 / p3 + 
  plot_layout(guides = "collect") &
  plot_annotation(title = "") &
  theme(legend.position='bottom')

print(Fig_DiffRsn_byEduW)

ggsave(filename = here(OutDir, "FigureA-1.pdf"),
       plot = Fig_DiffRsn_byEduW,
       width  = 12,
       height = 10)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Figure A-2: Self-reported reasons for not achieving the ideal number of children, by wife's age at marriage ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Set tibble ----
tbl_DiffRsn_byCAgeCMarW_temp <-
  DF_DiffRsn_temp |> 
  mutate(CAgeCMarW5y = case_match(CAgeCMarW5y,
                                  c("<25", "25-29") ~ "<30",
                                  c("30-34")        ~ "30-34",
                                  c("35-39", "40-44", "45-49") ~ "35+")
         ) |> 
  group_by(YearSvy, CAgeCMarW5y, DiffRsn, response) |> 
  reframe(Obs = sum(n)) |> 
  group_by(YearSvy, CAgeCMarW5y, DiffRsn) |> 
  mutate(Prop = Obs / sum(Obs)) |> 
  filter(response == 1) |> 
  mutate(Category = recode(DiffRsn, !!!category_map)) |> 
  select(-c(response, Obs))

tbl_DiffRsn_byCAgeCMarW <-
  tbl_DiffRsn_byCAgeCMarW_temp |> 
  mutate(DiffRsn = factor(DiffRsn, levels = ordering))

# Split into three subsets
econ_tbl_byCAgeCMarW   <- tbl_DiffRsn_byCAgeCMarW |> filter(Category == "Economic and Practical Constraints")
work_tbl_byCAgeCMarW   <- tbl_DiffRsn_byCAgeCMarW |> filter(Category == "Work–Life Balance and Lifestyle Preferences")
health_tbl_byCAgeCMarW <- tbl_DiffRsn_byCAgeCMarW |> filter(Category == "Health and Relationship Barriers")

# Helper function to create subplots
plot_category_CAgeCMarW <- function(sub_df, cat_title) {
  sub_df |> 
    ggplot(aes(x = YearSvy,
               y = Prop * 100,
               color = CAgeCMarW5y)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ DiffRsn, scales = "fixed", ncol = 4) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Survey Year", 
      y = "Percentage", 
      color = "Wife's Age at Marriage",
      title = cat_title
    ) +
    theme_light(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      strip.text = element_text(size =  9, face = "bold"),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(hjust = 1),
      panel.grid.minor = element_blank()
    )
}

# Create one plot per category
p1 <- plot_category_CAgeCMarW(econ_tbl_byCAgeCMarW,   "Economic and Practical Constraints")
p2 <- plot_category_CAgeCMarW(work_tbl_byCAgeCMarW,   "Work–Life Balance and Lifestyle Preferences")
p3 <- plot_category_CAgeCMarW(health_tbl_byCAgeCMarW, "Health and Relationship Barriers")

Fig_DiffRsn_byCAgeCMarW <- 
  p1 / p2 / p3 +
  plot_layout(guides = "collect") &
  plot_annotation(title = "") &
  theme(legend.position='bottom')

print(Fig_DiffRsn_byCAgeCMarW)

ggsave(filename = here(OutDir, "FigureA-2.pdf"),
       plot = Fig_DiffRsn_byCAgeCMarW,
       width  = 12,
       height = 10)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# TableA-1: Descriptive statistics of the total sample ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## Infertility concerns ----
DescTab_InfWorry <- 
  DF_InfWorry |> 
  uncount(n) |> 
  select(InfWorry_bi, Edu3GrpW, Edu3GrpH, CAgeCMarW5y, YearSvy) |>
  tbl_summary(
    type = list(
      all_categorical() ~ "categorical"
    ),
    statistic = list(
      all_categorical() ~ "{p}"
    ),
    digits = list(
      all_categorical() ~ c(1)
    )
  )

DescTab_InfWorry |> 
  as_flex_table() |> 
  flextable::save_as_docx(path = here(OutDir, "TableA-1_InfWorry.docx"))

## MAR use ----
DescTab_InfTreat <- 
  DF_InfTreat |> 
  uncount(n) |> 
  select(InfTreat_bi, Edu3GrpW, Edu3GrpH, CAgeCMarW5y, YearSvy) |>
  tbl_summary(
    type = list(
      all_categorical() ~ "categorical"
    ),
    statistic = list(
      all_categorical() ~ "{p}"
    ),
    digits = list(
      all_categorical() ~ c(1)
    )
  )

DescTab_InfTreat |> 
  as_flex_table() |> 
  flextable::save_as_docx(path = here(OutDir,  "TableA-1_InfTreat.docx"))

## MAR outcomes ----
DescTab_InfTreatRslt <- 
  DF_InfTreatRslt |> 
  uncount(n) |> 
  select(InfTreatRslt_bi, Edu3GrpW, Edu3GrpH, CAgeCMarW5y, YearSvy) |>
  tbl_summary(
    type = list(
      all_categorical() ~ "categorical"
    ),
    statistic = list(
      all_categorical() ~ "{p}"
    ),
    digits = list(
      all_categorical() ~ c(1)
    )
  )

DescTab_InfTreatRslt |> 
  as_flex_table() |> 
  flextable::save_as_docx(path = here(OutDir,  "TableA-1_InfTreatRslt.docx"))
