# Useful links ------------------------------------------------------------

# https://www.drizopoulos.com/courses/emc/basic_surivival_analysis_in_r
# https://stackoverflow.com/questions/32401706/r-proportional-hazard-assumption-in-coxme/67709136#67709136

# Interpretação dos coeficientes do modelo de cox
# https://medium.com/swlh/interpreting-cox-proportional-hazards-model-using-colon-dataset-in-r-fda1f9901292
# https://www.sthda.com/english/wiki/cox-proportional-hazards-model
# https://stats.stackexchange.com/questions/609091/how-to-interpret-cox-proportional-hazards-model-output-when-running-survival-ana


# Artigo
# https://socialsciences.mcmaster.ca/jfox/Books/Companion/appendices/Appendix-Cox-Regression.pdf


# Diagnostico do modelo
# https://shariq-mohammed.github.io/files/cbsa2019/1-intro-to-survival.html


# outra forma de ajuste de modelo cox
# https://argoshare.is.ed.ac.uk/healthyr_book/cox-proportional-hazards-regression.html


# comparação dos efeitos no modelo cox
# https://stats.stackexchange.com/questions/607076/cox-ph-model-hr-is-it-possible-to-measure-against-the-general-average
# https://argoshare.is.ed.ac.uk/healthyr_book/cox-proportional-hazards-regression.html
# https://stackoverflow.com/questions/67915896/pairwise-differences-between-survreg-survival-curves-survival-package-using-em


# Teste para o efeito aleatório no modelo de cox
# https://rpubs.com/kaz_yos/coxme1


# plotar curvas
# https://stackoverflow.com/questions/77654561/plotting-adjusted-survival-curve-for-a-mixed-effects-cox-regression-and-or-time


# Packages ----------------------------------------------------------------


library(tidyverse)
library(survival)
library(survminer)
library(emmeans)
library(coxme)
library(multcomp)
library(riskRegression)
library(adjustedCurves)
library(pammtools)



# Data --------------------------------------------------------------------


data_surv_broca <-
  readxl::read_excel('Data/Data_All_collections.xlsx',
                     sheet = "Broca")


data_surv_broca <- data_surv_broca |>
  mutate_at(vars(Collection:Area_Type), as_factor) |>
  mutate_at(vars(Time),
            ~ case_match(.,
                         NA ~ as.integer(as_datetime("2023-08-02") - Initial_Date),
                         .default = as.integer(.))) |>
  mutate_at(vars(Status), as.integer)


data_surv_broca |>
  add_column(Insect = factor("Broca")) ->
  data_surv_broca



data_surv_tenebrio <-
  readxl::read_excel('Data/Data_All_collections.xlsx',
                     sheet = "Tenebrio")



data_surv_tenebrio <- data_surv_tenebrio |>
  mutate_at(vars(Collection:Area_Type), as_factor) |>
  mutate_at(vars(Time),
            ~ case_match(.,
                         NA ~ as.integer(as_datetime("2023-08-02") - Initial_Date),
                         .default = as.integer(.))) |>
  mutate_at(vars(Status), as.integer)



data_surv_tenebrio |>
  add_column(Insect = factor("Tenebrio")) ->
  data_surv_tenebrio


data_surv_broca |>
  bind_rows(data_surv_tenebrio) |>
  mutate(Collection_Int = as.integer(as.character(Collection))) |>
  mutate(Area_Type = factor(Area_Type,
                            labels = c(
                              "Conventional",
                              "Agroforestry",
                              "Organic"
                            ))) |>
  arrange(Area_Type) |>
  as.data.frame() ->
  df



# Cox model (with random effect) ------------------------------------------


coxph_surv_rf <-
  coxph(Surv(Time, Status) ~ Area_Type + Insect + frailty(Collection_Int),
        data = df)


coxph_surv_rf


summary(coxph_surv_rf)


coxph_surv_rf2 <-
  coxme(Surv(Time, Status) ~ Area_Type + Insect + (1|Collection_Int),
        data = df)


test.ph <- cox.zph(coxph_surv_rf2)


test.ph


ggcoxzph(test.ph)



# Model diagnostic --------------------------------------------------------


ggcoxdiagnostics(
  coxph_surv_rf,
  type = "dfbeta",
  linear.predictions = FALSE,
  ggtheme = theme_bw()
)


ggcoxdiagnostics(
  coxph_surv_rf,
  type = "deviance",
  linear.predictions = FALSE,
  ggtheme = theme_bw()
)


ggcoxdiagnostics(
  coxph_surv_rf,
  type = "martingale",
  linear.predictions = FALSE,
  ggtheme = theme_bw()
)


ggcoxdiagnostics(
  coxph_surv_rf,
  type = "score",
  linear.predictions = FALSE,
  ggtheme = theme_bw()
)


ggcoxdiagnostics(
  coxph_surv_rf,
  type = "schoenfeld",
  linear.predictions = FALSE,
  ggtheme = theme_bw()
)



ggcoxdiagnostics(
  coxph_surv_rf,
  type = "dfbetas",
  linear.predictions = FALSE,
  ggtheme = theme_bw()
)



# Cox Forest --------------------------------------------------------------


forestmodel::forest_model(coxph_surv_rf)



# Comparação par a par
emmeans(coxph_surv_rf2, pairwise ~ Area_Type)


emmeans(coxph_surv_rf2, pairwise ~ Area_Type, type = "response")


# efeitos
emmeans(coxph_surv_rf2, eff ~ Area_Type)


emmeans(coxph_surv_rf2, eff ~ Area_Type, type = "response")

# dunnet
emmeans(coxph_surv_rf2, trt.vs.ctrl ~ Area_Type)


emmeans(coxph_surv_rf2, trt.vs.ctrl ~ Area_Type, type = "response")



(nd <- ref_grid(coxph_surv_rf2, at = list(x = c(.25, .5, .75)))@grid)



# Curves for Area ---------------------------------------------------------


df |>
  count(Area_Type)


surv_adjustedcurves(coxph_surv_rf,
                    data = df,
                    method = "marginal",
                    variable = "Area_Type") |>
  mutate(inv_surv = 1 - surv,
         surv_ind = 216 * surv) |>
  mutate(inv_surv_ind = 216 - surv_ind) |>
  ggplot(aes(
    x = time,
    y = surv,
    color = variable,
    shape = variable
  )) +
  geom_step(linewidth = 2) +
  scale_y_continuous(name = 'Survival probability', limits = c(0, 1)) +
  scale_x_continuous(name = 'Time (days)',
                     limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  theme_classic() +
  scale_color_manual(values = c("green3", "blue3", "red3")) +
  theme(
    plot.title = element_text(
      size = 16,
      color = "black",
      face = "bold"
    ),
    axis.text = element_text(size = 12,
                             color = "black"),
    axis.title = element_text(size = 16,
                              face = 'bold'),
    axis.ticks = element_line(colour = 'black'),
    axis.ticks.length = unit(0.15, 'cm'),
    legend.position = 'top',
    legend.text = ggtext::element_markdown(size = 12)
  ) +
  labs(linetype = NULL,
       color = NULL)




predict_fun <- function(...) {
  1 - predictRisk(...) # riskRegression package
}



adjsurv <- adjustedsurv(
  data = df,
  variable = "Area_Type",
  ev_time = "Time",
  event = "Status",
  method = "direct",
  outcome_model = coxph_surv_rf,
  predict_fun = predict_fun,
  conf_int = TRUE,
  bootstrap = TRUE,
  n_boot = 100
) # don't worry with warnings


plot(adjsurv, conf_int = TRUE, use_boot = TRUE)


adjsurv$adjsurv |> # for conf. int. use adjsurv$boot_adjsurv
  ggplot(aes(
    x = time,
    y = surv,
    color = group,
    shape = group
  )) +
  geom_step(linewidth = 2) +
  scale_y_continuous(name = 'Survival probability', limits = c(0, 1)) +
  scale_x_continuous(name = 'Time (days)',
                     limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  theme_classic() +
  scale_color_manual(values = c("green3", "blue3", "red3")) +
  theme(
    plot.title = element_text(
      size = 16,
      color = "black",
      face = "bold"
    ),
    axis.text = element_text(size = 12,
                             color = "black"),
    axis.title = element_text(size = 16,
                              face = 'bold'),
    axis.ticks = element_line(colour = 'black'),
    axis.ticks.length = unit(0.15, 'cm'),
    legend.position = 'top',
    legend.text = ggtext::element_markdown(size = 12)
  ) +
  labs(linetype = NULL,
       color = NULL)



# Curves for Insect -------------------------------------------------------



ggadjustedcurves(coxph_surv_rf,
                 data = df,
                 method = "marginal",
                 # "average" # marginal
                 variable = "Insect")



df |>
  count(Insect)



surv_adjustedcurves(coxph_surv_rf,
                    data = df,
                    method = "marginal",
                    variable = "Insect") |>
  mutate(inv_surv = 1 - surv,
         surv_ind = 314 * surv) |>
  mutate(inv_surv_ind = 314 - surv_ind) |>
  ggplot(aes(
    x = time,
    y = surv,
    color = variable,
    shape = variable
  )) +
  geom_step(linewidth = 2) +
  scale_y_continuous(name = 'Survival probability', limits = c(0, 1)) +
  scale_x_continuous(name = 'Time (days)',
                     limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  theme_classic() +
  scale_color_manual(values = c("blue3", "red3")) +
  theme(
    plot.title = element_text(
      size = 16,
      color = "black",
      face = "bold"
    ),
    axis.text = element_text(size = 12,
                             color = "black"),
    axis.title = element_text(size = 16,
                              face = 'bold'),
    axis.ticks = element_line(colour = 'black'),
    axis.ticks.length = unit(0.15, 'cm'),
    legend.position = 'top',
    legend.text = ggtext::element_markdown(size = 12)
  ) +
  labs(linetype = NULL,
       color = NULL)


predict_fun <- function(...) {
  1 - predictRisk(...) # riskRegression package
}


adjsurv2 <- adjustedsurv(
  data = df,
  variable = "Insect",
  ev_time = "Time",
  event = "Status",
  method = "direct",
  outcome_model = coxph_surv_rf,
  predict_fun = predict_fun,
  conf_int = TRUE,
  bootstrap = TRUE,
  n_boot = 100
) # don't worry with warnings


plot(adjsurv2, conf_int = TRUE, use_boot = TRUE)


adjsurv2$adjsurv |> # for conf. int. use adjsurv$boot_adjsurv
  ggplot(aes(
    x = time,
    y = surv,
    color = group,
    shape = group
  )) +
  geom_step(linewidth = 2) +
  scale_y_continuous(name = 'Survival probability', limits = c(0, 1)) +
  scale_x_continuous(name = 'Time (days)',
                     limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  theme_classic() +
  scale_color_manual(values = c("blue3", "red3")) +
  theme(
    plot.title = element_text(
      size = 16,
      color = "black",
      face = "bold"
    ),
    axis.text = element_text(size = 12,
                             color = "black"),
    axis.title = element_text(size = 16,
                              face = 'bold'),
    axis.ticks = element_line(colour = 'black'),
    axis.ticks.length = unit(0.15, 'cm'),
    legend.position = 'top',
    legend.text = ggtext::element_markdown(size = 12)
  ) +
  labs(linetype = NULL,
       color = NULL)



# A little more about the random model ------------------------------------


cox <-
  coxph(Surv(Time, Status) ~ Area_Type + Insect,
        data = df)


mixed_cox <-
  coxme(Surv(Time, Status) ~ Area_Type + Insect + (1 |
                                                     Collection_Int),
        data = df)


mixed_cox


car::Anova(mixed_cox)


summary(mixed_cox)


coxme::ranef(mixed_cox)


## Function to test random effects
TestRanef <- function(coxphModel, coxmeModel) {
  if (!class(coxphModel) == "coxph" | !class(coxmeModel) == "coxme") {
    stop("Wrong models")
  }

  ## Degrees of freedom
  coxphDf <- sum(!is.na(coef(coxphModel)))
  coxmeDf <- coxmeModel$df
  names(coxmeDf) <- c("Integrated", "Penalized")
  ## DF differnces
  dfDiff <- coxmeDf - coxphDf


  ## Log likelihodds
  coxphLogLik <- coxphModel$loglik[2]
  coxmeLogLik <- coxmeModel$loglik + c(0, 0, coxmeModel$penalty)
  coxmeLogLik <- coxmeLogLik[2:3]
  ## -2 logLik difference
  logLikNeg2Diff <-
    c(-2 * (coxphLogLik - coxmeLogLik["Integrated"]),
      -2 * (coxphLogLik - coxmeLogLik["Penalized"]))

  ## p-values
  pVals <-
    pchisq(q = logLikNeg2Diff,
           df = dfDiff,
           lower.tail = FALSE)

  ## Combine
  outDf <- data.frame(dfDiff, logLikNeg2Diff, pVals)
  colnames(outDf) <- c("df diff", "-2logLik diff", "p-values")
  outDf
}


TestRanef(cox, mixed_cox)


em_cox_mixed <- emmeans(mixed_cox, pairwise ~ Area_Type)


em_cox_mixed


cld(em_cox_mixed,
    adj = "holm",
    Letters = letters,
    reverse = TRUE)

