# Packages ----------------------------------------------------------------

library(tidyverse)
library(survival)
library(survminer)



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



# Kaplan-Meyer Fit --------------------------------------------------------


Surv_fit_broca <-
  survfit(Surv(Time, Status) ~ Area_Type, data = data_surv_broca)


summary(Surv_fit_broca)


# Simple Kaplan-Meyer Plot ------------------------------------------------


plot(Surv_fit_broca)


# Pairwise contrasts ------------------------------------------------------


(Surv_diff_broca <-
   survdiff(Surv(Time, Status) ~ Area_Type, data = data_surv_broca))
Surv_diff_broca$chisq


1 - pchisq(Surv_diff_broca$chisq, df = 2)


pairwise_survdiff(Surv(Time, Status) ~ Area_Type,
                  p.adjust.method = "bonferroni",
                  data = data_surv_broca) ->
  pw

pw


qchisq(pw$p.value, df = 1, lower.tail = F)



# Kaplan-Meyer Survival Plot ----------------------------------------------


ggsurvplot(
  # survfit object with calculated statistics.
  Surv_fit_broca,
  # data used to fit survival curves.
  data = data_surv_broca,

  censor = T,
  censor.shape = 19,
  size = 2,
  ylim = c(0, 1),
  legend.title = "",
  # risk.table.col = "strata",
  risk.table = T,
  # show risk table.
  #  linetype = "strata",
  # show confidence intervals for
  # point estimates of survival curves.
  conf.int = F,

  palette = c("green3", "blue3", "red3"),
  xlim = c(0, 63),
  # present narrower X axis, but not affect
  # survival estimates.
  # customize X axis label.
  xlab = "Time (days)",

  break.time.by = 7,
  # break X axis in time intervals by 500.
  ggtheme = theme_classic(),
  # customize plot and risk table with a theme.
  risk.table.y.text.col = T,
  # colour risk table text annotations.
  risk.table.height = 0.3,
  # the height of the risk table
  risk.table.y.text = F,
  # show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = F,
  # plot the number of censored subjects at time t
  #ncensor.plot.height = 0.25,
  conf.int.style = "ribbon",
  font.x = c("bold", 16),
  font.y = c("bold", 16),
  font.legend = 14,
  font.tickslab = c(12, 'plain', 'black'),
  #surv.median.line = "hv",  # add the median survival pointer.
  # change legend labels.
  legend.labs = c("Organic", "Agroforestry", "Conventional")

) -> p


p$table <- p$table +
  theme(
    plot.title = element_text(
      size = 16,
      color = "black",
      face = "bold"
    ),
    axis.text.x = element_text(size = 12,
                               color = "black"),
    axis.title.x = element_text(size = 16,
                                face = 'bold'),
    axis.ticks.length = unit(.15, 'cm'),
    axis.ticks = element_line(colour = 'black')
  )


p$plot <- p$plot +
  theme(
    legend.text = ggtext::element_markdown(),
    axis.ticks.length = unit(.15, 'cm'),
    axis.ticks = element_line(colour = 'black')
  )

p


ggpubr::ggarrange(
  p$plot,
  p$table,
  labels = paste0(LETTERS[1:2], ')'),
  nrow = 2,
  heights  = c(1, 0.4)
) -> p_with_letters


p_with_letters

#dir.create("Graphics")

ggsave(
  filename = 'Survival_broca.svg',
  plot = p_with_letters,
  units = 'in',
  width = 8.27,
  height = 8.81,
  path = 'Graphics'
)


# Cox model ---------------------------------------------------------------


data_surv_broca |>
  mutate(Area_Type = factor(Area_Type,
                            labels = c(
                              "Conventional",
                              "Agroforestry",
                              "Organic"
                            ))) |>
  arrange(Area_Type) -> data_surv_broca_2


coxph_surv_broca <-
  coxph(Surv(Time, Status) ~ Area_Type + strata(Collection),
        data = data_surv_broca_2)


coxph_surv_broca


summary(coxph_surv_broca)


# Cox Forest Model --------------------------------------------------------


forestmodel::forest_model(coxph_surv_broca)


df_broca <- as.data.frame(data_surv_broca_2)


df_broca |>
  rename(`strata(Collection)` = Collection) -> df_broca_2

ggforest(coxph_surv_broca, data = df_broca_2, fontsize = 0.95)


# Cox Survival Plot -------------------------------------------------------


ggadjustedcurves(
  coxph_surv_broca,
  data = df_broca,
  method = "conditional",
  variable = "Area_Type"
)


data_surv_broca |>
  count(Area_Type)


surv_adjustedcurves(
  coxph_surv_broca,
  data = df_broca,
  method = "conditional",
  variable = "Area_Type"
) |>
  mutate(inv_surv = 1 - surv,
         surv_ind = 108 * surv) |>
  mutate(inv_surv_ind = 1080 - surv_ind) |>
  ggplot(aes(
    x = time,
    y = surv,
    color = variable,
    shape = variable
  )) +
  geom_step(linewidth = 2) +
  scale_y_continuous(name = 'Survival probability', limits = c(0, 1)) +
  scale_x_continuous(name = 'Time (days)',
                     limits = c(0, 40),
                     breaks = seq(0, 40, 5)) +
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



################################ TENEBRIO #################################


# Data --------------------------------------------------------------------


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



# Kaplan-Meyer Fit --------------------------------------------------------


Surv_fit_tenebrio <-
  survfit(Surv(Time, Status) ~ Area_Type, data = data_surv_tenebrio)


summary(Surv_fit_tenebrio)



# Simple Kaplan-Meyer Plot ------------------------------------------------


plot(Surv_fit_tenebrio)


# Pairwise contrasts ------------------------------------------------------


(Surv_diff_tenebrio <-
   survdiff(Surv(Time, Status) ~ Area_Type, data = data_surv_tenebrio))
Surv_diff_tenebrio$chisq


1 - pchisq(Surv_diff_tenebrio$chisq, df = 2)


pairwise_survdiff(Surv(Time, Status) ~ Area_Type,
                  p.adjust.method = "bonferroni",
                  data = data_surv_tenebrio) ->
  pw


qchisq(pw$p.value, df = 1, lower.tail = F)



# Kaplan-Meyer Survival Plot ----------------------------------------------


ggsurvplot(
  # survfit object with calculated statistics.
  Surv_fit_tenebrio,
  # data used to fit survival curves.
  data = data_surv_tenebrio,

  censor = T,
  censor.shape = 19,
  size = 2,
  ylim = c(0, 1),
  legend.title = "",
  # risk.table.col = "strata",
  risk.table = T,
  # show risk table.
  #  linetype = "strata",
  # show confidence intervals for
  # point estimates of survival curves.
  conf.int = F,

  palette = c("green3", "blue3", "red3"),
  xlim = c(0, 88),
  # present narrower X axis, but not affect
  # survival estimates.
  # customize X axis label.
  xlab = "Time (days)",

  break.time.by = 8,
  # break X axis in time intervals by 500.
  ggtheme = theme_classic(),
  # customize plot and risk table with a theme.
  risk.table.y.text.col = T,
  # colour risk table text annotations.
  risk.table.height = 0.3,
  # the height of the risk table
  risk.table.y.text = F,
  # show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = F,
  # plot the number of censored subjects at time t
  #ncensor.plot.height = 0.25,
  conf.int.style = "ribbon",
  font.x = c("bold", 16),
  font.y = c("bold", 16),
  font.legend = 14,
  font.tickslab = c(12, 'plain', 'black'),
  #surv.median.line = "hv",  # add the median survival pointer.
  # change legend labels.
  legend.labs = c("Organic", "Agroforestry", "Conventional")

) -> p


p$table <- p$table +
  theme(
    plot.title = element_text(
      size = 16,
      color = "black",
      face = "bold"
    ),
    axis.text.x = element_text(size = 12,
                               color = "black"),
    axis.title.x = element_text(size = 16,
                                face = 'bold'),
    axis.ticks.length = unit(.15, 'cm'),
    axis.ticks = element_line(colour = 'black')
  )


p$plot <- p$plot +
  theme(
    legend.text = ggtext::element_markdown(),
    axis.ticks.length = unit(.15, 'cm'),
    axis.ticks = element_line(colour = 'black')
  )

p


ggpubr::ggarrange(
  p$plot,
  p$table,
  labels = paste0(LETTERS[1:2], ')'),
  nrow = 2,
  heights  = c(1, 0.4)
) -> p_with_letters


p_with_letters

#dir.create("Graphics")


ggsave(
  filename = 'Survival_tenebrio.svg',
  plot = p_with_letters,
  units = 'in',
  width = 8.27,
  height = 8.81,
  path = 'Graphics'
)


# Cox model ---------------------------------------------------------------


data_surv_tenebrio |>
  mutate(Area_Type = factor(Area_Type,
                            labels = c(
                              "Conventional",
                              "Agroforestry",
                              "Organic"
                            ))) |>
  arrange(Area_Type) -> data_surv_tenebrio_2

coxph_surv_tenebrio <-
  coxph(Surv(Time, Status) ~ Area_Type + strata(Collection),
        data = data_surv_tenebrio_2)


coxph_surv_tenebrio


summary(coxph_surv_tenebrio)



# Cox Forest Model --------------------------------------------------------


forestmodel::forest_model(coxph_surv_tenebrio)


df_tenebrio <- as.data.frame(data_surv_tenebrio_2)


df_tenebrio |>
  rename(`strata(Collection)` = Collection) -> df_tenebrio_2


ggforest(coxph_surv_tenebrio, data = df_tenebrio_2, fontsize = 0.95)



# Cox Survival Plot -------------------------------------------------------


ggadjustedcurves(
  coxph_surv_tenebrio,
  data = df_tenebrio,
  method = "conditional", # marginal
  variable = "Area_Type"
)


data_surv_tenebrio |>
  count(Area_Type)


surv_adjustedcurves(
  coxph_surv_tenebrio,
  data = df_tenebrio,
  method = "conditional", # marginal
  variable = "Area_Type"
) |>
  mutate(inv_surv = 1 - surv,
         surv_ind = 108 * surv) |>
  #mutate(inv_surv_ind = 1080 - surv_ind) |>
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



# Useful links ------------------------------------------------------------

# https://github.com/kassambara/survminer/issues/255
# http://rpkgs.datanovia.com/survminer/reference/ggsurvplot_arguments.html
