library(plyr)
library(tidyverse)
library(magrittr)
library(stargazer)

mt2 <- "https://github.com/thomasjwood/factbook_avaaz/raw/master/avaaz_num_corr.rds" %>% 
  url %>% 
  gzcon %>% 
  readRDS %>% 
  mutate(
    fact_corr = tote_corr %>% 
      mapvalues(
        0:5,
        c(0, "1", "2", "3", "4-5", "4-5")
      ) %>% 
      factor(
        c("1", "2", "3", "4-5")
      )
    ) %>% 
  group_by(study, treat) %>% 
  nest %>% 
  mutate(
    mods = data %>%
      map(
        function(i)
          lm(
            ans_num ~ cond * fact_corr,
            data = i
          )
      )
  ) 

mt2$mods %>%
  stargazer(
    no.space = T,
    dep.var.labels = "",
    dep.var.labels.include = F,
    type = "text",
    initial.zero = F,
    object.names = F,
    digits = 2, 
    model.numbers = F,
    keep.stat = c("n", "rsq"),
    covariate.labels = c(
      "Cond: Misinfo",
      "Cond: Corr",

      "Corrections(2)",
      "Corrections(3)",
      "Corrections(4-5)",

      "Misinfo * Corr.(2)",
      "Correction * Corr.(2)",

      "Misinfo * Corr.(3)",
      "Correction * Corr.(3)",

      "Misinfo * Corr.(4-5)",
      "Correction * Corr.(4-5)",

      "Constant"
    ),
    star.cutoffs = c(
      .05, .01, .001
    ),
    column.labels = mt2$treat
  )

