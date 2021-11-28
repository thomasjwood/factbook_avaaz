library(plyr)
library(tidyverse)
library(stargazer)
library(magrittr)

t1 <- "https://github.com/thomasjwood/factbook_avaaz/raw/master/factbook_avaaz_rc.RDS" %>% 
  url %>% 
  gzcon %>% 
  readRDS

# overall effects table

t2 <- t1 %>%
  bind_rows(
    t1 %>% 
      mutate(
        treat = "Overall"
      )
  ) %>% 
  group_by(
    study, treat
  ) %>% 
  nest

t2$mods <- t2$data %>% 
  map(
    function(i)
      
      lm(
        ans_num ~ cond,
        data = i
      ) 
  )

t2 %>%
  ungroup %>% 
  arrange(
    study
  ) %>% 
  use_series(mods) %>%
  stargazer(
    dep.var.labels = "",
    dep.var.labels.include = F,
    type = "text",
    initial.zero = F,
    object.names = F,
    digits = 2, 
    model.numbers = F,
    keep.stat = c("n", "rsq"),
    covariate.labels = c(
      "Condition: Misinformation",
      "Condition: Correction",
      "Constant"
    ),
    star.cutoffs = c(
      .05, .01, .001
    ),
    column.labels = t2 %>%
      ungroup %>% 
      arrange(
        study
      ) %>% 
      use_series(treat) %>% 
      mapvalues(
        t2$treat %>% 
          unique,
        c("Five G",
          "Greta",
          "Omar",
          "Measles",
          "Trump",
          "Overall")
      )
  )