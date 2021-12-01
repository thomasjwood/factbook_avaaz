library(plyr)
library(tidyverse)
library(magrittr)
library(emmeans)
library(broom)
library(ggstance)

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
      
      # i <- t2$data[[1]]
      
      lm(
        ans_num ~ cond,
        data = i
      ) 
  )

t2$emm <- t2$mods %>% 
  map(
    ~emmeans(., consec ~ cond)
  )

t4 <-  list(
  t2$emm,
  t2$mods,
  t2$treat,
  t2$study
  ) %>% 
  pmap_dfr(
    function(i, j, k, l)
      
      i %>% 
      eff_size(
        edf = j %>% df.residual, 
        sigma = j %>% sigma
      ) %>% 
      as.data.frame %>% 
      mutate(
        treat = k,
        study = l
      )
  ) %>% 
  tibble %>% 
  select(
    treat, study, everything()
  ) %>% 
  mutate(
    contrast = contrast %>% 
      str_sub(2, -2) %>% 
      mapvalues(
        c("misinfo - itemsonly",
          "correction - misinfo"),
        c("Misinformation effect",
          "Correction effects")
      )
  ) %>% 
  mutate(
    eff_grp = effect.size %>% 
      abs %>% 
      cut(
        c(-Inf, .1, .25, .4, Inf),
        c("Trivial", "Small", "Medium", "Large")
      )
  )

t4 %>% 
  select(
    -ends_with(".CL")
  ) %>% 
  arrange(
    contrast, desc(abs(effect.size))
  ) %>% 
  filter(
    treat != "Overall"
  ) %>%
  modify_at(
    4:5, ~round(., 3)
  )