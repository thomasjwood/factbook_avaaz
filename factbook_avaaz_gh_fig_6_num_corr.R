library(plyr)
library(tidyverse)
library(magrittr)
library(broom)
library(emmeans)
library(ggbeeswarm)


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
      ),
    contrast = mods %>%
      map(
        function(i)
          i %>% 
          emmeans(
            ~cond | fact_corr
          ) %>% 
          pairs(rev = T) %>% 
          tidy %>% 
          group_by(fact_corr) %>% 
          slice(-2)
      )
    )

ndf <- mt2 %>% 
  filter(
    study != "Overall"
  ) %>% 
  use_series(contrast) %>% 
  bind_rows %>% 
  mutate(
    vfac = case_when(
      contrast %>% 
        str_detect(
          " - itemsonly"
        ) ~ "Misinformation effect",
      contrast %>% 
        str_detect(
          " - misinfo"
        ) ~ "Correction effect"
    ) %>% 
      factor(
        c("Misinformation effect",
          "Correction effect")
      )
    )


ndf2 <- mt2 %>% 
  filter(
    study == "Overall"
  ) %>% 
  use_series(contrast) %>% 
  bind_rows %>% 
  mutate(
    vfac = case_when(
      contrast %>% 
        str_detect(
          " - itemsonly"
          ) ~ "Misinformation effect",
      contrast %>% 
        str_detect(
          " - misinfo"
        ) ~ "Correction effect"
      ) %>% 
      factor(
        c("Misinformation effect",
          "Correction effect")
      ),
    lo = estimate %>% 
      subtract(
        std.error %>% 
          multiply_by(1.96)
      ),
    hi = estimate %>% 
      add(
        std.error %>% 
          multiply_by(1.96)
      ),
    est_lab = estimate %>% 
      round(2) %>% 
      as.character %>% 
      str_replace(
        fixed("0."),
        "."
      ),
    est_lab = est_lab %>% 
      str_count("\\d") %>% 
      equals(1) %>% 
      ifelse(
        est_lab %>% 
          str_pad(width = 4, side = "right", pad = "0"),
        est_lab
      ) %>% 
      str_c(
        c("***",
          "**",
          "*",
          "") %>% 
          extract(
            adj.p.value %>% 
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          )
      )
  )

ggplot() +
  geom_hline(
    aes(yintercept = yint),
    data = data.frame(
      yint = 0,
      vfac = "Misinformation effect" %>% 
        factor(
          ndf2$vfac %>% 
            levels
        )
    ),
    linetype = "dashed",
    size = .5
  ) +
  geom_beeswarm(
    aes(
      fact_corr, estimate
    ),
    cex= 2, 
    groupOnX = T, 
    shape =  1,
    size = 2,
    data = ndf
  ) +
  geom_segment(
    aes(
      y = lo,
      yend = hi,
      x = fact_corr,
      xend = fact_corr
    ),
    data = ndf2
  ) +
  geom_point(
    aes(
      fact_corr, estimate
    ),
    shape = 21,
    size = 9,
    fill = "white",
    data = ndf2
  ) +
  geom_text(
    aes(
      fact_corr, estimate, label = est_lab
    ),
    data = ndf2,
    size = 2.15,
  ) +
  facet_wrap(
    ~vfac, 
    ncol = 1, 
    scales = "free_y"
    ) +
  labs(
    x = "Total corrections seen in second News Feed",
    y = ""
  )