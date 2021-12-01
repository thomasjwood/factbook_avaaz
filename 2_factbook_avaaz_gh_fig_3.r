library(plyr)
library(tidyverse)
library(magrittr)
library(emmeans)
library(broom)
library(estimatr)

t1 <- "https://github.com/thomasjwood/factbook_avaaz/raw/master/avaaz_hetero_tab.rds" %>% 
  url %>% 
  gzcon %>% 
  readRDS


t2 <- t1 %>% 
  select(
    ygid, treat2, ans, ans_num, pid3, cond
  ) %>% 
  group_by(treat2) %>% 
  nest

t2$lm <- t2$data %>% 
  map(
    function(i)
      
      lm_robust(
        ans_num ~ pid3 * cond, 
        i)
    )

t2$em <- t2$lm %>% 
  map(
    function(i)
      i %>% 
      emmeans(
        consec ~ cond | pid3,
        options = list(
          level = .95
          )
        )
    )

t2$em2 <- t2$lm %>% 
  map(
    function(i)
      i %>% 
      emmeans(
        pairwise ~ cond * pid3,
        options = list(
          level = .95,
          adjust = "none"
          ), 
      ) %>% 
      extract2("contrasts") %>% 
      pairs(
        options = list(
          level = .95,
          adjust = "none"
        )
      )
  )

t3 <- t2$em %>% 
  map2_dfr(
  t2$treat2,
  ~extract2(..1, 2) %>% 
      tidy %>% 
    mutate(
      treat2 = ..2
      )
    ) %>% 
  bind_rows(
    t2$em2 %>% 
      map2_dfr(
        t2$treat2,
        ~..1 %>% 
          as_tibble %>% 
          filter(
            contrast == "(misinfo Democrat - correction Democrat) - (misinfo Republican - correction Republican)"
            )%>% 
          mutate(
            treat2 = ..2,
            pid3 = "Overall"
          )
      ) %>% 
      rename(std.error = SE,
             statistic = t.ratio,
             adj.p.value = p.value)
    ) %>% 
  mutate(
    contrast = contrast %>% 
     mapvalues(
       c("misinfo - itemsonly",
         "correction - misinfo",
         "(misinfo Democrat - correction Democrat) - (misinfo Republican - correction Republican)"),
       c("Misinformation effect",
         "Correction effect",
         "Difference in correction effects")
     ) %>% 
      factor(
        c("Misinformation effect",
          "Correction effect",
          "Difference in correction effects")
        ),
    lo = estimate %>% 
      subtract(std.error %>% multiply_by(1.96)),
    hi =  estimate %>% 
      add(std.error %>% multiply_by(1.96)),
    pid3 = pid3 %>% 
      factor(
        c("Democrat",
          "Republican",
          "Overall")
      )
  )

t3$lab <- t3$estimate %>% 
  round(2)

t3$lab <- case_when(
  t3$lab %>%  
    str_extract("\\d") %>% 
    equals("0") &
  t3$lab %>% 
    str_detect("-") &
  t3$lab %>% 
    str_count("\\d") %>% 
    equals(2) ~ t3$lab %>%
        str_replace("0.", ".") %>% 
        str_pad(width = 4, side = "right", pad = 0),
  t3$lab %>%  
    str_extract("\\d") %>% 
    equals("0") &
  t3$lab %>% 
    str_detect("-") %>% 
    not &
  t3$lab %>% 
    str_count("\\d") %>% 
    equals(3) ~ t3$lab %>%
        str_replace("0.", ".") %>% 
        str_pad(width = 3, side = "right", pad = 0),
  t3$lab %>%  
    str_extract("\\d") %>% 
    equals("0") &
  t3$lab %>% 
    str_detect("-") & 
  t3$lab %>% 
    str_count("\\d") %>% 
    equals(3) ~ t3$lab %>%
    str_replace("0.", "."),
  TRUE ~ t3$lab %>% 
    str_replace(fixed("0."), ".") %>% 
    str_pad(width = 3, side = "right", pad = "0")
  )

t3$lab[t3$lab == "000"] <- t3$estimate[t3$lab == "000"] %>% 
  round(3) %>% 
  str_replace(fixed('0.'), ".")

t3$lab %<>% 
  str_c(
    t3$adj.p.value %>% 
      gtools::stars.pval()
  ) %>% 
  str_trim

# order effects by magnitude

t3$treat2 %<>% 
  factor(
    t3 %>%
      mutate(
        treat2 = treat2 %>%
          str_sub(, -5)
      ) %>% 
      group_by(treat2) %>% 
      summarize(
        mu = estimate %>% 
          abs %>% 
          mean
      ) %>% 
      arrange(desc(mu)) %>%
      use_series(treat2) %>%
      extract(c(1:4, 6)) %>% 
      c("Overall") %>% 
      rep(each = 2) %>% 
      str_c(
        str_c(
          " (", 1:2, ")"
        )
      )
    )

#  order effects by partisanship
t3$treat2 %<>% 
  factor(
    t3 %>% 
      filter(
        contrast %>% 
          str_detect("Difference") %>% 
          not
        ) %>% 
      mutate(
        treat2 = treat2 %>%
          str_sub(, -5)
      ) %>% 
      group_by(treat2, pid3, contrast) %>% 
      summarize(
        mu = estimate %>% 
          mean
      ) %>% 
      arrange(desc(mu)) %>%
      spread(pid3, mu) %>% 
      mutate(
        dif = Democrat - Republican
      ) %>% 
      group_by(treat2) %>% 
      summarize(mu = dif %>% mean) %>% 
      arrange(mu) %>% 
      use_series(treat2) %>%
      rep(each = 2) %>% 
      str_c(
        str_c(
          " (", 1:2, ")"
        )
      )
  )


t3 %>% 
  filter(
    pid3 %>% 
      equals("Independent") %>% 
      not
  ) %>% 
  ggplot() +
  geom_vline(
    xintercept = 0, linetype = "dashed",
    size = .35
  ) +
  geom_linerange(
    aes(
      xmin = lo, xmax = hi, y = treat2, color = pid3
    ),
    position = position_dodge(.4)
  ) +
  geom_path(
    aes(
      x = estimate, y = treat2, group = pid3
    ),
    data = t3 %>% 
      filter(
        pid3 %>% 
          equals("Republican") &
        contrast %>% 
          str_detect("Difference") %>% 
          not
        ) %>%
      arrange(
        contrast, pid3, treat2
        ),
    position = position_dodge(.4),
    size = .3
  ) +
  geom_point(
    aes(
      x = estimate, y =  treat2, group = pid3
    ),
    color = "black",
    shape = 21,
    size = 8.5,
    position = position_dodge(.4)
    ) +
  geom_point(
    aes(
      x = estimate, y =  treat2, color = pid3, fill = pid3
    ),
    size = 8,
    position = position_dodge(.4)
  ) +
  geom_point(
    aes(
      x = estimate, y =  treat2, group = pid3
    ),
    # shape = 21,
    color = "white",
    fill = "white",
    size = 7.5,
    position = position_dodge(.4),
    data = t3 %>% 
      filter(
        contrast %>% 
          str_detect("Difference")
      )
  ) +
  geom_text(
    aes(
      estimate, treat2, group = pid3, label = lab 
    ), 
    position = position_dodge(.4),
    size = 2,
    ) +
  geom_text(
    aes(
      x, y, label = lab, color = col
    ),
    data = tribble(
      ~x, ~y, ~lab, ~col,
      .95, 12.75, "Republicans", "Republican",
      1.51, 12.5, "Democrats", "Democrat"
    ) %>% 
      mutate(
        contrast = "Correction effect" %>% 
          factor(
            t3$contrast %>% 
              levels
          ),
        col = col %>% 
          factor(
            t3$pid3 %>% 
              levels
          )
      ),
    size = 2.5,
    fontface = "italic"
  ) +
  geom_segment(
    aes(
      x, y, xend = xend, yend = yend
    ),
    data = tribble(
      ~x,  ~y, ~xend, ~yend,
      -.05, 12.5, -.4, 12.5,
       .05, 12.5,  .4, 12.5,
    ) %>% 
      mutate(
        contrast = t3$contrast %>% 
          levels %>% 
          extract(3) %>% 
          # rep(each = 2) %>% 
          factor(
            t3$contrast %>% 
              levels
            )
      ),
    arrow = arrow(
      length = unit(0.03, "npc"), 
      type="closed" 
    )
  ) +
  geom_text(
    aes(
      x, y, label = lab, hjust = hjust
    ),
    data = tribble(
      ~x,  ~y, ~lab, ~hjust,
       .05,  13, "Republicans\nmore\ncorrectable",  0, 
      -.05,  13, "Democrats\nmore\ncorrectable", 1
      ) %>% 
      mutate(
        contrast = t3$contrast %>% 
          levels %>% 
          extract(3) %>% 
          # rep(each = 2) %>% 
          factor(
            t3$contrast %>% 
              levels
            )
        ),
    lineheight = .6,
    size = 2.5,
    fontface = "italic"
  ) +
  scale_x_continuous(
    breaks = seq(
      -1.5, 1.5, .5
    ),
    labels = c(
      "-1.5", "-1", "-.5", "0", ".5", "1", "1.5"
    )
  ) +
  facet_grid(
    . ~ contrast, 
    space = "free_x", 
    scales = "free_x"
    ) +
  scale_color_manual(
    values = c("grey55", "grey80", "grey5")
  ) +
  scale_fill_manual(
    values = c( "grey50", "grey85", "grey5")
  ) +
  scale_y_discrete(
    expand = expansion(add = c(.75, 1.5)) 
      ) +
  labs(
    x = "",
    y = ""
    ) +
  theme(
    legend.position = "none"
  )