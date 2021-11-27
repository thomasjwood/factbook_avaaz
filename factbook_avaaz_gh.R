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
# first, a fitted value table

t3 <- pmap_dfr(
  .l = t2 %>% 
    select(-data),
  function(treat, study, mods)
    mods %>% 
    emmeans(~cond) %>% 
    tidy %>%
    mutate(
      treat = treat,
      study = study
    )
) %>% 
  mutate(
    type = "fitted_val"
  ) %>% 
  select(
    -starts_with("conf.")
  ) %>% 
  bind_rows(
    
    pmap_dfr(
      .l = t2 %>% 
        select(-data) ,
      function(treat, study, mods)
        mods %>% 
        emmeans(
          ~cond, 
          contr = "revpairwise"
        ) %>% 
        extract2("contrasts") %>% 
        tidy %>% 
        mutate(
          treat = treat,
          study = study
        )
      ) %>% 
      filter(
        contrast %>% 
          is_in(
            c("misinfo - itemsonly",
              "correction - misinfo")
          )
        # (
        #   level1 ==  "misinfo" &
        #     level2 == "itemsonly"
        # ) |
        #   (
        #     level1 == "correction" &
        #       level2 == "misinfo"
        #   )
      ) %>% 
      mutate(
        cond = case_when(
          contrast %>% 
            equals("misinfo - itemsonly") ~ "Misinformation effect",
          contrast %>% 
            equals("correction - misinfo") ~ "Correction effect"
          # level1 == "misinfo" &
          #   level2 == "itemsonly"
          # level1 == "correction" &
          #   level2 == "misinfo" ~ "Correction effect"
        ),
        treat = treat,
        type = "contrasts"
      ) %>% 
      select(
        type, study, treat, cond, estimate, std.error, adj.p.value
      ) %>% 
      rename(
        p.value = adj.p.value
        )
  ) %>% 
  mutate(
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
    lab = estimate %>% 
      round(1) %>% 
      as.character
  )

t3$treat %<>% 
  factor(
    t3$treat %>%  
      fct_reorder(
        t3$estimate
      ) %>% 
      levels %>% 
      extract(-3) %>% 
      c("Overall")
  )

t3$type <- case_when(
  t3$type == "fitted_val" ~ "Fitted Values",
  t3$type != "fitted_val"  &
    t3$cond == "Misinformation effect" ~ "Misinformation Effect",
  t3$type != "fitted_val"  &
    t3$cond == "Correction effect" ~ "Correction Effect"
) %>% 
  factor(
    c("Fitted Values",
      "Misinformation Effect",
      "Correction Effect"
    )
  )


t3$study %<>% 
  mapvalues(
    1:2,
    c("Study 1",
      "Study 2")
  ) %>% 
  factor(
    c("Study 1",
      "Study 2")
  )

t3$treat <- t3$treat %>% 
  mapvalues(
    c("trump", "fiveg", "omar", "greta", "measles", "Overall"),
    c("Trump denigrates Republicans",
      "5G causes cancer",
      "Ilhan Omar attends terror camp",
      "Greta Thunberg highly paid",
      "Measles spread by immigrants",
      "Overall")
  ) %>% 
  factor(
    c("Trump denigrates Republicans",
      "5G causes cancer",
      "Ilhan Omar attends terror camp", 
      "Greta Thunberg highly paid",
      "Measles spread by immigrants", 
      "Overall")
  )


t3$lab <- t3$type %>%
  equals("Fitted Values") %>% 
  ifelse(
    t3$estimate %>% 
      round(2) %>% 
      as.character %>% 
      str_replace(fixed("0."), "."),
    t3$estimate %>% 
      round(2) %>% 
      as.character() %>% 
      str_replace(fixed("0."), ".") %>% 
      str_c(
        gtools::stars.pval(t3$p.value) %>% 
          str_replace(fixed("."), "")
      )
  )

t3$lab <- t3$lab %>% 
  str_replace_all(
    t3$lab %>%
      str_extract("\\.\\d{1,2}"),
    t3$lab %>%
      str_extract("\\.\\d{1,2}") %>%
      str_pad(width = 3, side = "right", pad = "0")
  )


t3$treat %<>% 
  factor(
    t3 %>% 
      filter(
        type == "Fitted Values" &
          treat != "Overall"
      ) %>% 
      group_by(
        treat
      ) %>% 
      summarize(
        mu = estimate %>% mean
      ) %>% 
      arrange(mu) %>%
      use_series(treat) %>% 
      as.character %>% 
      c("Overall")
  )


t3$type %<>% 
  mapvalues(
    t3$type %>% 
      levels,
    c("Group means\n(Agreement with correct position, 5pt scale)",
      "Misinformation Effect",
      "Correction Effect")
  ) %>% 
  factor(c("Group means\n(Agreement with correct position, 5pt scale)",
           "Misinformation Effect",
           "Correction Effect")
  )

ggplot() +
  geom_linerangeh(
    aes(
      xmin = lo, xmax = hi, y = treat, color = cond
    ),
    position = position_dodgev(.5),
    data = t3 %>% 
      filter(
        type %>% 
          str_detect("Group means")
      )
  ) +
  geom_path(
    aes(
      estimate, treat, color = cond, group = cond
    ),
    data = t3 %>% 
      filter(
        type  %>% 
          str_detect("Group means")
      ) %>% 
      arrange(
        study, cond, desc(treat)
      )
  ) +
  geom_text(
    aes(x, y, label = label, group = cond),
    position = position_dodgev(.5),
    data = tribble(
      ~x, ~y, ~label,
      3.8,  6,"Corrected",
      2.85, 6, "Misinformation",
      3.375, 6, "Items Only"
    ) %>% 
      mutate(
        type = t3$type %>% 
          levels %>% 
          extract2(1) %>% 
          factor(
            levels = t3$type %>% 
              levels
          ),
        study = "Study 1",
        cond = t3$cond[1:3] %>% 
          rev
      ),
    size  = 2,
    fontface = "italic"
  ) +
  geom_point(
    aes(
      x = estimate, y = treat, color = cond
    ),
    shape = 21,
    position = position_dodgev(.5),
    data = t3 %>% 
      filter(
        type %>% 
          str_detect("Group means")
      ),
    size = 6.5,
    fill = "white"
  ) +
  geom_point(
    aes(
      x = estimate, y = treat, group = cond
    ),
    shape = 21,
    position = position_dodgev(.5),
    data = t3 %>% 
      filter(
        type %>% 
          str_detect("Group means")
      ),
    size = 5.5,
    color = "white",
    fill = "white"
  ) +
  geom_vline(
    aes(xintercept = xint),
    data = data.frame(
      xint = 0,
      type = t3$type %>% 
        levels %>% 
        extract(2:3) %>% 
        factor(
          t3$type %>% 
            levels
          )
      ),
    size = .3,
    linetype = "dashed"
  ) +
  geom_text(
    aes(
      x = estimate, y = treat, group = cond, label = lab
    ),
    size = 1.5,
    position = position_dodgev(.5),
    data = t3 %>% 
      filter(
        type %>% 
          str_detect(
            "Group means"
          )
      ),
    family = "texgyre"
    ) +
  geom_linerangeh(
    aes(xmin = lo, xmax = hi, y = treat),
    data = t3 %>% 
      filter(
        type %>% 
          str_detect(
            "Group means"
          ) %>% 
          not
      )
  ) + 
  geom_point(
    aes(x = estimate, y = treat),
    data = t3 %>% 
      filter(
        type  %>% 
          str_detect("Group means") %>% 
          not
      ),
    shape = 21,
    size = 6.5,
    fill = "white"
  ) +
  geom_text(
    aes(x = estimate, y = treat, label = lab),
    data = t3 %>% 
      filter(
        type %>% 
          str_detect("Group means") %>% 
          not
      ),
    size = 1.5
  ) +
  scale_color_manual(
    values = c("grey1", "grey40", "grey79")
  ) +
  # scale_color_grey(start = .01, end = .85) +
  facet_grid(study ~ type, scales = "free_x", space = "free_x") +
  labs(
    x = "",
    y = ""
  )
