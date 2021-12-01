library(plyr)
library(tidyverse)
library(magrittr)
library(emmeans)
library(broom)
library(ggstance)
library(ggbeeswarm)

t1 <- "https://github.com/thomasjwood/factbook_avaaz/raw/master/avaaz_hetero_tab_2.rds" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  group_by(
    study, treat
  ) %>% 
  nest %>% 
  mutate(
    data = data %>% 
      map(
        function(i)
          
          i %>% 
          mutate(
            generation = birthyr %>% 
              cut(
                c(-Inf, 1945, 1964, 1980, 1997, Inf),
                c("gen_sil",
                  "gen_boom",
                  "gen_x",
                  "gen_mil",
                  "gen_z")
              ),
            educ = educ %>% 
              mapvalues(
                c("HSD or less", "Some college", "BA or more"),
                c("hsd", "somecollege", "ba")
              ),
            faminc_new = faminc_new %>% 
              mapvalues(
                c("$0-30k", "$30-70k", "$70-100k", "$100-150k", ">$150k"),
                c("inc_0_30",
                  "inc_30_70",
                  "inc_70_100",
                  "inc_100_150",
                  "inc_150_inf"
                )
              ) %>% 
              factor(
                c("inc_0_30",
                  "inc_30_70",
                  "inc_70_100",
                  "inc_100_150",
                  "inc_150_inf")
              ),
            newsint = newsint %>% 
              str_to_lower %>% 
              str_replace_all(" ", "_") %>% 
              factor(
                newsint %>% 
                  levels %>% 
                  str_to_lower %>% 
                  str_replace_all(" ", "_")
              ),
            pew_churatd = pew_churatd %>% 
              str_to_lower %>% 
              str_replace_all(
                "\\/|\\s", "_"
              ) %>% 
              factor(
                pew_churatd %>% 
                  levels %>% 
                  str_to_lower %>% 
                  str_replace_all(
                    "\\/|\\s", "_"
                  )
              )
          )
      )
  )


t2 <- str_c(
  "ans_num ~ ",
  c("cond * ideo5",
    "cond * educ",
    "cond * generation",
    "cond * presvote16post",
    "cond * faminc_new",
    "cond * newsint",
    "cond * pew_churatd",
    "cond * pid3"
  )
) %>% 
  map_dfr(
    function(i)
      
      
      t1 %>% 
      ungroup %>% 
      mutate(
        form = i
      )
  ) %>% 
  mutate(
    mod = map2(
      form, data,
      ~lm(.x, .y)
    )
  )


t2$pair <- pmap(
  list(
    t2$mod,
    t2$form %>% 
      str_sub(9),
    t2$data
  ),
  function(i, j, k)
    
    i %>%
    emmeans(
      as.formula(
        j %>% 
          str_replace("\\*", "\\|")
      ), 
      data = k
    ) %>% 
    pairs %>% 
    tidy
  )

t2$cont <- pmap(
  list(
    t2$mod,
    t2$form %>% 
      str_sub(9),
    t2$data
  ),
  function(i, j, k)
    
    i %>%
    emmeans(
      as.formula(
        j  %>% 
          str_replace("\\*", "\\|") %>% 
          str_c(
            "trt.vs.ctrl ~ ",
            .
          )
      ), 
      data = k,
      ref = "correction"
    ) %>% 
    pairs(by = NULL, rev = T) %>% 
    extract2(2) %>% 
    as.data.frame %>% 
    tbl_df %>% 
    filter(
      contrast %>% 
        str_count("misinfo - correction") %>% 
        equals(2) &
        contrast %>% 
        str_sub(
          contrast %>% 
            stringi::stri_locate_last_fixed(" ") %>% 
            extract(, 2) %>% 
            add(1),
          -2
        ) %>%
        equals(
          k %>% 
            extract2(
              j %>% 
                str_replace(fixed("~ cond * "), "")
            ) %>% 
            levels %>% 
            extract(1)
        )
    )
)

t3 <- t2 %>% 
  select(
    treat, study, form, pair
  ) %>% 
  pmap_dfr(
    function(
    treat, study, form, pair
    ){
      
      k <- pair %>%
        filter(
          contrast %>% 
            str_detect("misinfo - ") &
            contrast %>%
            str_detect(" - correction")
        ) %>%
        mutate(
          treat = treat,
          study = study,
          fac = form %>% 
            str_extract(
              "ideo5|educ|generation|presvote16post|faminc_new|newsint|pew_churatd|pid3"
            ) %>% 
            mapvalues(
              c("ideo5",
                "educ",
                "generation",
                "presvote16post",
                "faminc_new",
                "newsint",
                "pew_churatd",
                "pid3"),
              c("Ideology",
                "Educational attainment",
                "Generation",
                "2016 presidential vote",
                "Income",
                "News interest",
                "Church attendance",
                "Partisanship")
            )
        ) %>% 
        rename(
          p.value = adj.p.value
        )
      
      names(k)[1] <- "ylab"
      k
    }
  ) %>% 
  mutate(
    clab = "Correction effects (5pt scale)"
  ) %>% 
  bind_rows(
    t2 %>% 
      select(
        treat, study, form, cont
      ) %>% 
      pmap_dfr(
        function(
    treat, study, form, cont
        ){
          
          
          k <- cont %>%
            mutate(
              ylab = contrast %>% 
                str_sub(
                  ,
                  contrast %>% 
                    str_locate("\\) -") %>% 
                    extract(, 1) %>% 
                    subtract(1)
                ),
              ylab = ylab %>% 
                str_sub(
                  ylab %>% 
                    stringi::stri_locate_last_regex("\\s")  %>% 
                    extract(, 1) %>% 
                    add(1)
                ),
              treat = treat,
              study = study,
              fac = form %>% 
                str_extract(
                  "ideo5|educ|generation|presvote16post|faminc_new|newsint|pew_churatd|pid3"
                ) %>% 
                mapvalues(
                  c("ideo5",
                    "educ",
                    "generation",
                    "presvote16post",
                    "faminc_new",
                    "newsint",
                    "pew_churatd",
                    "pid3"),
                  c("Ideology",
                    "Educational attainment",
                    "Generation",
                    "2016 presidential vote",
                    "Income",
                    "News interest",
                    "Church attendance",
                    "Partisanship")
                )
            ) %>% 
            select(-contrast) %>% 
            rename(
              std.error = SE,
              statistic = t.ratio
            )
          k
        }
      ) %>% 
      mutate(
        clab = "Difference in correction effects, compared to reference category"
      )
  )

t3$ylab %<>%  
  fct_inorder

t3$ylab %<>% 
  plyr::mapvalues(
    c("Liberal", "Moderate", "Conservative",
      "hsd", "somecollege", "ba",
      "gen_sil", "gen_boom", "gen_x", "gen_mil", "gen_z",
      "clinton", "trump", "other", "did_not_vote",
      "inc_0_30", "inc_30_70", "inc_70_100", "inc_100_150", "inc_150_inf",
      "most_of_the_time", "some_of_the_time", "only_now_and_then", "hardly_at_all",
      "weekly", "monthly", "few_times_a_year","seldom_never",
      "Democrat", "Independent", "Republican"),
    c("Liberal", "Moderate", "Conservative",
      
      "HSD or less", "Some college", "BA or more",
      
      "Silent Generation (1928-1945)",
      "Boomers (1945-1964)",
      "Generation X (1965-1980)",
      "Millenial (1981-1996)",
      "Generation Z (1997-)",
      
      "Clinton", "Trump", "Other", "Didn't vote",
      
      "$0-30k",
      "$30-70k",
      "$70-100k",
      "$100-150k",
      ">$150k",
      
      "Most of the time", "Some of the time", "Only now and then", "Hardly at all",
      
      "Weekly", "Monthly", "Few times a year", "Seldom/Never",
      
      "Democrat", "Independent", "Republican"
    ),
  ) %>% 
  factor(
    c("Liberal", "Moderate", "Conservative",
      
      "HSD or less", "Some college", "BA or more",
      
      "Silent Generation (1928-1945)",
      "Boomers (1945-1964)",
      "Generation X (1965-1980)",
      "Millenial (1981-1996)",
      "Generation Z (1997-)",
      
      "Clinton", "Trump", "Other", "Didn't vote",
      
      "$0-30k",
      "$30-70k",
      "$70-100k",
      "$100-150k",
      ">$150k",
      
      "Most of the time", "Some of the time", "Only now and then", "Hardly at all",
      
      "Weekly", "Monthly", "Few times a year", "Seldom/Never",
      
      "Democrat", "Independent", "Republican"
    )
  )


t3$est_lab <- NA

t3$est_lab <- t3$treat %>% 
  equals("Overall") %>% 
  ifelse(
    t3$estimate %>% 
      round(2) %>% 
      as.character %>% 
      str_replace(
        fixed("0."),
        "."
      ) %>% 
      str_c(
        c("***",
          "**",
          "*",
          "") %>% 
          extract(
            t3$p.value %>% 
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          )
      ),
    NA
  )

t3$fac %<>% 
  mapvalues(
    c("Generation", "2016 presidential vote", "Ideology", "Income", 
      "Partisanship", "News interest", "Church attendance", "Educational attainment"
    ),
    c("Generation", "2016 president\nvote", "Ideology", "Income", 
      "Party", "News\ninterest", "Church\nattendance", "Education"
    )
  )

t3$clab %<>% 
  mapvalues(
    c("Correction effects (5pt scale)", "Difference in correction effects, compared to reference category"),
    c("Correction effects\n(5pt scale)",
      "Difference in correction effects,\ncompared to reference category"
    )
  )

t3$fac %<>% 
  factor(
    t3 %>% 
      filter(
        clab %>% 
          str_detect("Difference")
      ) %>% 
      group_by(fac) %>% 
      summarize(
        tot = estimate %>% 
          abs %>% 
          mean
      ) %>% 
      arrange(desc(tot)) %>% 
      use_series(fac)
  )


t3$fac %<>% 
  factor(
    c("Generation", "Ideology", "2016 president\nvote", "Income", 
      "Party", "News\ninterest", "Church\nattendance", "Education")
  )


ggplot() +
  geom_vline(
    aes(xintercept = xint),
    data = data.frame(
      xint = 0,
      clab = "Difference in correction effects,\ncompared to reference category" %>% 
        factor(
          c("Correction effects\n(5pt scale)",
            "Difference in correction effects,\ncompared to reference category")
        )
    ),
    linetype = "dashed"
  ) +
  geom_beeswarm(
    aes(
      estimate, ylab
    ),
    cex=2, 
    groupOnX = F, 
    shape =  1,
    size = 2,
    data = t3 %>% 
      filter(
        treat != "Overall"
      )
  ) +
  geom_segment(
    aes(
      x = estimate %>%
        subtract(
          std.error %>%
            multiply_by(1.96)
        ),
      xend = estimate %>%
        add(
          std.error %>%
            multiply_by(1.96)
        ),
      y = ylab,
      yend = ylab
    ),
    data = t3 %>%
      filter(
        treat == "Overall"
      )
  ) +
  geom_point(
    aes(
      estimate, ylab
    ),
    shape = 21,
    size = 9,
    fill = "white",
    data = t3 %>%
      filter(
        treat == "Overall"
      )
  ) +
  geom_text(
    aes(
      estimate, ylab, label = est_lab
    ),
    data = t3 %>% 
      filter(
        treat == "Overall"
      ),
    size = 2.15
  ) +
  scale_y_discrete(
    breaks = t3$ylab %>%
      levels,
    labels = t3$ylab %>%
      levels %>%
      str_replace(fixed(" ("), "\n(")
  ) +
  facet_grid(
    fac ~ clab, 
    scales = "free",
    space = "free"
  ) +
  labs(
    x = "",
    y = ""
  )