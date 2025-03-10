
library(tidyverse)
source("data_prep.R")

# appendix b: summary statistics 

# distribution of treatments 
georgia |> 
  count(Treat) |> 
  mutate(p = n/sum(n))

# summary of outcome variables ``
georgia |> 
  select(Belief, VoteFor, ProblemDemocracy, 
         MonitorJournalists, FineJournalists, ImprisonJournalists) |> 
  pivot_longer(everything(), names_to = "dv", values_to = "response") |> 
  filter(!is.na(response)) |> 
  group_by(dv) |> 
  summarize(n = n(),
            m_response = mean(response),
            sd_response = sd(response),
            range = str_c("(", min(response), ",", max(response), ")"))

# distribution of party affiliation
georgia |> 
  count(PartyPreference) |> 
  mutate(p = n/sum(n))

# distribution of sex 
georgia |> 
  mutate(sex = as_factor(sex)) |> 
  count(sex) |> 
  mutate(p = n/sum(n))

# summary of age
georgia |> 
  filter(!is.na(age)) |> 
  summarize(m_response = mean(age),
            sd_response = sd(age),
            range = str_c("(", min(age), ",", max(age), ")")) 

# distribution of stratum 
georgia |> 
  mutate(stratum = as_factor(stratum)) |> 
  count(stratum) |> 
  mutate(p = n/sum(n))

# distribution of education  
georgia |> 
  mutate(educ = as_factor(d3)) |> 
  count(educ) |> 
  mutate(p = n/sum(n))

# appendix C 
# C1 table 

tnp_list <- list(
  "Belief" = lm(Belief ~ treat_np, data = georgia),
  "Vote for" = lm(VoteFor ~ treat_np, data = georgia),
  "Problem dem." = lm(ProblemDemocracy ~ treat_np, data = georgia),
  "Monitor Js" = lm(MonitorJournalists ~ treat_np, data = georgia),
  "Fine Js" = lm(FineJournalists ~ treat_np, data = georgia),
  "Imprison Js" = lm(ImprisonJournalists ~ treat_np, data = georgia)
)

cm <- c( '(Intercept)' = 'Constant', 
         'treat_npID' = 'ID', 
         'treat_npIDR' = 'IDR',
         'treat_npIDRE' = 'IDRE')

modelsummary::modelsummary(tnp_list,
                           statistic = NULL,
                           estimate = "{estimate} ({std.error}){stars}", 
                           output = "gt",
                           coef_map = cm,
                           gof_omit = "IC|Log") 

# C2 table 

tpbv_list <- list(
  "Belief" = lm(Belief ~ Treat*pp, data = georgia),
  "Vote for" = lm(VoteFor ~ Treat*pp, data = georgia),
  "Problem dem." = lm(ProblemDemocracy ~ Treat*pp, data = georgia),
  "Monitor Js" = lm(MonitorJournalists ~ Treat*pp, data = georgia),
  "Fine Js" = lm(FineJournalists ~ Treat*pp, data = georgia),
  "Imprison Js" = lm(ImprisonJournalists ~ Treat*pp, data = georgia)
)

modelsummary::modelsummary(tpbv_list,
                           statistic = NULL,
                           estimate = "{estimate} ({std.error}){stars}", 
                           output = "gt",
                           # coef_map = cm,
                           gof_omit = "IC|Log")

# appendix D 
# randomization checks 

rand_checks <- list(
  "Party" = nnet::multinom(Treat ~ pp, data = georgia),
  "Sex" = nnet::multinom(Treat ~ female, data = georgia),
  "Stratum" = nnet::multinom(Treat ~ stratum, data = georgia),
  "Education" = nnet::multinom(Treat ~ degree, data = georgia),
  "Age" = nnet::multinom(Treat ~ age, data = georgia)
)

rand_checks |> summary()

model_names <- rand_checks |> names()
rand_checks |> map(summary)
rand_checks_tbl <- rand_checks |> 
  map(broom::tidy) |> 
  map2(model_names,
       \(x, y) x |> mutate(model = y)) |> 
  bind_rows() |> 
  filter(term != "(Intercept)") |> 
  mutate(stars = case_when(p.value <= 0.001 ~ "***",
                           p.value <= 0.01 ~ "**",
                           p.value <= 0.05 ~ "*",
                           T ~ ""),
         estimate = str_c(round(estimate, 3), " (", 
                          round(std.error, 3), ")", 
                          stars)) |> 
  select(y.level, term, estimate, model)  
  
gt::gt(rand_checks_tbl) 

# appendix E 

georgia |> 
  select(Belief = uw1, VoteFor = uw2, ProblemDemocracy = uw4, 
         MonitorJournalists = uw3_1, FineJournalists = uw3_2, 
         ImprisonJournalists = uw3_3) |> 
  mutate(across(everything(), ~ as_factor(.))) |> 
  pivot_longer(everything()) |> 
  mutate(`Don't know` = if_else(value == "Don't know", 1, 0),
         `Refuse to answer` = if_else(value == "Refuse to answer", 1, 0)) |> 
  group_by(name) |> 
  summarize(across(`Don't know`:`Refuse to answer`, 
                   ~ mean(., na.rm = T))) |> 
  ungroup() |> 
  rename(dv = name) |>
  pivot_longer(`Don't know`:`Refuse to answer`) |> 
  mutate(name = fct_rev(as_factor(name))) |> 
  ggplot(aes(x = value, y = dv, fill = name)) +
  geom_col(position = "stack") +
  theme_bw() +
  scale_fill_manual(values = c("gray70", "gray30")) +
  labs(x = "Proportion of sample", y = NULL, fill = NULL)
  
# appendix F 

georgia <- 
  georgia |> 
  mutate(pp = if_else(PartyPreference == "No answer", "Other", PartyPreference),
         pp = factor(pp, levels = c("UNM", "GD", "Other")),
         female = if_else(sex == 2, 1, 0),
         stratum = as_factor(stratum),
         urban = if_else(stratum == "Urban", 1, 0),
         capital = if_else(stratum == "Capital", 1, 0),
         rural = if_else(stratum == "Rural", 1, 0),
         higher_ed = if_else(d3 == 5, 1, 0),
         degree = as_factor(d3), 
         degree = case_when(
           str_detect(degree, "general|Incomp") ~ "Gen ed or less",
           str_detect(degree, "Secondary") ~ "Secondary tech ed",
           str_detect(degree, "Complete") ~ "Complete higher ed"
         ),
         degree = factor(degree,
                         c("Gen ed or less",
                           "Secondary tech ed",
                           "Complete higher ed")),
         georgian = if_else(d4 == 3, 1, 0),
         pp_unm = if_else(pp == "UNM", 1, 0),
         pp_gd = if_else(pp == "GD", 1, 0),
         pp_other = if_else(pp == "Other", 1, 0),
         tbilisi = if_else(region == 1, 1, 0),
         
         # new DVs 
         # agree 
         likely_belief = if_else(Belief > 3, 1, 0),
         likely_vote = if_else(VoteFor > 3, 1, 0),
         agree_problem = if_else(ProblemDemocracy > 3, 1, 0),
         agree_monitor = if_else(MonitorJournalists > 3, 1, 0),
         agree_fine = if_else(FineJournalists > 3, 1, 0),
         agree_imprison = if_else(ImprisonJournalists > 3, 1, 0),
         # disagree 
         unlikely_belief = if_else(Belief < 3, 1, 0),
         unlikely_vote = if_else(VoteFor < 3, 1, 0),
         disagree_problem = if_else(ProblemDemocracy < 3, 1, 0),
         disagree_monitor = if_else(MonitorJournalists < 3, 1, 0),
         disagree_fine = if_else(FineJournalists < 3, 1, 0),
         disagree_imprison = if_else(ImprisonJournalists < 3, 1, 0),
         # custom coding 
         belief_very_likely = if_else(Belief == 5, 1, 0),
         vote_very_unlikely = if_else(VoteFor == 1, 1, 0),
         big_problem = if_else(ProblemDemocracy == 5, 1, 0),
         comp_agree_monitor = if_else(MonitorJournalists == 5, 1, 0),
         comp_agree_fine = if_else(FineJournalists == 5, 1, 0),
         comp_disag_impris = if_else(ImprisonJournalists == 1, 1, 0)) 

# table F1 
georgia_2 <- georgia |> filter(!is.na(treat_np), !is.na(degree)) 

ed_int <- list(
  "Belief (bd)" = lm(Belief ~ treat_np, data = georgia_2),
  "Vote for (b)" = lm(VoteFor ~ treat_np, data = georgia_2),
  "Problem (b)" = lm(ProblemDemocracy ~ treat_np, data = georgia_2),
  "Monitor (b)" = lm(MonitorJournalists ~ treat_np, data = georgia_2),
  "Fine (b)" = lm(FineJournalists ~ treat_np, data = georgia_2),
  "Imprison (b)" = lm(ImprisonJournalists ~ treat_np, data = georgia_2), 
  
  #  treat-np degree interaction (tdi)
  "Belief (tdi)" = lm(Belief ~ treat_np*degree, data = georgia_2),
  "Vote for (tdi)" = lm(VoteFor ~ treat_np*degree, data = georgia_2),
  "Problem (tdi)" = lm(ProblemDemocracy ~ treat_np*degree, 
                       data = georgia_2), # *  10 
  "Monitor (tdi)" = lm(MonitorJournalists ~ treat_np*degree, 
                       data = georgia_2), # ** 11
  "Fine (tdi)" = lm(FineJournalists ~ treat_np*degree, data = georgia_2),
  "Imprison (tdi)" = lm(ImprisonJournalists ~ treat_np*degree, 
                        data = georgia_2), # * 12
  
  # treat-np + degree (no interaction) 
  "Belief (td)" = lm(Belief ~ treat_np + degree, data = georgia_2),
  "Vote for (td)" = lm(VoteFor ~ treat_np + degree, data = georgia_2),
  "Problem (td)" = lm(ProblemDemocracy ~ treat_np + degree, 
                      data = georgia_2),
  "Monitor (td)" = lm(MonitorJournalists ~ treat_np + degree, 
                      data = georgia_2),
  "Fine (td)" = lm(FineJournalists ~ treat_np + degree, 
                   data = georgia_2),
  "Imprison (td)" = lm(ImprisonJournalists ~ treat_np + degree, 
                       data = georgia_2), 
  
  # treat-np higher ed interaction
  "Belief (thi)" = lm(Belief ~ treat_np*higher_ed, data = georgia_2),
  "Vote for (thi)" = lm(VoteFor ~ treat_np*higher_ed, data = georgia_2),
  "Problem (thi)" = lm(ProblemDemocracy ~ treat_np*higher_ed, 
                       data = georgia_2),
  "Monitor (thi)" = lm(MonitorJournalists ~ treat_np*higher_ed, 
                       data = georgia_2),
  "Fine (thi)" = lm(FineJournalists ~ treat_np*higher_ed, data = georgia_2),
  "Imprison (thi)" = lm(ImprisonJournalists ~ treat_np*higher_ed, 
                        data = georgia_2),
  
  # treat-np higher ed (no interaction) 
  "Belief (th)" = lm(Belief ~ treat_np + higher_ed , data = georgia_2),
  "Vote for (th)" = lm(VoteFor ~ treat_np + higher_ed, data = georgia_2),
  "Problem (th)" = lm(ProblemDemocracy ~ treat_np + higher_ed, 
                      data = georgia_2),
  "Monitor (th)" = lm(MonitorJournalists ~ treat_np + higher_ed, 
                      data = georgia_2),
  "Fine (th)" = lm(FineJournalists ~ treat_np + higher_ed, 
                   data = georgia_2),
  "Imprison (th)" = lm(ImprisonJournalists ~ treat_np + higher_ed, 
                       data = georgia_2)
  
) 

ed_list <- list(
  anova(ed_int[[1]], ed_int[[7]], ed_int[[13]], ed_int[[19]],ed_int[[25]]),
  anova(ed_int[[2]], ed_int[[8]], ed_int[[14]], ed_int[[20]], ed_int[[26]]),
  anova(ed_int[[3]], ed_int[[9]], ed_int[[15]], ed_int[[21]], ed_int[[27]]),
  anova(ed_int[[4]], ed_int[[10]], ed_int[[16]], ed_int[[22]], ed_int[[28]]),
  anova(ed_int[[5]], ed_int[[11]], ed_int[[17]], ed_int[[23]], ed_int[[29]]),
  anova(ed_int[[6]], ed_int[[12]], ed_int[[18]], ed_int[[24]], ed_int[[30]])
) 

ed_list |>
  map(broom::tidy) |>
  bind_rows() |>
  filter(!is.na(df)) |>
  select(model = term, rss, df, p.value) |>
  mutate(stars = case_when(p.value < 0.001 ~ "***",
                           p.value < 0.01  ~ "**",
                           p.value < 0.05  ~ "*",
                           T ~ ""),
         p.value = round(p.value, 4),
         rss = round(rss, 1)) |> 
  gt::gt()

# table F2 
ed_table_data <- georgia |> mutate(` ` = treat_np, `  ` = degree) 

sig_list <- list(
  "Problem" = lm(ProblemDemocracy ~ ` `*`  `, data = ed_table_data), # *  09 
  "Monitor" = lm(MonitorJournalists ~ ` `*`  `, data = ed_table_data), # ** 10
  "Imprison" = lm(ImprisonJournalists ~ ` `*`  `, data = ed_table_data) # * 12
)

sig_list |> 
  modelsummary::modelsummary(
    estimate = "{estimate} ({std.error}){stars}", 
    statistic = NULL,
    gof_map = c("nobs", "r.squared", "adj.r.squared", "rmse")
  )

# figure F1 

list(
  "Belief" = lm(Belief ~ treat_np*degree, data = georgia_2),
  "Vote for" = lm(VoteFor ~ treat_np*degree, data = georgia_2),
  "Problem*" = lm(ProblemDemocracy ~ treat_np*degree, data = georgia_2), 
  "Monitor*" = lm(MonitorJournalists ~ treat_np*degree, data = georgia_2),
  "Fine" = lm(FineJournalists ~ treat_np*degree, data = georgia_2),
  "Imprison*" = lm(ImprisonJournalists ~ treat_np*degree, data = georgia_2)
) |> 
  map(ggeffects::ggpredict,
      terms = c("treat_np [all]", "degree [all]")) |> 
  map(as_tibble) |> 
  bind_rows() |> 
  rename(treat_np = x, degree = group) |> 
  mutate(dv = rep(c("Belief", "Vote for", "Problem*",
                    "Monitor*", "Fine", "Imprison*"), 
                  each = 12),
         dv = factor(dv, c("Belief", "Vote for", "Problem*",
                           "Monitor*", "Fine", "Imprison*")),
         treat_np = fct_rev(treat_np)) |> 
  ggplot(aes(x = predicted, y = treat_np, color = degree)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.1,
                  position = ggstance::position_dodgev(height = 0.6)) +
  theme_bw() +
  facet_wrap(~ dv) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(legend.position = "top") 

# table F3 

tb_int <- list(
  "Belief (b)" = lm(Belief ~ treat_np, data = georgia),
  "Vote for (b)" = lm(VoteFor ~ treat_np, data = georgia),
  "Problem (b)" = lm(ProblemDemocracy ~ treat_np, data = georgia),
  "Monitor (b)" = lm(MonitorJournalists ~ treat_np, data = georgia),
  "Fine (b)" = lm(FineJournalists ~ treat_np, data = georgia),
  "Imprison (b)" = lm(ImprisonJournalists ~ treat_np, data = georgia),
  
  "Belief*" = lm(Belief ~ treat_np*tbilisi, data = georgia),
  "Vote for" = lm(VoteFor ~ treat_np*tbilisi, data = georgia),
  "Problem" = lm(ProblemDemocracy ~ treat_np*tbilisi, data = georgia),
  "Monitor" = lm(MonitorJournalists ~ treat_np*tbilisi, data = georgia),
  "Fine" = lm(FineJournalists ~ treat_np*tbilisi, data = georgia),
  "Imprison" = lm(ImprisonJournalists ~ treat_np*tbilisi, data = georgia),
  
  "Belief (ni)" = lm(Belief ~ treat_np + tbilisi, data = georgia),
  "Vote for (ni)" = lm(VoteFor ~ treat_np + tbilisi, data = georgia),
  "Problem (ni)" = lm(ProblemDemocracy ~ treat_np + tbilisi, data = georgia),
  "Monitor (ni)" = lm(MonitorJournalists ~ treat_np + tbilisi, data = georgia),
  "Fine (ni)" = lm(FineJournalists ~ treat_np + tbilisi, data = georgia),
  "Imprison (ni)" = lm(ImprisonJournalists ~ treat_np + tbilisi, data = georgia)
) 

list(
  anova(tb_int[[1]], tb_int[[7]], tb_int[[13]]),
  anova(tb_int[[2]], tb_int[[8]], tb_int[[14]]),
  anova(tb_int[[3]], tb_int[[9]], tb_int[[15]]),
  anova(tb_int[[4]], tb_int[[10]], tb_int[[16]]),
  anova(tb_int[[5]], tb_int[[11]], tb_int[[17]]),
  anova(tb_int[[6]], tb_int[[12]], tb_int[[18]])
) |>
  map(broom::tidy) |>
  bind_rows() |>
  filter(!is.na(df)) |>
  select(model = term, rss, df, p.value) |>
  mutate(stars = case_when(p.value < 0.001 ~ "***",
                           p.value < 0.01  ~ "**",
                           p.value < 0.05  ~ "*",
                           T ~ ""),
         p.value = round(p.value, 4),
         rss = round(rss, 1)) |> 
  gt::gt()

# table F4 
tb_int[7] |> 
  modelsummary::modelsummary(
    estimate = "{estimate} ({std.error}){stars}", 
    statistic = NULL,
    gof_map = c("nobs", "r.squared", "adj.r.squared", "rmse")
  )

# figure F2 
tb_int[7:12] |> 
  map(ggeffects::ggpredict,
      terms = c("treat_np [all]", "tbilisi [all]")) |> 
  map(as_tibble) |> 
  bind_rows() |> 
  rename(treat_np = x, 
         tbilisi = group) |> 
  mutate(dv = rep(names(tb_int[7:12]), each = 8),
         tbilisi = if_else(tbilisi == 1, "Tbilisi", "Other"),
         dv = factor(dv,
                     c("Belief*", "Vote for", "Problem",
                       "Monitor", "Fine", "Imprison")),
         treat_np = fct_rev(treat_np)) |> 
  # filter(dv == "Belief*") |> 
  ggplot(aes(x = predicted, y = treat_np, color = tbilisi)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.1,
                  position = ggstance::position_dodgev(height = 0.6)) +
  theme_bw() +
  facet_wrap(~ dv) +
  labs(x = NULL, y = NULL, color = "Region") +
  theme(legend.position = "top")

# table F5 
list_1 <- list(
  # baseline 
  "Belief (b)" = glm(likely_belief ~ 1, 
                     data = georgia, family = "binomial"),
  "Vote for (b)" = glm(likely_vote ~ 1,
                       data = georgia, family = "binomial"),
  "Problem (b)" = glm(agree_problem ~ 1, 
                      data = georgia, family = "binomial"),
  "Monitor (b)" = glm(agree_monitor ~ 1, 
                      data = georgia, family = "binomial"),
  "Fine (b)" = glm(agree_fine ~ 1, 
                   data = georgia, family = "binomial"),
  "Imprison (b)" = glm(agree_imprison ~ 1,
                       data = georgia, family = "binomial"),
  # treat-no party 
  "Belief (tnp)" = glm(likely_belief ~ treat_np, 
                       data = georgia, family = "binomial"),
  "Vote for (tnp)" = glm(likely_vote ~ treat_np,
                         data = georgia, family = "binomial"),
  "Problem (tnp)" = glm(agree_problem ~ treat_np, 
                        data = georgia, family = "binomial"),
  "Monitor (tnp)" = glm(agree_monitor ~ treat_np, 
                        data = georgia, family = "binomial"),
  "Fine (tnp)" = glm(agree_fine ~ treat_np, 
                     data = georgia, family = "binomial"),
  "Imprison (tnp)" = glm(agree_imprison ~ treat_np,
                         data = georgia, family = "binomial"),
  # treat*pp tpi: treatment party interaction 
  "Belief (tpi)" = glm(likely_belief ~ Treat*pp, 
                       data = georgia, family = "binomial"),
  "Vote for (tpi)" = glm(likely_vote ~ Treat*pp, 
                         data = georgia, family = "binomial"),
  "Problem (tpi)" = glm(agree_problem ~ Treat*pp, 
                        data = georgia, family = "binomial"),
  "Monitor (tpi)" = glm(agree_monitor ~ Treat*pp, 
                        data = georgia, family = "binomial"),
  "Fine (tpi)" = glm(agree_fine ~ Treat*pp, 
                     data = georgia, family = "binomial"),
  "Imprison (tpi)" = glm(agree_imprison ~ Treat*pp, 
                         data = georgia, family = "binomial")
)

list_2 <- list(
  # baseline 
  "Don't believe (b)" = glm(unlikely_belief ~ 1, 
                            data = georgia, family = "binomial"),
  "Unlikely vote (b)" = glm(unlikely_vote ~ 1,
                            data = georgia, family = "binomial"),
  "Not problem (b)" = glm(disagree_problem ~ 1, 
                          data = georgia, family = "binomial"),
  "Don't monitor (b)" = glm(disagree_monitor ~ 1, 
                            data = georgia, family = "binomial"),
  "Don't fine (b)" = glm(disagree_fine ~ 1, 
                         data = georgia, family = "binomial"),
  "Don't imprison (b)" = glm(disagree_imprison ~ 1,
                             data = georgia, family = "binomial"),
  # treat-no party 
  "Don't believe (tnp)" = glm(unlikely_belief ~ treat_np, 
                              data = georgia, family = "binomial"),
  "Unlikely vote (tnp)" = glm(unlikely_vote ~ treat_np,
                              data = georgia, family = "binomial"),
  "Not problem (tnp)" = glm(disagree_problem ~ treat_np, 
                            data = georgia, family = "binomial"),
  "Don't monitor (tnp)" = glm(disagree_monitor ~ treat_np, 
                              data = georgia, family = "binomial"),
  "Don't fine (tnp)" = glm(disagree_fine ~ treat_np, 
                           data = georgia, family = "binomial"),
  "Don't imprison (tnp)" = glm(disagree_imprison ~ treat_np,
                               data = georgia, family = "binomial"),
  # treat*pp tpi: treatment party interaction 
  "Don't believe (tpi)" = glm(unlikely_belief ~ Treat*pp, 
                              data = georgia, family = "binomial"),
  "Unlikely vote (tpi)" = glm(unlikely_vote ~ Treat*pp, 
                              data = georgia, family = "binomial"),
  "Not problem (tpi)" = glm(disagree_problem ~ Treat*pp, 
                            data = georgia, family = "binomial"),
  "Don't monitor (tpi)" = glm(disagree_monitor ~ Treat*pp, 
                              data = georgia, family = "binomial"),
  "Don't fine (tpi)" = glm(disagree_fine ~ Treat*pp, 
                           data = georgia, family = "binomial"),
  "Don't imprison (tpi)" = glm(disagree_imprison ~ Treat*pp, 
                               data = georgia, family = "binomial")
) 

list_3 <- list(
  # baseline 
  "Belief VL (b)" = glm(belief_very_likely ~ 1, 
                        data = georgia, family = "binomial"),
  "VU vote (b)" = glm(vote_very_unlikely ~ 1,
                      data = georgia, family = "binomial"),
  "Big problem (b)" = glm(big_problem ~ 1, 
                          data = georgia, family = "binomial"),
  "CA monitor (b)" = glm(comp_agree_monitor ~ 1, 
                         data = georgia, family = "binomial"),
  "CA fine (b)" = glm(comp_agree_fine ~ 1, 
                      data = georgia, family = "binomial"),
  "CD imprison (b)" = glm(comp_disag_impris ~ 1,
                          data = georgia, family = "binomial"),
  # treat-no party 
  "Belief VL (tnp)" = glm(belief_very_likely ~ treat_np, 
                          data = georgia, family = "binomial"),
  "VU vote (tnp)" = glm(vote_very_unlikely ~ treat_np,
                        data = georgia, family = "binomial"),
  "Big problem (tnp)" = glm(big_problem ~ treat_np, 
                            data = georgia, family = "binomial"),
  "CA monitor (tnp)" = glm(comp_agree_monitor ~ treat_np, 
                           data = georgia, family = "binomial"),
  "CA fine (tnp)" = glm(comp_agree_fine ~ treat_np, 
                        data = georgia, family = "binomial"),
  "CD imprison (tnp)" = glm(comp_disag_impris ~ treat_np,
                            data = georgia, family = "binomial"),
  # treat*pp tpi: treatment party interaction 
  "Belief VL (tpi)" = glm(belief_very_likely ~ Treat*pp, 
                          data = georgia, family = "binomial"),
  "VU vote (tpi)" = glm(vote_very_unlikely ~ Treat*pp, 
                        data = georgia, family = "binomial"),
  "Not problem (tpi)" = glm(big_problem ~ Treat*pp, 
                            data = georgia, family = "binomial"),
  "CA monitor (tpi)" = glm(comp_agree_monitor ~ Treat*pp, 
                           data = georgia, family = "binomial"),
  "CA fine (tpi)" = glm(comp_agree_fine ~ Treat*pp, 
                        data = georgia, family = "binomial"),
  "CD imprison (tpi)" = glm(comp_disag_impris ~ Treat*pp, 
                            data = georgia, family = "binomial")
)  

anova_list <- list(
  anova(list_1[[1]], list_1[[7]], list_1[[13]], test = "Chisq"), # belief
  anova(list_1[[2]], list_1[[8]], list_1[[14]], test = "Chisq"), # vote
  anova(list_1[[3]], list_1[[9]], list_1[[15]], test = "Chisq"), # problem 
  anova(list_1[[4]], list_1[[10]], list_1[[16]], test = "Chisq"), # monitor
  anova(list_1[[5]], list_1[[11]], list_1[[17]], test = "Chisq"), # fine
  anova(list_1[[6]], list_1[[12]], list_1[[18]], test = "Chisq"), # imprison 
  anova(list_2[[1]], list_2[[7]], list_2[[13]], test = "Chisq"), # belief
  anova(list_2[[2]], list_2[[8]], list_2[[14]], test = "Chisq"), # vote
  anova(list_2[[3]], list_2[[9]], list_2[[15]], test = "Chisq"), # problem 
  anova(list_2[[4]], list_2[[10]], list_2[[16]], test = "Chisq"), # monitor
  anova(list_2[[5]], list_2[[11]], list_2[[17]], test = "Chisq"), # fine
  anova(list_2[[6]], list_2[[12]], list_2[[18]], test = "Chisq"), # imprison
  anova(list_3[[1]], list_3[[7]], list_3[[13]], test = "Chisq"), # belief
  anova(list_3[[2]], list_3[[8]], list_3[[14]], test = "Chisq"), # vote
  anova(list_3[[3]], list_3[[9]], list_3[[15]], test = "Chisq"), # problem 
  anova(list_3[[4]], list_3[[10]], list_3[[16]], test = "Chisq"), # monitor
  anova(list_3[[5]], list_3[[11]], list_3[[17]], test = "Chisq"), # fine
  anova(list_3[[6]], list_3[[12]], list_3[[18]], test = "Chisq") # imprison
)

anova_list |> 
  map(broom::tidy) |> 
  bind_rows() |> 
  filter(!is.na(df)) |> 
  select(model = term, resid_dev = residual.deviance, df, p.value) |> 
  mutate(set = rep(c(1:3), each = 12), 
         stars = case_when(p.value < 0.001 ~ "***",
                           p.value < 0.01 ~ "**",
                           p.value < 0.05 ~ "*",
                           T ~ ""),
         p.value = round(p.value, 4),
         resid_dev = round(resid_dev, 1)) |> 
  gt::gt() 

# table F6 

custom_list <- list(
  "Likely-belief" = glm(likely_belief ~ Treat*pp, 
                        data = georgia, family = "binomial"),
  "Unlikely-belief" = glm(unlikely_belief ~ treat_np, 
                          data = georgia, family = "binomial"),
  "Unlikely-belief" = glm(unlikely_belief ~ Treat*pp, 
                          data = georgia, family = "binomial"),
  "Unlikely-vote" = glm(unlikely_vote ~ treat_np, 
                        data = georgia, family = "binomial"),
  "Very unlikely-belief" = glm(belief_very_likely ~ Treat*pp, 
                               data = georgia, family = "binomial"),
  "Very unlikely-vote" = glm(vote_very_unlikely ~ treat_np, 
                             data = georgia, family = "binomial"),
  "Very unlikely-vote" = glm(vote_very_unlikely ~ Treat*pp,
                             data = georgia, family = "binomial")
)

custom_list[c(2,4,6)] |> 
  modelsummary::modelsummary(
    estimate = "{estimate} ({std.error}){stars}", 
    statistic = NULL
  )

# figure F3 

custom_list[c(2,4,6)] |> 
  map(ggeffects::ggpredict, terms = c("treat_np [all]")) |> 
  map(as_tibble) |> 
  bind_rows() |> 
  mutate(dv = rep(names(custom_list[c(2,4,6)]), each = 4),
         x = fct_rev(x)) |> 
  ggplot(aes(x = predicted, y = x)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.1) + 
  facet_wrap(~ dv) +
  labs(x = "Predicted probability", y = NULL) + 
  theme_bw()
