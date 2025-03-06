
library(tidyverse) 
source("data_prep.R")

# figure 1 
georgia |> 
  count(Treat, pp) |> 
  mutate(Treat = fct_rev(Treat),
         pp = fct_rev(pp)) |> 
  ggplot(aes(x = n, y = Treat, fill = pp)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = rev(c("#fc9272", "#6baed6", "#999999")),
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  labs(y = NULL, x = "Count", fill = NULL) +
  theme(legend.position = "top") 

# figure 2 
georgia |> 
  select(Belief, VoteFor, ProblemDemocracy, 
         MonitorJournalists, FineJournalists, ImprisonJournalists) |> 
  pivot_longer(everything(), names_to = "question", values_to = "response") |> 
  filter(!is.na(response)) |> 
  group_by(question) |> 
  count(response) |> 
  ungroup() |> 
  mutate(question = case_when(
    question == "Belief" ~ "Guilty",
    question == "VoteFor" ~ "Vote for",
    question == "ProblemDemocracy" ~ "Problem for democracy",
    question == "MonitorJournalists" ~ "Monitor journalists",
    question == "FineJournalists" ~ "Fine journalists",
    question == "ImprisonJournalists" ~ "Imprison journalists"
  ),
  question = factor(question,
                    levels = c("Guilty", "Vote for", "Problem for democracy", 
                               "Monitor journalists", "Fine journalists", 
                               "Imprison journalists"))) |> 
  ggplot(aes(x = response, y = n)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~ question) +
  labs(x = NULL, y = "Count") 

# table 1 
georgia |> 
  select(Belief, VoteFor, ProblemDemocracy, 
         MonitorJournalists, FineJournalists, ImprisonJournalists) |> 
  pivot_longer(everything(), names_to = "question", values_to = "response") |> 
  filter(!is.na(response)) |> 
  group_by(question) |> 
  summarize(Mean = mean(response), 
            SD = sd(response), 
            Median = median(response), 
            n = n())

# models for figure 3 
m1_list <- 
  list(
    "Belief" = lm(Belief ~ treat_np, data = georgia),
    "Vote for" = lm(VoteFor ~ treat_np, data = georgia),
    "Problem for dem." = lm(ProblemDemocracy ~ treat_np, data = georgia),
    "Monitor Js" = lm(MonitorJournalists ~ treat_np, data = georgia),
    "Fine Js" = lm(FineJournalists ~ treat_np, data = georgia),
    "Imprison Js" = lm(ImprisonJournalists ~ treat_np, data = georgia)
  ) 

m1_pred <- 
  m1_list |> 
  map2(names(m1_list),
       \ (x,y) 
       ggeffects::ggpredict(x, terms = c("treat_np [all]")) |> 
         as_tibble() |> 
         mutate(dv = y)) |> 
  bind_rows() |> 
  mutate(dv = factor(dv, levels = c(
    "Belief", "Vote for", "Problem for dem.", 
    "Monitor Js", "Fine Js", "Imprison Js"
  ))) 

# figure 3 
m1_pred |> 
  mutate(treat = fct_rev(x)) |> 
  ggplot(aes(x = predicted, y = treat)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.1,
                  position = ggstance::position_dodgev(height = 0.6)) +
  theme_bw() +
  facet_wrap(~ dv) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(legend.position = "top")


# models for figures 5 and 6 
m2_list <- 
  list(
    "Belief" = lm(Belief ~ Treat*pp, data = georgia),
    "Vote for" = lm(VoteFor ~ Treat*pp, data = georgia),
    "Problem for dem." = lm(ProblemDemocracy ~ Treat*pp, data = georgia),
    "Monitor Js" = lm(MonitorJournalists ~ Treat*pp, data = georgia),
    "Fine Js" = lm(FineJournalists ~ Treat*pp, data = georgia),
    "Imprison Js" = lm(ImprisonJournalists ~ Treat*pp, data = georgia)
  )

m2_pred <- 
  m2_list |> 
  map(ggeffects::ggpredict, 
      terms = c("Treat [all]", "pp [all]")) |> 
  map(as_tibble) |> 
  bind_rows() |> 
  bind_cols(tibble(dv = rep(names(m2_list), each = 24))) |> 
  mutate(dv = factor(dv, levels = c(
    "Belief", "Vote for", "Problem for dem.", 
    "Monitor Js", "Fine Js", "Imprison Js"
  ))) 

# figure 5 
m2_pred |> 
  filter(dv %in% c("Belief", "Vote for")) |> 
  mutate(treat = fct_rev(x)) |> 
  ggplot(aes(x = predicted, y = treat, color = group)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.1,
                  position = ggstance::position_dodgev(height = 0.6)) +
  scale_color_manual(values = c("#fc9272", "#6baed6",
                                # "aquamarine3",
                                "#999999")) +
  theme_bw() +
  facet_wrap(~ dv) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(legend.position = "top")

# figure 6 
m2_pred |> 
  filter(!dv %in% c("Belief", "Vote for")) |> 
  mutate(treat = fct_rev(x)) |> 
  ggplot(aes(x = predicted, y = treat, color = group)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.1,
                  position = ggstance::position_dodgev(height = 0.6)) +
  scale_color_manual(values = c("#fc9272", "#6baed6",
                                # "aquamarine3",
                                "#999999")) +
  theme_bw() +
  facet_wrap(~ dv) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(legend.position = "top")



