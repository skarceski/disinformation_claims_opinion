
library(tidyverse)
source("data_prep.R")

# appendix b: summary statistics 

georgia |> 
  count(PartyPreference) |> 
  mutate(p = n/sum(n))

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

georgia |> 
  select(age, VoteFor, ProblemDemocracy, 
         MonitorJournalists, FineJournalists, ImprisonJournalists) |> 
  pivot_longer(everything(), names_to = "dv", values_to = "response") |> 
  filter(!is.na(response)) |> 
  group_by(dv) |> 
  summarize(n = n(),
            m_response = mean(response),
            sd_response = sd(response),
            range = str_c("(", min(response), ",", max(response), ")"))


georgia |> 
  count(d3, sort = T) |> 
  mutate(p = n/sum(n)) 

georgia |> 
  filter(!is.na(age)) |> 
  summarize(m_response = mean(age),
            sd_response = sd(age),
            range = str_c("(", min(age), ",", max(age), ")"))

# fig 3 models 

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
cap <- 'A modelsummary table customized with gt'

modelsummary::modelsummary(tnp_list,
                           statistic = NULL,
                           estimate = "{estimate} ({std.error}){stars}", 
                           output = "gt",
                           coef_map = cm,
                           gof_omit = "IC|Log") 

# fig 5 models 

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

rand_checks <- list(
  "Party" = nnet::multinom(Treat ~ pp, data = georgia),
  "Sex" = nnet::multinom(Treat ~ female, data = georgia),
  "Stratum" = nnet::multinom(Treat ~ stratum, data = georgia),
  "Education" = nnet::multinom(Treat ~ degree, data = georgia),
  "Age" = nnet::multinom(Treat ~ age, data = georgia)
)


# age checks out
# female checks out 
#

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
  
write_csv(rand_checks_tbl, "random_checks.csv")

  
