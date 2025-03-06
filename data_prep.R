
library(tidyverse)
library(haven)

georgia <- 
  read_dta("CRRC_Omnibus_Wave13.dta") |> 
  mutate(Treatment = case_when(exp == -9 ~ NA_character_,
                               exp == 1 ~ "UNM innocent",
                               exp == 2 ~ "UNM innocent+victim", 
                               exp == 3 ~ "UNM innocent+victim+Russia", 
                               exp == 4 ~ "UNM innocent+victim+Russia+uncover", 
                               exp == 5 ~ "GD innocent", 
                               exp == 6 ~ "GD innocent+victim", 
                               exp == 7 ~ "GD innocent+victim+Russia", 
                               exp == 8 ~ "GD innocent+victim+Russia+uncover"),
         treat_unm = if_else(str_detect(Treatment, "UNM"), 1, 0),
         Belief = if_else(uw1 < 0, NA_real_, 6 - uw1),
         VoteFor = if_else(uw2 < 0, NA_real_, 6 - uw2),
         MonitorJournalists = if_else(uw3_1 < 0, NA_real_, 6 - uw3_1),
         FineJournalists = if_else(uw3_2 < 0, NA_real_, 6 - uw3_2),
         ImprisonJournalists = if_else(uw3_3 < 0, NA_real_, 6 - uw3_3),
         ProblemDemocracy = if_else(uw4 < 0, NA_real_, 6 - uw4),
         PartyPreference = case_when(m6 < 0 ~ "No answer",
                                     m6 == 1 ~ "GD",
                                     m6 == 2 ~ "UNM",
                                     TRUE ~ "Other"),
         PP_ = PartyPreference, 
         party = as_factor(m6),
         party_unm = if_else(PartyPreference == "UNM", 1, 0),
         # treat no party 
         treat_np = case_when(exp %in% c(1,5) ~ "I",
                              exp %in% c(2,6) ~ "ID",
                              exp %in% c(3,7) ~ "IDR",
                              exp %in% c(4,8) ~ "IDRE"),
         treat_party = if_else(exp < 5, "UNM", "GD"),
         treatment_2 = paste0(treat_party, " ", treat_np),
         Treat = treatment_2) 

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


