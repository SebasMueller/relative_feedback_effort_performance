library(stargazer)
library(huxtable)
library(xtable)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(betareg)
library(fitdistrplus)
library(DHARMa)
library(effsize)
library(reldist)
library(fBasics)
library(dplyr)
library(sjPlot)

#print(getwd())


# INDIVIDUAL LEVEL data with weights by stratum
get_data_ind <- function(fname, dep_var){
  
  # Load and prepare data
  df <- load_data_in(fname)
  
  # Get rid of strata with missing values on dep_var and old rating
  df <- clean_missing_in(df, 'old_rating_rank_norm')
  df <- clean_missing_in(df, dep_var)
  
  # Get weights
  weights <- cem_weight(df)
  df <- merge(df, weights, by = 'stratum')
  # All treated cases are weighted 1
  df[df$treatment == 1, 'weight'] <- 1
  # Check
  #print(length(df[df$treatment == 0, 'weight']))
  #print(sum(df[df$treatment == 0, 'weight']))
  
  return(df)
}

get_data_newcom <- function(fname, dep_var){
  
  # Load and prepare data
  df <- load_data_in(fname)
  
  # Get rid of strata with missing values on dep_var
  df <- clean_missing_newcom(df, dep_var)
  
  # Get weights
  weights <- cem_weight(df)
  df <- merge(df, weights, by = 'stratum')
  # All treated cases are weighted 1
  df[df$treatment == 1, 'weight'] <- 1
  
  return(df)
}


# INDIVIDUAL LEVEL: Load and prepare data
load_data_in <- function(fname){
  df <- read.csv(fname)
  # Create dummy variables for analysis
  df$treatment <- ifelse(df$scoring_type == 'Relative', 1, 0)
  df$tc_provisional_testing <- ifelse(df$provisional_testing == 'TC only', 1, 0)
  df$optimization <- ifelse(df$task_type == 'Optimization', 1, 0)
  df$quality_target <- ifelse(df$target == 'Quality', 1, 0)
  # Recode 1's otherwise beta regression will not work
  # See https://www.researchgate.net/profile/Jay_Verkuilen/publication/7184584_A_better_lemon_squeezer_Maximum-likelihood_regression_with_beta-distributed_dependent_variables/links/09e4150e4f9e56a692000000/A-better-lemon-squeezer-Maximum-likelihood-regression-with-beta-distributed-dependent-variables.pdf
  # y" = (y'(N-1) + 0.5) / N, where N = 29,075 is the total number of contestants in the 367 challenges
  df <- df %>%
    mutate(predictability_prov = (predictability_prov*29074 + 0.5) / 29075) %>%
    mutate(predictability_fin = (predictability_fin*29074 + 0.5) / 29075) %>%
    mutate(prov_score_norm_distance = (prov_score_norm_distance*29074 + 0.5) / 29075) %>%
    mutate(fin_score_norm_distance = (fin_score_norm_distance*29074 + 0.5) / 29075) %>%
    mutate(prov_rank_norm = (prov_rank_norm*29074 + 0.5) / 29075) %>%
    mutate(fin_rank_norm = (fin_rank_norm*29074 + 0.5) / 29075) %>%
    mutate(first_submission_time = (first_submission_time*29074 + 0.5) / 29075) %>%
    mutate(last_submission_time = (last_submission_time*29074 + 0.5) / 29075) %>%
    mutate(length_submission_period = (length_submission_period*29074 + 0.5) / 29075)
  return(df)
}


# INDIVIDUAL LEVEL: Get rid of strata/individuals with missing values on dep_var 
clean_missing_in <- function(df, var){
  # Remove coders who are marked with 0 submissions (data error)
  df <- df %>%
    filter(num_submissions != 0) 
  # Remove coders with missings on the dependent variable
  df <- df %>%
    filter(!is.na(eval(as.name(var))))
  # If looking at code length, remove two outliers!!!
  if (var %in% c('sum_code_lines')) {
    df <- df %>%
      filter(sum_insertions < 10000)
    #filter(UQ(as.symbol(var)) < 10000)
  }
  # Get rid of stratum-coders who don't have at least 1 treatment and 1 control obs per stratum 
  strata_to_keep <- df %>%
    group_by(stratum, coder_id, treatment) %>%
    count() %>%
    ungroup() %>%
    group_by(stratum, coder_id) %>%
    count() %>%
    filter(n == 2) %>%
    select(stratum, coder_id)
  df <- merge(df, strata_to_keep, by = c("stratum", "coder_id"))
  return(df)
}

clean_missing_newcom <- function(df, var){
  # Remove coders with missings on the dependent variable
  df <- df %>%
    filter(!is.na(eval(as.name(var))))
  # If depvar is a diff, remove those with val > 1  - REMOVE WHEN DATA FIXED!!!
  if (var %in% c('mean_equals', 'mean_insertions', 'mean_deletions')) {
    df <- df %>%
      filter(UQ(as.symbol(var)) <= 1)
  }
  # Get rid of strata that don't have at least 1 treatment and 1 control obs 
  strata_to_keep <- df %>%
    group_by(stratum, treatment) %>%
    count() %>%
    ungroup() %>%
    group_by(stratum) %>%
    count() %>%
    filter(n == 2) %>%
    pull(stratum)
  df <- df %>%
    filter(stratum %in% strata_to_keep)
  return(df)
}


# INDIVIDUAL LEVEL mixed effects beta regression with weights by stratum
# Random effects by individual and stratum/round_id, interaction between two
# https://www.jstor.org/stable/pdf/24709903.pdf?casa_token=FmmzaceNPe8AAAAA:Gh2Amee4vmNBQLlOsEVD1ccwkbiw2IRDF2b_WKy2ainFYjjI23I5pDlSOu9X2Bhlj8Qa7uTpQdJgtyc2FR2sfFDjifqTsYN01s-eeNLvMpRXCRueVps
# https://stats.stackexchange.com/questions/242250/interpreting-two-way-interaction-in-the-presence-of-quadratic-interaction


# Estimate 2 models with and without (treatment*old_rating_rank_norm  + treatment*I(old_rating_rank_norm^2))
# Then estimate F-test
estimate_predict <- function(df){
  model1 <- glmmTMB(predictability_prov ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + 
                      num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = beta_family())
  print(summary(model1))
  
  model2 <- glmmTMB(predictability_prov ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + 
                      treatment*old_rating_rank_norm + num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = beta_family())
  print(summary(model2))
  
  anova(model1, model2)
}

estimate_predictfin <- function(df){
  model1 <- glmmTMB(predictability_fin ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + 
                      num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = beta_family())
  print(summary(model1))
  
  model2 <- glmmTMB(predictability_fin ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + 
                      treatment*old_rating_rank_norm + num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = beta_family())
  print(summary(model2))
  
  # Chi-square difference between nested models
  anova(model1, model2)
}

estimate_numsub <- function(df){
  model1 <- glmmTMB(num_submissions ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
                      num_prev_ratings +
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model1))
  
  model2 <- glmmTMB(num_submissions ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
                      treatment*old_rating_rank_norm + num_prev_ratings +
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model2))
  
  # Chi-square difference between nested models
  anova(model1, model2)
}


estimate_dur <- function(df){
  model1 <- glmmTMB(length_submission_period ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
                     num_prev_ratings + 
                     (1|coder_id) + (1|round_id) + (1|stratum),
                   data = df, weights = weight,
                   family = beta_family())
  print(summary(model1))
  
  model2 <- glmmTMB(length_submission_period ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
                     treatment*old_rating_rank_norm + num_prev_ratings + 
                     (1|coder_id) + (1|round_id) + (1|stratum),
                   data = df, weights = weight,
                   family = beta_family())
  print(summary(model2))
  
  # Chi-square difference between nested models
  print(anova(model1, model2))
}

estimate_firstsub <- function(df){
  model1 <- glmmTMB(first_submission_time ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + 
                     num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = beta_family())
  print(summary(model1))
  model2 <- glmmTMB(first_submission_time ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + 
                      treatment*old_rating_rank_norm + num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = beta_family())
  print(summary(model2))
  # Chi-square difference between nested models
  anova(model1, model2)
}

estimate_lastsub <- function(df){
  model1 <- glmmTMB(last_submission_time ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + 
                      num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = beta_family())
  print(summary(model1))
  
  model2 <- glmmTMB(last_submission_time ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + 
                      treatment*old_rating_rank_norm + num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = beta_family())
  print(summary(model2))
  
  # Chi-square difference between nested models
  anova(model1, model2)
}

estimate_firstsublines <- function(df){
  model1 <- glmmTMB(first_submission_lines ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
            num_prev_ratings + 
            (1|coder_id) + (1|round_id) + (1|stratum),
          data = df, weights = weight,
          family = Gamma(link = "log"))
  print(summary(model1))
  
  model2 <- glmmTMB(first_submission_lines ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
                      treatment*old_rating_rank_norm + treatment*I(old_rating_rank_norm^2) + num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model2))
  
  # Chi-square difference between nested models
  anova(model1, model2)
}


estimate_sumcodelines <- function(df){
  model1 <- glmmTMB(sum_code_lines ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
                     num_prev_ratings + 
                     (1|coder_id) + (1|round_id) + (1|stratum),
                   data = df, weights = weight,
                   family = Gamma(link = "log"))
  print(summary(model1))
  
  model2 <- glmmTMB(sum_code_lines ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
                      treatment*old_rating_rank_norm + num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model2))
  
  print(anova(model1, model2))
  
  model3 <- glmmTMB(sum_code_lines ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) + 
                      treatment*old_rating_rank_norm + treatment*I(old_rating_rank_norm^2) + num_prev_ratings + 
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model3))
  
  # Chi-square difference between nested models
  print(anova(model2, model3))
  print(anova(model1, model3))
}


estimate_firstcomplexity <- function(df){
  model1 <- glmmTMB(first_complexity_est ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3)  +
                      num_prev_ratings +
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model1))
  
  model2 <- glmmTMB(first_complexity_est ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) +
                      treatment*old_rating_rank_norm + num_prev_ratings +
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model2))
  
  model3 <- glmmTMB(first_complexity_est ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) +
                      treatment*old_rating_rank_norm + treatment*I(old_rating_rank_norm^2) + num_prev_ratings +
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model3))
  
  # Chi-square difference between nested models
  print(anova(model1, model2))
  print(anova(model2, model3))
  print(anova(model1, model3))
}


estimate_lastcomplexity <- function(df){
  model1 <- glmmTMB(last_complexity_est ~ old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) +
                      num_prev_ratings +
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model1))
  
  model2 <- glmmTMB(last_complexity_est ~ treatment + old_rating_rank_norm + I(old_rating_rank_norm^2) + I(old_rating_rank_norm^3) +
                      treatment*old_rating_rank_norm + treatment*I(old_rating_rank_norm^2) + num_prev_ratings +
                      (1|coder_id) + (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model2))
  
  # Chi-square difference between nested models
  print(anova(model1, model2))
}



### NEWCOMERS

estimate_dur_newcom <- function(df){
  model <- glmmTMB(length_submission_period ~ treatment +  
                     (1|round_id) + (1|stratum),
                   data = df, weights = weight, 
                   family = beta_family())
  summary(model)
}

estimate_firstsub_newcom <- function(df){
  model <- glmmTMB(first_submission_time ~ treatment +  
                     (1|round_id) + (1|stratum),
                   data = df, weights = weight, 
                   family = beta_family())
  summary(model)
}

estimate_lastsub_newcom <- function(df){
  model <- glmmTMB(last_submission_time ~ treatment +  
                     (1|round_id) + (1|stratum),
                   data = df, weights = weight, 
                   family = beta_family())
  summary(model)
}

estimate_firstsublines_newcom <- function(df){
  model <- lmer(first_submission_lines ~ treatment + 
                   (1|round_id) + (1|stratum),
                    data = df, weights = weight)
  print(summary(model))
}



estimate_sumcodelines_newcom <- function(df){
  model <- glmmTMB(sum_code_lines ~ treatment + 
                      (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                   family = Gamma(link = "log"))
  print(summary(model))
}

estimate_numsub_newcom <- function(df){
  model <- glmmTMB(num_submissions ~ treatment +  
                     (1|round_id) + (1|stratum),
                   data = df, weights = weight,
                   family = Gamma(link = "log"))
  summary(model)
}



estimate_firstcomplexity_newcom <- function(df){
  model <- glmmTMB(first_complexity_est ~ treatment +
                      (1|round_id) + (1|stratum),
                    data = df, weights = weight,
                    family = Gamma(link = "log"))
  print(summary(model))
}

estimate_lastcomplexity_newcom <- function(df){
  model <- glmmTMB(last_complexity_est ~ treatment +
                     (1|round_id) + (1|stratum),
                   data = df, weights = weight,
                   family = Gamma(link = "log"))
  print(summary(model))
}

estimate_returned_newcom <- function(df){
  model <- glmer(returned ~ treatment + 
                   (1|round_id) + (1|stratum),
                 data = df, weights = weight,
                 family = binomial, nAGQ = 0)
  # nAGQ = 0 -> Fit the model with the adaptive Gaussian quadrature
  # See https://stats.stackexchange.com/questions/412185/model-is-nearly-unidentifiable-very-large-eigenvalue
  summary(model)
}

estimate_newrating_newcom <- function(df){
  # Remove non-varying covariate quality_target
  model <- lmer(new_rating ~ treatment + 
                  (1|round_id) + (1|stratum),
                data = df, weights = weight)
  summary(model)
}

estimate_provrank_newcom <- function(df){
  model <- glmmTMB(prov_rank_norm ~ treatment + 
                     (1|round_id) + (1|stratum),
                   data = df, weights = weight,
                   family = beta_family())
  summary(model)
}


# Weighting scheme 
cem_weight <- function(df){
  m_t <- df %>%
    filter(treatment == 1) %>%
    count()
  m_c <- length(df$treatment) - m_t
  m_ts <- df %>%
    group_by(stratum, treatment) %>%
    count() %>%
    filter(treatment == 1) %>%
    pull(n)
  m_cs <- df %>%
    group_by(stratum, treatment) %>%
    count() %>%
    filter(treatment == 0) %>%
    pull(n)
  
  weight <- (m_c/m_t)$n*(m_ts/m_cs)
  strata <- unique(df$stratum)
  strata_weights <- as.data.frame(cbind(stratum = strata[order(strata)], weight))
  
  return(strata_weights)
}

### MATCHED INDIVIDUALS ###

# # FIRST SUBMISSIONS
# # Timing of first submission - Does not vary systematically with rating, so no need to model that
# estimate_firstsub( get_data_ind('individuals_matched_sub_stats.csv', 'first_submission_time'))
# # Number of lines for first submission - N.S.
# estimate_firstsublines( get_data_ind('individuals_matched_sub_stats.csv', 'first_submission_lines'))
# # Complexity of first submission - N.S.
# estimate_firstcomplexity( get_data_ind('individuals_matched_sub_stats.csv', 'first_complexity_est'))
# 
# 
# # # CODE COMPLEXITY
# # Complexity of last submission - STEEPER SLOPE FOR RELATIVE
# df_lastcomp <- get_data_ind('individuals_matched_sub_stats.csv', 'last_complexity_est')
# hist(df_lastcomp$last_complexity_est)
# estimate_lastcomplexity(df_lastcomp)

# # PREDICTABILITY - N.S.
# # predictability_prov
# estimate_predict(get_data_ind('individuals_matched_sub_stats.csv', 'predictability_prov'))
# # estimate_predictfin(get_data_ind('individuals_matched.csv', 'predictability_fin'))
# 
# EFFORT
# Total number of code lines - STEEPER SLOPE FOR RELATIVE
df_sumcodelines <- get_data_ind('individuals_matched_sub_stats.csv', 'sum_code_lines')
hist(df_sumcodelines$sum_code_lines)
estimate_sumcodelines(df_sumcodelines)
# 
# # Number of submissions
# estimate_numsub(get_data_ind('individuals_matched_sub_stats.csv', 'num_submissions'))
# 
# # How long they stay in challenge - N.S.
# estimate_dur( get_data_ind('individuals_matched_sub_stats.csv', 'length_submission_period'))


# ### NEWCOMERS ###
# # x <- get_data_newcom('newcomers_matched_sub_stats.csv', 'returned')
# # print(x %>%
# #   group_by(x$round_id) %>%
# #   summarize(Count=n()) %>%
# #   arrange(Count))
# # # No cases of Freq < 2 per round_id

# # TIMING - NEWCOMERS STAY FOR LESS TIME IN RELATIVE
# # When they enter challenge - N.S.
# estimate_firstsub_newcom( get_data_newcom('newcomers_matched_sub_stats.csv', 'first_submission_time'))
# # When they leave challenge - N.S.
# estimate_lastsub_newcom( get_data_newcom('newcomers_matched_sub_stats.csv', 'last_submission_time'))

# # SUBMISSION SIZE - N.S.
# # Number of lines for first submission - N.S.
# estimate_firstsublines_newcom( get_data_newcom('newcomers_matched_sub_stats.csv', 'first_submission_lines'))
# 
# # COMPLEXITY - N.S.
# estimate_lastcomplexity_newcom( get_data_newcom('newcomers_matched_sub_stats.csv', 'last_complexity_est'))

# # RETURN - N.S.
# # Likelihood to return to Topcoder
# estimate_returned_newcom(get_data_newcom('newcomers_matched_sub_stats.csv', 'returned'))
# 
# # EFFORT - N.S.
# estimate_sumcodelines_newcom( get_data_newcom('newcomers_matched_sub_stats.csv', 'sum_code_lines'))
# 
# estimate_numsub_newcom( get_data_newcom('newcomers_matched_sub_stats.csv', 'num_submissions'))
# 
# # How long they stay in challenge - NEWCOMERS STAY FOR LESS TIME IN RELATIVE
# estimate_dur_newcom( get_data_newcom('newcomers_matched_sub_stats.csv', 'length_submission_period'))

# # ADDITIONAL PREDICTABILITY
# # First rating - N.S.
# estimate_newrating_newcom(get_data_newcom('newcomers_matched.csv', 'new_rating'))
# # Scored rank - N.S.
# estimate_provrank_newcom(get_data_newcom('newcomers_matched.csv', 'prov_rank_norm'))

# # xtable(coef(summary(model_predict_in_notreat))$cond, digits=3)
