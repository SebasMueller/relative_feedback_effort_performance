library(stargazer)
library(huxtable)
library(xtable)
library(lme4)
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

# GROUP LEVEL data with weights by stratum
get_data_ch <- function(fname, dep_var){
  
  # Load and prepare data
  df <- load_data_ch(fname)
  
  # Get rid of strata with missing values on dep_var
  df <- clean_missing_ch(df, dep_var)
  
  # Get weights
  weights <- cem_weight(df)
  df <- merge(df, weights, by = 'stratum')
  # All treated cases are weighted 1
  df[df$treatment == 1, 'weight'] <- 1
  # Check
  print(length(df[df$treatment == 0, 'weight']))
  print(sum(df[df$treatment == 0, 'weight']))
  
  return(df)
}

# GROUP LEVEL: Load and prepare data
load_data_ch <- function(fname){
  df <- read.csv(fname)
  df$treatment <- ifelse(df$scoring_type == 'Relative', 1, 0)
  df$tc_provisional_testing <- ifelse(df$provisional_testing == 'TC only', 1, 0)
  df$optimization <- ifelse(df$task_type == 'Optimization', 1, 0)
  df$quality_target <- ifelse(df$target == 'Quality', 1, 0)
  # Recode 1's otherwise beta regression will not work 
  # See https://www.researchgate.net/profile/Jay_Verkuilen/publication/7184584_A_better_lemon_squeezer_Maximum-likelihood_regression_with_beta-distributed_dependent_variables/links/09e4150e4f9e56a692000000/A-better-lemon-squeezer-Maximum-likelihood-regression-with-beta-distributed-dependent-variables.pdf
  # y" = (y'(N-1) + 0.5) / N, where N = 367 is the sample size
  #df$corr_old_prov_rank <- ifelse(df$corr_old_prov_rank == 1, 0.99, df$corr_old_prov_rank)
  #df$corr_old_fin_rank <- ifelse(df$corr_old_fin_rank == 1, 0.99, df$corr_old_fin_rank)
  df <- df %>%
    mutate(corr_old_prov_rank = (corr_old_prov_rank*366 + 0.5) / 367) %>%
    mutate(corr_old_fin_rank = (corr_old_fin_rank*366 + 0.5) / 367)
  return(df)
}

# GROUP LEVEL: Get rid of strata with missing values on dep_var
clean_missing_ch <- function(df, dep_var){
  df <- df %>%
    filter(!is.na(eval(as.name(dep_var))))
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

estimate_beta_compgini <- function(df){
  model <- betareg(gini_lastcomp ~ treatment + num_contestants + 
                 contestant_registrant_ratio + duration + average_experience + 
                 engagement_barrier + scoring_complexity + tc_provisional_testing +
                 optimization + quality_target,
               data = df, 
               weights = weight)
  return(model)
}

estimate_gamma_compcoeffvar <- function(df){
  model <- glm(coeff_var_lastcomp ~ treatment + num_contestants + 
                 contestant_registrant_ratio + duration + average_experience + 
                 engagement_barrier + scoring_complexity + tc_provisional_testing +
                 optimization + quality_target,
               data = df, 
               weights = weight, 
               family = Gamma(link = "log"))
  print(summary(model))
}

# GROUP LEVEL beta regression with weights by stratum, controling for matching variables that vary in matched data
estimate_beta_ineq <- function(df){
  model <- betareg(gini_prov ~ treatment + num_contestants + 
                     contestant_registrant_ratio + duration + average_experience + 
                     engagement_barrier + scoring_complexity + tc_provisional_testing +
                     optimization + quality_target, 
                   data = df, 
                   weights = weight)
  print(summary(model))
}

estimate_beta_ineqfin <- function(df){
  model <- betareg(gini_fin ~ treatment + num_contestants + 
                     contestant_registrant_ratio + duration + average_experience + 
                     engagement_barrier + scoring_complexity + tc_provisional_testing +
                     optimization + quality_target, 
                   data = df, 
                   weights = weight)
  print(summary(model))
}

estimate_glmmTMBbeta_ineq <- function(df){
  # Remove non-varying covariates tc_provisional_testing and quality_target
  model <- glmmTMB(gini_prov ~ treatment + num_contestants +
                     contestant_registrant_ratio + duration + average_experience +
                     engagement_barrier + scoring_complexity + tc_provisional_testing +
                     optimization + quality_target,
                   data = df,
                   weights = weight,
                   family = beta_family())
  print(summary(model))
}

estimate_beta_predict <- function(df){
  # Remove non-varying covariates tc_provisional_testing and quality_target
  model <- betareg(corr_old_prov_rank ~ treatment + num_contestants + 
                     contestant_registrant_ratio + duration + average_experience + 
                     engagement_barrier + scoring_complexity + optimization, 
                   data = df, 
                   weights = weight)
  return(model)
}

estimate_beta_predictfin <- function(df){
  # Remove non-varying covariates tc_provisional_testing and quality_target
  model <- betareg(corr_old_fin_rank ~ treatment + num_contestants + 
                     contestant_registrant_ratio + duration + average_experience + 
                     engagement_barrier + scoring_complexity + optimization, 
                   data = df, 
                   weights = weight)
  print(summary(model))
}

estimate_glmmTMBbeta_predict <- function(df){
  # Remove non-varying covariates tc_provisional_testing and quality_target
  model <- glmmTMB(corr_old_prov_rank ~ treatment + num_contestants +
                       contestant_registrant_ratio + duration + average_experience +
                       engagement_barrier + scoring_complexity + optimization,
                     data = df,
                     weights = weight,
                     family = beta_family())
  print(summary(model))
}

# GROUP LEVEL beta regression with weights by stratum, controling for matching variables that vary in matched data
estimate_beta_effort <- function(df){
  model <- betareg(gini_sum_code_lines ~ treatment + num_contestants + 
                 contestant_registrant_ratio + duration + average_experience + 
                 engagement_barrier + scoring_complexity + tc_provisional_testing +
                 optimization + quality_target, 
               data = df, 
               weights = weight)
  return(model)
}

estimate_beta_effort_2 <- function(df){
  model <- betareg(gini_num_submissions ~ treatment + num_contestants + 
                     contestant_registrant_ratio + duration + average_experience + 
                     engagement_barrier + scoring_complexity + tc_provisional_testing +
                     optimization + quality_target, 
                   data = df, 
                   weights = weight)
  return(model)
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

### CHALLENGE-LEVEL ANALYSES ###

#Define the covariates to be used in the challenge models
cov_labels <- c("Relative scoring", "Number of contestants",
                "Contestants/registrants ratio",
                "Duration", "Average contestant experience",
                "Engagement barrier", "Scoring complexity",
                "TC only provisional testing", "Optimization task",
                'Targets quality')

# INEQUALITY - coeff_var_prov, coeff_var_fin, gini_prov, gini_fin
model_ineq <- estimate_beta_compgini(get_data_ch('challenges_matched.csv', 'gini_lastcomp'))
summary(model_ineq)
#estimate_gamma_compcoeffvar(get_data_ch('challenges_matched.csv', 'coeff_var_lastcomp'))

#estimate_beta_ineq(get_data_ch('challenges_matched.csv', 'gini_prov'))
#estimate_glmmTMBbeta_ineq(get_data_ch('challenges_matched.csv', 'gini_prov')) # no differences
#estimate_beta_ineqfin(get_data_ch('challenges_matched.csv', 'gini_fin'))

# PREDICTABILITY - corr_old_prov_rank
model_predict <- estimate_beta_predict(get_data_ch('challenges_matched.csv', 'corr_old_prov_rank'))
summary(model_predict)
#estimate_glmmTMBbeta_predict(get_data_ch('challenges_matched.csv', 'corr_old_prov_rank'))

#estimate_beta_predictfin(get_data_ch('challenges_matched.csv', 'corr_old_fin_rank'))


# EFFORT
model_effort <- estimate_beta_effort(get_data_ch('challenges_matched.csv', 'gini_sum_code_lines'))
summary(model_effort)

model_effort_2 <- estimate_beta_effort_2(get_data_ch('challenges_matched.csv', 'gini_num_submissions'))
summary(model_effort_2)

# Export as LaTex table
stargazer(model_ineq, model_predict, model_effort,
          dep.var.labels = c("Gini for complexity",
                             "Spearman corr. bet. ratings and challenge ranks",
                             "Gini for code lines written"),
           covariate.labels = cov_labels,
           star.cutoffs = c(.05, .01, .001))
