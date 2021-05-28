#!/usr/bin/env Rscript

require(dplyr)
require(data.table)
require(stringr)
require(RJSONIO)

data2covmat <- function(data, var_selection, tau, perc_samples){
  TKs <- unique(pull(data, ICU_Admission_TK)) 
  TKs_selection <- sample(TKs, round(perc_samples*length(TKs))) # time series selection
  
  data %>%
    filter(ICU_Admission_TK %in% TKs_selection) %>% # filter time series selection
    select(c(ICU_Admission_TK, var_selection)) %>%
    group_by(ICU_Admission_TK) %>%
    do(data.frame(., shift(.[var_selection], 1:tau, fill = -1, give.names = T))) %>%
    filter_all(all_vars(. != -1)) %>% # remove redundant rows that follow from the operation above
    ungroup()%>%
    select(-ICU_Admission_TK) -> lagged_ts
  
  cov_matrix <- lagged_ts %>% cov(.)
  N <- nrow(lagged_ts)
  results <- list(N, cov_matrix)
  
  return(results)} # build covariance matrix

export_covmat <- function(cov_matrix, output_dir, N){
  var_names <- gsub(",", "", toString(colnames(cov_matrix))) # paste() somehow doesn't work 
  cov_matrix[upper.tri(cov_matrix)] <- ""
  
  # Export cov.txt with appropriate leading lines
  writeLines(c(N, var_names), paste0(output_dir,'cov.txt')) # first line: N
  # Lower triangular covariance matrix without trailing white spaces
  write.table(str_trim(apply(cov_matrix, 1, paste, collapse=' ')), paste0(output_dir,'cov.txt'), 
              quote=FALSE, row.names=FALSE, col.names=FALSE, append=TRUE)} # export covariance matrix

export_knowledge <- function(cov_matrix, output_dir, tau){
  filename <- paste0(output_dir, 'knowledge.txt')
  var_names <- colnames(cov_matrix)
  
  # Export knowledge.txt with appropriate leading lines
  writeLines('addtemporal', filename) # first line

  lags <- rev(1:tau) # continue
  for (tier in (1:tau)){ # subsequent tiers
    tier_names <- var_names[grepl(as.character(lags[tier]), var_names)]
    tier_names <- gsub(",", "", toString(tier_names)) # paste() somehow doesn't work
    write(paste(as.character(tier), tier_names), filename, append = TRUE)}
 
  no_lag_vars <- var_names[grepl("^[A-Za-z]+$", var_names)] # variables without lag
  no_lag_vars <- gsub(",", "", toString(no_lag_vars)) # paste() somehow doesn't work
  write(paste(as.character(tier+1), no_lag_vars), filename, append=TRUE)} # export knowledge file
