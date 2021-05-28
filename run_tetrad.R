#!/usr/bin/env Rscript

require(dplyr)
require(data.table)
require(stringr)
require(jsonlite)
require(pcalg)
require(tidyr)
require(tibble)

run_tetrad <- function(work_dir, cov_matrix, N, tau, algorithm, output_dir, single=FALSE){

  matrix_filename <- "cov.txt"
  knowledge_filename <- "knowledge.txt"
  verbose <- "false"
  stat_test <- "fisher-z-test"
  score <- "sem-bic"
  algorithm_location <- "/Users/emielpost/Dropbox/BS_ML/MODS/causal-cmd-1.1.3/"
  
  if(single==FALSE){
    results_path <- paste0(output_dir,'boot/')}
  else{
    results_path <- paste0(output_dir,'single/')
  }

  export_covmat(cov_matrix, output_dir, N)
  export_knowledge(cov_matrix, output_dir, tau)
  
  #### run tetrad ####
  if(algorithm == 'ts-fci'){
  args <- paste0(" --algorithm ", algorithm, " --test ", stat_test, " --data-type covariance", 
                 " --dataset ", output_dir, "/", matrix_filename, " --delimiter whitespace", 
                 " --verbose ", verbose, " --json-graph true", " --knowledge ", output_dir, "/",
                 knowledge_filename, " --out ", results_path)}
  
  else if(algorithm == 'ts-gfci'){
  args <- paste0(" --algorithm ", algorithm, " --test ", stat_test,  " --score ", score, " --data-type covariance", 
                 " --dataset ", output_dir, "/", matrix_filename, " --delimiter whitespace",
                 " --verbose ", verbose, " --json-graph true", " --knowledge ", output_dir, "/",
                 knowledge_filename, " --out ", results_path)}
  
  command <- "java -jar causal-cmd-1.1.3-jar-with-dependencies.jar"
  system(paste0("cd ", algorithm_location, "&&", command, args))
  
  return(results_path)} # run command line tetrad

read_jsons <- function(results_path, output_dir, cov_matrix){
  json_files <- list.files(results_path, '*.json')
  
  edge_list <- list()
  for (i in 1:length(json_files)){
    json_data <- fromJSON(paste0(results_path, json_files[[i]]), flatten = TRUE)
    edges <- json_data$edgesSet[c('node1.name', 'node2.name',
                                  'endpoint1.ordinal', 'endpoint2.ordinal')]
    P <- edges2P(edges, cov_matrix)
    conf_mat <- pcalg::pag2conf(P)
    dir_edge_mat <- pcalg::pag2edge(P)
    # IMPORTANT: in case of P[i,j] == 2 and P[j,i] == 3 (i --> j), edge[j,i] is always set to -1
    # Check with Joris/paper: in cyclic graph, whatever the status of i --> j, j <-- i can never be guaranteed to (also) be direct?
    
    edges_expanded <- expandmats(dir_edge_mat, conf_mat, edges)
    edges_expanded %>%
      mutate(arrowtail = recode(endpoint1.ordinal, 
                                `0` = 'none', `1` = 'normal', `2` = 'odot')) %>%
      mutate(arrowhead = recode(endpoint2.ordinal, 
                                `0` = 'none', `1` = 'normal', `2` = 'odot')) -> edge_list[[i]]}
  return(edge_list)} # read results, run pag2conf and pag2edge

edges2P <- function(edges, cov_matrix){
  edges %>% # from graphviz to pcalg encoding
    mutate(endpoint1.ordinal = 
             recode(endpoint1.ordinal, `0` = 3, `1` = 2, `2` = 1)) %>%
    mutate(endpoint2.ordinal = 
             recode(endpoint2.ordinal, `0` = 3, `1` = 2, `2` = 1)) -> edges_pcalg
  
  # Build P
  P <- matrix(rep(0,nrow(cov_matrix)*ncol(cov_matrix)), nrow(cov_matrix), ncol(cov_matrix))
  colnames(P) <- colnames(cov_matrix)
  rownames(P) <- rownames(cov_matrix)
  
  # IMPORTANT: in adjacency matrix of type amat.pag, edgemark-code refers to COLUMN index
  for (r in 1:nrow(edges_pcalg)){
    node1 <- edges_pcalg[r,'node1.name']
    node2 <- edges_pcalg[r,'node2.name']
    P[node2,node1] <- edges_pcalg[r, 'endpoint1.ordinal']
    P[node1,node2] <- edges_pcalg[r, 'endpoint2.ordinal']}
  
  return(P)
} # helper function for pag2conf and pag2edge

expandmats <- function(dir_edge_mat, conf_mat, edges){
  # reshape dir_edge_mat
  data.frame(dir_edge_mat) %>%
    rownames_to_column(var = "node1.name") %>%
    pivot_longer(!node1.name, names_to = 'node2.name', values_to = 'direct') -> dir_edge_long
  
  # reshape conf_long
  data.frame(conf_mat) %>%
    rownames_to_column(var = "node1.name") %>%
    pivot_longer(!node1.name, names_to = 'node2.name', values_to = 'conf') -> conf_long
  
  # join edges with dir_edge_mat and conf_long + recode
  edges %>%
    left_join(dir_edge_long) %>%
    left_join(conf_long) %>%
    mutate(arrowtail = recode(endpoint1.ordinal, `0` = 'none', `1` = 'normal', `2` = 'odot')) %>%
    mutate(arrowhead = recode(endpoint2.ordinal, `0` = 'none', `1` = 'normal', `2` = 'odot')) %>%
    mutate(color = recode(direct, `1` = 'black', `0` = 'dimgrey', `-1` = 'dimgrey')) %>%     # 1: definitely direct, 0: possibly direct, -1: possibly indirect
    mutate(style = recode(conf, `-1` = 'solid', `0` = 'dashed')) %>%
    arrange(desc(color))-> edges_expanded # -1: unconfounded. 0: possibly confounded
  
  return(edges_expanded)
} # helper function for read_jsons
