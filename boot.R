#!/usr/bin/env Rscript

source(paste0(work_dir,'run_tetrad.R'))
source(paste0(work_dir,'data2covmat.R'))

boot <- function(data, var_selection, tau, perc_samples, no_runs, work_dir, algorithm){
  
  # Create results dir
  output_dir <- paste0(work_dir, algorithm, '_', format(Sys.time(),format='%Y%m%d_%H'),'/')
  if (dir.exists(output_dir) == FALSE){
    dir.create(output_dir)
    dir.create(paste0(output_dir,'boot/'))
    }
  
  if (no_runs == 1 && dir.exists(paste0(output_dir,'single/')) == FALSE){
    dir.create(paste0(output_dir,'single/'))
  }
      
  # Run bootstrap
  for (i in 1:no_runs){
    covmat_results <- data2covmat(data, var_selection, tau, perc_samples)
    N <- covmat_results[[1]]
    cov_matrix <- covmat_results[[2]]
    
    print(paste0(i,'/',no_runs))
    if (no_runs == 1){
      results_path <- run_tetrad(work_dir, cov_matrix, N, tau, algorithm, output_dir, single=TRUE)
    }
    else{
      results_path <- run_tetrad(work_dir, cov_matrix, N, tau, algorithm, output_dir, single=FALSE)
    }
  }
  
  # Read results
  edge_list <- read_jsons(results_path, output_dir, cov_matrix) # Note: pag2conf and pag2edge are included
  
  # Initialize nested list of edges and their orientations, style and color
  edge_count_list <- list()
  for (i in edge_list){
    for (j in 1:nrow(i)){
      list_row <- i[j,]
      edge <- paste(list_row['node1.name'],list_row['node2.name'],sep=",")
      edge_orientation <- paste(list_row['arrowtail'],list_row['arrowhead'],sep=",")
      edge_color <- list_row[['color']]
      edge_style <- list_row[['style']]
      edge_count_list[[edge]][[edge_orientation]][[edge_color]][[edge_style]] <- 0
    }
  }
  
  # Fill nested list with edge counts
  for (i in edge_list){
    for (j in 1:nrow(i)){
      list_row <- i[j,]
      edge <- paste(list_row['node1.name'],list_row['node2.name'],sep=",")
      edge_orientation <- paste(list_row['arrowtail'],list_row['arrowhead'],sep=",")
      edge_color <- list_row[['color']]
      edge_style <- list_row[['style']]
      edge_count_list[[edge]][[edge_orientation]][[edge_color]][[edge_style]] <- 
        edge_count_list[[edge]][[edge_orientation]][[edge_color]][[edge_style]] + 1
    }
  }
  
  # Flatten nested list and write to .txt-file to use in Python script
  write(as.character(no_runs), file = paste0(output_dir,"/", "no_runs.txt"))
  write.table(unlist(edge_count_list), paste0(output_dir,"/", 'edge_counts.txt'), col.names = FALSE)
  
  boot_results <- list(edge_count_list, output_dir)
  
  
  return(boot_results)
}
