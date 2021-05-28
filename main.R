#!/usr/bin/env Rscript

require(reticulate)

#source(paste0(work_dir,'data_wrangling.R'))
work_dir <- '/Users/emielpost/Dropbox/BS_ML/MODS/'

source(paste0(work_dir,'data2covmat.R'))
source(paste0(work_dir,'run_tetrad.R'))
source(paste0(work_dir,'boot.R'))
source_python(paste0(work_dir,'plot_graphs.py'))

## Load data ##
data_directory <- '/Users/emielpost/Dropbox/BS_ML/Scripts/Data/'
data <- read.csv(paste0(data_directory,'data_sepsis_wo_CNS_d0.csv'))

## Select variables and set tau (leg) ##
#var_selection <- c('Circ','Coag','Liver','Renal','Resp','GIS')
#var_selection <- c('Circ','Coag','Liver','Renal','Resp','GIS', 'MV', 
#                   'RRT', 'Ent', 'furosemide', 'Crystalloids', 'nor')

var_selection <- c('Circ', 'Coag', 'Liver', 'Renal', 'Resp', 'GIS', 'MV', 'RRT',
                'Albumin', 'Blood', 'Colloids', 'Crystalloids', 'Ent', 'Parent',
                'dobutamine', 'erytromycine', 'furosemide', 'hydrocortison',
                'midazolam', 'nor', 'propofol', 'opioids', 'VP')

## Single run with all observations ##
perc_samples <- 1.0
no_runs <- 1
algorithm <- 'ts-gfci'
tau <- 1
threshold <- 0.0 # (single run, set threshold to 0.0)

results <- boot(data, var_selection, tau, perc_samples, no_runs, work_dir, algorithm)
output_dir <- results[[2]]
plot_dynamic_graph(output_dir, threshold) # plot
plot_flat_graph(output_dir, threshold) # plot flattened graph

## Bootstrapping ##
perc_samples <- 0.9
no_runs <- 250
threshold <- 0.5

boot_results <- boot(data, var_selection, tau, perc_samples, no_runs, work_dir, algorithm)
output_dir <- boot_results[[2]]
plot_dynamic_graph(output_dir, threshold) # plot
plot_flat_graph(output_dir, threshold) # plot flattened graph

# Fit SEM? Run fit_SEM.R, add graph labels with plot_SEM.py






