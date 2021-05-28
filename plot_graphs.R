#!/usr/bin/env Rscript

require(reticulate)
source_python(paste0(work_dir,'plot_dynamic_graph.py'))
source_python(paste0(work_dir,'plot_flat_graph.py'))