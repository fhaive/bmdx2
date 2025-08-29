library(InteractiveComplexHeatmap)
library(ComplexHeatmap)

library(bmdx)
library(AOPfingerprintR)
library(FunMappOnePackage)

library(shinyalert)
library(shiny)
library(shinyjs)
library(shinyBS)

library(xlsx)
library(XLConnect)
library(doParallel)
library(visNetwork)

library(gprofiler2)
library(ggplotify)
library(dendextend)
library(igraph)
library(randomcoloR)
library(easyGgplot2)

library(rlang)
library(openxlsx)
library(scam)
library(GO.db)
library(graph)
library(visNetwork)

source("server_modules/upload_pheno_server.R")
source("server_modules/glp_and_log_functions.R")
source("server_modules/upload_expression_server.R")
source("server_modules/gene_filtering_server.R")
source("server_modules/bmd_server.R")
source("server_modules/bmd_compare_time_point_server.R")
source("server_modules/bmd_compare_experiments_server.R")
source("server_modules/funmappone_server.R")
source("server_modules/server_gene_pairs_comparison_function.R")
source("server_modules/ke_server.R")

message_parallel <- function(...){
  system(sprintf('echo "\n%s\n"', paste0(..., collapse = "")))
}













