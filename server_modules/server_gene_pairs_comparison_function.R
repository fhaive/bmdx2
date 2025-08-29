plot_gene_profiles_in_pathways  =  function(gVars, input){
  selected_pathway  =  input$pathways_gene_comparison
  
  if (selected_pathway   ==   "None") return(make_empty_plot())
  
  experiment  =  input$Experiment_gene_comparison_filter_value
  time  =  input$Time_gene_comparison_filter_value
  exp_combination  =  paste(experiment,time,sep  =  "_")
  
  fitted_models  =  gVars$BMDMQ_latest #always to be used
  fitted_models  =  fitted_models[grep(pattern  =  exp_combination, x  =  names(fitted_models))]
  if (length(fitted_models)  ==  0) return(make_empty_plot())
  
  genes  =  unlist(strsplit(gVars$pathways_gene_pairs[selected_pathway,"gID"],split  =  ","))
  
  df  =  c()
  
  for (gi in genes) {
    mod  =  fitted_models[[grep(pattern  =  gi,x  =  names(fitted_models))]]
    mod  =  find_best_model_aic(model_list  =  mod)
    yhat  =  predict(mod$fitted, newdata  =  gVars$newdata)
    df  =  rbind(df, cbind(dose  =  gVars$newdata$dose, expression  =  yhat, feature  =  gi))
    
  }
  
  df  =  as.data.frame(df)
  df$dose  =  as.numeric(as.vector(df$dose))
  df$expression  =  as.numeric(as.vector(df$expression))
  
  p  =  ggplot(df, aes(x = dose, y = expression, group = feature, color = feature)) +
    geom_line(linewidth  =  1) +
    viridis::scale_color_viridis(discrete  =  TRUE) +
    ggtitle(selected_pathway) +   theme(legend.position = "none")

  return(p)
}

plot_gene_profiles_in_ke_shiny = function(gVars, input){
  
  selected_ke = input$KE_gene_comparison
  experiment = input$Experiment_gene_comparison_filter_value
  time = input$Time_gene_comparison_filter_value
  fitted_models = gVars$BMDMQ_latest
  ke_enrichment_results = gVars$KE_gene_pairs
  newdata = gVars$newdata
  
  p = plot_gene_profiles_in_ke(selected_ke,
                          experiment,
                          time,
                          fitted_models,
                          ke_enrichment_results,
                          newdata)
  
  return(p)
}
plot_gene_profiles_in_ke  =  function(selected_ke= "Event:1028",
                                      experiment,
                                      time,
                                      fitted_models,
                                      ke_enrichment_results,
                                      newdata){
  
  if (selected_ke   ==   "None") return(make_empty_plot())
  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  exp_combination  =  paste(experiment,time,sep  =  "_")
  
  fitted_models  =  fitted_models[grep(pattern  =  exp_combination, x  =  names(fitted_models))]
  if (length(fitted_models)  ==  0) return(make_empty_plot())
  
  KE = ke_enrichment_results[ke_enrichment_results$Experiment %in% experiment & ke_enrichment_results[,time_var] %in% time,]
  KE = unique(KE[,c("TermID","Ke_description","Genes")])
  rownames(KE) = KE$Ke_description
  genes  =  unlist(strsplit(KE[selected_ke,"Genes"],split  =  ";"))
  
  df  =  c()
  
  for (gi in genes) {
    mod  =  fitted_models[[grep(pattern  =  gi,x  =  names(fitted_models))]]
    mod  =  find_best_model_aic(model_list  =  mod)
    yhat  =  predict(mod$fitted, newdata  =  newdata)
    df  =  rbind(df, cbind(dose  =  newdata$dose, expression  =  yhat, feature  =  gi))
    
  }
  
  df  =  as.data.frame(df)
  df$dose  =  as.numeric(as.vector(df$dose))
  df$expression  =  as.numeric(as.vector(df$expression))
  
  p  =  ggplot(df, aes(x = dose, y = expression, group = feature, color = feature)) +
    geom_line(linewidth  =  1) +
    viridis::scale_color_viridis(discrete  =  TRUE) +
    ggtitle(selected_ke) +   theme(legend.position = "none")
  
  return(p)
}

#' Cluster Gene Expression Profiles Within a Pathway
#'
#' This function clusters genes in a selected enriched pathway based on the similarity
#' of their modeled expression profiles across dose levels or time points. It also supports
#' optional protein-protein interaction (PPI) visualization and clustering.
#'
#' @param selected_pathway A string specifying the name of the pathway to analyze.
#'        If set to `"None"`, an empty plot is returned.
#' @param experiment A string identifying the experiment label (used in model and enrichment naming).
#' @param time A string or numeric value identifying the time point of interest.
#' @param fitted_models A named list of fitted models (e.g., from dose-response fitting),
#'        with names structured by experiment and gene ID.
#' @param enrichedList A list that contains pathway enrichment results in the element
#'        `enriched_data_list`, named by experiment-time combinations.
#' @param newdata A data frame representing predictor values (e.g., dose levels) for which
#'        predictions should be made.
#' @param plot_ppi_info Logical; if `TRUE`, will include protein-protein interaction (PPI) network
#'        information in the clustering visualization.
#' @param gene_id_type A string indicating the gene identifier type (e.g., `"ENSEMBL"`, `"SYMBOL"`).
#' @param organism A string representing the organism, e.g., `"human"`, `"mouse"`.
#'
#' @return A `ComplexHeatmap` object (stored in `res_gene_clusters$complex_heatmap`) showing
#' clustered gene expression profiles. If the pathway is `"None"` or no models are found,
#' returns an empty plot.
#' @export
cluster_gene_profiles_in_KE  =  function(selected_ke= "Increase activation, Nuclear factor kappa B (NF-kB)",
                                         experiment,
                                         time,
                                         fitted_models,
                                         ke_enrichment_results,
                                         newdata,
                                         method = "combination",
                                         nclust = 1,
                                         plot_ppi_info = TRUE,
                                         gene_id_type = "ENSEMBL",
                                         organism = "human"){
  library(igraph)
  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  if (selected_ke   ==   "None") return(make_empty_plot())
  
  exp_combination  =  paste(experiment,time,sep  =  "_")
  
  fitted_models  =  fitted_models[grep(pattern  =  exp_combination, x  =  names(fitted_models))]
  if (length(fitted_models)  ==  0) return(make_empty_plot())
  
  KE = ke_enrichment_results[ke_enrichment_results$Experiment %in% experiment & ke_enrichment_results[,time_var] %in% time,]
  KE = unique(KE[,c("TermID","Ke_description","Genes")])
  rownames(KE) = KE$Ke_description
  genes  =  unlist(strsplit(KE[selected_ke,"Genes"],split  =  ";"))
  
  filtered_optimal_models = list()
  
  for (gi in genes) {
    mod  =  fitted_models[[grep(pattern  =  gi,x  =  names(fitted_models))]]
    mod  =  find_best_model_aic(model_list  =  mod)
    filtered_optimal_models[[paste(exp_combination,gi,sep = "_")]] = mod
    
  }
  
  pathways_statistics = gene_pairs_comparison(filtered_optimal_models = filtered_optimal_models,
                                              other_variables_id_col = "timepoint",
                                              newdata  =  newdata,nCores = 1)
  
  genes_to_be_converted =  union(pathways_statistics$`Feature 1`,pathways_statistics$`Feature 2` )
  
  genes_human = gVars$genes_human
  genes_mouse = gVars$genes_mouse
  genes_rat = gVars$genes_rat
  
  genes_converted = AOPfingerprintR::convert_genes_to_symbol(genes = genes_to_be_converted,
                                                             gene_id_type =gene_id_type,
                                                             organism = organism,
                                                             genes_human=genes_human, 
                                                             genes_mouse=genes_mouse, 
                                                             genes_rat=genes_rat)
  
  genes_converted_uniques = unlist(lapply(genes_converted,FUN = function(elem){
    unlist(strsplit(x = elem,split = ";"))[1]
  }))
  
  df = data.frame(genes_to_be_converted=genes_to_be_converted,
                  genes_converted=genes_converted,
                  genes_converted_uniques=genes_converted_uniques)
  rownames(df)=genes_to_be_converted
  
  pathways_statistics$`Feature 1`= df[pathways_statistics$`Feature 1`,"genes_converted_uniques"]
  pathways_statistics$`Feature 2`= df[pathways_statistics$`Feature 2`,"genes_converted_uniques"]
  
  res_gene_clusters = cluster_genes_pairs(comparison_pairs = pathways_statistics,
                                          nclust,method,
                                          plot_ppi_info = plot_ppi_info,
                                          gene_id_type = "SYMBOL",
                                          organism = organism)
  
  # res_gene_clusters = cluster_genes_pairs(comparison_pairs = pathways_statistics,
  #                                         nclust,
  #                                         method,
  #                                         plot_ppi_info = plot_ppi_info,
  #                                         gene_id_type = gene_id_type,
  #                                         organism = organism)
  res_gene_clusters$complex_heatmap
  
}


cluster_gene_profiles_in_pathways  =  function(selected_pathway = "Cytokine-cytokine receptor interaction",
                                               experiment,
                                               time,
                                               fitted_models,
                                               enrichedList,
                                               newdata,
                                               method = "combination",
                                               nclust = 1,
                                               plot_ppi_info = TRUE,
                                               gene_id_type = "ENSEMBL",
                                               organism = "human"){
  library(igraph)
  if (selected_pathway   ==   "None") return(make_empty_plot())
  
  exp_combination  =  paste(experiment,time,sep  =  "_")
  
  fitted_models  =  fitted_models[grep(pattern  =  exp_combination, x  =  names(fitted_models))]
  if (length(fitted_models)  ==  0) return(make_empty_plot())
  
  exp_combination  =  paste(experiment,time,sep  =  "_")
  
  PATWAY_tab <- enrichedList[[exp_combination]]
  rownames(PATWAY_tab)  =  PATWAY_tab$Description
  genes  =  unlist(strsplit(PATWAY_tab[selected_pathway,"gID"],split  =  ","))
  
  exp_combination  =  paste(experiment,time,sep  =  "_")
  
  filtered_optimal_models = list()
  
  for (gi in genes) {
    mod  =  fitted_models[[grep(pattern  =  gi,x  =  names(fitted_models))]]
    mod  =  find_best_model_aic(model_list  =  mod)
    filtered_optimal_models[[paste(exp_combination,gi,sep = "_")]] = mod
    
  }
  
  pathways_statistics = gene_pairs_comparison(filtered_optimal_models = filtered_optimal_models,
                                              other_variables_id_col = "timepoint",
                                              newdata  =  newdata,nCores = 1)
  
  genes_to_be_converted =  union(pathways_statistics$`Feature 1`,pathways_statistics$`Feature 2` )
  
  genes_human = gVars$genes_human
  genes_mouse = gVars$genes_mouse
  genes_rat = gVars$genes_rat
  
  genes_converted = AOPfingerprintR::convert_genes_to_symbol(genes = genes_to_be_converted,
                                                             gene_id_type =gene_id_type,
                                                             organism = organism,
                                                             genes_human=genes_human, 
                                                             genes_mouse=genes_mouse, 
                                                             genes_rat=genes_rat)
  
  genes_converted_uniques = unlist(lapply(genes_converted,FUN = function(elem){
    unlist(strsplit(x = elem,split = ";"))[1]
  }))
  
  df = data.frame(genes_to_be_converted=genes_to_be_converted,
                  genes_converted=genes_converted,
                  genes_converted_uniques=genes_converted_uniques)
  rownames(df)=genes_to_be_converted
  
  pathways_statistics$`Feature 1`= df[pathways_statistics$`Feature 1`,"genes_converted_uniques"]
  pathways_statistics$`Feature 2`= df[pathways_statistics$`Feature 2`,"genes_converted_uniques"]
  
  res_gene_clusters = cluster_genes_pairs(comparison_pairs = pathways_statistics,
                                          nclust,method,
                                          plot_ppi_info = plot_ppi_info,
                                          gene_id_type = "SYMBOL",
                                          organism = organism)
  
  # res_gene_clusters = cluster_genes_pairs(comparison_pairs = pathways_statistics,
  #                                         nclust,method,
  #                                         plot_ppi_info = plot_ppi_info,
  #                                         gene_id_type = gene_id_type,
  #                                         organism = organism)
  res_gene_clusters$complex_heatmap
  
}



# cluster_gene_profiles_in_pathways  =  function(gVars, input){
#   selected_pathway  =  input$pathways_gene_comparison
#   experiment  =  input$Experiment_gene_comparison_filter_value
#   time  =  input$Time_gene_comparison_filter_value
#   fitted_models  =  gVars$BMDMQ_latest #always to be used
#   newdata  =  gVars$newdata
#   
#   if (selected_pathway   ==   "None") return(make_empty_plot())
#   
# 
#   exp_combination  =  paste(experiment,time,sep  =  "_")
#   
#   fitted_models  =  fitted_models[grep(pattern  =  exp_combination, x  =  names(fitted_models))]
#   if (length(fitted_models)  ==  0) return(make_empty_plot())
#   
#   genes  =  unlist(strsplit(gVars$pathways_gene_pairs[selected_pathway,"gID"],split  =  ","))
#   
#   df  =  c()
#   filtered_optimal_models = list()
#   
#   for (gi in genes) {
#     mod  =  fitted_models[[grep(pattern  =  gi,x  =  names(fitted_models))]]
#     mod  =  find_best_model_aic(model_list  =  mod)
#     filtered_optimal_models[[paste(exp_combination,gi,sep = "_")]] = mod
#     yhat  =  predict(mod$fitted, )
#     df  =  rbind(df, cbind(dose  =  gVars$newdata$dose, expression  =  yhat, feature  =  gi))
#     
#   }
#   
#   pathways_statistics = gene_pairs_comparison(filtered_optimal_models = filtered_optimal_models,
#                         other_variables_id_col = "timepoint",
#                         newdata  =  gVars$newdata,nCores = 1)
#   
#   method = "combination"
#   n_clust = 1
#   
#   res_gene_clusters = cluster_genes_pairs(pathways_statistics,n_clust,method)
#   res_gene_clusters$complex_heatmap
#   
#   annotate_edges(graph = ens_human_gene_graph,edge_df = pathways_statistics[,c(3,7)],from_col = "Feature 1",to_col = "Feature 2")
#   
#   data(ens_human_gene_graph)
#   res = make_d3_network(statistics = pathways_statistics,g=ens_human_gene_graph, positive = TRUE,th = 0)
#   
#   organism = gVars$gene_comparison_organism
#   
#   load(paste("data/",organism,"_ppi_tfs_regulation_graph.RData",sep = ""))
#   
#   
#   df  =  as.data.frame(df)
#   df$dose  =  as.numeric(as.vector(df$dose))
#   df$expression  =  as.numeric(as.vector(df$expression))
#   
#   p  =  ggplot(df, aes(x = dose, y = expression, group = feature, color = feature)) +
#     geom_line(linewidth  =  1) +
#     viridis::scale_color_viridis(discrete  =  TRUE) +
#     ggtitle(selected_pathway) +   theme(legend.position = "none")
#   
#   return(p)
# }

buildPPI  =  function(gVars, input, logVars,g,organism,correction_method){
  # browser()
  statistics  =  gVars$gene_pair_comparison
  
  th  =  as.numeric(input$th_PPI)
  res  =  bmdx::make_d3_network(statistics,g, positive  =  TRUE,th  =  th)
  
  PPI_net_d3  =  res$p
  PPI_net_degree_betwenness_statisitcs  =  res$df
  g_info  =  res$g_mat
  
  gVars$PPI  =  PPI_net_d3
  gVars$PPI_stats  =  PPI_net_degree_betwenness_statisitcs
  
  df  =  res$df
  df  =  df[order(df$degree,decreasing  =  T),]
  df  =  df[df$degree > 0,]
  
  logVars$gene_pair_comparison_PPI  =  list("PPI"  =  gVars$PPI,"PPI_stats"  =  gVars$PPI_stats)
  
  
  if (nrow(df) > 0) {
    query  =  rownames(df)
    
    res_GSEA_over_genes_ranked_by_degree  =  gprofiler2::gost(query  =  list(query),
                                                organism  =  organism,
                                                ordered_query  =  TRUE, 
                                                correction_method  =  correction_method, 
                                                domain_scope  =  "annotated")
    
    gVars$res_gost_ppi  =  res_GSEA_over_genes_ranked_by_degree
    logVars$gene_pair_comparison_PPI[["res_gost_ppi"]]  =   gVars$res_gost_ppi
  }
  
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title  =  "GLP Mode Justification",
      text  =  "Please provide a justification for using the PPI network, and the selected parameters.",
      size  =  "s",
      closeOnEsc  =  TRUE,
      closeOnClickOutside  =  FALSE,
      html  =  FALSE,
      type  =  "input",
      inputType  =  "text",
      inputValue  =  "",
      inputPlaceholder  =  "",
      showConfirmButton  =  TRUE,
      showCancelButton  =  FALSE,
      confirmButtonText  =  "Submit",
      confirmButtonCol  =  "#AEDEF4",
      timer  =  0,
      imageUrl  =  "",
      animation  =  TRUE,
      callbackR  = function(x) {logVars$genepariscomparisonReasonsPPI  =  x}
      
    )
    
    #save for report
    ts  =  Sys.time()
    logVars$genePairComparisonPPITimestamp  =  ts
    logVars$genePairComparisonPPIorganism  =  organism
    logVars$genePairComparisonPPICorrelationTh  =  th
    
    if (nrow(df) > 0) {
      logVars$genePairComparisonPPIGostCorrectionMethod  =  correction_method
      logVars$genePairComparisonPPIGostQuery  =  paste(query, collapse  =  ",")
    }
    
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file gene pair comparison PPI obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "GENE PAIR COMPARISON PPI"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    
    fname  =  paste(logVars$location, "/plots/", toString(tsn), "_gene_comparisonsPPI.RData", sep = "")
    save(PPI_net_d3, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    
    fname  =  paste(logVars$location, "/tables/", toString(tsn), "_gene_comparisonsPPI.RData", sep = "")
    save(PPI_net_degree_betwenness_statisitcs,res_GSEA_over_genes_ranked_by_degree, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    
  }
  
  return(list(gVars, logVars))
  
  
}

#' Plot Enrichment Analysis as a Bubble Plot
#'
#' This function creates a bubble plot to visualize enrichment analysis results
#' by plotting -log10(p-value) against the enriched terms, with bubble size representing
#' intersection size and color indicating p-value significance.
#'
#' @param data A data frame containing enrichment analysis results with columns:
#'   `term_name`, `p_value`, `intersection_size`, and `source`.
#' @param source_filter A character vector specifying which sources to filter in the data.
#' @param top_n An integer indicating the number of top enriched terms to display (default: 10).
#' @param facet_column An optional string specifying a column to use for faceting the plot
#'   (e.g., grouping results by a specific variable).
#'
#' @return A ggplot2 object representing the enrichment analysis bubble plot.
#'
#' @details
#' The function filters the input `data` based on `source_filter`, selects the top `top_n`
#' enriched terms based on the smallest p-values, and creates a bubble plot where:
#' - The x-axis represents `-log10(p-value)`.
#' - The y-axis represents `term_name`, ordered by significance.
#' - The size of the bubbles represents `intersection_size`.
#' - The color of the bubbles corresponds to `p_value` (scaled from red to blue).
#'
#' If `facet_column` is provided, the function groups the data by that column and
#' selects the top `top_n` terms for each group.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   term_name = c("Term1", "Term2", "Term3", "Term4"),
#'   p_value = c(0.01, 0.05, 0.001, 0.02),
#'   intersection_size = c(10, 20, 15, 5),
#'   source = c("GO", "KEGG", "GO", "KEGG")
#' )
#' plot_enrichment_gost_bubble(data, source_filter = c("GO", "KEGG"), top_n = 5)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @export
plot_enrichment_gost_bubble <- function(data, source_filter, top_n = 10, facet_column = NULL) {
  
  # Load required libraries
  library(ggplot2)
  library(dplyr)
  library(forcats)
  
  idx = which(colnames(data)=="p_values")
  if(length(idx)>0){
    colnames(data)[idx] = "p_value"
  }
  
  idx = which(colnames(data)=="intersection_sizes")
  if(length(idx)>0){
    colnames(data)[idx] = "intersection_size"
  }
  
  # Filter data based on the specified sources
  data_filtered <- subset(data, source %in% source_filter)
  
  # Select the top_n most significant terms per facet (if specified)
  if (!is.null(facet_column) && facet_column %in% colnames(data_filtered)) {
    data_top <- data_filtered %>%
      group_by(.data[[facet_column]]) %>%  # Group by facet column
      arrange(p_value) %>%  # Order by ascending p-value
      slice_head(n = top_n) %>%  # Select the top_n per group
      ungroup()
  } else {
    # If no faceting, select only the top_n terms globally
    data_top <- data_filtered %>%
      arrange(p_value) %>%
      slice_head(n = top_n)
  }
  
  # Compute -log10(p-value) for better visualization
  data_top$log_p_value <- -log10(unlist(data_top$p_value))
  
  # Order terms by significance (most significant at the top)
  data_top$term_name <- factor(data_top$term_name, levels = unique(data_top$term_name[order(data_top$log_p_value, decreasing = TRUE)]))
  data_top$intersection_size = as.numeric(data_top$intersection_size)
  data_top$p_value = as.numeric(data_top$p_value)
  # Create the bubble plot
  p <- ggplot(data_top, aes(y = term_name, x = log_p_value, size = intersection_size, color = p_value)) +
    geom_point(alpha = 0.7) +  # Add points with transparency
    scale_color_gradient(low = "red", high = "blue") +  # Color gradient for p-values
    theme_minimal() +  # Use a clean theme
    labs(
      x = "-log10(P-Value)",
      y = "Term Name",
      size = "Intersection Size",
      color = "p_value",
      title = paste("Top", top_n, "Enriched Terms for Source:", paste(source_filter, collapse = ", "))
    ) +
    
    theme(
      axis.text.x = element_text(size = 12, hjust = 1),  # Increase x-axis label size
      axis.text.y = element_text(size = 12),  # Increase y-axis label size
      axis.title.x = element_text(size = 14, face = "bold"),  # Increase x-axis title size
      axis.title.y = element_text(size = 14, face = "bold"),  # Increase y-axis title size
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Increase and center title
      legend.title = element_text(size = 12, face = "bold"),  # Increase legend title size
      legend.text = element_text(size = 10),  # Increase legend label size
      # axis.text.x = element_text(hjust = 1),
      legend.position = "right"
    )
  
  # If faceting is requested, split the plot by the specified column
  if (!is.null(facet_column) && facet_column %in% colnames(data_top)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_column)), scales = "free_x", nrow = 1)
  }
  
  return(p)
}


cluster_gene_pairs_shiny  =  function(gVars, logVars, input){
  comparison_pairs  =  gVars$gene_pair_comparison
  
  if(input$convert_to_gene_symbol){
    genes_to_be_converted =  union(comparison_pairs$`Feature 1`,comparison_pairs$`Feature 2` )
    
    genes_human = gVars$genes_human
    genes_mouse = gVars$genes_mouse
    genes_rat = gVars$genes_rat
    
    gene_id_type = input$gene_comparison_gene_id_type
    organism  = input$gene_comparison_organism
    
    genes_converted = AOPfingerprintR::convert_genes_to_symbol(genes = genes_to_be_converted,
                                                               gene_id_type =gene_id_type,
                                                               organism = organism,
                                                               genes_human=genes_human, 
                                                               genes_mouse=genes_mouse, 
                                                               genes_rat=genes_rat)
    
    genes_converted_uniques = unlist(lapply(genes_converted,FUN = function(elem){
      unlist(strsplit(x = elem,split = ";"))[1]
    }))
    
    df = data.frame(genes_to_be_converted=genes_to_be_converted,
                    genes_converted=genes_converted,
                    genes_converted_uniques=genes_converted_uniques)
    rownames(df)=genes_to_be_converted
    
    comparison_pairs$`Feature 1`= df[comparison_pairs$`Feature 1`,"genes_converted_uniques"]
    comparison_pairs$`Feature 2`= df[comparison_pairs$`Feature 2`,"genes_converted_uniques"]
    
    to_rem = union(which(is.na(comparison_pairs$`Feature 1`)),which(is.na(comparison_pairs$`Feature 2`))) 
    if(length(to_rem)>0){
      comparison_pairs = comparison_pairs[-to_rem,] 
    }
    
    method = input$clustering_distance
    n_clust = as.numeric(input$nclust)
    
    res_gene_clusters = cluster_genes_pairs(comparison_pairs,n_clust,method,
                                            plot_ppi_info = input$ppi_data_complex_heatmap,
                                            gene_id_type = "SYMBOL",
                                            organism = input$cluster_organism)
  }else{
    
    method = input$clustering_distance
    n_clust = as.numeric(input$nclust)
    
    res_gene_clusters = cluster_genes_pairs(comparison_pairs,n_clust,method,
                                            plot_ppi_info = input$ppi_data_complex_heatmap,
                                            gene_id_type = input$cluster_gene_identifier,
                                            organism = input$cluster_organism)
  }
  
  
  # heatmap_analysis = res_gene_clusters$heatmap_analysis
  complex_heatmap = res_gene_clusters$complex_heatmap
  clusters = res_gene_clusters$clusters
  n_genes_par_cluster = res_gene_clusters$n_genes_par_cluster
  list_correlation_in_clusters = res_gene_clusters$list_correlation_in_clusters
  
  min_n_elem = input$n_min_element_in_cluster
  tab = table(clusters)
  to_rem = which(tab<min_n_elem)
  
  if(length(to_rem)>0){
    clusters = clusters[(clusters %in% to_rem)==FALSE]
  }
  
  query  =  list()
  for(i in min(clusters):max(clusters)){
    query[[paste("Cluster",i,sep = "_")]]  =  names(clusters)[clusters  ==  i] 
  }
  
  organism  =  gVars$gene_comparison_organism
  correction_method  =  input$gostclustcorrectiron
  
  if(organism == "human") organism = "hsapiens"
  if(organism == "mouse") organism = "mmusculus"
  if(organism == "rat") organism = "rnorvegicus"
  
  res_GSEA_over_corr_clust  =  gost(query  =  query,
                                  organism  =  organism,
                                  ordered_query  =  FALSE, 
                                  correction_method  =  correction_method, 
                                  domain_scope  =  "annotated")
  
  #TODO ADD PARAMETERS IN GUI
  # p <- plot_enrichment_gost_bubble(data = res_GSEA_over_corr_clust$result,
  #                                  source_filter = "GO:BP",
  #                                  top_n = 15,
  #                                  facet_column = "query")
  
  # gVars$heatmaply_input_mat  =  M2
  # gVars$heatmaply_cor  =  heatmap_analysis
  gVars$complex_heatmap  =  complex_heatmap
  gVars$clusters  =  clusters
  gVars$n_genes_par_cluster  =  n_genes_par_cluster
  gVars$list_correlation_in_clusters = list_correlation_in_clusters
  gVars$GSEA_over_corr_clust_query  =  query
  gVars$res_GSEA_over_corr_clust  =  res_GSEA_over_corr_clust
  gVars$res_GSEA_over_corr_clust_bubbleplot  =  p
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title  =  "GLP Mode Justification",
      text  =  "Please provide a justification the gene pairs clustering and the selected parameters.",
      size  =  "s",
      closeOnEsc  =  TRUE,
      closeOnClickOutside  =  FALSE,
      html  =  FALSE,
      type  =  "input",
      inputType  =  "text",
      inputValue  =  "",
      inputPlaceholder  =  "",
      showConfirmButton  =  TRUE,
      showCancelButton  =  FALSE,
      confirmButtonText  =  "Submit",
      confirmButtonCol  =  "#AEDEF4",
      timer  =  0,
      imageUrl  =  "",
      animation  =  TRUE,
      callbackR  = function(x) {logVars$genepariscomparisonclusteringReasons  =  x}
      
    )
    
    #save for report
    ts  =  Sys.time()
    logVars$genepariscomparisonclusteringTimestamp  =  ts
    logVars$genepariscomparisonclusteringNclust  =  as.numeric(input$nclust)
    logVars$clustering_similarity  =  input$clustering_distance
    logVars$genepariscomparisonclusteringOrganism  =  input$gene_comparison_organism
    logVars$genepariscomparisonclusteringCorrectionMethod  =  input$gostclustcorrection
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file gene pair comparison obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "GENE PAIR COMPARISON CLUSTERING AND ENRICHMENT"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/tables/", toString(tsn), "_gene_comparisons_clustering.RData", sep = "")
    save(complex_heatmap,query,res_GSEA_over_corr_clust, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    
  }
  
  return(list(gVars, logVars))
}

run_gene_pairs_comparison_all_experiments  =  function(gVars, logVars, input){
  
  if (is.null(gVars$BMDMQ_latest) || is.null(gVars$bmdlatestTable) ||
     is.null(input$n_new_data) || is.null(input$gene_comparison_NCores) || is.null(input$gene_comparison_organism) ||
     is.null( gVars$inputPh()) || is.null(input$Experiment_gene_comparison_filter_value) ||
     is.null(input$Time_gene_comparison_filter_value)) {
    
    return(NULL)
  }
  
  filtered_optimal_models  =  gVars$BMDMQ_latest #always to be used
  BMD_tab  =  gVars$bmdlatestTable #always to be used
  
  length_vectors  =  as.numeric(input$n_new_data)
  nCores  =  as.numeric(input$gene_comparison_NCores)
  
  phenoList  =  gVars$inputPh()
  phenoDt <- phenoList[[1]]
  doseColID  =  gVars$doseColID
  timeColID  =  gVars$TPColID
  
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  other_variables_id_col  =  c(other_variables_id_col,gVars$optVarsGroup)

  select_experiment  =  input$Experiment_gene_comparison_filter_value
  select_time  =  input$Time_gene_comparison_filter_value
  
  newdata  =  data.frame(dose  =  seq(min(phenoDt[,doseColID]), max(phenoDt[,doseColID]), length.out  =  length_vectors))
  
  experiment  =  unlist(lapply(strsplit(x  =  names(filtered_optimal_models),split  =  "_"),function(elem)elem[1]))
  times  =  unlist(lapply(strsplit(x  =  names(filtered_optimal_models),split  =  "_"),function(elem)elem[2]))
  
  combo  =  unique(expand.grid(select_experiment,select_time))
  list_gene_pairs_statistics  =  c()
  
  for (ii in 1:nrow(combo)) {
    idx  =  which(times  ==  combo[ii,2] & experiment  ==  combo[ii,1])
    
    if (length(idx) > 0) {
      models_tp3 =  filtered_optimal_models[idx]
      for (i in 1:length(models_tp3)) { 
        models_tp3[[i]]  =  find_best_model_aic(models_tp3[[i]])
      }
      statistics_gene_pairs  =  gene_pairs_comparison(filtered_optimal_models  =  models_tp3,
                                                    other_variables_id_col = other_variables_id_col,
                                                    newdata,
                                                    nCores  =  1)
      # list_gene_pairs_statistics[[paste(combo[ii,1],combo[ii,2],sep = "_")]]  =  statistics_gene_pairs
      list_gene_pairs_statistics  =  rbind(list_gene_pairs_statistics, statistics_gene_pairs)
    }
    
  }
  
  gVars$gene_pair_comparison  =  list_gene_pairs_statistics
  gVars$newdata  =  newdata
  gVars$gene_comparison_organism  =  input$gene_comparison_organism
  
  logVars$gene_pair_comparison  =  list_gene_pairs_statistics
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title  =  "GLP Mode Justification",
      text  =  "Please provide a justification for selecting Gene pairs comparison, and the selected parameters.",
      size  =  "s",
      closeOnEsc  =  TRUE,
      closeOnClickOutside  =  FALSE,
      html  =  FALSE,
      type  =  "input",
      inputType  =  "text",
      inputValue  =  "",
      inputPlaceholder  =  "",
      showConfirmButton  =  TRUE,
      showCancelButton  =  FALSE,
      confirmButtonText  =  "Submit",
      confirmButtonCol  =  "#AEDEF4",
      timer  =  0,
      imageUrl  =  "",
      animation  =  TRUE,
      callbackR  = function(x) {logVars$genepariscomparisonReasons  =  x}
      
    )
    
    #save for report
    ts  =  Sys.time()
    logVars$genePairComparisonTimestamp  =  ts
    logVars$genePairComparisonExperiment  =  input$Experiment_gene_comparison_filter_value
    logVars$genePairComparisonTimepoint  =  input$Time_gene_comparison_filter_value
    logVars$genePairComparisonCores  =  input$gene_comparison_NCores
    logVars$genePairComparisonNewDataLength  =  input$n_new_data
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file gene pair comparison obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "GENE PAIR COMPARISON"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/tables/", toString(tsn), "_gene_comparisons.RData", sep = "")
    save(statistics_gene_pairs,newdata, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    
  }
  
  return(list("gVars" = gVars, "logVars" = logVars))
  
}

# run_gene_pairs_comparison  =  function(gVars, logVars, input){
#   
#   
#   if (is.null(gVars$BMDMQ_latest) || is.null(gVars$bmdlatestTable) ||
#      is.null(input$n_new_data) || is.null(input$gene_comparison_NCores) ||
#      is.null( gVars$inputPh()) || is.null(input$Experiment_gene_comparison_filter_value) ||
#      is.null(input$Time_gene_comparison_filter_value)) {
#     
#     return(NULL)
#   }
#   
#   filtered_optimal_models  =  gVars$BMDMQ_latest #always to be used
#   BMD_tab  =  gVars$bmdlatestTable #always to be used
#   
#   length_vectors  =  as.numeric(input$n_new_data)
#   nCores  =  as.numeric(input$gene_comparison_NCores)
#   
#   phenoList  =  gVars$inputPh()
#   phenoDt <- phenoList[[1]]
#   doseColID  =  gVars$doseColID
#   
#   select_experiment  =  input$Experiment_gene_comparison_filter_value
#   select_time  =  input$Time_gene_comparison_filter_value
#   
#   newdata  =  data.frame(dose  =  seq(min(phenoDt[,doseColID]), max(phenoDt[,doseColID]), length.out  =  length_vectors))
# 
#   experiment  =  unlist(lapply(strsplit(x  =  names(filtered_optimal_models),split  =  "_"),function(elem)elem[1]))
#   times  =  unlist(lapply(strsplit(x  =  names(filtered_optimal_models),split  =  "_"),function(elem)elem[2]))
#  
#   idx  =  which(times  ==  select_time & experiment  ==  select_experiment)
#   
#   if (length(idx) > 0) {
#     models_tp3 =  filtered_optimal_models[idx]
#     for (i in 1:length(models_tp3)) {
#       models_tp3[[i]]  =  find_best_model_aic(models_tp3[[i]])
#     }
#     statistics_gene_pairs  =  gene_pairs_comparison(filtered_optimal_models  =  models_tp3, newdata,nCores  =  1)
#     
#   }
#   
#   # print(head(res))
#   gVars$gene_pair_comparison  =  statistics_gene_pairs
#   gVars$newdata  =  newdata
#   
#   logVars$gene_pair_comparison  =  statistics_gene_pairs
#   
#   if (gVars$in_glp_mode) {
# 
#     #GLP MODE
#     shinyalert(
#       title  =  "GLP Mode Justification",
#       text  =  "Please provide a justification for selecting Gene pairs comparison, and the selected parameters.",
#       size  =  "s",
#       closeOnEsc  =  TRUE,
#       closeOnClickOutside  =  FALSE,
#       html  =  FALSE,
#       type  =  "input",
#       inputType  =  "text",
#       inputValue  =  "",
#       inputPlaceholder  =  "",
#       showConfirmButton  =  TRUE,
#       showCancelButton  =  FALSE,
#       confirmButtonText  =  "Submit",
#       confirmButtonCol  =  "#AEDEF4",
#       timer  =  0,
#       imageUrl  =  "",
#       animation  =  TRUE,
#       callbackR  = function(x) {logVars$genepariscomparisonReasons  =  x}
# 
#     )
# 
#     #save for report
#     ts  =  Sys.time()
#     logVars$genePairComparisonTimestamp  =  ts
#     logVars$genePairComparisonExperiment  =  input$Experiment_gene_comparison_filter_value
#     logVars$genePairComparisonTimepoint  =  input$Time_gene_comparison_filter_value
#     logVars$genePairComparisonCores  =  input$gene_comparison_NCores
#     logVars$genePairComparisonNewDataLength  =  input$n_new_data
#     
#   }
# 
#   if (gVars$in_glp_mode){
#     print("writing log file gene pair comparison obj")
#     line = "-----------------------------------------------------"
#     write(line,file = logVars$file,append = TRUE)
# 
#     line = "GENE PAIR COMPARISON"
#     write(line,file = logVars$file,append = TRUE)
# 
#     ts  =  Sys.time()
#     tsn  =  as.numeric(ts)
#     fname  =  paste(logVars$location, "/tables/", toString(tsn), "_gene_comparisons.RData", sep = "")
#     save(statistics_gene_pairs,newdata, file  =  fname)
#     line = paste("The Current R Data Object is saved:", fname, sep = " ")
#     write(line,file = logVars$file,append = TRUE)
# 
#   }
# 
#   return(list(gVars, logVars))
# 
# }

gene_pairs_print_models  =  function(gVars, logVars, input){
  if (is.null(gVars$newdata) || 
      is.null(gVars$BMDMQ_latest) || 
      is.null(gVars$gene_pair_comparison) || 
      is.null(input$gene_pairs_table_rows_selected)) {
    p  =  make_empty_plot()
    return(p)
  }
  
  fitted_models  =  gVars$BMDMQ_latest #always to be used
  
  selectedrowindex  =  input$gene_pairs_table_rows_selected[length(input$gene_pairs_table_rows_selected)]
  selectedrowindex  =  as.numeric(selectedrowindex)
  
  # stat_table  =  gVars$gene_pair_comparison
  stat_table = gVars$edge_presence_ppi # this is the gVars$gene_pair_comparison with ppi info on top
  stringa  =  rownames(stat_table)[selectedrowindex]
  models  =  unlist(strsplit(stringa, split  =  "____"))
  
  p  =  bmdx::plot_gene_pairs(fitted_models[[models[1]]][[stat_table[selectedrowindex,"Model 1"]]],
                  fitted_models[[models[2]]][[stat_table[selectedrowindex,"Model 2"]]], 
                  gVars$newdata, 
                  main  =  "", 
                  feat1  =  stat_table[selectedrowindex,"Feature 1"],
                  feat2  =  stat_table[selectedrowindex,"Feature 2"]
                 )
  return(p)
}


render_compare_gene_pairs_filter_cols  =  function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)) { 
    return(NULL) 
  }
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options  =  (c("Experiment"))
  selectInput(id, "Filter By Column", choices = options, multiple = TRUE, selected = "Experiment", selectize = FALSE)
}

render_compare_gene_pairs_filter_vals  =  function(gVars, input){
  if (is.null(gVars$bmdlatestTable) || 
      is.null(input$gene_pairs_filter_experiment)) {
    return(NULL)
  }
  print("Rending UI: ")
  options  =  c()
  if (input$gene_pairs_filter_experiment !=  "None"){
    for (p in input$gene_pairs_filter_experiment) {
      print(p)
      print(gVars$bmdlatestTable[[p]])
      if (p   ==   "Model") {
        names  =  levels(gVars$bmdlatestTable[[p]])
      }else {
        names  =  unique(gVars$bmdlatestTable[[p]])
      }
      print(names)
      options  =  c(options, names)
      
    }
  }
  selectInput("Experiment_gene_comparison_filter_value", "Filter By Experiment", choices = options, multiple = TRUE, selectize = FALSE)
}

render_compare_gene_pairs_time_filter_cols  =  function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)){ 
    return(NULL) 
  }
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  #since setting the table names is hard coded we can do it here as well
  # options  =  (c("None","Experiment", other_variables_id_col, gVars$optVarsGroup, "Feature", "Model"))
  options  =  (c(other_variables_id_col))
  
  selectInput(id, "Filter By Column", choices = options, multiple = TRUE, selected = "Experiment", selectize = FALSE)
}

render_compare_gene_pairs_time_filter_vals  =  function(gVars, input){
  if (is.null(gVars$bmdlatestTable) || is.null(input$gene_pairs_filter_time)){
    return(NULL)
  }
  print("Rending UI: ")
  options  =  c()
  if (input$gene_pairs_filter_time !=  "None"){
    for (p in input$gene_pairs_filter_time) {
      print(p)
      print(gVars$bmdlatestTable[[p]])
      if (p   ==   "Model") {
        names  =  levels(gVars$bmdlatestTable[[p]])
      }else {
        names  =  unique(gVars$bmdlatestTable[[p]])
      }
      print(names)
      options  =  c(options, names)
      
    }
  }
  selectInput("Time_gene_comparison_filter_value", "Filter By Time Point", choices = options, multiple = TRUE, selectize = FALSE)
}

render_time_point_pathway_selection  =  function(gVars, input, id){
  if (is.null(gVars$EnrichDatList)){
    return(NULL)
  }
  selectInput(id, "Time Points", choices = names(gVars$EnrichDatList), selected  =  names(gVars$EnrichDatList)[1])#c("All",unique(gVars$phTable[,gVars$TPColID])))
}

render_pathway_genes_dose_dependent_patterns_comparison  =  function(gVars, input){
  experiment  =  input$Experiment_gene_comparison_filter_value
  time  =  input$Time_gene_comparison_filter_value
  
  exp_combination  =  paste(experiment,time,sep  =  "_")
  
  PATWAY_tab <- gVars$EnrichDatList[[exp_combination]]
  rownames(PATWAY_tab)  =  PATWAY_tab$Description
  gVars$pathways_gene_pairs  =  PATWAY_tab
  path_to_be_selected =  c("None",rownames(gVars$pathways_gene_pairs))
  names(path_to_be_selected)  =  path_to_be_selected
  
  selectInput("pathways_gene_comparison", "Enriched Pathways", 
              choices = path_to_be_selected,
              selected  =  "None")

}  


render_KE_genes_dose_dependent_patterns_comparison  =  function(gVars, input){
  experiment  =  input$Experiment_gene_comparison_filter_value
  time  =  input$Time_gene_comparison_filter_value
  
  ke_enrichment_results = gVars$KE_enrich_res
  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  
  KE = ke_enrichment_results[ke_enrichment_results$Experiment %in% experiment & ke_enrichment_results[,time_var] %in% time,]
  
  gVars$KE_gene_pairs  =  KE
  
  path_to_be_selected =  c("None",unique(KE$Ke_description))
  names(path_to_be_selected)  =  path_to_be_selected
  
  selectInput("KE_gene_comparison", "Enriched KEs", 
              choices = path_to_be_selected,
              selected  =  "None")
  
}  


