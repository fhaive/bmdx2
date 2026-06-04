render_experiment_selection_KEr_interface  =  function(gVars){
  if (is.null(gVars$filtered_bmd)) {
    return(NULL)
  }
  
  experiment_names  =  unique(unlist(lapply(strsplit(x  =  names(gVars$filtered_bmd), split  =  "_"),FUN  =  function(elem) elem[[1]])))
  # selectInput("experiment_KEr", "Experiment", choices  =  c("All",experiment_names), selected  =  "All")
  selectInput("experiment_KEr", "Experiment", choices  =  c(experiment_names), selected  =  experiment_names[1])
  
}

render_time_selection_KEr_interface  =  function(gVars, input){
  if (is.null(gVars$filtered_bmd) || is.null(input$experiment_KEr)) {
    return(NULL)
  }
  
  times  =  unique(unlist(lapply(strsplit(x  =  names(gVars$filtered_bmd), split  =  "_"),FUN  =  function(elem)elem[[2]])))
  # selectInput("time_point_KEr", "Time Points", choices  =  c("All",times), selected  =  "All")
  selectInput("time_point_KEr", "Time Points", choices  =  c(times), selected  =  times[1])
  
}


ke_render_compare_tp_filter_cols  =  function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable) || 
     is.null(gVars$TPColID) || 
     is.null(gVars$inputPh())) { 
    return(NULL) 
  }
  
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options  =  (c("None","Experiment", other_variables_id_col, gVars$optVarsGroup, "Feature", "Aop"))
  selectInput(id, "Filter By Column", choices  =  options, multiple  =  TRUE, selected  =  "Experiment", selectize  =  FALSE)
  
}

ke_render_compare_exp_group_by  =  function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)) { 
    return(NULL) 
  }
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options  =  (c("None","Experiment", "Aop",other_variables_id_col, gVars$optVarsGroup))
  selectInput(id, "Group By", choices = options, selected = other_variables_id_col)
}

ke_render_compare_exp_othervars  =  function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)) { 
    return(NULL) 
  }
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options  =  (c("None","Experiment","Aop", other_variables_id_col, gVars$optVarsGroup))

  selectInput(id, "Other Variables", choices = options, multiple = TRUE, selected = "Experiment", selectize = FALSE)
  
}


AOP_fingerprint_render_compare_compare_experiments_filterby  =  function(gVars, input){
  if (is.null(gVars$aop_fingerprint_results) ||
     is.null(input$range_AOP_fingerprint_filtercol_input)) {
    return(NULL)
  }
  options  =  c()
  if (input$range_AOP_fingerprint_filtercol_input !=  "None") {
    for (p in input$range_AOP_fingerprint_filtercol_input) {
      if (p  ==  "Model") {
        names  =  levels(gVars$aop_fingerprint_results$detailed_results_only_enriched[[p]])
      }else {
        names  =  unique(gVars$aop_fingerprint_results$detailed_results_only_enriched[[p]])
      }
      options  =  c(options, names)
    }
  }
  selectInput("range_AOP_fingerprint_filterby_input", "Filter By Column Values", choices = options, multiple = TRUE, selectize = FALSE)
}

AOP_fingerprint_bubble_plot_render_compare_exp_group_by  =  function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)) { 
    return(NULL) 
  }
  
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options  =  (c("None","Experiment", other_variables_id_col, gVars$optVarsGroup, "SSbD_category"))
  selectInput(id, "Group By", choices = options, selected = other_variables_id_col)
}

AOP_fingerprint_bubble_plot_render_compare_exp_x_axis  =  function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)) { 
    return(NULL) 
  }
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options  =  (c("None","Experiment", other_variables_id_col, gVars$optVarsGroup))
  selectInput(id, "X Axis", choices = options, selected = options[2])
}

AOP_fingerprint_bubble_plot_render_compare_tp_filter_cols  =  function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable) || 
     is.null(gVars$TPColID) || 
     is.null(gVars$inputPh())) { 
    return(NULL) 
  }
  
  other_variables_id_col  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options  =  (c("None","Experiment", other_variables_id_col, gVars$optVarsGroup, "AOP Name" = "a.name", "AOP ID" = "TermID", "SSbD category"= "SSbD_category"))
  selectInput(id, "Filter By Column", choices = options, multiple = TRUE, selected = "None", selectize = FALSE)
  
}

AOP_fingerprint_bubble_plot_render_compare_compare_experiments_filterby  =  function(gVars, input){
  if (is.null(gVars$aop_fingerprint_results) ||
     is.null(input$range_AOP_fingerprint_bubble_plot_filtercol_input)) {
    return(NULL)
  }
  options  =  c()
  if (input$range_AOP_fingerprint_bubble_plot_filtercol_input !=  "None") {
    for (p in input$range_AOP_fingerprint_bubble_plot_filtercol_input) {
      names  =  unique(gVars$aop_fingerprint_results$detailed_results_only_enriched[[p]])
      options  =  c(options, names)
    }
  }
  selectInput("range_AOP_fingerprint_bubble_plot_filterby_input", "Filter By Column Values", choices = options, multiple = TRUE, selectize = FALSE)
}


ke_render_compare_compare_experiments_filterby  =  function(gVars, input){
  if (is.null(gVars$KE_enrich_res) ||
     is.null(input$range_ke_filtercol_input)) {
    return(NULL)
  }
  options  =  c()
  if (input$range_ke_filtercol_input !=  "None") {
    for (p in input$range_ke_filtercol_input) {
      if (p  ==  "Model") {
        names  =  levels(gVars$KE_enrich_res[[p]])
      }else {
        names  =  unique(gVars$KE_enrich_res[[p]])
      }
      options  =  c(options, names)
    }
  }
  selectInput("range_ke_filterby_input", "Filter By Column Values", choices = options, multiple = TRUE, selectize = FALSE)
}

perform_ke_enrichment  =  function(gVars, logVars, input){
  if (is.null(gVars$bmdlatestTable) ||
     is.null(input$ke_sig_pval) ||
     is.null(input$ke_correction_method) ||
     is.null(input$ke_only_significant) ||
     is.null(input$min_aop_length_fingerprint) ||
     is.null(input$percentage_enriched_ke) ||
     is.null(input$aop_only_significant) ||
     is.null(input$aop_sig_pval) ||
     is.null(input$ke_organism) ||
     is.null(input$ke_gene_identifier) ||
     is.null(input$aop_correction_method) ||
     is.null(input$annotated_background)) {
    return(NULL)
  }
  

  if(input$annotated_background == FALSE){
    if(is.null(input$background)){
      shinyalert("Error with background data!", "The parameter 'annotate background' is false, but no custom background is provided", type = "error")
      return(NULL)
    }else{
      path_to_background = input$background$datapath
      background = readxl::read_excel(path = path_to_background,col_names = F)
      background = as.data.frame(background)
      background = as.character(background[,1])
      
      user_background = TRUE
    }
  }else{
    user_background = FALSE
  }
  
  
  ke_organism  =  input$ke_organism
  ke_gene_identifier  =  input$ke_gene_identifier
  
  
  if (ke_organism  ==  "Human") {
    if (ke_gene_identifier  ==  "ENSEMBL") {
      clustered_kes  =  human_ens_clusters
      aop_list_new  =  human_ens_aop
    }
    if (ke_gene_identifier  ==  "SYMBOL") {
      clustered_kes  =  human_symbol_clusters
      aop_list_new  =  human_symbol_aop
    }
    if (ke_gene_identifier  ==  "ENTREZID") {
      clustered_kes  =  human_entrez_clusters
      aop_list_new  =  human_entrez_aop
    }
  }
  
  if (ke_organism  ==  "Mouse") {
    if (ke_gene_identifier  ==  "ENSEMBL") {
      clustered_kes  =  mouse_ens_clusters
      aop_list_new  =  mouse_ens_aop
    }
    if (ke_gene_identifier  ==  "SYMBOL") {
      clustered_kes  =  mouse_symbol_clusters
      aop_list_new  =  mouse_symbol_aop
    }
    if (ke_gene_identifier  ==  "ENTREZID") {
      clustered_kes  =  mouse_entrez_clusters
      aop_list_new  =  mouse_entrez_aop
    }
  }
  
  if (ke_organism  ==  "Rat") {
    if (ke_gene_identifier  ==  "ENSEMBL") {
      clustered_kes  =  rat_ens_clusters
      aop_list_new  =  rat_ens_aop
    }
    if (ke_gene_identifier  ==  "SYMBOL") {
      clustered_kes  =  rat_symbol_clusters
      aop_list_new  =  rat_symbol_aop
    }
    if (ke_gene_identifier  ==  "ENTREZID") {
      clustered_kes  =  rat_entrez_clusters
      aop_list_new  =  rat_entrez_aop
    }
  }
  
  #mapped_genes  =  union(unlist(clustered_kes), unlist(aop_list_new)) # TO BE REMOVED
  
  only_significant  =  input$ke_only_significant
  pval_th  =  as.numeric(input$ke_sig_pval)
  adj.method  =  input$ke_correction_method
  aggregation_function = input$KE_aggregation_function
  
  experiment_var  =  "Experiment"
  time_var  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  #other_variables_id_col  =  c(time_var, gVars$optVarsGroup) 
  
  BMD_TAB  =  gVars$bmdlatestTable
  # original_experimental_data  =  gVars$original_experimental_data 
  
  
  experiments = unique(BMD_TAB[,experiment_var])  
  tp = unique(BMD_TAB[,time_var])  
  GList = list()
  for (expi in experiments) {
    for (tpi in tp) {
      idx = intersect(which(BMD_TAB[, experiment_var] == expi), which(BMD_TAB[,time_var] == tpi))
      GList[[paste(expi,tpi,sep = "_")]] = BMD_TAB[idx,]
    }
  }
  
  ### ENRICH KEs
  
  if(user_background){
    ke_enrichment_results = enrich_KEs_AOPs(GList = GList,
                                            list_gene_sets = clustered_kes,
                                            only_significant = only_significant,
                                            pval_th = pval_th,
                                            adj.method = adj.method,
                                            merge_by = "Ke",
                                            numerical_properties = c("BMDL","BMD","BMDU"),
                                            background = background,
                                            aggregation_function = aggregation_function )  
  }else{
    ke_enrichment_results = enrich_KEs_AOPs(GList = GList,
                                            list_gene_sets = clustered_kes,
                                            only_significant = only_significant,
                                            pval_th = pval_th,
                                            adj.method = adj.method,
                                            merge_by = "Ke",
                                            numerical_properties = c("BMDL","BMD","BMDU"),
                                            aggregation_function = aggregation_function )
  }

  
  only_significant_aop  =  input$aop_only_significant
  pval_th_aop  =  as.numeric(input$aop_sig_pval)
  adj.method_aop  =  input$aop_correction_method
  
  gene_sets = aop_list_new
  
  if(user_background){
    
    aop_enrichment_results = enrich_KEs_AOPs(GList = GList,
                                             list_gene_sets = gene_sets,
                                             only_significant = only_significant_aop,
                                             pval_th = pval_th_aop,
                                             adj.method = adj.method_aop,
                                             merge_by = "Aop",
                                             numerical_properties = c("BMDL","BMD","BMDU"),
                                             background = background,
                                             aggregation_function = aggregation_function )
  }else{
    
    aop_enrichment_results = enrich_KEs_AOPs(GList = GList,
                                             list_gene_sets = gene_sets,
                                             only_significant = only_significant_aop,
                                             pval_th = pval_th_aop,
                                             adj.method = adj.method_aop,
                                             merge_by = "Aop",
                                             numerical_properties = c("BMDL","BMD","BMDU"),
                                             aggregation_function = aggregation_function )
  }
  

  if(is.null(aop_enrichment_results) || is.null(ke_enrichment_results)){
    gVars$aop_fingerprint_results  =  NULL
    
  }else{
    res_aop_fingerprints = build_aop_for_aop_fingeprints(aop_enrichment_results,
                                                         ke_enrichment_results,
                                                         min_aop_length = as.numeric(input$min_aop_length_fingerprint),
                                                         percentage_enriched_ke = as.numeric(input$percentage_enriched_ke))

    DF = res_aop_fingerprints$detailed_results_only_enriched
    ER = DF$Experiment
    M = do.call(rbind, strsplit(ER, "_"))
    colnames(M) = c("Experiment",time_var)
    to_rem = which(colnames(DF) == "Experiment")
    if(length(to_rem)>0){
      DF = DF[,-to_rem]
    }
    DF = cbind(DF[,1], M, DF[,-1])
    colnames(DF)[1] = "TermID"
    
    res_aop_fingerprints$detailed_results_only_enriched = DF
    
    DF= res_aop_fingerprints$detailed_results_all_ke_in_aop
    ER = DF$Experiment
    M = do.call(rbind, strsplit(ER, "_"))
    colnames(M) = c("Experiment",time_var)
    to_rem = which(colnames(DF) == "Experiment")
    if(length(to_rem)>0){
      DF = DF[,-to_rem]
    }
    DF = cbind(DF[,1], M, DF[,-1])
    colnames(DF)[1] = "TermID"
    
    res_aop_fingerprints$detailed_results_all_ke_in_aop = DF
    
    if("SSbD_category.x" %in% colnames(res_aop_fingerprints$detailed_results_only_enriched)){
      idx = which(colnames(res_aop_fingerprints$detailed_results_only_enriched) == "SSbD_category.x")
      colnames(res_aop_fingerprints$detailed_results_only_enriched)[idx] = "SSbD_category"
    }
    
    gVars$aop_fingerprint_results  =  res_aop_fingerprints
    
  }
  
  if(is.null(ke_enrichment_results)){
    gVars$KE_enrich_res  =  NULL


  }else{
    ke_enrichment_results = merge(x = ke_enrichment_results, y = Biological_system_annotations, by.x = "TermID",by.y = "ke")
    M = do.call(rbind, strsplit(ke_enrichment_results$Experiment, "_"))
    colnames(M) = c("Experiment",time_var)
    to_rem = which(colnames(ke_enrichment_results) == "Experiment")
    if(length(to_rem)>0){
      ke_enrichment_results = ke_enrichment_results[,-to_rem]
    }
    ke_enrichment_results = cbind(ke_enrichment_results[,1], M, ke_enrichment_results[,-1])
    colnames(ke_enrichment_results)[1] = "TermID"
    gVars$KE_enrich_res  =  ke_enrichment_results
  }
  
  if(is.null(aop_enrichment_results)){
    gVars$aop_enrichment_results  =  NULL
  }else{
    M = do.call(rbind, strsplit(aop_enrichment_results$Experiment, "_"))
    colnames(M) = c("Experiment",time_var)
    to_rem = which(colnames(aop_enrichment_results) == "Experiment")
    if(length(to_rem)>0){
      aop_enrichment_results = aop_enrichment_results[,-to_rem]
    }
    aop_enrichment_results = cbind(aop_enrichment_results[,1], M, aop_enrichment_results[,-1])
    colnames(aop_enrichment_results)[1] = "TermID"

    aop_enrichment_results = merge(x = aop_enrichment_results, y = Annotate_AOPs, by.x = "TermID",by.y = "AOP")
    gVars$aop_enrichment_results = aop_enrichment_results
  }
  
  if (gVars$in_glp_mode) {
    #GLP MODE
    shinyalert(
      title  =  "GLP Mode Justification",
      text  =  "Please provide a justification for all your selected paramters",
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
      callbackR  =  function(x) {logVars$enreachAOPReasons  =  x }
      
    )
    
    #save for report
    ts  =  Sys.time()
    logVars$enrichAOPTimestamp  =  ts
    logVars$ke_sig_pval  =  input$ke_sig_pval
    logVars$ke_correction_method  =  input$ke_correction_method
    logVars$ke_only_significant  =  input$ke_only_significant    
    
    logVars$aop_sig_pval  =  input$aop_sig_pval
    logVars$aop_correction_method  =  input$aop_correction_method
    logVars$aop_only_significant  =  input$aop_only_significant    
    
    logVars$min_aop_length_fingerprint  =  input$min_aop_length_fingerprint
    logVars$percentage_enriched_ke  =  input$percentage_enriched_ke
    logVars$aggregation_function = input$KE_aggregation_function
    
    if(user_background){
      logVars$user_provided_background = TRUE
      logVars$user_provided_background_file = input$background$name
    }else{
      logVars$user_provided_background = FALSE
      logVars$user_provided_background_file = ""
    }
  }
 

  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line  =  "-----------------------------------------------------"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    line  =  "ENRICH AOP"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    line  =  "ENRICH AOP PARAMETERS"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    # fname  =  paste(logVars$location, "/tables/", toString(tsn), "_AOP.RData", sep  =  "")
    # save(BMD_TAB, ke_enrichment_results, aop_enrichment_results, res_aop_fingerprints, file  =  fname)
    # line  =  paste("The Current R Data Object is saved:", fname, sep  =  " ")
    # write(line,file  =  logVars$file,append  =  TRUE)
    # 
  }
  
  return(list("gVars"  =  gVars, "logVars"  =  logVars))

}

#' #' Plot BMD, BMDL, and BMDU for a Set of Genes
#' #'
#' #' This function generates a plot showing BMD, BMDL, and BMDU values for a given set of genes.
#' #'
#' #' @param BMDFilMat A data frame containing BMD values for multiple genes.
#' #' @param gi A vector of gene names for which to plot BMD values.
#' #'
#' #' @return A plot showing BMD, BMDL, and BMDU values for the selected set of genes.
#' #' @export
#' plot_bmd_bmdl_bmdu_set_of_genes = function(BMDFilMat, gi){
#'   idx = which(tolower(BMDFilMat[,"Feature"]) %in% tolower(gi))
#'   BMD = as.numeric(as.vector(BMDFilMat[idx,"BMD"]))
#'   BMDL = as.numeric(as.vector(BMDFilMat[idx,"BMDL"]))
#'   BMDU = as.numeric(as.vector(BMDFilMat[idx,"BMDU"]))
#'   
#'   names(BMD) = BMDFilMat[idx,"Feature"]
#'   names(BMDL) = names(BMDU) = names(BMD)
#'   
#'   
#'   BMD = data.frame(gene = names(BMD), bmd = BMD, bmdl = BMDL, bmdu = BMDU)
#'   to_rem = which(is.na(BMD$bmd) | is.na(BMD$bmdl) | is.na(BMD$bmdu))
#'   if (length(to_rem) > 0) BMD = BMD[-to_rem,]
#'   BMD = BMD[order(BMD$bmd),]
#'   
#'   BMD = BMD %>%  dplyr::group_by(gene) %>% dplyr::summarise( dplyr::across(everything(), list(mean)))
#'   colnames(BMD) = c("gene","bmd","bmdl","bmdu")
#'   BMD$gene = factor(x = BMD$gene, levels = BMD$gene)
#'   BMD$gene <- forcats::fct_reorder(BMD$gene, BMD$bmd, .desc = FALSE)
#'   BMD= as.data.frame(BMD)
#'   rownames(BMD)= BMD$gene
#'   BMD=BMD[BMD$gene,]
#'   
#'   if(nrow(BMD)<2){
#'     shinyalert("Plotting error","Only one gene contribute to the enrichment! The plot needs at least two genes.", type = "error")
#'     p = make_empty_plot()
#'     
#'   }else{
#'     p = ggplot2::ggplot(data = as.data.frame(BMD), ggplot2::aes(x = gene, y = bmd, group = 1, label1 = bmdl, label2 = bmdu)) +
#'       ggplot2::geom_line() +
#'       ggplot2::geom_point() +
#'       ggplot2::geom_ribbon(aes(ymin=bmdl, ymax = bmdu), linetype = 2, alpha = 0.1) +
#'       ggplot2::labs(y = "BMDL - BMD - BMDU", x = "Gene") +
#'       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
#'   }
#'   
#'   return(p)
#' }

plot_bmd_of_the_genes_in_a_ke  =  function(gVars,logVars,input, output,
                                           experiment_col  =  "Experiment",
                                           enrich_ppi_info = TRUE,
                                           gene_id_type = "ENSEMBL",
                                           organism = "human"){
  
  if ( is.null(gVars$KE_enrich_res) || 
       # is.null(input$KE_enrichment_stat_dataframe_rows_selected) ||  
       is.null(gVars$bmdlatestTable) ||
       is.null(gVars$inputPh()) ||
       is.null(gVars$TPColID)) {
    return(NULL)
  }
  
  selectedrowindex  =  input$KE_enrichment_stat_dataframe_rows_selected[length(input$KE_enrichment_stat_dataframe_rows_selected)]
  selectedrowindex  =  as.numeric(selectedrowindex)

  time_col   =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  exp_tp  =  as.vector(gVars$KE_enrich_res[selectedrowindex,c("Experiment", time_col)])
  
  BMDFilMat  =  gVars$bmdlatestTable
  good_idx  =  which(BMDFilMat[,experiment_col]  ==  exp_tp[[1]] & BMDFilMat[,time_col]  ==  exp_tp[[2]])
  BMDFilMat  =  BMDFilMat[good_idx,]

  # gi  =  unlist(strsplit(gVars$KE_enrich_res[selectedrowindex,"Genes"],split  =  ";"))
  
  convert_to_symbols = input$KEr_enrichment_table_convert_to_symbol
  if(convert_to_symbols){
    genes_to_be_converted = unique(BMDFilMat$Feature)
    
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
    
    BMDFilMat$Feature= df[BMDFilMat$Feature,"genes_converted_uniques"]
    
    to_rem = which(is.na(BMDFilMat$Feature))
    if(length(to_rem)>0){
      BMDFilMat = BMDFilMat[-to_rem,] 
    }
    
    gi  =  unlist(strsplit(gVars$KE_enrich_res[selectedrowindex[1],"Genes"],split  =  ";"))
    genes_to_be_converted = gi
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
    
    gi = df[gi,"genes_converted_uniques"]
    
    gene_id_type = "SYMBOL"
    
    
  }else{
    gi  =  unlist(strsplit(gVars$KE_enrich_res[selectedrowindex[1],"Genes"],split  =  ";"))
    
  }
  
  if (length(gi)  ==  0) {
    p  =  make_empty_plot()
  }else{
    p  =  plot_bmd_bmdl_bmdu_set_of_genes(BMDFilMat, gi,
                                          enrich_ppi_info = enrich_ppi_info,
                                          gene_id_type = gene_id_type,
                                          organism = organism)
  }
  
  gVars$BMD_dist_in_ke  =  p
  
  
  #save plot
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line  =  "-----------------------------------------------------"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    line  =  "ENRICHMENT KE"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    line  =  "GENE BMD IN KE"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/plots/", toString(tsn), "_geneBMDinKEPlot.RData", sep  =  "")
    save(p, file  =  fname)
    line  =  paste("The Current R Data Object is saved:", fname, sep  =  " ")
    write(line,file  =  logVars$file,append  =  TRUE)
    
  }
  
  return(p)
}

render_ke_range  =  function(gVars, logVars,input){
  if (is.null(gVars$KE_enrich_res) ||
    is.null(gVars$inputPh()) ||
    is.null(gVars$TPColID) ||
    is.null(input$range_ke_groupby_input) ||
    is.null(input$range_ke_groupby2_input) ||
    is.null(input$range_ke_filtercol_input) ||
    is.null(input$range_ke_is_group_by_numeric)
  ) {
    return(NULL)
  }
  
  ke_enrichment_results  =  gVars$KE_enrich_res
  
  experiment_col  =  "Experiment"
  other_variables_id_col   =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  optiona_val  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$optVarsGroup)]

  input_group_by  =  input$range_ke_groupby_input
  input_other_variables  =  input$range_ke_groupby2_input
  input_filter_column  =  input$range_ke_filtercol_input
  input_filter_column_value  =  input$range_ke_filterby_input
  is_group_by_numeric  =  input$range_ke_is_group_by_numeric
  
  if (input_other_variables  ==  "None") {
    other_variables  =  NULL
  }else{
    other_variables  =  input_other_variables
  }
  if (input_group_by  ==  "None") {
    groupby  =  NULL
  }else {
    groupby  =  input_group_by
  }
  if (input_filter_column  ==  "None") {
    fcol  =  NULL
  }else {
    fcol  =  input_filter_column
    
  }
  if ("None" %in% input_filter_column | is.null(input_filter_column_value)) {
    fby  =  NULL
  }else {
    #each value of fcol is its own list
    fby  =  NULL
    for (valg in fcol) {
      temp  =  c()
      
      for (vals in input_filter_column_value) {
        
        if (is.element(vals, ke_enrichment_results[[valg]])) {
          temp  =  c(temp,vals)
          
        }
      }
      if (is.null(fby)) {
        fby  =  list(temp)
      }else {
        fby  =  append(fby, list(temp))
      }
    }
  }
  
  
  group_by  =  groupby
  group_by2  =  other_variables
  filter_column  =  fcol
  filter_by  =  fby
  
  p  =  render_range_plot(ke_enrichment_results,group_by, group_by2, filter_column, filter_by,is_group_by_numeric)
  
  #save plot
  if (gVars$in_glp_mode) { 
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "ENRICHMENT"
    write(line,file = logVars$file,append = TRUE)
    
    line = "RANGE PLOT"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/plots/", toString(tsn), "_rangePlot.RData", sep = "")
    save(p, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    
  }
  
  gVars$KE_range_plot  =  p
    
  return(list("gVars" = gVars,"logVars" = logVars))
}


make_render_aop_fingerprint_bubble_plot = function(gVars, logVars, input){
  
  shiny::validate(need(!is.null(gVars$aop_fingerprint_results), "No enrichment performed!"))
  shiny::validate(need(!is.null(gVars$inputPh()), "No phenodata available!"))
  shiny::validate(need(!is.null(gVars$TPColID), "No column ID for time point!"))
  
  input_group_by = input$range_AOP_fingerprint_bubble_plot_groupby_input # This should be time
  input_other_variables = "None" #input$range_AOP_fingerprint_bubble_plot_groupby2_input # This could be experiment
  input_filter_column = input$range_AOP_fingerprint_bubble_plot_filtercol_input
  input_filter_column_value = input$range_AOP_fingerprint_bubble_plot_filterby_input
  is_group_by_numeric = input$range_AOP_fingerprint_bubble_plot_is_group_by_numeric
  
  enrichement_data = gVars$aop_fingerprint_results$detailed_results_only_enriched
  
  
  if (input_other_variables == "None") {
    other_variables = NULL
  }else{
    other_variables = input_other_variables
  }
  if (input_group_by == "None") {
    groupby = NULL
  }else {
    groupby = input_group_by
  }
  if (input_filter_column == "None") {
    fcol = NULL
  }else {
    fcol = input_filter_column
    
  }
  if ("None" %in% input_filter_column | is.null(input_filter_column_value)) {
    fby = NULL
  }else {
    #each value of fcol is its own list
    fby = NULL
    for (valg in fcol) {
      temp = c()
      
      for (vals in input_filter_column_value) {
        
        if (is.element(vals, enrichement_data[[valg]])) {
          temp = c(temp,vals)
          
        }
      }
      if (is.null(fby)) {
        fby = list(temp)
      }else {
        fby = append(fby, list(temp))
      }
    }
  }
  
  
  group_by = groupby
  group_by2 = other_variables
  filter_column = fcol
  filter_by = fby
  
  threshold_proportion = as.numeric(input$min_proportion_AOP_fingerprint)
  font_size = as.numeric(input$AOP_fingerprint_font_size)
  time_var  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  if(input$range_AOP_fingerprint_bubble_plot_is_group_by_SSbD == TRUE){
    group_AOPs = "SSbD_category"
  }else{
    group_AOPs = NULL
  }
  
  p = render_aop_fingerprint_bubble_plot(enrichement_data = enrichement_data,
                                         group_by = group_by,
                                         group_by2 = group_by2,
                                         x_axis_var = input$range_AOP_fingerprint_bubble_plot_x_axis_input,
                                         filter_column = filter_column,
                                         filter_by = filter_by,
                                         is_group_by_numeric = is_group_by_numeric,
                                         threshold_proportion = threshold_proportion,
                                         text_cex = font_size,
                                         group_AOPs = group_AOPs,
                                         y_axis_var = input$range_AOP_fingerprint_bubble_plot_y_axis_input)

  
    if (gVars$in_glp_mode) {
      print("writing log file obj")
      line = "-----------------------------------------------------"
      write(line,file = logVars$file,append = TRUE)

      line = "AOP FINGERPRINT BUBBLE PLOT"
      write(line,file = logVars$file,append = TRUE)

      line = "BUBBLE PLOT AOP FINGERPRINT"
      write(line,file = logVars$file,append = TRUE)

      ts  =  Sys.time()
      tsn  =  as.numeric(ts)
      # fname  =  paste(logVars$location, "/plots/", toString(tsn), "_AOP_bibble_plot.RData", sep = "")
      # save(p, file  =  fname)
      # line = paste("The Current R Data Object is saved:", fname, sep = " ")
      # write(line,file = logVars$file,append = TRUE)

    }

   gVars$bubble_plot_AOP_fingerprint = p
  

    return(list("gVars" = gVars,"logVars" = logVars))
  
}

render_AOP_fingerprint_range  =  function(gVars, logVars,input){
  if (is.null(gVars$aop_fingerprint_results$detailed_results_only_enriched) ||
      is.null(gVars$inputPh()) ||
      is.null(gVars$TPColID) ||
      is.null(input$range_AOP_fingerprint_groupby_input) ||
      is.null(input$range_AOP_fingerprint_groupby2_input) ||
      is.null(input$range_AOP_fingerprint_filtercol_input) ||
      is.null(input$range_AOP_fingerprint_is_group_by_numeric)
  ) {
    return(NULL)
  }
  
  ke_enrichment_results  =  gVars$aop_fingerprint_results$detailed_results_only_enriched
  
  input_group_by  =  input$range_AOP_fingerprint_groupby_input
  input_other_variables  =  input$range_AOP_fingerprint_groupby2_input
  input_filter_column  =  input$range_AOP_fingerprint_filtercol_input
  input_filter_column_value  =  input$range_AOP_fingerprint_filterby_input
  is_group_by_numeric  =  input$range_AOP_fingerprint_is_group_by_numeric
  
  if (input_other_variables  ==  "None") {
    other_variables  =  NULL
  }else{
    other_variables  =  input_other_variables
  }
  if (input_group_by  ==  "None") {
    groupby  =  NULL
  }else {
    groupby  =  input_group_by
  }
  if (input_filter_column  ==  "None") {
    fcol  =  NULL
  }else {
    fcol  =  input_filter_column
    
  }
  if ("None" %in% input_filter_column | is.null(input_filter_column_value)) {
    fby  =  NULL
  }else {
    #each value of fcol is its own list
    fby  =  NULL
    for (valg in fcol) {
      temp  =  c()
      
      for (vals in input_filter_column_value) {
        
        if (is.element(vals, ke_enrichment_results[[valg]])) {
          temp  =  c(temp,vals)
          
        }
      }
      if (is.null(fby)) {
        fby  =  list(temp)
      }else {
        fby  =  append(fby, list(temp))
      }
    }
  }
  
  
  group_by  =  groupby
  group_by2  =  other_variables
  filter_column  =  fcol
  filter_by  =  fby
  
  p  =  render_range_plot(ke_enrichment_results,group_by, group_by2, filter_column, filter_by,is_group_by_numeric,display  =  "Aop")
  
  #save plot
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "ENRICHMENT"
    write(line,file = logVars$file,append = TRUE)
    
    line = "RANGE PLOT AOP FINGERPRINT"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/plots/", toString(tsn), "_rangePlot.RData", sep = "")
    save(p, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    
  }
  
  gVars$AOP_fingerprint_range_plot  =  p
  
  return(list("gVars" = gVars,"logVars" = logVars))
}

render_AOP_enriched_data_frame  =  function(gVars, logVars, input){
  shiny::validate(need(!is.null(gVars$aop_enrichment_results), "No enrichment performed!"))
  shiny::validate(need(!is.null(input$time_point_selection_AOP), "No time point selection!"))
  shiny::validate(need(!is.null(input$experiment_selection_AOP), "No experiment selection!"))
  shiny::validate(need(!is.null(gVars$inputPh()), "No phenodata available!"))
  shiny::validate(need(!is.null(gVars$TPColID), "No column ID for time point!"))
  
  if (input$experiment_selection_AOP  ==  "All") {
    exp_idx  =  1:nrow(gVars$aop_enrichment_results)
  }else{
    exp_idx  =  which(gVars$aop_enrichment_results[,"Experiment"]  ==  input$experiment_selection_AOP)
  }
  
  if (input$time_point_selection_AOP  ==  "All") {
    time_idx  =  1:nrow(gVars$aop_enrichment_results)
  }else{
    time_var  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
    time_idx  =  which(gVars$aop_enrichment_results[,time_var]  ==  input$time_point_selection_AOP)
  }
  
  good_idx  =  intersect(exp_idx, time_idx)
  
  shiny::validate(need(length(good_idx) > 0, "Nothing in the intersection!"))
  
  dataTable  =  DT::datatable(gVars$aop_enrichment_results[good_idx,], filter  =  "top",
                            selection  =  'single',
                            options  =  list(
                              search  =  list(regex  =  TRUE, caseInsensitive  =  FALSE), #MAYBE remove search bars
                              scrollX  =  TRUE,
                              ordering  =  T))
  
  return(dataTable)
}

render_KE_enriched_data_frame  =  function(gVars, logVars, input){
  shiny::validate(need(!is.null(gVars$KE_enrich_res), "No enrichment performed!"))
  shiny::validate(need(!is.null(input$time_point_selection_KE), "No time point selection!"))
  shiny::validate(need(!is.null(input$experiment_selection_KE), "No experiment selection!"))
  shiny::validate(need(!is.null(gVars$inputPh()), "No phenodata available!"))
  shiny::validate(need(!is.null(gVars$TPColID), "No column ID for time point!"))
  
  if (input$experiment_selection_KE  ==  "All") {
    exp_idx  =  1:nrow(gVars$KE_enrich_res)
  }else{
    exp_idx  =  which(gVars$KE_enrich_res[,"Experiment"]  ==  input$experiment_selection_KE)
  }
  
  if (input$time_point_selection_KE  ==  "All") {
    time_idx  =  1:nrow(gVars$KE_enrich_res)
  }else{
    time_var  =  colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
    time_idx  =  which(gVars$KE_enrich_res[,time_var]  ==  input$time_point_selection_KE)
  }
  
  good_idx  =  intersect(exp_idx, time_idx)
  
  shiny::validate(need(length(good_idx) > 0, "Nothing in the intersection!"))
  
  toPlotData = gVars$KE_enrich_res[good_idx,]

  return(toPlotData)
}

#' Render Range Plot
#'
#' This function generates a range plot for rendering enrichment data.
#'
#' @param enrichement_data A data frame containing the enrichment data.
#' @param group_by Variable used for grouping the data.
#' @param group_by2 Second variable used for subgrouping the data.
#' @param filter_column Column for filtering the data.
#' @param filter_by Value for filtering the data.
#' @param is_group_by_numeric Logical, whether the grouping variable is numeric.
#' @param display Display option: "Ke" or "a.name".
#'
#' @return A range plot visualizing enrichment data.
#' @export
#' Render Range Plot
#'
#' This function generates a range plot for rendering enrichment data.
#'
#' @param enrichement_data A data frame containing the enrichment data.
#' @param group_by Variable used for grouping the data.
#' @param group_by2 Second variable used for subgrouping the data.
#' @param filter_column Column for filtering the data.
#' @param filter_by Value for filtering the data.
#' @param is_group_by_numeric Logical, whether the grouping variable is numeric.
#' @param display Display option: "Ke" or "a.name".
#'
#' @return A range plot visualizing enrichment data.
#' @export
render_range_plot = function(enrichement_data,group_by, group_by2, filter_column, filter_by, is_group_by_numeric,
                             display = "Ke"){
  
  if (!is.null(filter_column)) {
    enrichement_data = filter_df_no(mod_stats = enrichement_data, filter_column = filter_column,filter_by = filter_by)
    if (is.null(enrichement_data)) return(make_empty_plot())
  }
  
  if (is_group_by_numeric) {
    enrichement_data[,group_by] = factor( enrichement_data[,group_by], levels = sort(unique(as.numeric( enrichement_data[,group_by]))))
  }
  
  enrichement_data = as.data.frame(enrichement_data)
  if (display == "Ke") {
    enrichement_data$Variable = enrichement_data$Ke_description
  }else{
    enrichement_data$Variable = enrichement_data$a.name
  }
  
  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  
  enrichement_data = enrichement_data[,c("Experiment",time_var,"BMDL","BMD","BMDU","Variable")]
  enrichement_data = unique(enrichement_data)
  
  # enrichement_data$Variable = as.factor(enrichement_data$Variable)
  
  enrichement_data$Variable = as.character(as.vector(enrichement_data$Variable))
  enrichement_data$BMDL = as.numeric(enrichement_data$BMDL)
  enrichement_data$BMDU = as.numeric(enrichement_data$BMDU)
  enrichement_data$BMD = as.numeric(enrichement_data$BMD)
  
  if(!is.null(group_by)){
    library(dplyr)
    
    enrichement_data <- enrichement_data %>%
      dplyr::group_by({{group_by}}) %>%  # Group by "Experiment"
      arrange(BMD, .by_group = TRUE) %>%  # Order within each Experiment
      mutate(Variable = factor(Variable, levels = unique(Variable))) %>%  # Reorder factors
      ungroup()  # Ungroup after processing
    
  }else{
    enrichement_data = enrichement_data[order(enrichement_data$BMD),]
    enrichement_data$Variable = factor(enrichement_data$Variable, levels = unique(enrichement_data$Variable))
    
  }
  
  # enrichement_data$Variable <- forcats::fct_reorder(enrichement_data$Variable, as.numeric(enrichement_data$BMDL), .desc = F)
  
  Doses <- c("BMDL" = "red", "BMD" = "green", "BMDU" = "blue")
  
  p = ggplot2::ggplot(data = enrichement_data,ggplot2::aes(x = Variable)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = BMDL, ymax = BMDU, x = Variable),size = 1.5, alpha = 0.25) +
    ggplot2::geom_point(ggplot2::aes(y = BMDL, color = "BMDL")) +
    ggplot2::geom_point(ggplot2::aes(y = BMDU, color = "BMDU")) +
    ggplot2::geom_point(ggplot2::aes(y = BMD, color = "BMD")) +
    ggplot2::coord_flip() +
    ggplot2::ylab("BMD") +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(name = "Effective doses", values = Doses)
  
  if (!is.null(group_by)) {
    if (!is.null(group_by2)) {
      f = stats::formula(paste0(group_by2,"~",group_by,sep = ""))
    }else{
      f = stats::formula(paste0("~",group_by,sep = ""))
    }
    p = p + ggplot2::facet_grid(f)#,scales = "free_y"
  }
  
  return(p)
}

# render_range_plot = function(enrichement_data,group_by, group_by2, filter_column, filter_by, is_group_by_numeric,
#                              display = "Ke"){
#   
#   if (!is.null(filter_column)) {
#     enrichement_data = filter_df_no(mod_stats = enrichement_data, filter_column = filter_column,filter_by = filter_by)
#     if (is.null(enrichement_data)) return(make_empty_plot())
#   }
#   
#   if (is_group_by_numeric) {
#     enrichement_data[,group_by] = factor( enrichement_data[,group_by], levels = sort(unique(as.numeric( enrichement_data[,group_by]))))
#   }
#   
#   enrichement_data = as.data.frame(enrichement_data)
#   if (display == "Ke") {
#     enrichement_data$Variable = enrichement_data$Ke_description
#   }else{
#     enrichement_data$Variable = enrichement_data$a.name
#   }
#   enrichement_data$Variable = as.factor(enrichement_data$Variable)
#   enrichement_data$Variable <- forcats::fct_reorder(enrichement_data$Variable, enrichement_data$BMDL, .desc = TRUE)
#   
#   Doses <- c("BMDL" = "red", "BMD" = "green", "BMDU" = "blue")
#   
#   p = ggplot2::ggplot(data = enrichement_data,ggplot2::aes(x = Variable)) +
#     ggplot2::geom_linerange(ggplot2::aes(ymin = BMDL, ymax = BMDU, x = Variable),size = 1.5, alpha = 0.25) +
#     ggplot2::geom_point(ggplot2::aes(y = BMDL, color = "BMDL")) +
#     ggplot2::geom_point(ggplot2::aes(y = BMDU, color = "BMDU")) +
#     ggplot2::geom_point(ggplot2::aes(y = BMD, color = "BMD")) +
#     ggplot2::coord_flip() +
#     ggplot2::ylab("BMD") +
#     ggplot2::theme_bw(base_size = 16) +
#     ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
#     ggplot2::scale_color_manual(name = "Effective doses", values = Doses)
#   
#   if (!is.null(group_by)) {
#     if (!is.null(group_by2)) {
#       f = stats::formula(paste0(group_by2,"~",group_by,sep = ""))
#     }else{
#       f = stats::formula(paste0("~",group_by,sep = ""))
#     }
#     p = p + ggplot2::facet_grid(f,scales = "free_y")
#   }
#   
#   return(p)
# }

# group_by either aop or ssbd
compute_ke_networks = function(gVars, logVars, input, group_by = "aop"){
  
  shiny::validate(need(!is.null(gVars$filtered_bmd), "No enrichment performed!"))
  
  shiny::validate(need(!is.null(gVars$KE_enrich_res), "No enrichment performed!"))
  shiny::validate(need(!is.null(gVars$aop_fingerprint_results), "No enrichment performed!"))
  
  shiny::validate(need(!is.null(input$KEr_filter_ke_by_aop_fingerprint), "Missing KE filtering related parameters!"))
  shiny::validate(need(!is.null(input$KEr_enlarge_ke_selection), "Missing KE filtering related parameters!"))
  
  shiny::validate(need(!is.null(input$experiment_KEr), "Experiment selection is missing!"))
  # shiny::validate(need(!is.null(input$time_point_KEr), "Time selection is missing!"))
  
  shiny::validate(need(!is.null(gVars$inputPh()), "No phenodata available!"))
  shiny::validate(need(!is.null(gVars$TPColID), "No column ID for time point!"))
  
  genes_human = gVars$genes_human
  genes_mouse = gVars$genes_mouse
  genes_rat = gVars$genes_rat 
  
  filter_ke_by_aop_fingerprint = input$KEr_filter_ke_by_aop_fingerprint

  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  enlarge_ke_selection = input$KEr_enlarge_ke_selection
  
  # chose either all the KE enriched, or only those that belongs to AOPs that pass the fingerprint selection process
  if(filter_ke_by_aop_fingerprint){
    ### TO FIX THIS PART IS NOT WORKING!
    detailed_results =   gVars$aop_fingerprint_results$detailed_results_only_enriched
    detailed_results  = detailed_results[,c(2:14)]
    detailed_results = unique(detailed_results)
    
    KE = gVars$KE_enrich_res
    KE = KE[KE$TermID %in% detailed_results$Ke,]
    detailed_results = KE
    
    ke_id = "TermID"
  }else{
    detailed_results = gVars$KE_enrich_res 
    ke_id = "TermID"
  }
  
  all_experiments  = as.character(unique(detailed_results$Experiment))
  all_time_points  = unique(detailed_results[,time_var]) 
  
  netList = list()
  
  for(expi in all_experiments) {
    
    detailed_results_expi = detailed_results[detailed_results$Experiment == expi, ]
    
    for(tp in all_time_points) {
      
      detailed_results_expi_tp = detailed_results_expi[detailed_results_expi[,time_var] %in% tp, ]
      
      if(nrow(detailed_results_expi_tp)>0){
        enlarge_ke_selection = enlarge_ke_selection
        numerical_variables = c("BMDL","BMD","BMDU")
        pval_variable = "padj"
        gene_variable = "Genes"
        selected_experiment = paste(expi, tp,sep = "_") 
        # group_by = "ssbd"
        
        ke_organism  =  tolower(input$ke_organism)
        ke_gene_identifier  =  input$ke_gene_identifier
        
        nodes_edges = make_visNetwork(detailed_results_expi_tp,
                                      experiment = expi,
                                      enlarge_ke_selection = enlarge_ke_selection,
                                      ke_id, numerical_variables = numerical_variables,
                                      pval_variable,
                                      gene_variable,
                                      max_path_length = input$max_path_length,
                                      n_AOs = input$n_AOs ,
                                      n_MIEs = input$n_MIEs,
                                      convert_to_gene_symbols = input$convert_to_gene_symbol,
                                      organism=ke_organism, 
                                      gene_id_type=ke_gene_identifier,
                                      genes_human=genes_human, 
                                      genes_mouse=genes_mouse, 
                                      genes_rat=genes_rat)
        
        vn = plot_visNetwork(nodes = nodes_edges$nodes,
                             edges = nodes_edges$edges,
                             group_by = group_by,
                             numerical_variables = numerical_variables)
        
        vn
        
        netList[[paste(expi, tp, sep = "_")]] = vn
      }
      
    }
  }
  
  if (gVars$in_glp_mode) {
    #GLP MODE
    shinyalert(
      title  =  "GLP Mode Justification",
      text  =  "Please provide a justification for all your selected paramters",
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
      callbackR  =  function(x) {logVars$KEKENetworkReason  =  x }
      
    )
    
    #save for report
    ts  =  Sys.time()
    logVars$Create_KEr_networkTimestamp  =  ts
    logVars$Create_KEr_network_KEr_filter_ke_by_aop_fingerprint  =  input$KEr_filter_ke_by_aop_fingerprint
    logVars$Create_KEr_network_KEr_KEr_enlarge_ke_selection  =  input$KEr_enlarge_ke_selection
    logVars$Create_KEr_network_KEr_KEr_group_by_ssbd = input$KEr_group_by_ssbd
    
  }
  
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line  =  "-----------------------------------------------------"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    line  =  "CREATE KE-KE network"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    line  =  "KE-KE network PARAMETERS"
    write(line,file  =  logVars$file,append  =  TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/tables/", toString(tsn), "_KEr_Network.RData", sep  =  "")
    save(detailed_results, netList, file  =  fname)
    line  =  paste("The Current R Data Object is saved:", fname, sep  =  " ")
    write(line,file  =  logVars$file,append  =  TRUE)
    
  }
  
  gVars$KEr_net_list = netList
  return(list("gVars" = gVars,"logVars" = logVars))
  
  
}



#' Boxplot of BMD by Biological System Level and KE Type
#'
#' This function prepares KE enrichment data by formatting numeric fields,
#' re-coding KE types, merging biological system annotations, and generating
#' a boxplot of BMD values by biological system level and experiment.
#'
#' @param ke_enrichment_results A data frame containing KE enrichment results.
#'   Required columns: "TermID", "BMD", "BMDL", "BMDU", "Ke_type", and "Experiment".
#' @param bio_annotations A data frame with biological system annotations.
#'   Required columns: "ke" (matching TermID in enrichment data), and "level".
#' @param fill_colors A vector of fill colors for biological levels
#'   (default: 5 specific hex codes).
#' @param text_size Numeric. Base font size for titles and labels (default: 16).
#' @param facet_scales Character. Scaling option for facets: "fixed", "free", "free_x", or "free_y" (default: "fixed").
#' @param angle_x Numeric. Angle for x-axis text labels (default: 45).
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#'   p <- plot_bmd_by_ke_and_level(ke_enrichment_results, Biological_system_annotations)
#'   print(p)
#' }
#'
#' @import ggplot2
#' @export
plot_bmd_by_ke_and_level <- function(ke_enrichment_results,
                                     bio_annotations,
                                     fill_colors = c("#235888", "#F7BD03", "#357D8A", "#FFDD00", "#68A5D4","#112240"),
                                     text_size = 16,
                                     facet_scales = "fixed",
                                     angle_x = 45) {
  library(ggplot2)
  
  # Ensure bio_annotations is a data frame
  bio_annotations <- as.data.frame(bio_annotations)
  
  # Select and process columns from enrichment results
  KER <- ke_enrichment_results[, c("TermID", "BMDL", "BMD", "BMDU", "Ke_type", "Experiment")]
  KER$BMD  <- as.numeric(KER$BMD)
  KER$BMDL <- as.numeric(KER$BMDL)
  KER$BMDU <- as.numeric(KER$BMDU)
  
  # Recode KE types
  KER$Ke_type[KER$Ke_type == "MolecularInitiatingEvent"] <- "MIE"
  KER$Ke_type[KER$Ke_type == "KeyEvent"] <- "KE"
  KER$Ke_type[KER$Ke_type == "AdverseOutcome"] <- "AO"
  KER$Ke_type <- factor(KER$Ke_type, levels = c("MIE", "KE", "AO"))
  
  # Merge with biological system annotations
  KER <- merge(KER, bio_annotations, by.x = "TermID", by.y = "ke")
  
  # Set biological system level as ordered factor
  KER$level <- factor(KER$level,
                      levels = c("Molecular", "Cellular", "Tissue", "Organ", "Individual","Population"))
  
  # Create the boxplot
  p <- ggplot(KER, aes(x = level, y = BMD, fill = level)) +
    geom_boxplot() +
    facet_wrap(~Experiment, scales = facet_scales) +
    labs(title = "Boxplot of BMD by Biological Level",
         x = "Biological Level",
         y = "BMD") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = text_size),
      axis.title.y = element_text(size = text_size),
      axis.text.x  = element_text(size = text_size - 1, angle = angle_x, hjust = 1),
      axis.text.y  = element_text(size = text_size),
      strip.text   = element_text(size = text_size),
      legend.title = element_text(size = text_size),
      legend.text  = element_text(size = text_size - 2),
      legend.position = "none"
    ) +
    scale_fill_manual(values = fill_colors)
  
  return(p)
}




#' Generate Boxplots of BMD Metrics by KE Type and Experiment
#'
#' This function takes key event enrichment results and returns a list of
#' three `ggplot2` boxplots showing distributions of BMD, BMDL, and BMDU
#' across KE types and experiments.
#'
#' @param ke_enrichment_results A data frame containing KE enrichment results
#'   with at least the columns: "TermID", "BMDL", "BMD", "BMDU", "Ke_type", and "Experiment".
#' @param fill_colors A named vector of fill colors for KE types (default: MIE = "#235888", KE = "#F7BD03", AO = "#357D8A").
#' @param text_size Numeric. Font size used for plot text elements (default: 16).
#' @param facet_scales Character. Scale option for `facet_wrap()`, e.g., "free", "fixed", "free_x", or "free_y" (default: "free").
#' @param angle_x Numeric. Angle in degrees to rotate x-axis labels (default: 0).
#'
#' @return A named list of three `ggplot` objects: `BMDL`, `BMD`, and `BMDU` plots.
#'
#' @examples
#' \dontrun{
#'   plots <- plot_bmd_boxplots(ke_enrichment_results)
#'   plots$BMDL
#'   plots$BMD
#'   plots$BMDU
#' }
#'
#' @import ggplot2
#' @export
plot_bmd_boxplots_by_ke_type <- function(ke_enrichment_results,
                                         fill_colors = c(MIE = "#235888", KE = "#F7BD03", AO = "#357D8A"),
                                         text_size = 16,
                                         facet_scales = "free",
                                         angle_x = 0) {
  library(ggplot2)
  
  # Prepare data
  KER <- ke_enrichment_results[, c("TermID", "BMDL", "BMD", "BMDU", "Ke_type", "Experiment")]
  KER$BMD  <- as.numeric(KER$BMD)
  KER$BMDL <- as.numeric(KER$BMDL)
  KER$BMDU <- as.numeric(KER$BMDU)
  KER$Ke_type <- gsub("MolecularInitiatingEvent", "MIE", KER$Ke_type)
  KER$Ke_type <- gsub("KeyEvent", "KE", KER$Ke_type)
  KER$Ke_type <- gsub("AdverseOutcome", "AO", KER$Ke_type)
  KER$Ke_type <- factor(KER$Ke_type, levels = c("MIE", "KE", "AO"))
  
  # Helper to generate each plot
  make_plot <- function(KER,metric, ylab, title) {
    ggplot(KER, aes(x = Ke_type, y = .data[[metric]], fill = Ke_type)) +
      geom_boxplot() +
      facet_wrap(~Experiment, scales = facet_scales) +
      labs(title = title, x = "KE Type", y = ylab) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = text_size),
        axis.title.y = element_text(size = text_size),
        axis.text.x  = element_text(size = text_size - 1, angle = angle_x, hjust = 0.5),
        axis.text.y  = element_text(size = text_size),
        strip.text   = element_text(size = text_size),
        legend.title = element_text(size = text_size),
        legend.text  = element_text(size = text_size - 2),
        legend.position = "none"
      ) +
      scale_fill_manual(values = fill_colors)
  }
  
  # Create and return plots
  list(
    BMDL = make_plot(KER,"BMDL", "BMDL", "Boxplot of BMDL by KE Type"),
    BMD  = make_plot(KER,"BMD",  "BMD",  "Boxplot of BMD by KE Type"),
    BMDU = make_plot(KER,"BMDU", "BMDU", "Boxplot of BMDU by KE Type")
  )
}


#' Boxplot of log(BMD) Distribution by Biological System
#'
#' This function takes KE enrichment results and biological system annotations,
#' merges them, filters rare systems (fewer than 6 entries), and creates
#' a boxplot of log-transformed BMD values grouped by biological systems.
#'
#' @param ke_enrichment_results A data frame containing KE enrichment data.
#'   Must include columns: "TermID", "BMD", "BMDL", "BMDU", "Ke_type", "Experiment".
#' @param Biological_system_annotations A data frame containing annotations for biological systems.
#'   Must include columns: "ke" (to match "TermID"), "level", and "system".
#' @param fill_colors A vector of colors to fill the biological system groups in the boxplot.
#'   Default includes 8 distinct color values.
#' @param text_size Numeric. Font size for plot elements (default: 16).
#' @param angle_x Numeric. Angle to rotate x-axis labels (default: 45).
#'
#' @return A `ggplot` object representing the distribution of log(BMD) by biological system.
#'
#' @examples
#' \dontrun{
#'   p <- ke_bmd_distribution_by_system(ke_enrichment_results, Biological_system_annotations)
#'   print(p)
#' }
#'
#' @import ggplot2
#' @export
ke_bmd_distribution_by_system <- function(ke_enrichment_results,
                                          Biological_system_annotations,
                                          fill_colors = c("#235888", "#F7BD03", "#357D8A", "#FFDD00", 
                                                          "#68A5D4", "#FFB600", "#D9D7CD", "#641DFF"),
                                          text_size = 16,
                                          angle_x = 45) {
  library(ggplot2)
  
  # Select and format key columns
  KER <- ke_enrichment_results[, c("TermID", "BMDL", "BMD", "BMDU", "Ke_type", "Experiment")]
  KER$BMD <- as.numeric(KER$BMD)
  KER$BMDL <- as.numeric(KER$BMDL)
  KER$BMDU <- as.numeric(KER$BMDU)
  
  # Recode KE types
  KER$Ke_type[KER$Ke_type == "MolecularInitiatingEvent"] <- "MIE"
  KER$Ke_type[KER$Ke_type == "KeyEvent"] <- "KE"
  KER$Ke_type[KER$Ke_type == "AdverseOutcome"] <- "AO"
  KER$Ke_type <- factor(KER$Ke_type, levels = c("MIE", "KE", "AO"))
  
  # Merge with system annotations
  KER <- merge(KER, Biological_system_annotations, by.x = "TermID", by.y = "ke")
  KER$level <- factor(KER$level,
                      levels = c("Molecular", "Cellular", "Tissue", "Organ", "Individual"))
  
  # Clean and filter system values
  KER2 <- KER
  KER2$system[is.na(KER2$system)] <- "Not Annotated"
  KER2 <- KER2[, -5]  # Drop Ke_type column
  KER2 <- unique(KER2)
  
  # Remove systems with <6 entries
  to_rem <- which(KER2$system %in% names(table(KER2$system)[table(KER2$system) < 6]))
  KER2 <- KER2[-to_rem, ]
  
  # Clean system names
  KER2$system <- gsub("/", "\n", KER2$system)
  
  if(is.null(fill_colors)){
    fill_colors = heat.colors(length(unique(KER2$system)))
  }
  
  # Create ggplot
  p <- ggplot(KER2, aes(x = system, y = log(BMD), fill = system)) +
    geom_boxplot() +
    facet_wrap(~Experiment) +
    labs(title = "Boxplot of BMD by Biological System",
         x = "Biological System",
         y = "log(BMD)") +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = angle_x, hjust = 1, size = text_size - 4),
      axis.text.y  = element_text(size = text_size),
      axis.title.x = element_text(size = text_size),
      axis.title.y = element_text(size = text_size),
      strip.text   = element_text(size = text_size),
      legend.title = element_text(size = text_size),
      legend.text  = element_text(size = text_size - 2),
      legend.position = "none"
    ) +
    scale_fill_manual(values = fill_colors)
  
  return(p)
}

#' Barplot of AOP Distribution Over SSbD Categories
#'
#' This function merges AOP enrichment results with SSbD category annotations,
#' reformats specific category labels (e.g., organ-specific STO), and computes
#' relative frequency of AOPs per category across experiments.
#' It then returns a faceted horizontal barplot of category distributions.
#'
#' @param aop_enrichment_results A data frame containing AOP enrichment results.
#'   Must include columns: "TermID", "Experiment", "BMD", "BMDL", "BMDU".
#' @param Annotate_AOPs A data frame mapping AOPs to SSbD categories and organs.
#'   Must include columns: "AOP", "SSbD_category", and "Organ".
#' @param fill_colors A vector of colors for experiment groups in the barplot.
#'   Default: c("#235888", "#F7BD03", "#357D8A").
#' @param text_size Numeric. Base font size for plot elements (default: 16).
#'
#' @return A `ggplot` object representing a barplot of AOP frequency across SSbD categories.
#'
#' @examples
#' \dontrun{
#'   p <- plot_aop_distribution_over_ssbd_categories(aop_enrichment_results, Annotate_AOPs)
#'   print(p)
#' }
#'
#' @import ggplot2
#' @export
# plot_aop_distribution_over_ssbd_categories <- function(aop_enrichment_results,
#                                                        Annotate_AOPs,
#                                                        fill_colors = c("#235888", "#F7BD03", "#357D8A"),
#                                                        text_size = 16) {
#   library(ggplot2)
#   
#   # Extract unique AOP enrichment entries
#   AER <- unique(aop_enrichment_results[, c("TermID", "Experiment", "BMD", "BMDL", "BMDU")])
#   
#   # Handle annotations
#   Annotate_AOPs2 <- Annotate_AOPs
#   idx <- which(Annotate_AOPs2$SSbD_category == "Specific target organ toxicity")
#   Annotate_AOPs2$Organ[is.na(Annotate_AOPs2$Organ)] <- "Unspecified"
#   Annotate_AOPs2$SSbD_category[idx] <- paste0("STO_tox_", Annotate_AOPs2$Organ[idx])
#   
#   # Count AOPs in each SSbD category
#   categories_count <- table(Annotate_AOPs2$SSbD_category)
#   
#   # Merge enrichment results with SSbD categories
#   AER <- merge(AER, Annotate_AOPs2, by.x = "TermID", by.y = "AOP")
#   
#   # Summarize counts and compute percentages
#   df <- as.data.frame(table(AER$SSbD_category, AER$Experiment))
#   colnames(df) <- c("SSbD_category", "Experiment", "Counts")
#   df$TotCounts <- as.numeric(categories_count[as.character(df$SSbD_category)])
#   df$Percentages <- df$Counts / df$TotCounts
#   df$SSbD_category <- paste0(df$SSbD_category, " (", df$TotCounts, ")")
#   df$timepoint <- vapply(strsplit(as.character(df$Experiment), "_"), function(x) x[2], character(1))
#   
#   if(!is.null(fill_colors)){
#     fill_colors = terrain.colors(length(unique(df$Experiment)))
#   }
#   
#   # Create the barplot
#   p <- ggplot(df, aes(x = SSbD_category, y = Percentages, fill = Experiment)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
#     coord_flip() +
#     facet_wrap(~timepoint) +
#     labs(title = "Barplot of Frequency by SSbD Category",
#          x = "SSbD Categories",
#          y = "Relative Frequency") +
#     theme_minimal(base_size = text_size) +
#     theme(
#       axis.title.x = element_text(size = text_size),
#       axis.title.y = element_text(size = text_size),
#       axis.text.x = element_text(size = text_size - 4),
#       axis.text.y = element_text(size = text_size - 4),
#       legend.title = element_text(size = text_size - 2),
#       legend.text = element_text(size = text_size - 4)
#     ) +
#     scale_fill_manual(values = fill_colors)
#   
#   return(p)
# }

plot_aop_distribution_over_ssbd_categories <- function(aop_enrichment_results,
                                                       Annotate_AOPs,
                                                       # fill_colors = c("#235888", "#F7BD03", "#357D8A"),
                                                       text_size = 16) {
  library(ggplot2)
  
  # Extract unique AOP enrichment entries
  AER <- unique(aop_enrichment_results[, c("TermID", "Experiment", "BMD", "BMDL", "BMDU")])
  
  # Handle annotations
  Annotate_AOPs2 <- Annotate_AOPs
  idx <- which(Annotate_AOPs2$SSbD_category == "Specific target organ toxicity")
  Annotate_AOPs2$Organ[is.na(Annotate_AOPs2$Organ)] <- "Unspecified"
  Annotate_AOPs2$SSbD_category[idx] <- paste0("STO_tox_", Annotate_AOPs2$Organ[idx])
  
  # Count AOPs in each SSbD category in the whole annotation
  categories_count <- table(Annotate_AOPs2$SSbD_category)
  Annotate_AOPs2$SSbD_category = as.factor(Annotate_AOPs2$SSbD_category)
  
  # Count AOPs in each SSbD category in the enriched AOPs
  Annotate_AOPs3 = Annotate_AOPs2[Annotate_AOPs2$AOP %in% AER$TermID,]
  categories_count_enriched <- table(Annotate_AOPs3$SSbD_category)
  categories_count_enriched = categories_count_enriched[names(categories_count)]
  
  percentages = categories_count_enriched/categories_count
  names(percentages) = names(categories_count_enriched)
  
  # Merge enrichment results with SSbD categories
  Annotate_AOPs2$SSbD_category = as.character(as.vector(Annotate_AOPs2$SSbD_category))
  AER <- merge(AER, Annotate_AOPs2, by.x = "TermID", by.y = "AOP")
  
  AER = cbind(AER, percentages = as.numeric(percentages[AER$SSbD_category]))
  AER = cbind(AER, categories_count = as.numeric(categories_count[AER$SSbD_category]))
  AER = cbind(AER, categories_count_enriched = as.numeric(categories_count_enriched[AER$SSbD_category]))
  
  AER$BMD = as.numeric(AER$BMD)
  
  AER$SSbD_category[AER$SSbD_category == "Reproductive/developmental toxicity, Endocrine disruption (human health)"] = "Reproductive/developmental toxicity \n Endocrine disruption (human health)"
  AER$SSbD_category[AER$SSbD_category == "Carcinogenicity, Endocrine disruption (human health)"] = "Carcinogenicity \n Endocrine disruption (human health)"
  tab = table(AER$SSbD_category, AER$Experiment)
  
  AER = AER[order(AER$categories_count,decreasing = F),]
  AER$SSbD_category = factor(AER$SSbD_category, levels = unique(AER$SSbD_category))
  AER$SSbD_category = paste(AER$SSbD_category, " (", AER$categories_count_enriched, ")", sep = "")

  AER$SSbD_category = as.factor(AER$SSbD_category)
  

  # For each Experiment, order SSbD_category by decreasing categories_count_enriched
  AER <- AER %>%
    group_by(Experiment) %>%
    mutate(
      SSbD_category_ordered = factor(
        SSbD_category,
        levels = unique(SSbD_category[order(-categories_count)])
      )
    ) %>%
    ungroup()
  
  p = ggplot(AER, aes(x = SSbD_category_ordered, y = BMD, fill = categories_count_enriched)) +
    geom_boxplot() +
    labs(
      title = "Distribution of BMD by SSbD Category",
      x = "SSbD Category",
      y = "BMD"
    ) +
    facet_wrap(~Experiment, scales = "free") +
    theme_minimal(base_size = text_size) +
    theme(
      axis.title.x = element_text(size = text_size),
      axis.title.y = element_text(size = text_size),
      axis.text.x = element_text(size = text_size - 4, angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(size = text_size - 4),
      legend.title = element_text(size = text_size - 2),
      legend.text = element_text(size = text_size - 4))
  
  return(p)
}


#' Heatmap of Average BMD Values for AOPs Within a Specific SSbD Category
#'
#' This function filters AOP enrichment results for a selected SSbD category,
#' calculates the log-transformed BMD values, and generates a heatmap showing
#' the average BMD values for each AOP across different experiments.
#'
#' @param aop_enrichment_results A data frame of AOP enrichment results.
#'   Must include columns: "TermID", "Experiment", "BMD", "BMDL", "BMDU".
#' @param Annotate_AOPs A data frame of AOP annotations.
#'   Must include columns: "AOP", "AOP_name", "SSbD_category", and "Organ".
#' @param ssbd_category Character. The name of the SSbD category to filter (e.g., "Carcinogenicity").
#' @param low_color Character. Color for low BMD values (default: "#235888").
#' @param high_color Character. Color for high BMD values (default: "#F7BD03").
#' @param text_size Numeric. Base font size for plot elements (default: 16).
#'
#' @return A `ggplot` object displaying a heatmap of log(BMD) values for AOPs in the selected SSbD category.
#'
#' @examples
#' \dontrun{
#'   p <- plot_average_bmd_in_aop_of_a_ssbd_category(aop_enrichment_results, Annotate_AOPs, "Carcinogenicity")
#'   print(p)
#' }
#'
#' @import ggplot2
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter
#' @export
plot_average_bmd_in_aop_of_a_ssbd_category <- function(aop_enrichment_results,
                                                       Annotate_AOPs,
                                                       ssbd_category = "Carcinogenicity",
                                                       low_color = "#235888",
                                                       high_color = "#F7BD03",
                                                       text_size = 16) {
  
  # Extract unique AOP enrichment entries
  AER <- unique(aop_enrichment_results[, c("TermID", "Experiment", "BMD", "BMDL", "BMDU")])
  
  # Adjust SSbD category labels for STO types
  Annotate_AOPs2 <- Annotate_AOPs
  idx <- which(Annotate_AOPs2$SSbD_category == "Specific target organ toxicity")
  Annotate_AOPs2$Organ[is.na(Annotate_AOPs2$Organ)] <- "Unspecified"
  Annotate_AOPs2$SSbD_category[idx] <- paste0("STO_tox_", Annotate_AOPs2$Organ[idx])
  
  # Merge enrichment and annotation data
  AER <- merge(AER, Annotate_AOPs2, by.x = "TermID", by.y = "AOP")
  
  # Filter for specific SSbD category
  AER_filtered <- AER[AER$SSbD_category == ssbd_category, ]
  
  # Reshape data to wide format
  df_heatmap <- AER_filtered %>%
    select(AOP_name, Experiment, BMD) %>%
    pivot_wider(names_from = Experiment, values_from = BMD)
  
  # Convert to long format
  df_long <- df_heatmap %>%
    pivot_longer(cols = -AOP_name, names_to = "Experiment", values_to = "BMD")
  df_long$BMD <- as.numeric(df_long$BMD)
  df_long$BMD[is.na(df_long$BMD)] <- 0
  # df_long$BMD <- log(df_long$BMD)
  
  # Create heatmap
  heatmap_plot <- ggplot(df_long, aes(x = Experiment, y = AOP_name, fill = BMD)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = low_color, high = high_color, na.value = "grey90") +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(size = text_size - 4, angle = 45, hjust = 1),
      axis.title.x = element_text(size = text_size),
      axis.title.y = element_text(size = text_size),
      axis.text.y  = element_text(size = text_size - 4),
      legend.title = element_text(size = text_size - 2),
      legend.text  = element_text(size = text_size - 4)
    ) +
    labs(title = paste("Average BMD"),
         x = "Experiment",
         y = "AOP Name",
         fill = "BMD")
  
  return(heatmap_plot)
}

#' Boxplot of BMD Distribution Across SSbD Categories
#'
#' This function processes AOP enrichment results and annotations to produce
#' a boxplot of BMD values grouped by SSbD categories across experiments.
#' It filters out underrepresented categories (fewer than 3 entries) and
#' transforms specific target organ toxicity categories to include organ names.
#'
#' @param aop_enrichment_results A data frame containing AOP enrichment data.
#'   Must include columns: "TermID", "Experiment", "BMD", "BMDL", "BMDU".
#' @param Annotate_AOPs A data frame containing SSbD annotations.
#'   Must include: "AOP", "SSbD_category", "Organ".
#' @param fill_colors A vector of fill colors for the categories.
#'   Default includes 12 distinct colors.
#' @param text_size Numeric. Base text size for plot elements (default: 16).
#' @param angle_x Numeric. Angle for rotating x-axis labels (default: 80).
#'
#' @return A `ggplot` object showing BMD distributions across SSbD categories.
#'
#' @examples
#' \dontrun{
#'   p <- boxplot_bmd_distribution_across_ssbd_categories(aop_enrichment_results, Annotate_AOPs)
#'   print(p)
#' }
#'
#' @import ggplot2
#' @export
boxplot_bmd_distribution_across_ssbd_categories <- function(aop_enrichment_results,
                                                            Annotate_AOPs,
                                                            fill_colors = c("#235888", "#F7BD03", "#D9D7CD",
                                                                            "navyblue", "brown", "orange",
                                                                            "#357D8A", "royalblue", "#68A5D4",
                                                                            "#999999", "#E69F00", "#56B4E9"),
                                                            text_size = 16,
                                                            angle_x = 80) {
  library(ggplot2)
  
  # Extract relevant data
  AER <- unique(aop_enrichment_results[, c("TermID", "Experiment", "BMD", "BMDL", "BMDU")])
  
  # Adjust annotation categories
  Annotate_AOPs2 <- Annotate_AOPs
  idx <- which(Annotate_AOPs2$SSbD_category == "Specific target organ toxicity")
  Annotate_AOPs2$Organ[is.na(Annotate_AOPs2$Organ)] <- "Unspecified"
  Annotate_AOPs2$SSbD_category[idx] <- paste0("STO_tox_", Annotate_AOPs2$Organ[idx])
  
  # Merge and filter
  AER <- merge(AER, Annotate_AOPs2, by.x = "TermID", by.y = "AOP")
  AER$BMD <- as.numeric(AER$BMD)
  
  # Remove SSbD categories with <= 2 entries
  tot_count <- table(AER$SSbD_category)
  represented_categories <- names(tot_count)[tot_count > 2]
  AER <- AER[AER$SSbD_category %in% represented_categories, ]
  
  # Create boxplot
  p <- ggplot(AER, aes(x = SSbD_category, y = BMD, fill = SSbD_category)) +
    geom_boxplot() +
    facet_wrap(~Experiment, scales = "free") +
    labs(
      title = "Boxplot of BMD by SSbD Category",
      x = "SSbD Category",
      y = "BMD"
    ) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = angle_x, hjust = 1, size = text_size - 1),
      axis.text.y  = element_text(size = text_size),
      axis.title.x = element_text(size = text_size),
      axis.title.y = element_text(size = text_size),
      strip.text   = element_text(size = text_size),
      legend.position = "none",
      legend.title = element_text(
        
        size = text_size),
      legend.text  = element_text(size = text_size - 2)
    ) +
    scale_fill_manual(values = fill_colors)
  
  return(p)
}


render_AOP_for_visnet  =  function(gVars, input){
  experiment  =  input$experiment_selection_AOP_visplot
  time  =  input$time_point_selection_AOP_visplot
  
  ke_enrichment_results = gVars$KE_enrich_res
  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  
  KE = ke_enrichment_results[ke_enrichment_results$Experiment %in% experiment & ke_enrichment_results[,time_var] %in% time,]
  
  gVars$KE_gene_pairs  =  KE
  
  KE = unique(KE[,c("Aop","a.name")])
  
  path_to_be_selected = KE$Aop
  named_vector <- setNames(path_to_be_selected, KE[,"a.name"])
  
  # selectInput("AOP_visnetwork", "Enriched AOPs", 
  #             choices = named_vector,
  #             selected  =  named_vector[1])
  
  selectizeInput("AOP_visnetwork", 
                 label= "Enriched AOPs", 
                 choices=named_vector, 
                 selected = named_vector[1], 
                 multiple = FALSE,
                 options = NULL)
  
}  


compute_logFC_adj_matrix <- function(bmd_vector, log_base = exp(1)) {
  n <- length(bmd_vector)
  
  if (n < 2) {
    stop("The vector must have at least two values.")
  }
  
  # Initialize an n x n matrix with NA values.
  adj_matrix <- matrix(NA, nrow = n, ncol = n)
  rownames(adj_matrix) <- names(bmd_vector)
  colnames(adj_matrix) <- names(bmd_vector)
  
  # Compute logFC for each consecutive pair and fill the matrix.
  for (i in 1:(n - 1)) {
    ratio <- bmd_vector[i + 1] / bmd_vector[i]
    adj_matrix[i, i + 1] <- log(ratio, base = log_base)
  }
  
  return(adj_matrix)
}

create_aop_visnetwork <- function(
    aop_id,
    experiment,
    timepoint,
    ker,
    all_AOPs_and_their_KEs,
    ke_enrichment_results,
    optimal_models_stats,
    gene_id_type = "ENSEMBL",
    organism = "human",
    time_var = "timepoint",
    AOP_visnet_only_direct= T
) {
  library(visNetwork)
  library(scales)
  library(igraph)
  
  
  if(!aop_id %in% ke_enrichment_results$Aop){
    print("AOP not enriched! ")
    return(NULL)
  }
  
  genes_human = gVars$genes_human
  genes_mouse = gVars$genes_mouse
  genes_rat = gVars$genes_rat
  
  #loading graphs with precompiled knownedge on TF and Mirna regulation and PPI info
  graph_tf = load_ppi_data(organism,gene_id_type,graph_type = "tf")
  graph_mirna = load_ppi_data(organism,gene_id_type,graph_type = "mirna")
  graph = load_ppi_data(organism,gene_id_type,graph_type = "ppi")
  
  ker = ker[ker$aop==aop_id,]
  if(AOP_visnet_only_direct){
    ker = ker[ker$r.type=="adjacent",]
    
  }
  # get the edges between KEs in a specified AOP
  dataframe_edges = ker[ker$aop==aop_id,c(2,3)]
  colnames(dataframe_edges) = c("from","to")
  
  if(nrow(dataframe_edges)==0){
    print("No edges for this AOP")
    return(NULL)
  }
  
  g =  graph_from_data_frame(dataframe_edges, directed = TRUE)
  E(g)$type.r = ker[ker$aop==aop_id,"r.type"]
  
  # find AO of the AOP
  AO_df = all_AOPs_and_their_KEs[which(all_AOPs_and_their_KEs$aopwiki_id == aop_id & 
                                         all_AOPs_and_their_KEs$type == "AdverseOutcome"),"ke_wiki_id",drop = FALSE]
  target_AO = as.character(AO_df$ke_wiki_id)
  target_AO = target_AO[target_AO %in% V(g)$name]
  
  # find the MIE of the AOP
  
  MIE_df = all_AOPs_and_their_KEs[which(all_AOPs_and_their_KEs$aopwiki_id == aop_id & 
                                          all_AOPs_and_their_KEs$type == "MolecularInitiatingEvent"),"ke_wiki_id", drop = FALSE]
  starting_MIE = as.character(MIE_df$ke_wiki_id)
  starting_MIE = starting_MIE[starting_MIE %in% V(g)$name]
  
  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  # Get info on the enrichment of the KEs of a specified AOP
  RR = ke_enrichment_results[ke_enrichment_results$TermID %in% V(g)$name &  
                             ke_enrichment_results$Experiment== experiment & 
                             ke_enrichment_results[,time_var] == timepoint,]
  
  # Get info on the enrichment of the KEs of a specified AOP
  # RR = ke_enrichment_results[ke_enrichment_results$TermID %in% V(g)$name &  
  #                              ke_enrichment_results$Experiment== paste(experiment, timepoint,sep="_"),]
  RR = unique(RR[, c("TermID","Ke_description","Experiment","relevantGenesInGeneSet","pval","Genes","BMD","BMDL","BMDU", "padj")])
  rownames(RR) = RR$TermID
  
  # Getting statistics for the genes in that KEs
  mod_stats = optimal_models_stats[optimal_models_stats$Experiment==experiment & 
                                     optimal_models_stats[,time_var] == timepoint,]
  rownames(mod_stats) = mod_stats$Feature
  
  if(length(starting_MIE)==0){
    print("No edges for this AOP")
    return(NULL)
  }
  
  # BFS visit of the AOP graph starting from the MIE
  bfs_result <- names(bfs(g, root = starting_MIE, mode = "out")$order)
  
  nodes = bfs_result
  nodes_bmd = RR[bfs_result,"BMD"]
  nodes_bmdl = RR[bfs_result,"BMDL"]
  nodes_bmdu = RR[bfs_result,"BMDU"]
  nodes_padj = RR[bfs_result,"padj"]
  nodes_genes = RR[bfs_result,"Genes"]
  nodes_Ke_description = RR[bfs_result,"Ke_description"]
  
  # For each KE in the AOP in the BFS visit order
  for(i in 1:(length(bfs_result)-1)){
    kei = bfs_result[i]
    
    # Taking the genes that contributes to the enrichment of the i-th node
    good_idx_i = which(RR$TermID %in% kei)
    if(length(good_idx_i)> 0){
      gi = unlist(strsplit(RR[good_idx_i,"Genes"][1],";"))
    }else{
      gi = c()
    }
    
    j = i+1 #fixing the next node in the visit
    kej = bfs_result[j]
    
    # Taking the genes that contribute to the enrichment of the j-th node
    good_idx_j = which(RR$TermID %in% kej)
    if(length(good_idx_j)> 0){
      gj = unlist(strsplit(RR[good_idx_j,"Genes"][1],";"))
    }else{
      gj = c()
    }
    
    # If both nodes have contributing genes, we can check if there is any regulatory or ppi edge between them
    if(length(gi)>0 & length(gj)>0){
      
      # Filter genes that actually exist in the graph
      gi_in_graph <- gi[gi %in% V(graph_tf)$name]
      gj_in_graph <- gj[gj %in% V(graph_tf)$name]
      
      # Create all possible pairs from gi → gj
      gene_pairs <- expand.grid(from = gi_in_graph, to = gj_in_graph, stringsAsFactors = FALSE)
      
      # find TF relationships
      res_tf = check_edges_in_graph(graph_tf, gene_pairs, feature1 = "from", feature2 = "to")
      res_tf = res_tf[res_tf$`presence[rownames(edge_df)]` == T,]
      if(nrow(res_tf)>0) {
        res_tf = cbind(res_tf, type = "TF",weight = 1)
        nodes = c(nodes,res_tf$from,res_tf$to)
        nodes_bmd = c(nodes_bmd, mod_stats[c(res_tf$from,res_tf$to),"BMD"])
        
        nodes_bmdl = c(nodes_bmdl, mod_stats[c(res_tf$from,res_tf$to),"BMDL"])
        nodes_bmdu = c(nodes_bmdu, mod_stats[c(res_tf$from,res_tf$to),"BMDU"])
        nodes_padj = c(nodes_padj, mod_stats[c(res_tf$from,res_tf$to),"padj"])
        
        ms_genes = mod_stats[c(res_tf$from,res_tf$to),"Genes"]
        if(length(ms_genes)>0){
          ms_genes = unlist(strsplit(x = ms_genes,split = ";"))
          ms_genes_symb = paste(convert_genes_to_symbol(ms_genes, gene_id_type,organism,
                                                        genes_human=genes_human, 
                                                        genes_mouse=genes_mouse, 
                                                        genes_rat=genes_rat), collapse = ";")
          nodes_genes = c(nodes_genes, ms_genes_symb)
        }else{
          nodes_genes = c(nodes_genes, ms_genes)
        }
        
        
        nodes_Ke_description = c(nodes_Ke_description, mod_stats[c(res_tf$from,res_tf$to),"Ke_description"])
      }
      
      # Filter genes that actually exist in the graph
      gi_in_graph <- gi[gi %in% V(graph_mirna)$name]
      gj_in_graph <- gj[gj %in% V(graph_mirna)$name]
      
      # Create all possible pairs from gi → gj
      gene_pairs <- expand.grid(from = gi_in_graph, to = gj_in_graph, stringsAsFactors = FALSE)
      
      # Find mirna relationships
      res_mirna = check_edges_in_graph(graph_mirna, gene_pairs, feature1 = "from", feature2 = "to")
      res_mirna = res_mirna[res_mirna$`presence[rownames(edge_df)]` == T,]
      if(nrow(res_mirna)>0) {
        res_mirna = cbind(res_mirna, type = "Mirna",weight = 1)
        nodes = c(nodes,res_mirna$from,res_mirna$to)
        nodes_bmd = c(nodes_bmd, mod_stats[c(res_mirna$from,res_mirna$to),"BMD"])
        
        nodes_bmdl = c(nodes_bmdl, mod_stats[c(res_mirna$from,res_mirna$to),"BMDL"])
        nodes_bmdu = c(nodes_bmdu, mod_stats[c(res_mirna$from,res_mirna$to),"BMDU"])
        nodes_padj = c(nodes_padj, mod_stats[c(res_mirna$from,res_mirna$to),"padj"])
        
        # nodes_genes = c(nodes_genes, mod_stats[c(res_mirna$from,res_mirna$to),"Genes"])
        
        ms_genes = mod_stats[c(res_mirna$from,res_mirna$to),"Genes"]
        
        if(length(ms_genes)>0){
          ms_genes = unlist(strsplit(x = ms_genes,split = ";"))
          ms_genes_symb = paste(convert_genes_to_symbol(ms_genes, gene_id_type,organism,
                                                        genes_human=genes_human, 
                                                        genes_mouse=genes_mouse, 
                                                        genes_rat=genes_rat), collapse = ";")
          nodes_genes = c(nodes_genes, ms_genes_symb)
        }else{
          nodes_genes = c(nodes_genes, ms_genes)
        }
        
        nodes_Ke_description = c(nodes_Ke_description, mod_stats[c(res_mirna$from,res_mirna$to),"Ke_description"])
        
      }
      
      # Filter genes that actually exist in the graph
      gi_in_graph <- gi[gi %in% V(graph)$name]
      gj_in_graph <- gj[gj %in% V(graph)$name]
      
      # Create all possible pairs from gi → gj
      gene_pairs <- cbind(expand.grid(from = gi_in_graph, to = gj_in_graph, stringsAsFactors = FALSE))
      
      # Find PPI support
      res_ppi = check_edges_in_graph(graph, gene_pairs, feature1 = "from", feature2 = "to")
      res_ppi = res_ppi[res_ppi$`presence[rownames(edge_df)]` == T,]
      if(nrow(res_ppi)>0) {
        res_ppi = cbind(res_ppi, "PPI")
        res_ppi = data.frame(from = kei, to = kej, presence= TRUE, type = "PPI", weight = nrow(res_ppi))
        # Initialize result (assume res is already defined or create empty if not)
        res <- res_ppi[0,c(1,2,4,5)]  # use correct structure if res is pre-existing
        colnames(res)=c("from","to","type","weight")
        # Append only non-empty data frames
      }else{
        res = data.frame()
      }
      
      # combind triples related to TF and Mirnas
      dfs <- list(res_tf,res_mirna)
      for (df in dfs) {
        if (nrow(df) > 0) {
          df = df[,c(1,2,4,5)]
          colnames(df) = c("from","to","type","weight")
          res <- rbind(res, df)
        }
      }
      
      # If these triplets exist, we can add them to the edges dataframe
      if(nrow(res)>0){
        
        #add edges from KE to genes
        df1 = cbind(kei, res$from, type = "KE_gene",weight = 1)
        
        # add edges from genes to KE
        df2 = cbind(res$to, kej, type = "gene_KE",weight=1)
        colnames(df1) = colnames(df2) = c("from","to","type","weight")
        
        # add edges from genes to genes
        df3 = cbind(res[c("from","to")], type = res$type, weight = res$weight)
        colnames(df1) = colnames(df2) = colnames(df3) = c("from","to","type","weight")
        
        new_edges = rbind(df1, df2, df3)
        
        #ppi support
        if(nrow(res_ppi)>0){
          new_edges = rbind(new_edges,res_ppi[,c("from","to","type","weight")])
        }
        if(ncol(dataframe_edges)==2){ # if no extra info have been added to the dataframe yet
          dataframe_edges = rbind(cbind(dataframe_edges, type="KEr", weight = 1),new_edges)
        }else{
          dataframe_edges = rbind(dataframe_edges,new_edges,res_ppi[,c("from","to","type","weight")])
        }
      }else{ #if not triples for regulatory sypport exists
        
        new_edges = c()
        if(nrow(res_ppi)>0){ # check if ppi support exists
          new_edges = rbind(new_edges,res_ppi[,c("from","to","type","weight")])
        }
        if(!is.null(new_edges)){
          if(ncol(dataframe_edges)==2){ # if no extra info have been added to the dataframe yet
            dataframe_edges = rbind(cbind(dataframe_edges, type="KEr", weight = 1),new_edges)
          }else{
            dataframe_edges = rbind(dataframe_edges,new_edges,res_ppi[,c("from","to","type","weight")])
          }
        }
      }
    } # end length(gi)>0 & length(gj)>0
    print(dataframe_edges)
    # } end for j
  } # end for i
  
  # add info about nodes in the graph
  
  nodes_bmd = as.numeric(nodes_bmd)
  names(nodes_bmd) = names(nodes_bmdl) = 
    names(nodes_bmdu)  = nodes
  
  # convert node_genes to symbols  
  for(i in 1:length(nodes_genes)){
    if(length(nodes_genes[i])>0){
      ggi = unlist(strsplit(x = nodes_genes[i], split = ";"))
      nodes_genes[i] = paste(convert_genes_to_symbol(genes = ggi, gene_id_type,organism,
                                                     genes_human=genes_human, 
                                                     genes_mouse=genes_mouse, 
                                                     genes_rat=genes_rat),collapse = ";")
    }
  }
  
  idx = grep(pattern = "Event",x = nodes)
  names(nodes_padj) = names(nodes_genes) = names(nodes_Ke_description) = nodes[idx]
  
  
  if(ncol(dataframe_edges) == 2){ # in case no TF, mirna nor PPI support is found
    dataframe_edges$type = "KEr"
    dataframe_edges$weight = 1
  }
  
  # retrieve avg. BMD values of KE
  bmd_vector = nodes_bmd[grep(pattern = "Event:",x = names(nodes_bmd))]
  
  # Identify relationships between avg BMD across consecutive KEs
  logFC_values_log2 <- compute_logFC_adj_matrix(bmd_vector = bmd_vector, log_base = 2)
  
  # Set edge weight for KEr relationship to the log2 ratio of the BMD values of the KE
  idx_rows = which(dataframe_edges$type == "KEr")
  for(i in idx_rows){
    wi = logFC_values_log2[dataframe_edges[i,"from"], dataframe_edges[i,"to"]]
    if(!is.na(wi)){
      dataframe_edges$weight[i] = wi
    }
  }
  
  g =  graph_from_data_frame(dataframe_edges, directed = TRUE)
  
  # group node by node type
  group = rep("gene", length(V(g)$name))
  group[grep(pattern = "Event",x = V(g)$name)] = "Ke"
  
  # build nodes Data frames
  nodes <- data.frame(id = V(g)$name,
                      label = V(g)$name,
                      group = group,  # shapes by group
                      value = as.numeric(nodes_bmd[V(g)$name]), 
                      bmdl = as.numeric(nodes_bmdl[V(g)$name]),
                      bmdu = as.numeric(nodes_bmdu[V(g)$name]),
                      padj = as.numeric(nodes_padj[V(g)$name]),
                      genes = nodes_genes[V(g)$name],
                      nodes_Ke_description = nodes_Ke_description[V(g)$name]
  ) # continuous value
  
  for(i in 1:nrow(nodes)){
    if(nodes$group[i]=="gene"){
      ggi = nodes$id[i]
      ggi_s = paste(convert_genes_to_symbol(genes = ggi, gene_id_type,organism,
                                            genes_human=genes_human, 
                                            genes_mouse=genes_mouse, 
                                            genes_rat=genes_rat),collapse = ";")
      nodes$label[i] = ggi_s
    }
  }
  
  # Fix node description and grouping
  idx_nodes = intersect(which(is.na(nodes$nodes_Ke_description)), grep(pattern = "Event:",x = nodes$label))
  all_ke_id = unique(merge(nodes,aop_ke_table_hure,by.x = "id",by.y = "Ke")[,c("id","Ke_description")])
  rownames(all_ke_id) = all_ke_id$id
  nodes$nodes_Ke_description[idx_nodes] = all_ke_id[nodes$id[idx_nodes],"Ke_description"]
  # if(length(idx_nodes)>0){
  #   nodes$nodes_Ke_description[idx_nodes] = RR[RR$TermID[idx_nodes],"Ke_description"]
  # }
  
  nodes$group[which(nodes$id == starting_MIE)] = "MIE"
  nodes$group[which(nodes$id == target_AO)] = "AO"
  
  # Map groups to shapes
  nodes$shape <- rep("ellipse", length(V(g)$name))
  nodes$shape[nodes$group == "Ke"] = "box"
  nodes$shape[nodes$group == "MIE"] = "triangle"
  nodes$shape[nodes$group == "AO"] = "star"
  
  nodes$title <- mapply(function(name, desc, value, bmdl, bmdu, padj, genes) {
    paste0(
      if (!is.na(name)) paste0("<p> Node: ", name, "</p>") else "",
      if (!is.na(desc)) paste0("<p> Description: ", desc, "</p>") else "",
      if (!is.na(value)) paste0("<p> BMD: ", value, "</p>") else "",
      if (!is.na(bmdl)) paste0("<p> BMDL: ", bmdl, "</p>") else "",
      if (!is.na(bmdu)) paste0("<p> BMDU: ", bmdu, "</p>") else "",
      if (!is.na(padj)) paste0("<p> padj: ", padj, "</p>") else "",
      if (!is.na(genes)) paste0("<p> genes: ", genes, "</p>") else ""
    )
  },
  nodes$label,
  nodes$nodes_Ke_description,
  nodes$value,
  nodes$bmdl,
  nodes$bmdu,
  nodes$padj,
  nodes$genes
  )
  
  # Define color palette function
  pal <- colorRampPalette(c("#8AC4D0", "#d89b00"))
  

    # Scale non-NA values to [0, 1] and assign colors
  value_non_na <- nodes$value[!is.na(nodes$value)]
  if(!all(is.na(value_non_na))){
    scaled_values <- scales::rescale(value_non_na, to = c(0, 1))
    colors <- pal(100)[as.numeric(cut(scaled_values, breaks = 100))]
    nodes$color[!is.na(nodes$value)] <- colors
    nodes$color[is.na(nodes$value)] <- "#D3D3D3"
  }else{
    nodes$color <- "#D3D3D3"
  }

  
  idx_ke_n = grep(x = nodes$id, pattern = "Event:")
  nodes$label[idx_ke_n] = nodes$nodes_Ke_description[idx_ke_n]
  
  # Create edges dataframe
  edges <- igraph::as_data_frame(upgrade_graph(g), what = "edges")
  
  # Define custom colors for each edge group
  edge_colors <- c(
    KEr = "black",
    KE_gene = "gray",
    gene_KE = "gray",
    TF = "#DC143C",
    Mirna = "#FF8C00",
    PPI = "#228B22"
  )
  
  # Add the color column to edges
  edges$color <- edge_colors[edges$type]
  edges$width <- scales::rescale(abs(as.numeric(edges$weight)), to = c(1, 4))
  edges$sign <- sign(as.numeric(edges$weight))
  
  edges = unique(edges)
  
  # if(sum(edges$type =="PPI")>0){
  #   # Create corresponding legend entries
  #   ppi_legend <- data.frame(
  #     color = "#228B22",
  #     label = paste("", edges$weight[edges$type =="PPI"]),
  #     width = edges$width[edges$type =="PPI"]
  #   )
  #   ledges <- rbind(
  #     data.frame(
  #       color = c("black", "gray", "gray", "#DC143C", "#FF8C00"),
  #       label = c("KEr", "KE_gene", "gene_KE", "TF", "Mirna"),
  #       width = NA
  #     ),
  #     ppi_legend
  #   )
  # }else{
  #   ledges <- rbind(
  #     data.frame(
  #       color = c("black", "gray", "gray", "#DC143C", "#FF8C00"),
  #       label = c("KEr", "KE_gene", "gene_KE", "TF", "Mirna"),
  #       width = NA
  #     )
  #   )
  # }
  
  ledges <- rbind(
    data.frame(
      color = c("black", "gray", "gray", "#DC143C", "#FF8C00","#228B22"),
      label = c("KEr", "KE_gene", "gene_KE", "TF", "Mirna","PPI"),
      width = NA,
      arrows = c("to", "", "", "to", "", ""),  # "to" = arrow, "" = none
      dashes = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)  # TRUE = dashed
    )
  )
  
  ledges = ledges[which(ledges$label %in% edges$type),]
  
  ledges <- ledges %>%
    # collapse KE_gene + gene_KE into a single row
    mutate(label = ifelse(label %in% c("KE_gene", "gene_KE"),
                          "KE_gene / gene_KE", label)) %>%
    distinct(color, label, width, arrows, dashes, .keep_all = TRUE)
  
  # build legend for BMD values
  legend_color_nodes <- list()
  if (length(value_non_na) == 1) {
    legend_color_nodes <- list(
      list(label = paste0("BMD: ", round(scaled_values, 2)), shape = "dot",
           color = list(background = colors, border = "black"))
    )
  } else if (length(value_non_na) == 2 ) {
    min_val <- which.min(value_non_na)
    max_val <- which.max(value_non_na)
    
    legend_color_nodes <- list(
      list(label = paste0("Min: ", round(value_non_na[min_val], 2)), shape = "dot",
           color = list(background = colors[min_val], border = "black")),
      list(label = paste0("Max: ", round(value_non_na[max_val], 2)), shape = "dot",
           color = list(background = colors[max_val], border = "black"))
    )
  }else if (length(value_non_na) > 2 ) {
    min_val <- which.min(value_non_na)
    max_val <- which.max(value_non_na)
    med <- mean(value_non_na)
    avg_val <- which.min(abs(value_non_na - med))
    
    legend_color_nodes <- list(
      list(label = paste0("Min: ", round(value_non_na[min_val], 2)), shape = "dot",
           color = list(background = colors[min_val], border = "black")),
      list(label = paste0("Median: ", round(value_non_na[avg_val], 2)), shape = "dot",
           color = list(background = colors[avg_val], border = "black")),
      list(label = paste0("Max: ", round(value_non_na[max_val], 2)), shape = "dot",
           color = list(background = colors[max_val], border = "black"))
    )
  }
  
  edges$title = paste0("<p> Edge Type: ", edges$type,  "</p>",
                       "<p> Weight: ", round(as.numeric(edges$weight),2), "</p>")
  
  edges$arrows = "to"
  edges$arrows[edges$type == "KE_gene"] = ""
  edges$arrows[edges$type == "gene_KE"] = ""
  edges$arrows[edges$type == "PPI"] = ""
  edges$arrows[edges$type == "Mirna"] = ""
  
  edges$dashes = FALSE
  edges$dashes[edges$type == "KE_gene"] = TRUE
  edges$dashes[edges$type == "gene_KE"] = TRUE
  edges$dashes[edges$type == "Mirna"] = TRUE
  
  nodes$physics = TRUE
  nodes$physics[nodes$group == "gene"] = FALSE
  
  # visNetwork(nodes, edges) %>%
  #   visNodes(shape = "dot") %>%
  #   visEdges(arrows = "to") %>%
  #   visOptions(highlightNearest = T, nodesIdSelection = TRUE) %>%
  #   visLegend(
  #     ncol = 3,
  #     width = 0.3,
  #     useGroups = F,
  #     addEdges = ledges,
  #     addNodes = list(
  #       list(label = "Not enriched", shape = "box", color = list(background = "lightgray",border = "lightgray")),
  #       list(label = "MIE", shape = "triangle"),
  #       list(label = "AO", shape = "star"),
  #       list(label = "Key event", shape = "box"),
  #       list(label = "Gene", shape = "ellipse")) %>% append(legend_color_nodes)   
  #   ) %>% visLayout(improvedLayout = T)
  
  visNetwork(nodes, edges) %>%
    visNodes(shape = "dot") %>%
    visEvents(doubleClick = "
    function(params) {
      if(params.nodes.length > 0){
        var nodeId = params.nodes[0];
        this.body.data.nodes.update({id: nodeId, physics: false});
      }
    }")%>%
    visOptions(highlightNearest = T, nodesIdSelection = TRUE) %>%
    visLegend(
      ncol = 3,
      width = 0.3,
      useGroups = F,
      addEdges = ledges,
      addNodes = list(
        list(label = "Not enriched", shape = "box", color = list(background = "lightgray",border = "lightgray")),
        list(label = "", shape = "box", color = list(background = "transparent", border = "transparent")), # blank
        list(label = "", shape = "box", color = list(background = "transparent", border = "transparent")), # blank
        
        list(label = "MIE", shape = "triangle"),
        list(label = "Key event", shape = "box"),
        list(label = "AO", shape = "star"),
        
        list(label = "Gene", shape = "ellipse"),
        list(label = "", shape = "box", color = list(background = "transparent", border = "transparent")), # blank
        list(label = "", shape = "box", color = list(background = "transparent", border = "transparent"))  # blank
      ) %>% append(legend_color_nodes)   
      
    ) %>% visLayout(improvedLayout = T)
  
}

compute_borda_rank <- function(gi_in_graph, graph, tf_list, mod_stats, stats_pairs) {
  library(igraph)
  
  # Check inputs
  if (!all(gi_in_graph %in% V(graph)$name)) {
    stop("Some genes in gi_in_graph are not found in the graph nodes.")
  }
  
  if(!is.null(stats_pairs)){
    idx = intersect(which(stats_pairs$`Feature 1` %in% gi_in_graph),
                    which(stats_pairs$`Feature 2` %in% gi_in_graph))
    stats_pairs = stats_pairs[idx,]
    colnames(stats_pairs)[c(3,7)] = c("f1","f2")
    library(reshape2)
    square_matrix <- acast(stats_pairs, f1 ~ f2, value.var = "CorGenePatteerns")
    avg_corr = rowSums(square_matrix)/nrow(square_matrix)
    
    
    # Compute feature values
    is_gi_TF <- gi_in_graph %in% tf_list
    names(is_gi_TF) = gi_in_graph
    degree_gi <- igraph::degree(graph, v = gi_in_graph)
    closeness_gi <- igraph::closeness(graph, v = gi_in_graph, normalized = TRUE)
    eigenvector_gi <- eigen_centrality(graph)$vector[gi_in_graph]
    bmd_gi <- mod_stats$BMD[match(gi_in_graph, mod_stats$Feature)]
    names(bmd_gi) = gi_in_graph
    
    # Create feature table
    df <- data.frame(
      gene = gi_in_graph,
      is_TF = is_gi_TF,
      degree = degree_gi,
      closeness = closeness_gi,
      eigenvector = eigenvector_gi,
      bmd = bmd_gi,
      avg_corr = avg_corr,
      stringsAsFactors = FALSE
    )
    
    # Rank features (higher is better for all except BMD)
    df$is_TF_rank     <- rank(-df$is_TF, ties.method = "average")
    df$degree_rank    <- rank(-df$degree, ties.method = "average")
    df$closeness_rank <- rank(-df$closeness, ties.method = "average")
    df$eigen_rank     <- rank(-df$eigenvector, ties.method = "average")
    df$avg_corr_rank  <- rank(-df$avg_corr, ties.method = "average")
    df$bmd_rank       <- rank(df$bmd, ties.method = "average")  # lower BMD is better
    
    # Compute Borda score and rank
    df$borda_score <- rowSums(df[, c("is_TF_rank", "degree_rank", "closeness_rank", "eigen_rank", "bmd_rank")], na.rm = TRUE)
    df$borda_rank <- rank(df$borda_score, ties.method = "min")
    
    borda_rank = df[order(df$borda_rank), c("gene", "borda_rank", "borda_score")]
    # Return sorted table
    return(list(borda_rank = borda_rank,df = df, square_matrix=square_matrix))
    
  }else{
    # Compute feature values
    is_gi_TF <- gi_in_graph %in% tf_list
    names(is_gi_TF) = gi_in_graph
    degree_gi <- igraph::degree(graph, v = gi_in_graph)
    closeness_gi <- igraph::closeness(graph, v = gi_in_graph, normalized = TRUE)
    eigenvector_gi <- eigen_centrality(graph)$vector[gi_in_graph]
    bmd_gi <- mod_stats$BMD[match(gi_in_graph, mod_stats$Feature)]
    names(bmd_gi) = gi_in_graph
    
    # Create feature table
    df <- data.frame(
      gene = gi_in_graph,
      is_TF = is_gi_TF,
      degree = degree_gi,
      closeness = closeness_gi,
      eigenvector = eigenvector_gi,
      bmd = bmd_gi,
      stringsAsFactors = FALSE
    )
    
    # Rank features (higher is better for all except BMD)
    df$is_TF_rank     <- rank(-df$is_TF, ties.method = "average")
    df$degree_rank    <- rank(-df$degree, ties.method = "average")
    df$closeness_rank <- rank(-df$closeness, ties.method = "average")
    df$eigen_rank     <- rank(-df$eigenvector, ties.method = "average")
    df$bmd_rank       <- rank(df$bmd, ties.method = "average")  # lower BMD is better
    
    # Compute Borda score and rank
    df$borda_score <- rowSums(df[, c("is_TF_rank", "degree_rank", "closeness_rank", "eigen_rank", "bmd_rank")], na.rm = TRUE)
    df$borda_rank <- rank(df$borda_score, ties.method = "min")
    
    borda_rank = df[order(df$borda_rank), c("gene", "borda_rank", "borda_score")]
    # Return sorted table
    return(list(borda_rank = borda_rank,df = df))
  }
  
}


#statistics must be the result of the gene pair comparison for experiment at timepoint.
create_aop_based_ppi_genegene_network <- function(
    aop_id,
    experiment,
    timepoint,
    statistics = NULL,
    ker,
    all_AOPs_and_their_KEs,
    ke_enrichment_results,
    optimal_models_stats,
    gene_id_type = "ENSEMBL",
    organism = "human",
    time_var = "timepoint",
    top_n_genes = 10
) {
  library(visNetwork)
  library(scales)
  library(igraph)
  
  if(!aop_id %in% ke_enrichment_results$Aop){
    print("AOP not enriched! ")
    return(NULL)
  }
  
  genes_human = gVars$genes_human
  genes_mouse = gVars$genes_mouse
  genes_rat = gVars$genes_rat
  
  #loading graphs with precompiled knownedge on TF and Mirna regulation and PPI info
  graph_tf = load_ppi_data(organism,gene_id_type,graph_type = "tf")
  graph_mirna = load_ppi_data(organism,gene_id_type,graph_type = "mirna")
  graph = load_ppi_data(organism,gene_id_type,graph_type = "ppi")
  
  #Get heads of all edges
  tf_list    <- unique(ends(graph_tf, E(graph_tf), names = T)[, 1])
  mirna_list <- unique(ends(graph_mirna, E(graph_mirna), names = FALSE)[, 1])
  
  ker = ker[ker$aop==aop_id,]
  ker = ker[ker$r.type=="adjacent",]
  # get the edges between KEs in a specified AOP
  dataframe_edges = ker[ker$aop==aop_id,c(2,3)]
  colnames(dataframe_edges) = c("from","to")
  
  if(nrow(dataframe_edges)==0){
    print("No edges for this AOP")
    return(NULL)
  }
  
  g =  graph_from_data_frame(dataframe_edges, directed = TRUE)
  E(g)$type.r = ker[ker$aop==aop_id,"r.type"]
  
  # find AO of the AOP
  AO_df = all_AOPs_and_their_KEs[which(all_AOPs_and_their_KEs$aopwiki_id == aop_id & 
                                         all_AOPs_and_their_KEs$type == "AdverseOutcome"),"ke_wiki_id",drop = FALSE]
  target_AO = as.character(AO_df$ke_wiki_id)
  target_AO = target_AO[target_AO %in% V(g)$name]
  
  # find the MIE of the AOP
  
  MIE_df = all_AOPs_and_their_KEs[which(all_AOPs_and_their_KEs$aopwiki_id == aop_id & 
                                          all_AOPs_and_their_KEs$type == "MolecularInitiatingEvent"),"ke_wiki_id", drop = FALSE]
  starting_MIE = as.character(MIE_df$ke_wiki_id)
  starting_MIE = starting_MIE[starting_MIE %in% V(g)$name]
  
  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  # Get info on the enrichment of the KEs of a specified AOP
  RR = ke_enrichment_results[ke_enrichment_results$TermID %in% V(g)$name &  
                               ke_enrichment_results$Experiment== experiment & 
                               ke_enrichment_results[,time_var] == timepoint,]
  
  
  # Get info on the enrichment of the KEs of a specified AOP
  # RR = ke_enrichment_results[ke_enrichment_results$TermID %in% V(g)$name &  
  #                              ke_enrichment_results$Experiment== paste(experiment, timepoint,sep="_"),]
  RR = unique(RR[, c("TermID","Ke_description","Experiment","relevantGenesInGeneSet","pval","Genes","BMD","BMDL","BMDU", "padj")])
  rownames(RR) = RR$TermID
  
  # Getting statistics for the genes in that KEs
  mod_stats = optimal_models_stats[optimal_models_stats$Experiment==experiment & 
                                     optimal_models_stats[,time_var] == timepoint,]
  rownames(mod_stats) = mod_stats$Feature
  
  # BFS visit of the AOP graph starting from the MIE
  bfs_result <- names(bfs(g, root = starting_MIE, mode = "out")$order)
  
  nodes = c()
  edges = c()
  
  # For each KE in the AOP in the BFS visit order
  for(i in 1:(length(bfs_result)-1)){
    kei = bfs_result[i]
    
    # Taking the genes that contributes to the enrichment of the i-th node
    good_idx_i = which(RR$TermID %in% kei)
    if(length(good_idx_i)> 0){
      gi = unlist(strsplit(RR[good_idx_i,"Genes"][1],";"))
      # Filter genes that actually exist in the graph
      gi_in_graph <- gi[gi %in% V(graph)$name]
      res = compute_borda_rank(gi_in_graph, graph, tf_list, mod_stats, stats_pairs = statistics)
      
      square_matrix_i = res$square_matrix
      gi_borda = res$borda_rank
      
      # if(nrow(gi_borda)> top_n_genes){
      #   idtfi = which(res$df$is_TF)
      #   g_index= union(idtfi, 1:(top_n_genes - length(idtfi)))
      #   # CONTINUE FROM HERE
      #   gi_borda = gi_borda[g_index,]
      # } 
      
      if(nrow(gi_borda)> top_n_genes){
        idtfi = which(res$df$is_TF)
        tf_genes = res$df$gene[idtfi]
        idtfi = which(gi_borda$gene %in% tf_genes)
        g_index= union(idtfi, 1:(top_n_genes - length(idtfi)))
        gi_borda = gi_borda[g_index,]
      } 
      
      nodes_i = res$df[rownames(gi_borda),]
      
      subnetwork_i = igraph::simplify(igraph::induced_subgraph(graph = graph, vids = rownames(gi_borda)))
      
      di = igraph::degree(subnetwork_i)
      gene_zero_degree = names(di)[di == 0]
      
      corr_based_edges = c()
      if(length(gene_zero_degree)>0){
        for(gzdi in 1:length(gene_zero_degree)){
          
          good_genes = rownames(gi_borda)
          
          if(nrow(square_matrix_i)<=1){
            next()
          }
          
          square_matrix_i = square_matrix_i[good_genes,good_genes]
          
          ggi_corr = names(
            which.max(as.data.frame(square_matrix_i[gene_zero_degree[gzdi], -which(colnames(square_matrix_i)== gene_zero_degree[gzdi]), drop = F]))
          )
          
          # ggi_corr = names(which.max(square_matrix_i[gene_zero_degree[gzdi], -which(colnames(square_matrix_i)== gene_zero_degree[gzdi])]))
          corr_based_edges = rbind(corr_based_edges, c(from = gene_zero_degree[gzdi],to = ggi_corr,type= "CORR"))
        }
      }
      
      edge_df_i <- igraph::as_data_frame(subnetwork_i, what = "edges")
      if(nrow(edge_df_i)>0){
        edge_df_i = cbind(edge_df_i, type = "PPI")
      }
      # edge_df_i = cbind(edge_df_i, type = "PPI")
      
      if(!is.null(corr_based_edges)){
        edge_df_i = rbind(edge_df_i, corr_based_edges)
      }
      
      nodes_i = cbind(kei,nodes_i[,c("gene", "is_TF", "degree", "closeness", "eigenvector", "bmd")])
      colnames(nodes_i)[1] = "TermID"
      
      nodes = rbind(nodes,nodes_i)
      edges = rbind(edges,edge_df_i)
      
    }else{
      gi = c()
    }
    
    j = i+1 #fixing the next node in the visit
    kej = bfs_result[j]
    
    # Taking the genes that contribute to the enrichment of the j-th node
    good_idx_j = which(RR$TermID %in% kej)
    if(length(good_idx_j)> 0){
      gj = unlist(strsplit(RR[good_idx_j,"Genes"][1],";"))
      gj_in_graph <- gj[gj %in% V(graph)$name]
      res_j = compute_borda_rank(gj_in_graph, graph, tf_list, mod_stats,stats_pairs = statistics)
      square_matrix_j = res_j$square_matrix
      
      gj_borda = res_j$borda_rank
      
      # if(nrow(gj_borda)> top_n_genes){
      #   idtfj = which(res_j$df$is_TF)
      #   gj_index= union(idtfj, 1:(top_n_genes - length(idtfj)))
      #   # CONTINUE FROM HERE
      #   gj_borda = gj_borda[gj_index,]
      # } 
      
      if(nrow(gj_borda)> top_n_genes){
        idtfj = which(res_j$df$is_TF)
        tf_genes = res_j$df$gene[idtfj]
        idtfj = which(gj_borda$gene %in% tf_genes)
        
        if(all(idtfj< top_n_genes)){
          g_index = 1:top_n_genes
        }else{
          g_index= union(idtfj, 1:(top_n_genes - length(idtfj[idtfj>top_n_genes])))
        }
        
        gj_borda = gj_borda[g_index,]
      } 
      
      nodes_j = res_j$df[rownames(gj_borda),]
      
      subnetwork_j = igraph::simplify(igraph::induced_subgraph(graph = graph, vids = rownames(gj_borda)))
      
      dj = igraph::degree(subnetwork_j)
      gene_zero_degree_j = names(dj)[dj == 0]
      
      corr_based_edges_j = c()
      
      if(length(gene_zero_degree_j)>0){
        for(gzdj in 1:length(gene_zero_degree_j)){
          
          good_genes_j = rownames(gj_borda)
          
          if(nrow(square_matrix_j)<=1){
            next()
          }
          
          square_matrix_j = square_matrix_j[good_genes_j,good_genes_j]
          
          ggi_corr = names(
            which.max(as.data.frame(square_matrix_j[gene_zero_degree_j[gzdj], -which(colnames(square_matrix_j)== gene_zero_degree_j[gzdj]), drop = F]))
          )
          
          # ggi_corr = names(which.max(square_matrix_j[gene_zero_degree_j[gzdj], -which(colnames(square_matrix_j)== gene_zero_degree_j[gzdj])]))
          corr_based_edges_j = rbind(corr_based_edges_j, c(from = gene_zero_degree_j[gzdj],to = ggi_corr,type= "CORR"))
        }
      }
      
      edge_df_j <- igraph::as_data_frame(subnetwork_j, what = "edges")
      if(nrow(edge_df_j)>0){
        edge_df_j = cbind(edge_df_j, type = "PPI")
      }
      # edge_df_j = cbind(edge_df_j, type = "PPI")
      
      if(!is.null(corr_based_edges_j)){
        edge_df_j = rbind(edge_df_j, corr_based_edges_j)
      }
      
      nodes_j = cbind(kej,nodes_j[,c("gene", "is_TF", "degree", "closeness", "eigenvector", "bmd")])
      colnames(nodes_j)[1] = "TermID"
      
      nodes = rbind(nodes,nodes_j)
      edges = rbind(edges,edge_df_j)
      
    }else{
      gj = c()
    }
    
    # If both nodes have contributing genes, we can check if there is any regulatory or ppi edge between them
    if(length(gi)>0 & length(gj)>0){
      
      # Create all possible pairs from gi → gj
      gene_pairs <- expand.grid(from = rownames(gi_borda), to = rownames(gj_borda), stringsAsFactors = FALSE)
      # find TF relationships
      res_tf = check_edges_in_graph(graph_tf, gene_pairs, feature1 = "from", feature2 = "to")
      res_tf = res_tf[res_tf$`presence[rownames(edge_df)]` == T,]
      
      if(nrow(res_tf)>0){
        edges = rbind(edges,cbind(res_tf[,c("from","to")], type = "TF"))
      }
      
      res_ppi = check_edges_in_graph(graph, gene_pairs, feature1 = "from", feature2 = "to")
      res_ppi = res_ppi[res_ppi$`presence[rownames(edge_df)]` == T,]
      if(nrow(res_ppi)>0){
        edges = rbind(edges,cbind(res_ppi[,c("from","to")], type = "PPI"))
        
      }
    }
    # } end for j
  } # end for i
  
  # nodes$group = nodes$TermID
  
  XX = as.data.frame(all_AOPs_and_their_KEs[which(all_AOPs_and_their_KEs$aopwiki_id == aop_id),])
  rownames(XX) = XX$ke_wiki_id
  
  ke_desc = c()
  ke_type = c()
  for(ti in nodes$TermID){
    ke_desc = c(ke_desc, Biological_system_annotations$key_event_name[Biological_system_annotations$ke %in% ti])
    ke_type = c(ke_type, XX$type[which(XX$ke_wiki_id %in% ti)])
  }
  
  
  nodes$group = paste(nodes$TermID, ke_type , ke_desc, sep = " ")
  
  # nodes$group = paste("ID:", nodes$TermID, " Type:", ke_type, " Name:", ke_desc, sep = "")
  
  # colnames(nodes)[1] = "group"
  
  nodes$degree = round(nodes$degree,2)
  nodes$closeness = round(nodes$closeness,2)
  nodes$eigenvector = round(nodes$eigenvector,2)
  nodes$bmd = round(nodes$bmd,2)
  
  nodes = unique(nodes)
  
  nodes_reduced <- nodes %>%
    group_by(gene) %>%
    summarise(
      group = paste(unique(group), collapse = ";\n"),
      is_TF = first(is_TF),  # assuming same value for each gene
      degree = first(degree),
      closeness = first(closeness),
      eigenvector = first(eigenvector),
      bmd = first(bmd),
      .groups = "drop"
    )
  
  nodes_reduced = as.data.frame(nodes_reduced)
  
  nodes_reduced$shape = "box"
  nodes_reduced$label = convert_genes_to_symbol(genes = nodes_reduced$gene, gene_id_type,organism,
                                                genes_human=genes_human, 
                                                genes_mouse=genes_mouse, 
                                                genes_rat=genes_rat)
  
  
  nodes_reduced$title <- mapply(function(gene,group, is_TF, degree, closeness, eigenvector, bmd) {
    paste0(
      if (!is.na(gene)) paste0("<p> Node: ", gene, "</p>") else "",
      if (!is.na(group)) paste0("<p> KE: ", group, "</p>") else "",
      if (!is.na(is_TF)) paste0("<p> Is TF?: ", is_TF, "</p>") else "",
      if (!is.na(degree)) paste0("<p> Degree: ", degree, "</p>") else "",
      if (!is.na(closeness)) paste0("<p> Closeness: ", closeness, "</p>") else "",
      if (!is.na(eigenvector)) paste0("<p> Eigenvector: ", eigenvector, "</p>") else "",
      if (!is.na(bmd)) paste0("<p> BMD: ", bmd, "</p>") else ""
    )
  },
  nodes_reduced$gene,
  nodes_reduced$group,
  nodes_reduced$is_TF,
  nodes_reduced$degree,
  nodes_reduced$closeness,
  nodes_reduced$eigenvector,
  nodes_reduced$bmd
  )
  
  # Define color palette function
  pal <- colorRampPalette(c("#8AC4D0", "#d89b00"))
  
  # Scale non-NA values to [0, 1] and assign colors
  value_non_na <- nodes_reduced$bmd[!is.na(nodes_reduced$bmd)]
  scaled_values <- scales::rescale(value_non_na, to = c(0, 1))
  colors <- pal(100)[as.numeric(cut(scaled_values, breaks = 100))]
  
  # nodes_reduced$color[!is.na(nodes_reduced$bmd)] <- colors
  # nodes_reduced$color[is.na(nodes_reduced$bmd)] <- "#D3D3D3"
  
  # Define custom colors for each edge group
  edge_colors <- c(
    PPI = "black",
    CORR = "#DC143C",
    TF = "darkgreen"
  )
  
  # Add the color column to edges
  edges$color <- edge_colors[edges$type]
  # edges$width <- scales::rescale(abs(as.numeric(edges$weight)), to = c(1, 4))
  
  edges = unique(edges)
  
  ledges <- rbind(
    data.frame(
      color = c("black","#DC143C"),#"darkgreen"),
      label = c("PPI", "CORR"),# "TF"),
      width = NA,
      arrows = c("","")
    )
  )
  
  # build legend for BMD values
  # legend_color_nodes <- list()
  # if (length(value_non_na) == 1) {
  #   legend_color_nodes <- list(
  #     list(label = paste0("BMD: ", round(scaled_values, 2)), shape = "dot",
  #          color = list(background = colors, border = "black"))
  #   )
  # } else if (length(value_non_na) == 2 ) {
  #   min_val <- which.min(value_non_na)
  #   max_val <- which.max(value_non_na)
  #   
  #   legend_color_nodes <- list(
  #     list(label = paste0("Min: ", round(value_non_na[min_val], 2)), shape = "dot",
  #          color = list(background = colors[min_val], border = "black")),
  #     list(label = paste0("Max: ", round(value_non_na[max_val], 2)), shape = "dot",
  #          color = list(background = colors[max_val], border = "black"))
  #   )
  # }else if (length(value_non_na) > 2 ) {
  #   min_val <- which.min(value_non_na)
  #   max_val <- which.max(value_non_na)
  #   med <- mean(value_non_na)
  #   avg_val <- which.min(abs(value_non_na - med))
  #   
  #   legend_color_nodes <- list(
  #     list(label = paste0("Min: ", round(value_non_na[min_val], 2)), shape = "dot",
  #          color = list(background = colors[min_val], border = "black")),
  #     list(label = paste0("Median: ", round(value_non_na[avg_val], 2)), shape = "dot",
  #          color = list(background = colors[avg_val], border = "black")),
  #     list(label = paste0("Max: ", round(value_non_na[max_val], 2)), shape = "dot",
  #          color = list(background = colors[max_val], border = "black"))
  #   )
  # }
  
  if(nrow(edges)>0){
    edges$title = paste0("<p> Edge Type: ", edges$type,  "</p>")
    edges$weight = 1
    
  }
  nodes_reduced$id = nodes_reduced$gene
  
  library(RColorBrewer)
  # group_levels <- unique(nodes_reduced$group)
  # color_palette <- brewer.pal(length(group_levels), "Set3")
  # group_colors <- setNames(color_palette, group_levels)
  # nodes_reduced$color <- group_colors[nodes_reduced$group]
  # 
  # visNetwork(nodes_reduced, edges) %>%
  #   visNodes(shape = "dot") %>%
  #   visEdges(length = 1) %>%
  #   visClusteringByGroup(groups = unique(nodes$group)) %>%
  #   visOptions(highlightNearest = T, nodesIdSelection = TRUE) %>%
  #   visLegend(
  #     ncol = 1,
  #     width = 0.4,
  #     useGroups = T,
  #     addEdges = ledges,
  #     #addNodes = list() #%>% append(legend_color_nodes)
  #   ) %>% visLayout(improvedLayout = T)
  
  group_levels <- unique(nodes_reduced$group)
  color_palette <- brewer.pal(length(group_levels), "Set3")
  group_colors <- setNames(color_palette, group_levels)
  nodes_reduced$color <- group_colors[nodes_reduced$group]
  
  # Build visNetwork plot
  net <- visNetwork(nodes_reduced, edges) %>%
    visNodes(shape = "dot")
  
  # Apply group styling
  for (grp in group_levels) {
    net <- net %>%
      visGroups(groupname = grp, color = unname(group_colors[grp]))
  }
  
  net <- net %>%
    visClusteringByGroup(groups = group_levels, label = "") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLegend(
      ncol = 1,
      width = 0.5,
      useGroups = TRUE,
      addEdges = ledges,
      addNodes = list()
    ) %>%
    visLayout(improvedLayout = TRUE)
  
  net
  
}



# create_aop_visnetwork <- function(
#     aop_id,
#     experiment,
#     timepoint,
#     ker,
#     all_AOPs_and_their_KEs,
#     ke_enrichment_results,
#     optimal_models_stats,
#     gene_id_type = "ENSEMBL",
#     organism = "human",
#     time_var = "timepoint"
# ) {
#   library(visNetwork)
#   library(scales)
#   library(igraph)
#   
#   
#   if(!aop_id %in% ke_enrichment_results$Aop){
#     print("AOP not enriched! ")
#     return(NULL)
#   }
#   
#   genes_human = gVars$genes_human
#   genes_mouse = gVars$genes_mouse
#   genes_rat = gVars$genes_rat 
#   
#   
#   #loading graphs with precompiled knownedge on TF and Mirna regulation and PPI info
#   graph_tf = load_ppi_data(organism,gene_id_type,graph_type = "tf")
#   graph_mirna = load_ppi_data(organism,gene_id_type,graph_type = "mirna")
#   graph = load_ppi_data(organism,gene_id_type,graph_type = "ppi")
#   
#   # get the edges between KEs in a specified AOP
#   dataframe_edges = ker[ker$aop==aop_id,c(2,3)]
#   colnames(dataframe_edges) = c("from","to")
#   
#   if(nrow(dataframe_edges)==0){
#     print("No edges for this AOP")
#     return(NULL)
#   }
#   
#   g =  graph_from_data_frame(dataframe_edges, directed = TRUE)
#   E(g)$type.r = ker[ker$aop==aop_id,"r.type"]
#   
#   # find AO of the AOP
#   AO_df = all_AOPs_and_their_KEs[which(all_AOPs_and_their_KEs$aopwiki_id == aop_id & 
#                                          all_AOPs_and_their_KEs$type == "AdverseOutcome"),"ke_wiki_id",drop = FALSE]
#   target_AO = as.character(AO_df$ke_wiki_id)
#   target_AO = target_AO[target_AO %in% V(g)$name]
#   
#   # find the MIE of the AOP
#   
#   MIE_df = all_AOPs_and_their_KEs[which(all_AOPs_and_their_KEs$aopwiki_id == aop_id & 
#                                           all_AOPs_and_their_KEs$type == "MolecularInitiatingEvent"),"ke_wiki_id", drop = FALSE]
#   starting_MIE = as.character(MIE_df$ke_wiki_id)
#   starting_MIE = starting_MIE[starting_MIE %in% V(g)$name]
#   
#   time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
#   # Get info on the enrichment of the KEs of a specified AOP
#   RR = ke_enrichment_results[ke_enrichment_results$TermID %in% V(g)$name &  
#                                ke_enrichment_results$Experiment== experiment & 
#                                ke_enrichment_results[,time_var] == timepoint,]
#                               
#   RR = unique(RR[, c("TermID","Ke_description","Experiment","relevantGenesInGeneSet","pval","Genes","BMD","BMDL","BMDU", "padj")])
#   rownames(RR) = RR$TermID
#   
#   # Getting statistics for the genes in that KEs
#   mod_stats = optimal_models_stats[optimal_models_stats$Experiment==experiment & 
#                                      optimal_models_stats[,time_var] == timepoint,]
#   rownames(mod_stats) = mod_stats$Feature
#   
#   # BFS visit of the AOP graph starting from the MIE
#   bfs_result <- names(bfs(g, root = starting_MIE, mode = "out")$order)
#   
#   nodes = bfs_result
#   nodes_bmd = RR[bfs_result,"BMD"]
#   nodes_bmdl = RR[bfs_result,"BMDL"]
#   nodes_bmdu = RR[bfs_result,"BMDU"]
#   nodes_padj = RR[bfs_result,"padj"]
#   nodes_genes = RR[bfs_result,"Genes"]
#   nodes_Ke_description = RR[bfs_result,"Ke_description"]
#   
#   # For each KE in the AOP in the BFS visit order
#   for(i in 1:(length(bfs_result)-1)){
#     kei = bfs_result[i]
#     
#     # Taking the genes that contributes to the enrichment of the i-th node
#     good_idx_i = which(RR$TermID %in% kei)
#     if(length(good_idx_i)> 0){
#       gi = unlist(strsplit(RR[good_idx_i,"Genes"][1],";"))
#     }else{
#       gi = c()
#     }
#     
#     j = i+1 #fixing the next node in the visit
#     kej = bfs_result[j]
#     
#     # Taking the genes that contribute to the enrichment of the j-th node
#     good_idx_j = which(RR$TermID %in% kej)
#     if(length(good_idx_j)> 0){
#       gj = unlist(strsplit(RR[good_idx_j,"Genes"][1],";"))
#     }else{
#       gj = c()
#     }
#     
#     # If both nodes have contributing genes, we can check if there is any regulatory or ppi edge between them
#     if(length(gi)>0 & length(gj)>0){
#       
#       # Filter genes that actually exist in the graph
#       gi_in_graph <- gi[gi %in% V(graph_tf)$name]
#       gj_in_graph <- gj[gj %in% V(graph_tf)$name]
#       
#       # Create all possible pairs from gi → gj
#       gene_pairs <- expand.grid(from = gi_in_graph, to = gj_in_graph, stringsAsFactors = FALSE)
#       
#       # find TF relationships
#       res_tf = check_edges_in_graph(graph_tf, gene_pairs)
#       res_tf = res_tf[res_tf$`presence[rownames(edge_df)]` == T,]
#       if(nrow(res_tf)>0) {
#         res_tf = cbind(res_tf, type = "TF",weight = 1)
#         nodes = c(nodes,res_tf$from,res_tf$to)
#         nodes_bmd = c(nodes_bmd, mod_stats[c(res_tf$from,res_tf$to),"BMD"])
#         
#         nodes_bmdl = c(nodes_bmdl, mod_stats[c(res_tf$from,res_tf$to),"BMDL"])
#         nodes_bmdu = c(nodes_bmdu, mod_stats[c(res_tf$from,res_tf$to),"BMDU"])
#         nodes_padj = c(nodes_padj, mod_stats[c(res_tf$from,res_tf$to),"padj"])
#         
#         ms_genes = mod_stats[c(res_tf$from,res_tf$to),"Genes"]
#         if(length(ms_genes)>0){
#           ms_genes = unlist(strsplit(x = ms_genes,split = ";"))
#           ms_genes_symb = paste(convert_genes_to_symbol(ms_genes, gene_id_type,organism,genes_human, genes_mouse, genes_rat), collapse = ";")
#           nodes_genes = c(nodes_genes, ms_genes_symb)
#         }else{
#           nodes_genes = c(nodes_genes, ms_genes)
#         }
#         
#         
#         nodes_Ke_description = c(nodes_Ke_description, mod_stats[c(res_tf$from,res_tf$to),"Ke_description"])
#       }
#       
#       # Filter genes that actually exist in the graph
#       gi_in_graph <- gi[gi %in% V(graph_mirna)$name]
#       gj_in_graph <- gj[gj %in% V(graph_mirna)$name]
#       
#       # Create all possible pairs from gi → gj
#       gene_pairs <- expand.grid(from = gi_in_graph, to = gj_in_graph, stringsAsFactors = FALSE)
#       
#       # Find mirna relationships
#       res_mirna = check_edges_in_graph(graph_mirna, gene_pairs)
#       res_mirna = res_mirna[res_mirna$`presence[rownames(edge_df)]` == T,]
#       if(nrow(res_mirna)>0) {
#         res_mirna = cbind(res_mirna, type = "Mirna",weight = 1)
#         nodes = c(nodes,res_mirna$from,res_mirna$to)
#         nodes_bmd = c(nodes_bmd, mod_stats[c(res_mirna$from,res_mirna$to),"BMD"])
#         
#         nodes_bmdl = c(nodes_bmdl, mod_stats[c(res_mirna$from,res_mirna$to),"BMDL"])
#         nodes_bmdu = c(nodes_bmdu, mod_stats[c(res_mirna$from,res_mirna$to),"BMDU"])
#         nodes_padj = c(nodes_padj, mod_stats[c(res_mirna$from,res_mirna$to),"padj"])
#         
#         # nodes_genes = c(nodes_genes, mod_stats[c(res_mirna$from,res_mirna$to),"Genes"])
#         
#         ms_genes = mod_stats[c(res_mirna$from,res_mirna$to),"Genes"]
#         
#         if(length(ms_genes)>0){
#           ms_genes = unlist(strsplit(x = ms_genes,split = ";"))
#           ms_genes_symb = paste(convert_genes_to_symbol(ms_genes, gene_id_type,organism,genes_human, genes_mouse, genes_rat), collapse = ";")
#           nodes_genes = c(nodes_genes, ms_genes_symb)
#         }else{
#           nodes_genes = c(nodes_genes, ms_genes)
#         }
#         
#         nodes_Ke_description = c(nodes_Ke_description, mod_stats[c(res_mirna$from,res_mirna$to),"Ke_description"])
#         
#       }
#       
#       # Filter genes that actually exist in the graph
#       gi_in_graph <- gi[gi %in% V(graph)$name]
#       gj_in_graph <- gj[gj %in% V(graph)$name]
#       
#       # Create all possible pairs from gi → gj
#       gene_pairs <- cbind(expand.grid(from = gi_in_graph, to = gj_in_graph, stringsAsFactors = FALSE))
#       
#       # Find PPI support
#       res_ppi = check_edges_in_graph(graph, gene_pairs)
#       res_ppi = res_ppi[res_ppi$`presence[rownames(edge_df)]` == T,]
#       if(nrow(res_ppi)>0) {
#         res_ppi = cbind(res_ppi, "PPI")
#         res_ppi = data.frame(from = kei, to = kej, presence= TRUE, type = "PPI", weight = nrow(res_ppi))
#         # Initialize result (assume res is already defined or create empty if not)
#         res <- res_ppi[0,c(1,2,4,5)]  # use correct structure if res is pre-existing
#         colnames(res)=c("from","to","type","weight")
#         # Append only non-empty data frames
#       }else{
#         res = data.frame()
#       }
#       
#       # combind triples related to TF and Mirnas
#       dfs <- list(res_tf,res_mirna)
#       for (df in dfs) {
#         if (nrow(df) > 0) {
#           df = df[,c(1,2,4,5)]
#           colnames(df) = c("from","to","type","weight")
#           res <- rbind(res, df)
#         }
#       }
#       
#       # If these triplets exist, we can add them to the edges dataframe
#       if(nrow(res)>0){
#         
#         #add edges from KE to genes
#         df1 = cbind(kei, res$from, type = "KE_gene",weight = 1)
#         
#         # add edges from genes to KE
#         df2 = cbind(res$to, kej, type = "gene_KE",weight=1)
#         colnames(df1) = colnames(df2) = c("from","to","type","weight")
#         
#         # add edges from genes to genes
#         df3 = cbind(res[c("from","to")], type = res$type, weight = res$weight)
#         colnames(df1) = colnames(df2) = colnames(df3) = c("from","to","type","weight")
#         
#         new_edges = rbind(df1, df2, df3)
#         
#         #ppi support
#         if(nrow(res_ppi)>0){
#           new_edges = rbind(new_edges,res_ppi[,c("from","to","type","weight")])
#         }
#         if(ncol(dataframe_edges)==2){ # if no extra info have been added to the dataframe yet
#           dataframe_edges = rbind(cbind(dataframe_edges, type="KEr", weight = 1),new_edges)
#         }else{
#           dataframe_edges = rbind(dataframe_edges,new_edges,res_ppi[,c("from","to","type","weight")])
#         }
#       }else{ #if not triples for regulatory sypport exists
#         
#         new_edges = c()
#         if(nrow(res_ppi)>0){ # check if ppi support exists
#           new_edges = rbind(new_edges,res_ppi[,c("from","to","type","weight")])
#         }
#         if(!is.null(new_edges)){
#           if(ncol(dataframe_edges)==2){ # if no extra info have been added to the dataframe yet
#             dataframe_edges = rbind(cbind(dataframe_edges, type="KEr", weight = 1),new_edges)
#           }else{
#             dataframe_edges = rbind(dataframe_edges,new_edges,res_ppi[,c("from","to","type","weight")])
#           }
#         }
#       }
#     } # end length(gi)>0 & length(gj)>0
#     print(dataframe_edges)
#     # } end for j
#   } # end for i
#   
#   # add info about nodes in the graph
#   
#   nodes_bmd = as.numeric(nodes_bmd)
#   names(nodes_bmd) = names(nodes_bmdl) = 
#     names(nodes_bmdu)  = nodes
#   
#   # convert node_genes to symbols  
#   for(i in 1:length(nodes_genes)){
#     if(length(nodes_genes[i])>0){
#       ggi = unlist(strsplit(x = nodes_genes[i], split = ";"))
#       ggi_c = convert_genes_to_symbol(genes = ggi, gene_id_type,organism,genes_human, genes_mouse, genes_rat)
#       nodes_genes[i] = paste(ggi_c,collapse = ";")
#     }
#   }
#   
#   idx = grep(pattern = "Event",x = nodes)
#   names(nodes_padj) = names(nodes_genes) = names(nodes_Ke_description) = nodes[idx]
#   
#   
#   if(ncol(dataframe_edges) == 2){ # in case no TF, mirna nor PPI support is found
#     dataframe_edges$type = "KEr"
#     dataframe_edges$weight = 1
#   }
#   
#   # retrieve avg. BMD values of KE
#   bmd_vector = nodes_bmd[grep(pattern = "Event:",x = names(nodes_bmd))]
#   
#   # Identify relationships between avg BMD across consecutive KEs
#   logFC_values_log2 <- compute_logFC_adj_matrix(bmd_vector = bmd_vector, log_base = 2)
#   
#   # Set edge weight for KEr relationship to the log2 ratio of the BMD values of the KE
#   idx_rows = which(dataframe_edges$type == "KEr")
#   for(i in idx_rows){
#     wi = logFC_values_log2[dataframe_edges[i,"from"], dataframe_edges[i,"to"]]
#     if(!is.na(wi)){
#       dataframe_edges$weight[i] = wi
#     }
#   }
#   
#   g =  graph_from_data_frame(dataframe_edges, directed = TRUE)
#   
#   # group node by node type
#   group = rep("gene", length(V(g)$name))
#   group[grep(pattern = "Event",x = V(g)$name)] = "Ke"
#   
#   # build nodes Data frames
#   nodes <- data.frame(id = V(g)$name,
#                       label = V(g)$name,
#                       group = group,  # shapes by group
#                       value = as.numeric(nodes_bmd[V(g)$name]), 
#                       bmdl = as.numeric(nodes_bmdl[V(g)$name]),
#                       bmdu = as.numeric(nodes_bmdu[V(g)$name]),
#                       padj = as.numeric(nodes_padj[V(g)$name]),
#                       genes = nodes_genes[V(g)$name],
#                       nodes_Ke_description = nodes_Ke_description[V(g)$name]
#   ) # continuous value
#   
#   for(i in 1:nrow(nodes)){
#     if(nodes$group[i]=="gene"){
#       ggi = nodes$id[i]
#       ggi_s = paste(convert_genes_to_symbol(genes = ggi, gene_id_type,organism,genes_human, genes_mouse, genes_rat),collapse = ";")
#       nodes$label[i] = ggi_s
#     }
#   }
#   
#   # Fix node description and grouping
#   idx_nodes = intersect(which(is.na(nodes$nodes_Ke_description)), grep(pattern = "Event:",x = nodes$label))
#   if(length(idx_nodes)>0){
#     nodes$nodes_Ke_description[idx_nodes] = RR[RR$TermID[idx_nodes],"Ke_description"]
#   }
#   
#   nodes$group[which(nodes$id == starting_MIE)] = "MIE"
#   nodes$group[which(nodes$id == target_AO)] = "AO"
#   
#   # Map groups to shapes
#   nodes$shape <- rep("ellipse", length(V(g)$name))
#   nodes$shape[nodes$group == "Ke"] = "box"
#   nodes$shape[nodes$group == "MIE"] = "triangle"
#   nodes$shape[nodes$group == "AO"] = "star"
#   
#   nodes$title <- mapply(function(name, desc, value, bmdl, bmdu, padj, genes) {
#     paste0(
#       if (!is.na(name)) paste0("<p> Node: ", name, "</p>") else "",
#       if (!is.na(desc)) paste0("<p> Description: ", desc, "</p>") else "",
#       if (!is.na(value)) paste0("<p> BMD: ", value, "</p>") else "",
#       if (!is.na(bmdl)) paste0("<p> BMDL: ", bmdl, "</p>") else "",
#       if (!is.na(bmdu)) paste0("<p> BMDU: ", bmdu, "</p>") else "",
#       if (!is.na(padj)) paste0("<p> padj: ", padj, "</p>") else "",
#       if (!is.na(genes)) paste0("<p> genes: ", genes, "</p>") else ""
#     )
#   },
#   nodes$label,
#   nodes$nodes_Ke_description,
#   nodes$value,
#   nodes$bmdl,
#   nodes$bmdu,
#   nodes$padj,
#   nodes$genes
#   )
#   
#   # Define color palette function
#   pal <- colorRampPalette(c("#8AC4D0", "#d89b00"))
#   
#   # Scale non-NA values to [0, 1] and assign colors
#   value_non_na <- nodes$value[!is.na(nodes$value)]
#   scaled_values <- scales::rescale(value_non_na, to = c(0, 1))
#   colors <- pal(100)[as.numeric(cut(scaled_values, breaks = 100))]
#   
#   nodes$color[!is.na(nodes$value)] <- colors
#   nodes$color[is.na(nodes$value)] <- "#D3D3D3"
#   
#   
#   # Create edges dataframe
#   edges <- igraph::as_data_frame(upgrade_graph(g), what = "edges")
#   
#   # Define custom colors for each edge group
#   edge_colors <- c(
#     KEr = "black",
#     KE_gene = "gray",
#     gene_KE = "gray",
#     TF = "#DC143C",
#     Mirna = "#FF8C00",
#     PPI = "#228B22"
#   )
#   
#   # Add the color column to edges
#   edges$color <- edge_colors[edges$type]
#   edges$width <- scales::rescale(abs(as.numeric(edges$weight)), to = c(1, 4))
#   edges$sign <- sign(as.numeric(edges$weight))
#   
#   edges = unique(edges)
#   
#   if(sum(edges$type =="PPI")>0){
#     # Create corresponding legend entries
#     ppi_legend <- data.frame(
#       color = "#228B22",
#       label = paste("", edges$weight[edges$type =="PPI"]),
#       width = edges$width[edges$type =="PPI"]
#     )
#     ledges <- rbind(
#       data.frame(
#         color = c("black", "gray", "gray", "#DC143C", "#FF8C00"),
#         label = c("KEr", "KE_gene", "gene_KE", "TF", "Mirna"),
#         width = NA
#       ),
#       ppi_legend
#     )
#   }else{
#     ledges <- rbind(
#       data.frame(
#         color = c("black", "gray", "gray", "#DC143C", "#FF8C00"),
#         label = c("KEr", "KE_gene", "gene_KE", "TF", "Mirna"),
#         width = NA
#       )
#     )
#   }
#   
#   # build legend for BMD values
#   legend_color_nodes <- list()
#   if (length(value_non_na) == 1) {
#     legend_color_nodes <- list(
#       list(label = paste0("BMD: ", round(scaled_values, 2)), shape = "dot",
#            color = list(background = colors, border = "black"))
#     )
#   } else if (length(value_non_na) == 2 ) {
#     min_val <- which.min(value_non_na)
#     max_val <- which.max(value_non_na)
#     
#     legend_color_nodes <- list(
#       list(label = paste0("Min: ", round(value_non_na[min_val], 2)), shape = "dot",
#            color = list(background = colors[min_val], border = "black")),
#       list(label = paste0("Max: ", round(value_non_na[max_val], 2)), shape = "dot",
#            color = list(background = colors[max_val], border = "black"))
#     )
#   }else if (length(value_non_na) > 2 ) {
#     min_val <- which.min(value_non_na)
#     max_val <- which.max(value_non_na)
#     med <- mean(value_non_na)
#     avg_val <- which.min(abs(value_non_na - med))
#     
#     legend_color_nodes <- list(
#       list(label = paste0("Min: ", round(value_non_na[min_val], 2)), shape = "dot",
#            color = list(background = colors[min_val], border = "black")),
#       list(label = paste0("Median: ", round(value_non_na[avg_val], 2)), shape = "dot",
#            color = list(background = colors[avg_val], border = "black")),
#       list(label = paste0("Max: ", round(value_non_na[max_val], 2)), shape = "dot",
#            color = list(background = colors[max_val], border = "black"))
#     )
#   }
#   
#   edges$title = paste0("<p> Edge Type: ", edges$type,  "</p>",
#                        "<p> Weight: ", round(as.numeric(edges$weight),2), "</p>")
#   
#   visNetwork(nodes, edges) %>%
#     visNodes(shape = "dot") %>%
#     visEdges(arrows = "to") %>%
#     visOptions(highlightNearest = T, nodesIdSelection = TRUE) %>%
#     visLegend(
#       ncol = 3,
#       width = 0.3,
#       useGroups = F,
#       addEdges = ledges,
#       addNodes = list(
#         list(label = "Not enriched", shape = "box", color = list(background = "lightgray",border = "lightgray")),
#         list(label = "MIE", shape = "triangle"),
#         list(label = "AO", shape = "star"),
#         list(label = "Key event", shape = "box"),
#         list(label = "Gene", shape = "ellipse")) %>% append(legend_color_nodes)   
#     ) 
#   
# }
# 
