compute_gene_freq = function(gVars, logVars,input){
  mod_stats = gVars$bmdlatestTable
  
  th = as.numeric(input$percentage_th_gene_freq) * 100
  correction = input$correction_gene_freq
  organism = input$organism_gene_freq
  rel_variable = "Experiment"
  group_by = input$gene_frequency_groupby_input
  split_by = input$gene_frequency_splitby_input
  
  res_gene_freq = compute_gene_frequencies(mod_stats = mod_stats,
                                 th = th,
                                 rel_variable = rel_variable,
                                 group_by = group_by,
                                 split_by = split_by)
  
  if(!is.null(res_gene_freq)){
    gVars$Error_compute_freq = FALSE
    gene_list = res_gene_freq$gene_list
    lollipol_plot_list = res_gene_freq$lollipol_plot_list
    lineplot_plot_list = res_gene_freq$line_plot_list
    
    if (!is.null(split_by)) {
      if (sum(unlist(lapply(gene_list, length))) == 0) {
        gsea_most_freq_genes = NULL
      }else{
        gsea_most_freq_genes = gost(query = gene_list,organism = organism,multi_query = F,ordered_query = TRUE, correction_method = correction, domain_scope = "annotated")
      }
    } else{
      if (sum(unlist(lapply(gene_list, length))) == 0) {
        gsea_most_freq_genes = NULL
      } else{
        gsea_most_freq_genes = gost(query = gene_list, organism = organism, ordered_query = TRUE, correction_method = correction, domain_scope = "annotated")
        if (is.null(gsea_most_freq_genes)) {
          shinyalert(title = "No enrichment result",text = "Are you sure the organism is corerct?")
        }
      }
      
    }
    
    gVars$gene_freq_m_list = res_gene_freq$m_list
    gVars$gene_freq_lollipol_plot_list = lollipol_plot_list
    gVars$gene_freq_lineplot_plot_list = lineplot_plot_list
    gVars$gene_freq_gene_list = gene_list
    gVars$gene_freq_gsea_most_freq_genes = gsea_most_freq_genes
    
    
    th = as.numeric(input$percentage_th_gene_freq) * 100
    correction = input$correction_gene_freq
    organism = input$organism_gene_freq
    rel_variable = "Experiment"
    group_by = input$gene_frequency_groupby_input
    split_by = input$gene_frequency_splitby_input
    
    #save for report
    ts = Sys.time()
    logVars$GeneFreqTimestamp = ts
    logVars$GeneFreqTh = th
    logVars$GeneFreqCorrection = correction
    logVars$GeneFreqOrganism = organism
    logVars$GeneFreqRelVariable = rel_variable
    
    
    
    if (gVars$in_glp_mode) {
      print("writing log file obj")
      line = "-----------------------------------------------------"
      write(line,file = logVars$file,append = TRUE)
      
      line  =  "COMPUTE BMD"
      write(line,file = logVars$file,append = TRUE)
      
      line = "FILTERING"
      write(line,file = logVars$file,append = TRUE)
      
      ts  =  Sys.time()
      tsn  =  as.numeric(ts)
      # fname  =  paste(logVars$location, "/tables/", toString(tsn), "_BMDFiltered.RData", sep = "")
      # save(filtered_models_statistics, file  =  fname)
      # line = paste("The Current R Data Object is saved:", fname, sep = " ")
      # write(line,file = logVars$file,append = TRUE)
      
    }
  
    
  }else{
    gVars$Error_compute_freq = TRUE
  }
  
  
  return(list("gVars" = gVars, "logVars" = logVars))
}

# compute_gene_freq = function(gVars, logVars,input){
#   
#   mod_stats = gVars$bmdlatestTable
#   
#   th = as.numeric(input$percentage_th_gene_freq) * 100
#   correction = input$correction_gene_freq
#   organism = input$organism_gene_freq
#   rel_variable = "Experiment"
#   group_by = input$gene_frequency_groupby_input
#   split_by = input$gene_frequency_splitby_input
#   
#   if(group_by=="None") group_by = NULL
#   if(split_by=="None") split_by = NULL
#   
#   if(!is.null(split_by)){
#     possible_values = as.character(unique(mod_stats[,split_by]))
#     
#     lollipol_plot_list = list()
#     gene_list  = list()
#     m_list = list()
#     for(vl in possible_values){
#       if(!is.null(group_by)){
#         mod_stats2 = cbind(paste(mod_stats[mod_stats[,split_by]==vl,rel_variable], mod_stats[mod_stats[,split_by]==vl,group_by],sep = "_"), mod_stats[mod_stats[,split_by]==vl,])
#         colnames(mod_stats2)[1] = "combExp"
#       }else{
#         mod_stats2 = cbind(mod_stats[mod_stats[,split_by]==vl,rel_variable], mod_stats[mod_stats[,split_by]==vl,])
#         colnames(mod_stats2)[1] = "combExp"
#       }
#       
#       experiments = unique(as.character(mod_stats2$combExp))
#       M1 = table(mod_stats2[,"combExp"], mod_stats2[,"Feature"])
#       variables_of_interests = colnames(M1)
#       M = as.matrix(M1)
#       
#       M[M>0]=1
#       index = sort(colSums(M), decreasing = T)
#       M = M[,names(index)]
#       M = t(M)
#       
#       df = data.frame(gene = factor(variables_of_interests, levels = rev(variables_of_interests)),
#                       percentage = (rowSums(M)/ncol(M) * 100))
#       
#       df = df[df$percentage>th,]
#       
#       if(nrow(df)==0) {
#         lollipop_plot = make_empty_plot()
#       }else{
#         lollipop_plot = ggplot(df, aes(x = gene, y = percentage)) +
#           geom_segment(aes(x = gene, xend = gene, y = 0, yend = percentage),
#                        color = "gray", lwd = 1) +
#           geom_point(size = 4, pch = 21, bg = 4, col = 1) +
#           scale_x_discrete(labels = df$gene) +
#           coord_flip() +
#           theme_minimal()
#       }
#       
#       if(nrow(M)==1) {
#         Mnew = matrix(0, nrow = ncol(M), nrow(M))
#         rownames(Mnew) = colnames(M1)
#         colnames(Mnew) = rownames(M1)
#         Mnew[1:nrow(Mnew), 1:ncol(Mnew)] = M[1:nrow(M), 1:ncol(M)] 
#       }else{
#         Mnew = matrix(0, nrow = nrow(M), ncol(M))
#         rownames(Mnew) = rownames(M)
#         colnames(Mnew) = colnames(M)
#         Mnew[1:nrow(Mnew), 1:ncol(Mnew)] = M[1:nrow(M), 1:ncol(M)] 
#       }
#       
#       
#       m_list[[vl]] = Mnew
#       lollipol_plot_list[[vl]] = lollipop_plot
#       gene_list[[vl]] = df$gene
#     }
#     
#     if(sum(unlist(lapply(gene_list, length)))==0){
#       gsea_most_freq_genes = NULL
#     }else{
#       gsea_most_freq_genes = gost(query = gene_list,organism = organism,multi_query = TRUE,ordered_query = TRUE, correction_method = correction, domain_scope = "annotated")
#     }
#     
#   }else{
#     
#     if(!is.null(group_by)){
#       mod_stats2 = cbind(paste(mod_stats[,rel_variable], mod_stats[,group_by],sep = "_"), mod_stats)
#       colnames(mod_stats2)[1] = "combExp"
#       
#       # experiment_ann = cbind(mod_stats2[,"combExp"],mod_stats2[,group_by])
#       # experiment_ann = unique(experiment_ann)
#       # experiment_ann = as.data.frame(experiment_ann)
#       # experiment_ann = as.matrix(experiment_ann)
#       # rownames(experiment_ann) = experiment_ann[,2]
#       # experiment_ann = experiment_ann[order(as.numeric(experiment_ann[,3])),]
#       
#     }else{
#       mod_stats2 = cbind(mod_stats[,rel_variable], mod_stats)
#       colnames(mod_stats2)[1] = "combExp"
#       
#       # experiment_ann = cbind(mod_stats2[,"combExp"])
#       # experiment_ann = unique(experiment_ann)
#       # experiment_ann = as.data.frame(experiment_ann)
#       # experiment_ann = as.matrix(experiment_ann)
#       # rownames(experiment_ann) = experiment_ann[,1]
#       # experiment_ann = experiment_ann[order(as.numeric(as.factor(experiment_ann[,1]))),]
#     }
#     
#     experiments = unique(as.character(mod_stats2$combExp))
#     
#     M1 = table(mod_stats2[,"combExp"], mod_stats2[,"Feature"])
#     print(M1)
#     
#     M = as.matrix(M1)
#     print(M)
#     
#     M[M>0]=1
#     index = sort(colSums(M), decreasing = T)
#     M = M[,names(index)]
#     print(M)
#     
#     variables_of_interests = colnames(M)
#     
#     M = t(M)
#     print(M)
#     
#     
#     percentage = (rowSums(M) / ncol(M) * 100)
#     # df = data.frame(gene = factor(rownames(M), levels = rev(rownames(M))),
#     #                 percentage = (rowSums(M)/ncol(M) * 100))
#     # df = data.frame(gene = factor(variables_of_interests, levels = variables_of_interests),
#     #                 percentage = (rowSums(M)/ncol(M) * 100))
#     
#     df_all = data.frame(gene = factor(names(percentage), levels = names(percentage)),
#                     percentage = percentage)
#     
#     
#     df = df_all[df_all$percentage>th,]
#     
#     if(nrow(df)>100){
#       df = df[1:100,]
#       shinyalert::shinyalert("Too many genes to plot. Only the top 100 genes will be printed in the lollipop plot. Complete results are in the table below.")
#     }
#     
#     if(nrow(df)==0) {
#       lollipop_plot = make_empty_plot()
#     }else{
#       lollipop_plot = ggplot(df, aes(x = gene, y = percentage)) +
#         geom_segment(aes(x = gene, xend = gene, y = 0, yend = percentage),
#                      color = "gray", lwd = 1) +
#         geom_point(size = 4, pch = 21, bg = 4, col = 1) +
#         scale_x_discrete(labels = df$gene) +
#         coord_flip() +
#         theme_minimal()
#     }
#     
#     
#     # categories = cbind("genes", rownames(M))
#     # g = plot_grid_bmdx(path_mat=t(M),
#     #                    path_hier=categories, title="",
#     #                    experiment_ann=experiment_ann,
#     #                    discrete=TRUE,square_colors= c("0"="gray","1"="red"),
#     #                    color_leg=c("0"="0","1"="1"),level_col=1,
#     #                    treat_text_size=8,path_text_size=6, asRatio = FALSE)
#     
#     if(nrow(M)==1) {
#       Mnew = matrix(0, nrow = ncol(M), nrow(M))
#       rownames(Mnew) = colnames(M)
#       colnames(Mnew) = rownames(M)
#       Mnew[1:nrow(Mnew), 1:ncol(Mnew)] = M[1:nrow(M), 1:ncol(M)] 
#     }else{
#       Mnew = matrix(0, nrow = nrow(M), ncol(M))
#       rownames(Mnew) = rownames(M)
#       colnames(Mnew) = colnames(M)
#       Mnew[1:nrow(Mnew), 1:ncol(Mnew)] = M[1:nrow(M), 1:ncol(M)] 
#     }
#     
#     freq = rowSums(Mnew)/ncol(Mnew)
#     Mnew = cbind(Mnew, freq)
#     
#     m_list = list("All" =Mnew)
#     # funmappone_list = list("All" =g)
#     lollipol_plot_list = list("All"=lollipop_plot)
#     gene_list  = list("All"=df$gene)
#     
#     if(sum(unlist(lapply(gene_list, length)))==0){
#       gsea_most_freq_genes = NULL
#     }else{
#       gsea_most_freq_genes = gost(query = gene_list,organism = organism, ordered_query = TRUE, correction_method = correction, domain_scope = "annotated")
#       if(is.null(gsea_most_freq_genes)){
#         shinyalert(title = "No enrichment result",text = "Are you sure the organism is corerct?")
#       }
#     }
#     
#   }
#   
#   gVars$gene_freq_m_list= m_list
#   gVars$gene_freq_lollipol_plot_list= lollipol_plot_list
#   gVars$gene_freq_gene_list= gene_list
#   gVars$gene_freq_gsea_most_freq_genes= gsea_most_freq_genes
#   
#   return(list("gVars"=gVars, "logVars"=logVars))
# }

render_compare_tp_color_by = function(gVars, input, id){
  if(is.null(gVars$bmdlatestTable) || 
     is.null(gVars$TPColID) || 
     is.null(gVars$inputPh())){ 
    return(NULL) 
  }
  

  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options = (c("Experiment", other_variables_id_col, gVars$optVarsGroup, "Feature", "Model","Adverse_direction","is_monotonic"))
  selectInput(id, "Color By", choices=options, selected="Model")
}

render_compare_gene_frequency_group_by = function(gVars, input, id){
  if(is.null(gVars$bmdlatestTable) || 
     is.null(gVars$inputPh()) ||
     is.null(gVars$TPColID)){ 
    return(NULL) 
  }
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options = (c("None",gVars$optVarsGroup,other_variables_id_col)) 
  selectInput(id, "Main grouping variable", choices=options, selected="None") 
}

render_compare_gene_frequency_split_by = function(gVars, input, id){
  if(is.null(gVars$bmdlatestTable) || 
     is.null(gVars$inputPh()) ||
     is.null(gVars$TPColID)){ 
    return(NULL) 
  }
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options = (c("None",gVars$optVarsGroup,other_variables_id_col, "Model","Adverse_direction","is_monotonic"))
  selectInput(id, "Second grouping variable", choices=options, selected="None")
}



render_compare_tp_group_by = function(gVars, input, id, add_time = TRUE){
  if(is.null(gVars$bmdlatestTable) ||
     is.null(gVars$inputPh()) ||
     is.null(gVars$TPColID)){ 
    return(NULL) 
  }
  
  if(add_time){
    other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
    options = (c("Experiment", "Model","Adverse_direction","is_monotonic",other_variables_id_col, gVars$optVarsGroup))
  }else{
    options = (c("Experiment", "Model","Adverse_direction","is_monotonic", gVars$optVarsGroup))
    
  }
  
  selectInput(id, "Group By X", choices=options, selected="Experiment")#other_variables_id_col)
}


render_compare_tp_group_by2 = function(gVars, input, id){

  if(is.null(gVars$bmdlatestTable) || 
     is.null(gVars$TPColID) || 
     is.null(gVars$inputPh())){ 
    return(NULL) 
  }
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options = (c("None", "Experiment","Model","Adverse_direction","is_monotonic", other_variables_id_col, gVars$optVarsGroup))
  selectInput(id, "Group By Y (optional)", choices=options, selected="None")
}

render_compare_tp_filter_cols = function(gVars, input, id){
  if(is.null(gVars$bmdlatestTable) || 
     #is.null(gVars$TPColID) || 
     is.null(gVars$TPColID) || 
     is.null(gVars$inputPh())){ 
    return(NULL) 
  }
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[gVars$TPColID]#[as.numeric(gVars$TPColID)]
  options = (c("None","Experiment","Model", "Adverse_direction","is_monotonic",other_variables_id_col, gVars$optVarsGroup))
  selectInput(id, "Filter By Column", choices=options, multiple=TRUE, selected="None", selectize=FALSE)
  
}



render_tp_bmd_filt_val = function(gVars, input){
  if (is.null(gVars$bmdlatestTable) || is.null(input$BMDVals_filtercol_input)) { return(NULL) }
  
  options = c()
  if (input$BMDVals_filtercol_input != "None") {
    for (p in input$BMDVals_filtercol_input) {
      if (p == "Model") {
        names = levels(gVars$bmdlatestTable[[p]])
      }else {
        names = unique(gVars$bmdlatestTable[[p]])
      }
      options = c(options, names)
    }
  }
  selectInput("BMDVals_filterby_input", "Filter By Column Values", 
              choices = options, multiple = TRUE, selectize = FALSE)
  
}

create_histogram_plot_from_bmd_table = function(gVars, logVars, input){
  if (is.null(gVars$bmdlatestTable) || 
     is.null(gVars$inputPh()) ||
     is.null(gVars$TPColID) ||
     is.null(input$BMDVals_groupby2_input) ||
     is.null(input$BMDVals_filtercol_input)) { 
    return(NULL)
  }
  
  BMDVF = gVars$bmdlatestTable
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]

  if (input$BMDVals_groupby2_input == "None") {
    groupby2 = NULL
  } else {
    groupby2 = input$BMDVals_groupby2_input
  }
  if (input$BMDVals_filtercol_input == "None") {
    fcol = NULL
  } else {
    fcol = input$BMDVals_filtercol_input
  }
  if (input$BMDVals_filtercol_input == "None" | is.null(input$BMDVals_filterby_input)) {
    fby = NULL
  } else {
    fby = NULL
    for (valg in fcol) {
      temp = c()
      for (vals in input$BMDVals_filterby_input) {
        if (is.element(vals, gVars$bmdlatestTable[[valg]])) {
          temp = c(temp,vals)
        }
      }
      if (is.null(fby)) {
        fby = list(temp)
      } else {
        fby = append(fby, list(temp))
      }
    }
  }
  
  BMDVF[,input$BMDVals_colorby_input] = as.factor(BMDVF[,input$BMDVals_colorby_input])
  
  p_bmd = bmdx::plot_histogram(BMDVF, y_val = "BMD", 
                     color_by = input$BMDVals_colorby_input,
                     group_by = input$BMDVals_groupby1_input, 
                     group_by2 = groupby2,
                     filter_column = fcol,
                     filter_by = fby
  )
  
  p_bmdl = bmdx::plot_histogram(BMDVF, y_val = "BMDL", 
                           color_by = input$BMDVals_colorby_input,
                           group_by = input$BMDVals_groupby1_input, 
                           group_by2 = groupby2,
                           filter_column = fcol,
                           filter_by = fby
  )
  
  p_bmdu = bmdx::plot_histogram(BMDVF, y_val = "BMDU", 
                                color_by = input$BMDVals_colorby_input,
                                group_by = input$BMDVals_groupby1_input, 
                                group_by2 = groupby2,
                                filter_column = fcol,
                                filter_by = fby
  )
  
  p_ic50 = bmdx::plot_histogram(BMDVF, y_val = "AC50", 
                                color_by = input$BMDVals_colorby_input,
                                group_by = input$BMDVals_groupby1_input, 
                                group_by2 = groupby2,
                                filter_column = fcol,
                                filter_by = fby
  )
  
  p_bmr = bmdx::plot_histogram(BMDVF, y_val = "BMR", 
                                color_by = input$BMDVals_colorby_input,
                                group_by = input$BMDVals_groupby1_input, 
                                group_by2 = groupby2,
                                filter_column = fcol,
                                filter_by = fby
  )
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    # shinyalert(
    #   title = "GLP Mode Justification",
    #   text = "Please provide a justification for performing this step & the selected parameters.",
    #   size = "s", 
    #   closeOnEsc = TRUE,
    #   closeOnClickOutside = FALSE,
    #   html = FALSE,
    #   type = "input",
    #   inputType = "text",
    #   inputValue = "",
    #   inputPlaceholder = "",
    #   showConfirmButton = TRUE,
    #   showCancelButton = FALSE,
    #   confirmButtonText = "Submit",
    #   confirmButtonCol = "#AEDEF4",
    #   timer = 0,
    #   imageUrl = "",
    #   animation = TRUE,
    #   callbackR = function(x) {logVars$BMDValuesJustification = x}
    #   
    # )
    
    #save for report
    ts = Sys.time()
    logVars$BMDValuesTimestamp = ts
    logVars$BMDValuesColorby = input$BMDVals_colorby_input
    logVars$BMDValuesGroupbyX = input$BMDVals_groupby1_input
    logVars$BMDValuesGroupByY = input$BMDVals_groupby2_input
    logVars$BMDValuesFilterbyCol  = input$BMDVals_filtercol_input
    logVars$BMDValuesFilterbyVals  = input$BMDVals_filterby_input
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPARE DIFFERENT TIME POINTS"
    write(line,file = logVars$file,append = TRUE)
    
    line = "BMD VALUES"
    write(line,file = logVars$file,append = TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    # fname = paste(logVars$location, "/plots/", toString(tsn), "_BMDValuesPlot.RData", sep = "")
    # save(p, file = fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
    # 
  }
  
  
  gVars$BMD_dist_TP = p_bmd
  gVars$BMDL_dist_TP = p_bmdl
  gVars$BMDU_dist_TP = p_bmdu
  gVars$BMR_dist_TP = p_bmr
  gVars$IC50_dist_TP = p_ic50
  
  return(list(gVars, logVars))
}

render_tp_filter_loof = function(gVars, input) {
  if (is.null(gVars$bmdlatestTable) || is.null(input$LOOF_filtercol_input)) {
    return(NULL)
  }
  options = c()
  if (input$LOOF_filtercol_input != "None") {
    for (p in input$LOOF_filtercol_input) {
      if (p == "Model") {
        names = levels(gVars$bmdlatestTable[[p]])
      } else {
        names = unique(gVars$bmdlatestTable[[p]])
      }
      options = c(options, names)
      
    }
  }
  selectInput("LOOF_filterby_input", "Filter By Column Values", 
              choices = options, multiple = TRUE, selectize = FALSE)
}

create_histrogram_plot_for_lack_of_fit_pvalue = function(gVars, logVars, input){
  if (is.null(gVars$bmdlatestTable) ||
     is.null(gVars$inputPh()) ||
     is.null(gVars$TPColID) ||
     is.null(input$LOOF_groupby2_input)) { 
    return(NULL)
  }
  
  BMDVF = gVars$bmdlatestTable
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]

  if (input$LOOF_groupby2_input == "None") {
    groupby2 = NULL
  } else {
    groupby2 = input$LOOF_groupby2_input
  }
  if (input$LOOF_filtercol_input == "None") {
    fcol = NULL
  }else {
    fcol = input$LOOF_filtercol_input
  }
  if (input$LOOF_filtercol_input == "None" | is.null(input$LOOF_filterby_input)) {
    fby = NULL
  } else {
    fby = NULL
    for (valg in fcol) {
      temp = c()
      for (vals in input$LOOF_filterby_input) {
        if (is.element(vals, gVars$bmdlatestTable[[valg]])) {
          temp = c(temp,vals)
        }
      }
      if (is.null(fby)) {
        fby = list(temp)
      } else {
        fby = append(fby, list(temp))
      }
    }
  }
  
  p_aic = bmdx::plot_histogram(BMDVF, y_val = "AIC", 
                                   color_by = input$LOOF_colorby_input,
                                   group_by = input$LOOF_groupby1_input, 
                                   group_by2 = groupby2,
                                   filter_column = fcol,
                                   filter_by = fby)
  
  p_r2 = bmdx::plot_histogram(BMDVF, y_val = "R2", 
                                   color_by = input$LOOF_colorby_input,
                                   group_by = input$LOOF_groupby1_input, 
                                   group_by2 = groupby2,
                                   filter_column = fcol,
                                   filter_by = fby)
  
  
  p_lackfit = bmdx::plot_histogram(BMDVF, y_val = "Lack_of_fit", 
                     color_by = input$LOOF_colorby_input,
                     group_by = input$LOOF_groupby1_input, 
                     group_by2 = groupby2,
                     filter_column = fcol,
                     filter_by = fby)
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    # shinyalert(
    #   title = "GLP Mode Justification",
    #   text = "Please provide a justification for performing this step & the selected parameters.",
    #   size = "s", 
    #   closeOnEsc = TRUE,
    #   closeOnClickOutside = FALSE,
    #   html = FALSE,
    #   type = "input",
    #   inputType = "text",
    #   inputValue = "",
    #   inputPlaceholder = "",
    #   showConfirmButton = TRUE,
    #   showCancelButton = FALSE,
    #   confirmButtonText = "Submit",
    #   confirmButtonCol = "#AEDEF4",
    #   timer = 0,
    #   imageUrl = "",
    #   animation = TRUE,
    #   callbackR = function(x) {logVars$BMDLOOFJustification = x}
    #   
    # )
    
    ts = Sys.time()
    logVars$BMDValuesTimestamp = ts
    logVars$BMDLOOFColorby = input$LOOF_colorby_input
    logVars$BMDLOOFGroupbyX = input$LOOF_groupby1_input
    logVars$BMDLOOFsGroupByY = input$LOOF_groupby2_input
    logVars$BMDLOOFFilterbyCol  = input$LOOF_filtercol_input
    logVars$BMDLOOFFilterbyVals  = input$LOOF_filterby_input
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPARE DIFFERENT TIME POINTS"
    write(line,file = logVars$file,append = TRUE)
    
    line = "LACK OF FIT PVALUES"
    write(line,file = logVars$file,append = TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    # fname = paste(logVars$location, "/plots/", toString(tsn), "_BMDLOOFPlot.RData", sep = "")
    # save(p, file = fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
  }
  
  gVars$BMD_pval_fitting = p_lackfit
  gVars$BMD_r2_fitting = p_r2
  gVars$BMD_aic_fitting = p_aic
  
  return(list(gVars, logVars))
  
}
# 
# 
# create_histrogram_plot_r2 = function(gVars, logVars, input){
#   if (is.null(gVars$bmdlatestTable) ||
#       is.null(gVars$inputPh()) ||
#       is.null(gVars$TPColID) ||
#       is.null(input$R2_groupby2_input)) { 
#     return(NULL)
#   }
#   
#   BMDVF = gVars$bmdlatestTable
#   other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
#   
#   if (input$R2_groupby2_input == "None") {
#     groupby2 = NULL
#   } else {
#     groupby2 = input$R2_groupby2_input
#   }
#   if (input$R2_filtercol_input == "None") {
#     fcol = NULL
#   }else {
#     fcol = input$R2_filtercol_input
#   }
#   if (input$R2_filtercol_input == "None" | is.null(input$R2_filterby_input)) {
#     fby = NULL
#   } else {
#     fby = NULL
#     for (valg in fcol) {
#       temp = c()
#       for (vals in input$R2_filterby_input) {
#         if (is.element(vals, gVars$bmdlatestTable[[valg]])) {
#           temp = c(temp,vals)
#         }
#       }
#       if (is.null(fby)) {
#         fby = list(temp)
#       } else {
#         fby = append(fby, list(temp))
#       }
#     }
#   }
#   
#   p = bmdx::plot_histogram(BMDVF, y_val = "R2", 
#                            color_by = input$R2_colorby_input,
#                            group_by = input$R2_groupby1_input, 
#                            group_by2 = groupby2,
#                            filter_column = fcol,
#                            filter_by = fby)
#   
#   if (gVars$in_glp_mode) {
#     
#     #GLP MODE
#     shinyalert(
#       title = "GLP Mode Justification",
#       text = "Please provide a justification for performing this step & the selected parameters.",
#       size = "s", 
#       closeOnEsc = TRUE,
#       closeOnClickOutside = FALSE,
#       html = FALSE,
#       type = "input",
#       inputType = "text",
#       inputValue = "",
#       inputPlaceholder = "",
#       showConfirmButton = TRUE,
#       showCancelButton = FALSE,
#       confirmButtonText = "Submit",
#       confirmButtonCol = "#AEDEF4",
#       timer = 0,
#       imageUrl = "",
#       animation = TRUE,
#       callbackR = function(x) {logVars$BMDR2Justification = x}
#       
#     )
#     
#     ts = Sys.time()
#     logVars$BMDValuesTimestamp = ts
#     logVars$BMDR2Colorby = input$R2_colorby_input
#     logVars$BMDR2GroupbyX = input$R2_groupby1_input
#     logVars$BMDR2sGroupByY = input$R2_groupby2_input
#     logVars$BMDR2FilterbyCol  = input$R2_filtercol_input
#     logVars$BMDR2FilterbyVals  = input$R2_filterby_input
#     
#   }
#   
#   if (gVars$in_glp_mode) {
#     print("writing log file obj")
#     line = "-----------------------------------------------------"
#     write(line,file = logVars$file,append = TRUE)
#     
#     line = "COMPARE DIFFERENT TIME POINTS"
#     write(line,file = logVars$file,append = TRUE)
#     
#     line = "LACK OF FIT PVALUES"
#     write(line,file = logVars$file,append = TRUE)
#     
#     ts = Sys.time()
#     tsn = as.numeric(ts)
#     fname = paste(logVars$location, "/plots/", toString(tsn), "_BMDR2Plot.RData", sep = "")
#     save(p, file = fname)
#     line = paste("The Current R Data Object is saved:", fname, sep = " ")
#     write(line,file = logVars$file,append = TRUE)
#   }
#   
#   gVars$BMD_R2_fitting = p
#   return(list(gVars, logVars))
#   
# }


render_tp_filterby = function(gVars, input){
  if (is.null(gVars$bmdlatestTable) ||
     is.null(input$tp_filtercol_input)){
    return(NULL)
  }
  options = c()
  if (input$tp_filtercol_input != "None"){
    for (p in input$tp_filtercol_input) {
      if(p == "Model") {
        names = levels(gVars$bmdlatestTable[[p]])
      }else {
        names = unique(gVars$bmdlatestTable[[p]])
      }
      options = c(options, names)
    }
  }
  selectInput("tp_filterby_input", "Filter By Column Values", choices=options, multiple=TRUE, selectize=FALSE)
}

plot_number_of_genes_by_time_point = function(gVars, logVars, input){

  if(is.null(gVars$bmdlatestTable) || 
     is.null(gVars$inputPh()) ||
     is.null(gVars$TPColID) ||
     is.null(input$tp_filtercol_input) ||
     is.null(input$tp_groupby1_input)
     ){ 
    return(NULL)
  }
  
  
  BMDVF = gVars$bmdlatestTable

  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]

  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  if (input$tp_filtercol_input == "None") {
    fcol = NULL
  }else {
    fcol = input$tp_filtercol_input
    
  }
  if (input$tp_groupby1_input == "None") {
    grby = NULL
  }else {
    grby = c(input$tp_groupby1_input)
    
  }
  if (input$tp_filtercol_input == "None" | is.null(input$tp_filterby_input)) {
    fby = NULL
  }else {
    #each value of fcol is its own list
    fby = NULL
    for (valg in fcol) {
      temp = c()
      for (vals in input$tp_filterby_input) {
        if (is.element(vals, gVars$bmdlatestTable[[valg]])) {
          temp = c(temp,vals)
        }
      }
      if (is.null(fby)) {
        fby = list(temp)
      } else {
        fby = append(fby, list(temp))
      }
    }
  }
  
  if (length(grby) > 2 & (!is.null(grby))) { 
    shinyalert(
      title = "Error",
      text = "The Group By Variable is only allowed to be of length 1, 2 or be None",
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    return(NULL)
  }
  
  p = aggregate_rows_time(BMDVF, 
                          gen_feat = "Feature", 
                          first_feat = other_variables_id_col, 
                          group_by = grby, 
                          filter_column = fcol,
                          filter_by = fby )
  
  
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    # shinyalert(
    #   title = "GLP Mode Justification",
    #   text = "Please provide a justification for performing this step & the selected parameters.",
    #   size = "s", 
    #   closeOnEsc = TRUE,
    #   closeOnClickOutside = FALSE,
    #   html = FALSE,
    #   type = "input",
    #   inputType = "text",
    #   inputValue = "",
    #   inputPlaceholder = "",
    #   showConfirmButton = TRUE,
    #   showCancelButton = FALSE,
    #   confirmButtonText = "Submit",
    #   confirmButtonCol = "#AEDEF4",
    #   timer = 0,
    #   imageUrl = "",
    #   animation = TRUE,
    #   callbackR = function(x) {logVars$BMDGTPJustification = x}
    #   
    # )
    
    #save for report
    ts = Sys.time()
    logVars$BMDGTPTimestamp = ts
    logVars$BMDGTPGroupbyX = input$tp_groupby1_input
    logVars$BMDGTPFilterbyCol  = input$tp_filtercol_input
    logVars$BMDGTPFilterbyVals  = input$tp_filterby_input
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPARE DIFFERENT TIME POINTS"
    write(line,file = logVars$file,append = TRUE)
    
    line = "NR. OF DOSE-DEPENDENT GENES"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    # fname  =  paste(logVars$location, "/plots/", toString(tsn), "_BMDGTPPlot.RData", sep = "")
    # save(p, file  =  fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
  }
  
  gVars$NGTime = p
  return(list(gVars, logVars))
}


render_compare_tp_bmd_bmdl_filterby = function(gVars, input){
  if (is.null(gVars$bmdlatestTable) || is.null(input$BMDBMDL_filtercol_input)) {
    return(NULL)
  }
  options = c()
  if (input$BMDBMDL_filtercol_input != "None") {
    for (p in input$BMDBMDL_filtercol_input) {
      if (p == "Model") {
        names = levels(gVars$bmdlatestTable[[p]])
      } else {
        names = unique(gVars$bmdlatestTable[[p]])
      }
      options = c(options, names)
    }
  }
  selectInput("BMDBMDL_filterby_input", "Filter By Column Values", choices = options, multiple = TRUE, selectize = FALSE)
}


render_compare_compare_experiments_filterby = function(gVars, input){
  if(is.null(gVars$bmdlatestTable) ||
     is.null(input$compare_experiments_filtercol_input)){
    return(NULL)
  }
  options = c()
  if(input$compare_experiments_filtercol_input != "None"){
    for (p in input$compare_experiments_filtercol_input) {
      if(p == "Model") {
        names = levels(gVars$bmdlatestTable[[p]])
      }else {
        names = unique(gVars$bmdlatestTable[[p]])
      }
      options = c(options, names)
    }
  }
  selectInput("compare_experiments_filterby_input", "Filter By Column Values", choices=options, multiple=TRUE, selectize=FALSE)
}

render_compare_compare_experiments_filterby_upset = function(gVars, input){
  if(is.null(gVars$bmdlatestTable) ||
     is.null(input$compare_experiments_filtercol_input_upset)){
    return(NULL)
  }
  options = c()
  if(input$compare_experiments_filtercol_input_upset != "None"){
    for (p in input$compare_experiments_filtercol_input_upset) {
      if(p == "Model") {
        names = levels(gVars$bmdlatestTable[[p]])
      }else {
        names = unique(gVars$bmdlatestTable[[p]])
      }
      options = c(options, names)
    }
  }
  selectInput("compare_experiments_filterby_input_upset", "Filter By Column Values", choices=options, multiple=TRUE, selectize=FALSE)
}

plot_bmd_bmdl = function(gVars, logVars, input){
  if(is.null(gVars$bmdlatestTable) || 
     is.null(gVars$inputPh()) || 
     is.null(gVars$TPColID) ||
     is.null(input$BMDBMDL_groupby2_input) ||
     is.null(input$BMDBMDL_filtercol_input)
     ){  return(NULL) }
  
  
  BMDVF = gVars$bmdlatestTable
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]

  if(input$BMDBMDL_groupby2_input == "None") {
    groupby2 = NULL
  }else {
    groupby2 = input$BMDBMDL_groupby2_input
  }
  if(input$BMDBMDL_filtercol_input == "None") {
    fcol = NULL
  }else {
    fcol = input$BMDBMDL_filtercol_input
    
  }
  if(input$BMDBMDL_filtercol_input == "None" | is.null(input$BMDBMDL_filterby_input)) {
    fby = NULL
  }else {
    fby = NULL
    for(valg in fcol) {
      temp = c()
      for(vals in input$BMDBMDL_filterby_input){
        if(is.element(vals, gVars$bmdlatestTable[[valg]])){
          temp = c(temp,vals)
        }
      }
      if(is.null(fby)){
        fby = list(temp)
      }else {
        fby = append(fby, list(temp))
      }
    }
  }
  

  p=plot_scatter(BMDVF, x_val = "BMDL",y_val = "BMD",
                 color_by = input$BMDBMDL_colorby_input,
                 group_by = input$BMDBMDL_groupby1_input, 
                 group_by2 = groupby2,
                 filter_column = fcol,
                 filter_by = fby )
  
  
  if(gVars$in_glp_mode){
    
    #GLP MODE
    # shinyalert(
    #   title = "GLP Mode Justification",
    #   text = "Please provide a justification for performing this step & the selected parameters.",
    #   size = "s", 
    #   closeOnEsc = TRUE,
    #   closeOnClickOutside = FALSE,
    #   html = FALSE,
    #   type = "input",
    #   inputType = "text",
    #   inputValue = "",
    #   inputPlaceholder = "",
    #   showConfirmButton = TRUE,
    #   showCancelButton = FALSE,
    #   confirmButtonText = "Submit",
    #   confirmButtonCol = "#AEDEF4",
    #   timer = 0,
    #   imageUrl = "",
    #   animation = TRUE,
    #   callbackR =function(x) {logVars$BMDBMD_BMDLJustification = x}
    #   
    # )
    
    #save for report
    ts = Sys.time()
    logVars$BMDValuesTimestamp = ts
    logVars$BMDBMD_BMDLColorby = input$BMDBMDL_colorby_input
    logVars$BMDBMD_BMDLGroupbyX = input$BMDBMDL_groupby1_input
    logVars$BMDBMD_BMDLGroupByY = input$BMDBMDL_groupby2_input
    logVars$BMDBMD_BMDLFilterbyCol  = input$BMDBMDL_filtercol_input
    logVars$BMDBMD_BMDLFilterbyVals  = input$BMDBMDL_filterby_input
    
  }
  
  if(gVars$in_glp_mode){
    print("writing log file obj")
    line="-----------------------------------------------------"
    write(line,file=logVars$file,append=TRUE)
    
    line="COMPARE DIFFERENT TIME POINTS"
    write(line,file=logVars$file,append=TRUE)
    
    line="BMD/BMDL"
    write(line,file=logVars$file,append=TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    # fname = paste(logVars$location, "/plots/", toString(tsn), "_BMDBDLPlot.RData", sep="")
    # save(p, file = fname)
    # line=paste("The Current R Data Object is saved:", fname, sep=" ")
    # write(line,file=logVars$file,append=TRUE)
    
  }
  
  
  #ggplotly(p)
  gVars$BMD_BMDL = p
  return(list(gVars, logVars))
}


render_compare_tp_fitted_models_filterby = function(gVars, input){
  if(is.null(gVars$bmdlatestTable) || 
     is.null(input$Fittedmodels_filtercol_input)){
    return(NULL)
  }
  options = c()
  if(input$Fittedmodels_filtercol_input != "None"){
    for (p in input$Fittedmodels_filtercol_input) {
      if(p == "Model") {
        names = levels(gVars$bmdlatestTable[[p]])
      }else {
        names = unique(gVars$bmdlatestTable[[p]])
      }
      options = c(options, names)
    }
  }
  selectInput("Fittedmodels_filterby_input", "Filter By Column Values", choices=options, multiple=TRUE, selectize=FALSE)
}


plot_fitted_models = function(gVars, logVars, input){
  if(is.null(gVars$bmdlatestTable) || 
     is.null(gVars$inputPh()) || 
     is.null(gVars$TPColID) ||
     is.null(input$Fittedmodels_groupby2_input) ||
     is.null(input$Fittedmodels_filtercol_input)
     ){ 
    return(NULL)
  }
  
  BMDVF = gVars$bmdlatestTable
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]

  if (input$Fittedmodels_groupby2_input == "None") {
    groupby2 = NULL
  } else {
    groupby2 = input$Fittedmodels_groupby2_input
  }
  if (input$Fittedmodels_filtercol_input == "None") {
    fcol = NULL
  }else {
    fcol = input$Fittedmodels_filtercol_input
    
  }
  if (input$Fittedmodels_filtercol_input == "None" | is.null(input$Fittedmodels_filterby_input)) {
    fby = NULL
  } else {
    fby = NULL
    for (valg in fcol) {
      temp = c()
      for (vals in input$Fittedmodels_filterby_input) {
        if (is.element(vals, gVars$bmdlatestTable[[valg]])) {
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
  
  p = bmdx::plot_pie_chart(BMDVF, category = "Model",
                     group_by = input$Fittedmodels_groupby1_input, 
                     group_by2 = groupby2,
                     filter_column = fcol,
                     filter_by = fby)

  if (gVars$in_glp_mode) {
    
    #GLP MODE
    # shinyalert(
    #   title = "GLP Mode Justification",
    #   text = "Please provide a justification for performing this step & the selected parameters.",
    #   size = "s", 
    #   closeOnEsc = TRUE,
    #   closeOnClickOutside = FALSE,
    #   html = FALSE,
    #   type = "input",
    #   inputType = "text",
    #   inputValue = "",
    #   inputPlaceholder = "",
    #   showConfirmButton = TRUE,
    #   showCancelButton = FALSE,
    #   confirmButtonText = "Submit",
    #   confirmButtonCol = "#AEDEF4",
    #   timer = 0,
    #   imageUrl = "",
    #   animation = TRUE,
    #   callbackR =function(x) {logVars$BMDFMJustification = x}
    #   
    # )
    
    #save for report
    ts = Sys.time()
    logVars$BMDFMTimestamp = ts
    logVars$BMDFMGroupbyX = input$Fittedmodels_groupby1_input
    logVars$BMDFMGroupByY = input$Fittedmodels_groupby2_input
    logVars$BMDFMFilterbyCol  = input$Fittedmodels_filtercol_input
    logVars$BMDFMFilterbyVals  = input$Fittedmodels_filterby_input
    
  }
  
  if(gVars$in_glp_mode){
    print("writing log file obj")
    line="-----------------------------------------------------"
    write(line,file=logVars$file,append=TRUE)
    
    line="COMPARE DIFFERENT TIME POINTS"
    write(line,file=logVars$file,append=TRUE)
    
    line="FITTED MODELS"
    write(line,file=logVars$file,append=TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    # fname = paste(logVars$location, "/plots/", toString(tsn), "_BMDFittedModelsPlot.RData", sep="")
    # save(p, file = fname)
    # line=paste("The Current R Data Object is saved:", fname, sep=" ")
    # write(line,file=logVars$file,append=TRUE)
    
  }
  
  gVars$BMD_dist_models = p
  return(list(gVars, logVars))
}

make_compare_experiments_ecdf_plot = function(gVars, logVars,input){
  
  shiny::validate(need(!is.null(gVars$bmdlatestTable), "No BMD values estimated yet"))
  
  if(is.null(input$BMDComExp_relvar_input)) return(NULL)
  if(is.null(input$BMDComExp_groupby2_input)) return(NULL)
  if(is.null(input$compare_experiments_filtercol_input)) return(NULL)
  
  BMDVF = gVars$bmdlatestTable
  
  if(input$BMDComExp_relvar_input == input$BMDComExp_groupby2_input)
    shinyalert("The relevant variable and the group by variable cannot be the same!", "", type = "info")
  
  shiny::validate(need(input$BMDComExp_relvar_input != input$BMDComExp_groupby2_input, "The Relevant variable and group variable should be different"))
  
  if(input$BMDComExp_othervars_input=="None"){
    other_variables = NULL
  }else{
    other_variables = input$BMDComExp_othervars_input
  }
  if(input$BMDComExp_groupby2_input == "None") {
    groupby = NULL
  }else {
    groupby = input$BMDComExp_groupby2_input
  }
  if(input$compare_experiments_filtercol_input == "None") {
    fcol = NULL
  }else {
    fcol = input$compare_experiments_filtercol_input
  }
  if("None" %in% input$compare_experiments_filtercol_input | is.null(input$compare_experiments_filterby_input)) {
    fby = NULL
  }else {
    #each value of fcol is its own list
    fby = NULL
    for(valg in fcol) {
      temp = c()
      for(vals in input$compare_experiments_filterby_input){
        if(is.element(vals, gVars$bmdlatestTable[[valg]])){
          temp = c(temp,vals)
        }
      }
      if(is.null(fby)){
        fby = list(temp)
      }else {
        fby = append(fby, list(temp))
      }
    }
  }
  
  ecdf_plot =  bmdx::ecdf_plots(mod_stats = BMDVF, 
                                rel_variable = input$BMDComExp_relvar_input,
                                group_by = groupby,
                                is_group_by_numeric = input$BMD_is_group_by_numeric,
                                other_variables = other_variables,
                                number_of_column = 2,
                                scaling = input$BMD_compare_scale,
                                filter_column = fcol,
                                filter_by = fby,
                                plot_type = "ecdf") 

  gVars$bmdcompareexpRes_ecdf_plot  = ecdf_plot
  gVars$BMDCompExpTimestamp_ecdf = ts
  gVars$BMDCompExpRelVar_ecdf = input$BMDComExp_relvar_input
  gVars$BMDCompExpGroupBy_ecdf = input$BMDComExp_groupby2_input
  gVars$BMDCompExpOtherVar_ecdf = input$BMDComExp_othervars_input
  gVars$BMDCompExpnCl_ecdf = input$BMDComExp_nclust
  gVars$BMDCompExpnBins_ecdf = input$BMDComExp_nbins
  gVars$BMDCompExpmInt_ecdf = input$maxInt
  gVars$BMDCompExpgb_ecdf = input$groupby
  gVars$BMDCompExpob_ecdf = input$orderby
  
  if( gVars$in_glp_mode) {
    
    #GLP MODE
    # shinyalert(
    #   title = "GLP Mode Justification",
    #   text = "Please provide a justification for the BMD Compare Experiment Parameters.",
    #   size = "s", 
    #   closeOnEsc = TRUE,
    #   closeOnClickOutside = FALSE,
    #   html = FALSE,
    #   type = "input",
    #   inputType = "text",
    #   inputValue = "",
    #   inputPlaceholder = "",
    #   showConfirmButton = TRUE,
    #   showCancelButton = FALSE,
    #   confirmButtonText = "Submit",
    #   confirmButtonCol = "#AEDEF4",
    #   timer = 0,
    #   imageUrl = "",
    #   animation = TRUE,
    #   callbackR = function(x) {logVars$bmdcompexpReason_ecdf = x}
    # )
    
    #save for report
    ts = Sys.time()
    logVars$BMDCompExpTimestamp_ecdf = ts
    logVars$BMDCompExpRelVar_ecdf = input$BMDComExp_relvar_input
    logVars$BMDCompExpGroupBy_ecdf = input$BMDComExp_groupby2_input
    logVars$BMDCompExpOtherVar_ecdf = input$BMDComExp_othervars_input
    logVars$BMDCompExpnCl_ecdf = input$BMDComExp_nclust
    logVars$BMDCompExpnBins_ecdf = input$BMDComExp_nbins
    logVars$BMDCompExpmInt_ecdf = input$maxInt
    logVars$BMDCompExpgb_ecdf = input$groupby
    logVars$BMDCompExpob_ecdf = input$orderby
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "BMD"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPARE EXPERIMENTS ECDF PLOT"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    # fname  =  paste(logVars$location, "/plots/", toString(tsn), "_bmdCompareExperimentsECDF.RData", sep = "")
    # save(res, file  =  fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
  }
  
  return(list(gVars, logVars))
}


make_compare_experiments_upset_plot = function(gVars, logVars,input){
  
  shiny::validate(need(!is.null(gVars$bmdlatestTable), "No BMD values estimated yet"))
  
  if (is.null(input$BMDComExp_relvar_input_upset)) return(NULL)
  if (is.null(input$BMDComExp_groupby2_input_upset)) return(NULL)
  if (is.null(input$compare_experiments_filtercol_input_upset)) return(NULL)
  
  BMDVF = gVars$bmdlatestTable
  
  if (input$BMDComExp_relvar_input_upset == input$BMDComExp_groupby2_input_upset)
    shinyalert("The relevant variable and the group by variable cannot be the same!", "", type = "info")
  
  shiny::validate(need(input$BMDComExp_relvar_input_upset != input$BMDComExp_groupby2_input_upset, "The Relevant variable and group variable should be different"))
  
  if (input$BMDComExp_othervars_input_upset == "None") {
    other_variables = NULL
  }else{
    other_variables = input$BMDComExp_othervars_input_upset
  }
  if (input$BMDComExp_groupby2_input_upset == "None") {
    groupby = NULL
  }else {
    groupby = input$BMDComExp_groupby2_input_upset
  }
  if (input$compare_experiments_filtercol_input_upset == "None") {
    fcol = NULL
  }else {
    fcol = input$compare_experiments_filtercol_input_upset
  }
  if ("None" %in% input$compare_experiments_filtercol_input_upset | is.null(input$compare_experiments_filterby_input)) {
    fby = NULL
  } else {
    #each value of fcol is its own list
    fby = NULL
    for (valg in fcol) {
      temp = c()
      for (vals in input$compare_experiments_filterby_input) {
        if (is.element(vals, gVars$bmdlatestTable[[valg]])) {
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
  
  upset_plot = bmdx::upset_plot(mod_stats = BMDVF, 
                                rel_variable = input$BMDComExp_relvar_input_upset,
                                group_by = groupby,
                                other_variables = other_variables,
                                filter_column = fcol,
                                filter_by = fby,
                                nintersects = input$maxInt, 
                                group.by = input$groupby,
                                order.by = input$orderby)
  
  gVars$bmdcompareexpRes_upset_plot  = upset_plot
  gVars$BMDCompExpTimestamp_upset = ts
  gVars$BMDCompExpRelVar_upset = input$BMDComExp_relvar_input_upset
  gVars$BMDCompExpGroupBy_upset = input$BMDComExp_groupby2_input_upset
  gVars$BMDCompExpOtherVar_upset = input$BMDComExp_othervars_input_upset
  gVars$BMDCompExpnCl_upset = input$BMDComExp_nclust
  gVars$BMDCompExpnBins_upset = input$BMDComExp_nbins
  gVars$BMDCompExpmInt_upset = input$maxInt
  gVars$BMDCompExpgb_upset = input$groupby
  gVars$BMDCompExpob_upset = input$orderby
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a justification for the BMD Compare Experiment Parameters.",
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "input",
      inputType = "text",
      inputValue = "",
      inputPlaceholder = "",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Submit",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) {logVars$bmdcompexpReason_upset = x}
    )
    
    #save for report
    ts = Sys.time()
    logVars$BMDCompExpTimestamp_upset = ts
    logVars$BMDCompExpRelVar_upset = input$BMDComExp_relvar_input_upset
    logVars$BMDCompExpGroupBy_upset = input$BMDComExp_groupby2_input_upset
    logVars$BMDCompExpOtherVar_upset = input$BMDComExp_othervars_input_upset
    logVars$BMDCompExpnCl_upset = input$BMDComExp_nclust
    logVars$BMDCompExpnBins_upset = input$BMDComExp_nbins
    logVars$BMDCompExpmInt_upset = input$maxInt
    logVars$BMDCompExpgb_upset = input$groupby
    logVars$BMDCompExpob_upset = input$orderby
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "BMD"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPARE EXPERIMENTS UPSET PLOT"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    # fname  =  paste(logVars$location, "/plots/", toString(tsn), "_bmdCompareExperimentsUpset.RData", sep = "")
    # save(res, file  =  fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
  }
  
  return(list(gVars, logVars))
}