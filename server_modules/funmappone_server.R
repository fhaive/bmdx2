convert_data_for_funmappone = function(gVars, logVars, input, value_selected="BMD"){
  
  bmd_table = gVars$bmdlatestTable
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  unique_experiments = unique(bmd_table[,c("Experiment",other_variables_id_col)])
  
  GList = list()
  Pheno = c()
  for(i in 1:nrow(unique_experiments)){
    idx = which(bmd_table[,"Experiment"]==unique_experiments[i,1] & 
                  bmd_table[,other_variables_id_col]==unique_experiments[i,2])
    
    ith_bmd_values = bmd_table[idx, c("Feature",value_selected)]
    ith_bmd_values = ith_bmd_values[!is.na(ith_bmd_values[,2]),]
    GList[[paste(unique_experiments[i,], collapse= "_")]] - ith_bmd_values
    Pheno = rbind(Pheno, c(paste(unique_experiments[i,], collapse= "_"),unique_experiments[i,2]))
    
  }
  
  colnames(Pheno) = c("Experiment","Group")
  Pheno = as.data.frame(Pheno)
  
}


# TODO:: get experiment_col and time_col from input
plot_mean_BMD_time_plot = function(gVars, input, output, experiment_col = "Experiment"){
  if(is.null(gVars$EnrichDatList) || 
     is.null(gVars$inputPh()) || 
     is.null(gVars$TPColID)){ 
    print("Null enrichment")
    return(NULL)
  }
  
   time_col  = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  PD = c()
  
  ER <- gVars$EnrichDatList # result of enrichment
  Hier = gVars$hierarchy
  
  for(i in 1:length(ER)){
    print(i)
    
    ERi = ER[[i]]
    if(nrow(ERi)==0) next
    
    # print(ERi)
    elemn = names(ER)[i]
    exper = strsplit(x = elemn, split = "_")[[1]][1]
    timep = strsplit(x = elemn, split = "_")[[1]][2]
    
    # if(is.null(gVars$MQ_BMD_filtered)){
    #   BMDFilMat = gVars$MQ_BMD[[exper]][[as.character(timep)]]$BMDValues #filtered BMD genes
    #   
    # }else{
    #   BMDFilMat = gVars$MQ_BMD_filtered[[exper]][[as.character(timep)]]$BMDValues_filtered #filtered BMD genes
    #   
    # }
    
    BMDFilMat = gVars$bmdlatestTable
    good_idx = which(BMDFilMat[,experiment_col] == exper & BMDFilMat[,time_col] == timep)
    BMDFilMat = BMDFilMat[good_idx,]
    
    if(!is.null(BMDFilMat) & nrow(ERi)>0){
      
      for(npat in 1:nrow(ER[[i]])){
        gi = unlist(strsplit(x = as.character(ERi[npat,2]),split = ","))
        
        goID = ERi[npat,"annID"]
        PatName = ERi[npat,"Description"]
        PatPval = ERi[npat,"pValueAdj"]
        NGenes = length(gi)
        # PatNGenes = ERi[npat,"gID"]
        # PatSize = ERi[npat,]
        
        idx = which(tolower(BMDFilMat[,"Feature"]) %in% tolower(gi))
        BMD = as.numeric(BMDFilMat[idx,"BMD"])
        names(BMD) = BMDFilMat[idx,"Gene"]
        bmd = mean(BMD)
        
        levidx = which(Hier[,"ID"] %in% goID)
        levName = as.character(Hier[levidx[1],1])
        if(is.na(levName)) levName = "Root"
        
        #PD = rbind(PD, cbind(names(gVars$MQ_BMD_filtered)[i],PatName,PatPval, bmd, NGenes, levName))
        PD = rbind(PD, cbind(exper, timep, PatName,PatPval, bmd, NGenes, levName))
        
      }
    }
  }
  
  colnames(PD) = c("Experiment","TimePoint","FunCategory", "Adj.pval","MeanBMD","NGenes", "LevName")
  PD = as.data.frame(PD)
  PD$TimePoint = factor(PD$TimePoint, level=sort(as.numeric(as.vector(unique(PD$TimePoint)))))
  PD$Adj.pval = as.numeric(as.vector(PD$Adj.pval))
  PD$MeanBMD = as.numeric(as.vector(PD$MeanBMD))
  PD$logPval = -log(PD$Adj.pval)
  PD$logPval = -log(PD$Adj.pval)
  PD$NGenes = as.numeric(as.vector(PD$NGenes))
  gVars$MeanBMDTimePoint = PD
  ggp = ggplot(data=PD, aes(MeanBMD, fill=TimePoint)) + 
    geom_histogram() + facet_wrap(. ~ Experiment,scales = "free_x")
  
  # #save plot
  if(gVars$in_glp_mode){
    print("writing log file obj")
    line="-----------------------------------------------------"
    write(line,file=logVars$file,append=TRUE)
    
    line="ENRICHMENT"
    write(line,file=logVars$file,append=TRUE)
    
    line="MEAN BMD FOR TIME POINT"
    write(line,file=logVars$file,append=TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    fname = paste(logVars$location, "/plots/", toString(tsn), "_MeanBMDTimePointPlot.RData", sep="")
    save(ggp, file = fname)
    line=paste("The Current R Data Object is saved:", fname, sep=" ")
    write(line,file=logVars$file,append=TRUE)
    
  }
  
  
  return(ggp)
}

render_patways_table = function(exp_combination = input$time_point_id_visualPat,gVars, input, output){
  if(is.null(gVars$bmdlatestTable) ||
     sum(unlist(lapply(gVars$EnrichDatList, nrow))) ==0 ){
    return(NULL)
  }
  
  if((exp_combination %in% names(gVars$EnrichDatList))==FALSE){
    print("No enrichment for this TP")
    return(NULL)
  }
  
  PATWAY_tab <- gVars$EnrichDatList[[exp_combination]]
  PATWAY_tab = PATWAY_tab[,c(1,3,4,5)]
  gVars$PATWAY_tab=PATWAY_tab
  
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    line = "ENRICHMENT"
    write(line,file = logVars$file,append = TRUE)
    line = "GENE BMD in PATHWAY"
    write(line,file = logVars$file,append = TRUE)
    line = paste("Selection:", input$time_point_id_visualPat , sep = " ")
    write(line,file = logVars$file,append = TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    fname = paste(logVars$location, "/tables/", toString(tsn), "_enrichmentGeneBMDPathway.RData", sep = "")
    save(PATWAY_tab, file = fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
  }
  
  DT::datatable(PATWAY_tab, filter = "top", selection = 'single',
                options = list(
                  search = list(regex = TRUE, caseInsensitive = FALSE),
                  scrollX = TRUE,
                  ordering = T
                )
  )
}

render_pathways_table_for_download = function(gVars, input, output){
  shiny::validate(
    need(!is.null(gVars$EnrichDatList), "No Enrichment!")
  )
  
  if(is.null(input$time_point_id_table) || sum(unlist(lapply(gVars$EnrichDatList, nrow))) ==0 ){
    return(NULL)
  }
  
  ER <- gVars$EnrichDatList[[input$time_point_id_table]]
  ER = ER[,c(5,1:4)]
  
  
  DT::datatable(ER, filter="top",
                options = list(
                  search = list(regex=TRUE, caseInsensitive=FALSE),
                  scrollX=TRUE,
                  ordering=T
                )
  )
}

plot_bmd_of_the_genes_in_a_pathway = function(gVars,input, output,experiment_col = "Experiment",
                                              enrich_ppi_info = TRUE,
                                              gene_id_type = "ENSEMBL",
                                              organism = "human"){
  # print("prova")
  if((length(input$PAT_table_rows_selected) == 0) ||
     is.null(input$time_point_id_visualPat) ||
     is.null(gVars$bmdlatestTable) ||
     is.null(gVars$inputPh()) ||
     is.null(gVars$TPColID) ||
     is.null(gVars$EnrichDatList) ||
     sum(unlist(lapply(gVars$EnrichDatList, nrow))) ==0 ){
    return(NULL)
  }
  
  time_col  = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]

  selectedrowindex = input$PAT_table_rows_selected[length(input$PAT_table_rows_selected)]
  selectedrowindex = as.numeric(selectedrowindex)
  
  ER = gVars$EnrichDatList[[input$time_point_id_visualPat]]
  PatName = ER[selectedrowindex,"annID"]
  PatGenes = ER[selectedrowindex,"gID"]
  
  exp_tp = unlist(strsplit(input$time_point_id_visualPat,"_"))
  
  BMDFilMat = gVars$bmdlatestTable
  good_idx = which(BMDFilMat[,experiment_col] == exp_tp[1] & BMDFilMat[,time_col] == exp_tp[2])
  BMDFilMat = BMDFilMat[good_idx,]
   
  gi = unlist(strsplit(x = ER[input$PAT_table_rows_selected,2],split = ","))
  
  pathway_name = ER[selectedrowindex,"Description"]
  experiment = exp_tp[1]
  timep = exp_tp[2]
  
  p = plot_bmd_bmdl_bmdu_set_of_genes(BMDFilMat, gi, 
                                      enrich_ppi_info = enrich_ppi_info,
                                      gene_id_type = gene_id_type,
                                      organism = organism)
  
  p = p + ggplot2::ggtitle(paste("Experiment: ", experiment, "; Timepoint: ",timep, "\n", pathway_name, sep = ""))  
  
  gVars$BMD_dist_in_path = p
  
  
  #save plot
  if(gVars$in_glp_mode){
    print("writing log file obj")
    line="-----------------------------------------------------"
    write(line,file=logVars$file,append=TRUE)
    
    line="ENRICHMENT"
    write(line,file=logVars$file,append=TRUE)
    
    line="GENE BMD IN PATHWAY"
    write(line,file=logVars$file,append=TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    fname = paste(logVars$location, "/plots/", toString(tsn), "_geneBMDinPathwayPlot.RData", sep="")
    save(p, file = fname)
    line=paste("The Current R Data Object is saved:", fname, sep=" ")
    write(line,file=logVars$file,append=TRUE)
    
  }
  
  
  
  
  return(p)
}

funmappone_render_compare_compare_experiments_filterby = function(gVars, input){
  if(is.null(gVars$bmdlatestTable) ||
     is.null(input$range_funmappone_filtercol_input)){
    return(NULL)
  }
  options = c()
  if("None" %in% input$range_funmappone_filtercol_input ==FALSE ){
    for (p in input$range_funmappone_filtercol_input) {
      if(p == "Model") {
        names = levels(gVars$bmdlatestTable[[p]])
      }else {
        names = unique(gVars$bmdlatestTable[[p]])
      }
      options = c(options, names)
    }
  }
  selectInput("range_funmappone_filterby_input", "Filter By Column Values", choices=options, multiple=TRUE, selectize=FALSE)
}



# EnrichmentResList: res of funmappone enrichment from function gene_set_enrichment_analysis
# bmdlatestTable optimal model stat
make_pathway_range_plot = function(EnrichmentResList,
                                   bmdlatestTable,
                                   experiment_col,
                                   other_variables_id_col,
                                   optiona_val,
                                   input_group_by,
                                   input_other_variables,
                                   input_filter_column,
                                   input_filter_column_value,
                                   is_group_by_numeric){
  
  
  if(input_other_variables=="None"){
    other_variables = NULL
  }else{
    other_variables = input_other_variables
  }
  if(input_group_by == "None") {
    groupby = NULL
  }else {
    groupby = input_group_by
  }
  if("None" %in% input_filter_column) {
    fcol = NULL
  }else {
    fcol = input_filter_column
    
  }
  if("None" %in% input_filter_column | is.null(input_filter_column_value)) {
    fby = NULL
  }else {
    #each value of fcol is its own list
    fby = NULL
    for(valg in fcol) {
      temp = c()
      
      for(vals in input_filter_column_value){
        
        if(is.element(vals, bmdlatestTable[[valg]])){
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
  
  
  group_by = groupby
  group_by2 = other_variables
  filter_column = fcol
  filter_by = fby
  
  joint_BMD_table = c()
  
  if(length(EnrichmentResList)==0) {
    p = make_empty_plot()
    
  }else{
    
    for(j in 1:length(EnrichmentResList)){
      
      info = unlist(strsplit(split = "_",x = names(EnrichmentResList)[j]))
      
      ER = EnrichmentResList[[j]]
      
      if(nrow(ER)==0) next()
      
      for(selectedrowindex in 1:nrow(ER)){
        
        PatName = ER[selectedrowindex,"Description"]
        PatGenes = ER[selectedrowindex,"gID"]
        
        #exp_tp = unlist(strsplit(input$time_point_id_range_plot,"_"))
        
        BMDFilMat = bmdlatestTable
        good_idx = which(BMDFilMat[,experiment_col] == info[1] & BMDFilMat[,other_variables_id_col] == info[2])
        BMDFilMat = BMDFilMat[good_idx,]
        
        gi = unlist(strsplit(x = ER[selectedrowindex,2],split = ","))
        idx = which(tolower(BMDFilMat[,"Feature"])%in% tolower(gi))
        
        BMD_table = BMDFilMat[idx,]
        BMD_table = BMD_table[, colnames(BMD_table) %in% c("Experiment","Feature",other_variables_id_col, optiona_val, "BMDL","BMD","BMDU")]
        to_rem = which(is.na(BMD_table$BMD) | is.na(BMD_table$BMDL) | is.na(BMD_table$BMDU))
        if(length(to_rem)>0) BMD_table = BMD_table[-to_rem,]
        if(nrow(BMD_table)==0) next()
        
        BMD_table = BMD_table[order(BMD_table$BMD),]
        BMD_table = cbind(BMD_table, PatName)
        
        BMD_table = BMD_table %>%  dplyr::group_by_at(unique(c("Feature","Experiment", group_by, group_by2,filter_column,"PatName"))) %>% dplyr::summarise( dplyr::across(c("BMD","BMDL","BMDU"), list(mean)))
        
        BMD_table2 = BMD_table[,2:ncol(BMD_table)] %>%  dplyr::group_by_at(unique(c("PatName","Experiment", group_by, group_by2,filter_column))) %>% dplyr::summarise( dplyr::across(everything(), list(mean)))
        colnames(BMD_table2) = c(unique(c("Pathway","Experiment",group_by,group_by2,filter_column)),"BMD","BMDL","BMDU")
        
        joint_BMD_table = rbind(joint_BMD_table,BMD_table2)
      }
      
    }
    
    if(!is.null(filter_column)){
      joint_BMD_table = filter_df_no(mod_stats = as.data.frame(joint_BMD_table), filter_column = filter_column,filter_by = filter_by)
      if(is.null(joint_BMD_table)){
        shinyalert(title = "Message", text = "No results after filtering!")
        return(make_empty_plot())
      } 
    }
    
    if(is_group_by_numeric){
      joint_BMD_table = as.data.frame(joint_BMD_table)
      joint_BMD_table[,group_by] = factor( joint_BMD_table[,group_by], levels = sort(unique(as.numeric( joint_BMD_table[,group_by]))))
    }
    
    joint_BMD_table = as.data.frame(joint_BMD_table)
    # joint_BMD_table = joint_BMD_table[order(joint_BMD_table$BMDL, decreasing = T),]
    joint_BMD_table$Pathway = as.factor(joint_BMD_table$Pathway)
    joint_BMD_table$Pathway <- forcats::fct_reorder(joint_BMD_table$Pathway, joint_BMD_table$BMDL, .desc = TRUE)
    
    
    Doses <- c("BMDL" = "red", "BMD" = "green", "BMDU" = "blue")
    
    funmappone_range_plot = joint_BMD_table %>%
      ggplot(aes(x = Pathway)) +
      geom_linerange(aes(ymin = BMDL, ymax = BMDU, x = Pathway),
                     size = 1.5, alpha = 0.25) +
      geom_point(aes(y = BMDL, color = "BMDL")) +
      geom_point(aes(y = BMDU, color = "BMDU")) +
      geom_point(aes(y = BMD, color = "BMD")) +
      coord_flip() +
      ylab("BMD") +
      theme_bw(base_size = 13) +
      theme(axis.title.y = element_blank()) +
      scale_color_manual(name = "Effective doses", values = Doses)
    
    
    if(!is.null(group_by)){
      if(!is.null(group_by2)){
        f = formula(paste0(group_by2,"~",group_by,sep = ""))
      }else{
        f = formula(paste0("~",group_by,sep = ""))
      }
      funmappone_range_plot = funmappone_range_plot +facet_grid(f, scales = "free_y")
    }
    
    
  }
  
  return(funmappone_range_plot)
}

render_pathway_range = function(gVars, logVars,input){
  
  if(
    is.null(gVars$bmdlatestTable) ||
    is.null(gVars$inputPh()) ||
    is.null(gVars$TPColID) ||
    is.null(gVars$EnrichDatList) ||
    sum(unlist(lapply(gVars$EnrichDatList, nrow))) ==0 ||
    is.null(input$range_funmappone_groupby_input) ||
    is.null(input$range_funmappone_groupby2_input) ||
    is.null(input$range_funmappone_filtercol_input) ||
    is.null(input$range_funmappone_is_group_by_numeric)
  ){
    return(NULL)
  }
  
  experiment_col = "Experiment"
  other_variables_id_col  = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  optiona_val = gVars$optVarsGroup 
  EnrichmentResList = gVars$EnrichDatList
  bmdlatestTable = gVars$bmdlatestTable
  
  input_group_by = input$range_funmappone_groupby_input
  
  input_other_variables = input$range_funmappone_groupby2_input
  input_filter_column = input$range_funmappone_filtercol_input
  input_filter_column_value = input$range_funmappone_filterby_input
  is_group_by_numeric = input$range_funmappone_is_group_by_numeric
  
  
  funmappone_range_plot = make_pathway_range_plot(EnrichmentResList,
                                                  bmdlatestTable,
                                                  experiment_col,
                                                  other_variables_id_col,
                                                  optiona_val,
                                                  input_group_by,
                                                  input_other_variables,
                                                  input_filter_column,
                                                  input_filter_column_value,
                                                  is_group_by_numeric)
  
  # if(is.null(input$time_point_id_range_plot) ||
  #    is.null(gVars$bmdlatestTable) ||
  #    is.null(gVars$inputPh()) ||
  #    is.null(gVars$TPColID) ||
  #    is.null(gVars$EnrichDatList)){
  #   return(make_empty_plot())
  # }
  # 
  # experiment_col = "Experiment"
  # time_col  = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  # 
  # BMD_table = c()
  # ER = gVars$EnrichDatList[[input$time_point_id_range_plot]]
  # 
  # if(nrow(ER)==0) {
  #   p = make_empty_plot()
  #   
  # }else{
  #   
  #   for(selectedrowindex in 1:nrow(ER)){
  #     
  #     PatName = ER[selectedrowindex,"Description"]
  #     PatGenes = ER[selectedrowindex,"gID"]
  #     
  #     exp_tp = unlist(strsplit(input$time_point_id_range_plot,"_"))
  #     
  #     BMDFilMat = gVars$bmdlatestTable
  #     good_idx = which(BMDFilMat[,experiment_col] == exp_tp[1] & BMDFilMat[,time_col] == exp_tp[2])
  #     BMDFilMat = BMDFilMat[good_idx,]
  #     
  #     gi = unlist(strsplit(x = ER[selectedrowindex,2],split = ","))
  #     idx = which(tolower(BMDFilMat[,"Feature"])%in% tolower(gi))
  #     BMD = as.numeric(BMDFilMat[idx,"BMD"])
  #     BMDL = as.numeric(BMDFilMat[idx,"BMDL"])
  #     BMDU = as.numeric(as.vector(BMDFilMat[idx,"BMDU"]))
  #     
  #     names(BMD) = BMDFilMat[idx,"Feature"]
  #     names(BMDL) = names(BMDU) = names(BMD)
  #     
  #     BMD = data.frame(gene = names(BMD), bmd = BMD, bmdl = BMDL, bmdu = BMDU)
  #     to_rem = which(is.na(BMD$bmd) | is.na(BMD$bmdl) | is.na(BMD$bmdu))
  #     if(length(to_rem)>0) BMD = BMD[-to_rem,]
  #     BMD = BMD[order(BMD$bmd),]
  #     
  #     BMD = BMD %>%  dplyr::group_by(gene) %>% dplyr::summarise( dplyr::across(everything(), list(mean)))
  #     colnames(BMD) = c("gene","bmd","bmdl","bmdu")
  #     BMD$gene = factor(x = BMD$gene, levels = BMD$gene)
  #     
  #     BMD = cbind(BMD ,PatName)
  #     BMD_table = rbind(BMD_table, BMD)
  #   }
  #   
  #   
  #   BMD_table2 = BMD_table[,2:5] %>%  dplyr::group_by(PatName) %>% dplyr::summarise( dplyr::across(everything(), list(mean)))
  #   colnames(BMD_table2) = c("Pathway","bmd","bmdl","bmdu")
  #   BMD_table2$Pathway = factor(x = BMD_table2$Pathway, levels = BMD_table2$Pathway)
  #   
  #   BMD_table2$Pathway <- fct_reorder(BMD_table2$Pathway, BMD_table2$bmdl, .desc = TRUE)
  #   
  #   
  #   p = BMD_table2 %>%
  #     ggplot(aes(x = Pathway)) +
  #     geom_linerange(aes(ymin = bmdl, ymax = bmdu, x = Pathway),
  #                    size = 1.5, alpha = 0.25) +
  #     geom_point(aes(y = bmdl), colour = "#0E26BD") +
  #     geom_point(aes(y = bmdu), colour = "#0E26BD") +
  #     geom_point(aes(y = bmd), colour = "#0E26BD") +
  #     coord_flip() +
  #     ylab("BMD") +
  #     theme_bw(base_size = 16) +
  #     theme(axis.title.y = element_blank())
  #   
  # }
  
  #save plot
  if(gVars$in_glp_mode){
    print("writing log file obj")
    line="-----------------------------------------------------"
    write(line,file=logVars$file,append=TRUE)
    
    line="ENRICHMENT"
    write(line,file=logVars$file,append=TRUE)
    
    line="RANGE PLOT"
    write(line,file=logVars$file,append=TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    # fname = paste(logVars$location, "/plots/", toString(tsn), "_rangePlot.RData", sep="")
    # save(funmappone_range_plot, file = fname)
    # line=paste("The Current R Data Object is saved:", fname, sep=" ")
    # write(line,file=logVars$file,append=TRUE)
    
  }
  
  return(funmappone_range_plot)
}


enrichment_bubble_plot = function(gVars, input, output){

  if(is.null(gVars$EnrichDatList) || 
     sum(unlist(lapply(gVars$EnrichDatList, nrow))) ==0 ||
     is.null(gVars$hierarchy) || 
     is.null(gVars$bmdlatestTable) || 
     is.null(gVars$inputPh()) ||
     is.null(gVars$TPColID) 
     ){ 
    return(NULL)
  }
  
  experiment_col = "Experiment"
  time_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  print(time_col)
  
  PD = c()
  
  ER <- gVars$EnrichDatList # result of enrichment
  Hier = gVars$hierarchy
  
  for(i in 1:length(ER)){
    # print(i)
    
    ERi = ER[[i]]
    if(nrow(ERi)==0) next
    
    # print(ERi)
    elemn = names(ER)[i]
    exper = strsplit(x = elemn, split = "_")[[1]][1]
    timep = strsplit(x = elemn, split = "_")[[1]][2]
    
    BMDFilMat = gVars$bmdlatestTable
    good_idx = which(BMDFilMat[,experiment_col] == exper & BMDFilMat[,time_col] == timep)
    BMDFilMat = BMDFilMat[good_idx,]
    # if(is.null(gVars$MQ_BMD_filtered)){
    #   
    #   BMDFilMat = gVars$MQ_BMD[[exper]][[as.character(timep)]]$BMDValues #filtered BMD genes
    # }else{
    #   BMDFilMat = gVars$MQ_BMD_filtered[[exper]][[as.character(timep)]]$BMDValues_filtered #filtered BMD genes
    # }
    
    if(!is.null(BMDFilMat) & nrow(ERi)>0){
      
      for(npat in 1:nrow(ER[[i]])){
        gi = unlist(strsplit(x = as.character(ERi[npat,2]),split = ","))
        
        goID = ERi[npat,"annID"]
        PatName = ERi[npat,"Description"]
        PatPval = ERi[npat,"pValueAdj"]
        NGenes = length(gi)
        
        idx = which(tolower(BMDFilMat[,"Feature"])%in% tolower(gi))
        BMD = as.numeric(BMDFilMat[idx,"BMD"])
        names(BMD) = BMDFilMat[idx,"Feature"]
        BMD = BMD[!is.na(BMD)]
        bmd = mean(BMD)
        
        levidx = which(Hier[,"ID"] %in% goID)
        levName = as.character(Hier[levidx[1],1])
        if(is.na(levName)) levName = "Root"
        PD = rbind(PD, cbind(exper, timep,PatName,PatPval, bmd, NGenes, levName))
      }
    }
  }
  
  colnames(PD) = c("Experiment","TimePoint","FunCategory", "Adj.pval","MeanBMD","NGenes", "LevName")
  PD = as.data.frame(PD)
  PD$TimePoint = factor(PD$TimePoint, level=sort(as.numeric(as.vector(unique(PD$TimePoint)))))
  PD$Adj.pval = as.numeric(as.vector(PD$Adj.pval))
  PD$MeanBMD = as.numeric(as.vector(PD$MeanBMD))
  PD$logPval = -log(PD$Adj.pval)
  PD$logPval = -log(PD$Adj.pval)
  PD$NGenes = as.numeric(as.vector(PD$NGenes))
  gVars$MeanBMDTimePoint = PD
  
  p <- PD %>%
    ggplot(aes(MeanBMD,logPval, label = FunCategory, color = LevName, size = NGenes)) + # color=FunCategory
    geom_point() +
    scale_x_log10() +
    theme_bw() + labs(x = "Mean BMD", y = "-log(P.Value)") +
    facet_grid(Experiment ~ TimePoint)
  
  
  #save plot
  if(gVars$in_glp_mode){
    print("writing log file obj")
    line="-----------------------------------------------------"
    write(line,file=logVars$file,append=TRUE)
    
    line="ENRICHMENT"
    write(line,file=logVars$file,append=TRUE)
    
    line="CLUSTER BUBBLE PLOT"
    write(line,file=logVars$file,append=TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    fname = paste(logVars$location, "/plots/", toString(tsn), "_clusterBubblePlot.RData", sep="")
    save(p, file = fname)
    line=paste("The Current R Data Object is saved:", fname, sep=" ")
    write(line,file=logVars$file,append=TRUE)
    
  }
  
  
  return(p)
}

render_time_point_pathway_selection = function(gVars, input, id="time_point_id_visualPat"){
  if(is.null(gVars$EnrichDatList)){
    return(NULL)
  }
  selectInput(id, "Time Points", choices=names(gVars$EnrichDatList), selected = names(gVars$EnrichDatList)[1])#c("All",unique(gVars$phTable[,gVars$TPColID])))
}

render_level_2_selection = function(gVars, input, id = "lev2"){
  need(input$lev1 != "", "Please select a level 1 object")
  if(is.null(input$lev1)){
    selectInput(id, "Level 2", gVars$lev2_h,multiple = TRUE,selected = "All")
  }else{
    if("All" %in% input$lev1){
      selectInput(id, "Level 2", gVars$lev2_h,multiple = TRUE,selected = "All")
    }else{
      if(length(input$lev1)==1){
        selectInput(id, "Level 2", as.list(c("All",gVars$lev2_h[[input$lev1]])),multiple = TRUE,selected = "All")
      }else{
        selectInput(id, "Level 2", c("All",gVars$lev2_h[input$lev1]),multiple = TRUE,selected = "All")
      }
    }      
  }
}

render_level_3_selection = function(gVars, input, id = "lev3"){
  need(input$lev1 != "", "Please select a level 1 object")
  need(input$lev2 != "", "Please select a level 2 object")
  
  if(is.null(input$lev2)){
    selectInput(id, "Level 3", gVars$lev3_h,multiple = TRUE,selected = "All")
  }else{
    if("All" %in% input$lev2){
      selectInput(id, "Level 3", gVars$lev3_h,multiple = TRUE,selected = "All")
    }else{
      if(length(input$lev2)==1){
        selectInput(id, "Level 3", as.list(c("All",gVars$lev3_h[[input$lev2]])),multiple = TRUE,selected = "All")
      }else{
        selectInput(id, "Level 3", c("All",gVars$lev3_h[input$lev2]),multiple = TRUE,selected = "All")
      }
    }      
  }
}


render_time_point_select_time = function(gVars, input, id="time_point_id_table"){
  shiny::validate(
    need(!is.null(gVars$EnrichDatList), "No Enrichment!")
  )
  selectInput(id, "Time Points", choices=names(gVars$EnrichDatList), selected = names(gVars$EnrichDatList)[1])#c("All",unique(gVars$phTable[,gVars$TPColID])))
  
}


render_choice_pathway_for_gene_heatmap = function(gVars, input, id = "selPath"){
  if(input$levelGene==1){
    levLocal <- gVars$lev1_h
  }else if(input$levelGene==2){
    levLocal <- gVars$lev2_h
  }else if(input$levelGene==3){
    levLocal <- gVars$lev3_h
  }else{
    print("Incorrect level provided!")
    levLocal <- c("NONE")
  }
  
  idxAll <- which(tolower(levLocal)=="all")
  if(length(idxAll)>0){
    print("!!!!!!!! REPLACING 'ALL' !!!!!!!")
    levLocal[idxAll] <- "NONE"
    if(length(names(levLocal))>0){
      names(levLocal)[idxAll] <- "NONE"
    }
  }else{
    print("!!!!!!!! ALL NOT FOUND !!!!!!!")
  }
  selectInput(id, "Choose Term", levLocal, multiple=FALSE, selected="NONE")
  
  
  
}

build_gene_list = function(BMD_TAB,time_variable){
  
  library(dplyr)
  unique_combo = unique(BMD_TAB[,c("Experiment",time_variable)])
  
  
  GList = list()
  grouping_experiment = c()
  grouping_TP = c()
  grouping_unique = c()
  index = 1
  
  for(i in 1:nrow(unique_combo)){
    experiment_id = unique_combo[i,1]
    time_point_id = unique_combo[i,2]
    idx = which(BMD_TAB[,"Experiment"] == experiment_id & BMD_TAB[,time_variable] == time_point_id)
    
    genelist = BMD_TAB[idx,c("Feature","BMD")]
    genelist = as.data.frame(genelist)
    genelist$BMD = as.numeric(genelist$BMD)
    genelist = genelist[!is.na(genelist[,2]),]
    
    genelist = genelist %>%  dplyr::group_by(Feature) %>% dplyr::summarise( dplyr::across(everything(), list(mean)))
    genelist = as.data.frame(genelist)
    colnames(genelist) = c("Feature","BMD")
    
    GList[[paste(experiment_id,time_point_id,sep="_")]] = genelist
    grouping_experiment = c(grouping_experiment,experiment_id)
    grouping_TP = c(grouping_TP,time_point_id)
    grouping_unique = c(grouping_unique,index)
    index = index + 1
  }
  
  Mp = cbind(names(GList),1:length(GList),grouping_experiment,grouping_TP)
  colnames(Mp) = c("Chemical","Grouping", "Experiment","Timepoint")
  pheno = cbind(Mp[,4],Mp[,1])
  return(list(GList=GList,pheno = pheno,grouping_experiment=grouping_experiment,grouping_TP=grouping_TP,grouping_unique=grouping_unique))
  
}

funmappone_perform_enrichment = function(gVars, logVars, input){
  time_variable = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  BMD_TAB = gVars$bmdlatestTable
  organism = input$organism
  annType = input$idtype
  
  tryCatch(
    {
      res  = build_gene_list(BMD_TAB,time_variable)
      GList = res$GList
      grouping_experiment = res$grouping_experiment
      grouping_TP = res$grouping_TP
      grouping_unique = res$grouping_unique
      pheno = res$pheno
      GList = convert_genes(organism = input$organism, GList=GList, annType = input$idtype)
      
      # TODO: consider removing Mp
      # Mp = cbind(names(GList),1:length(GList),grouping_experiment,grouping_TP)
      # colnames(Mp) = c("Chemical","Grouping", "Experiment","Timepoint")
      # pheno = cbind(Mp[,4],Mp[,1])
      
      DTa = matrix("",ncol = length(GList),nrow = max(unlist(lapply(GList, FUN = nrow))))
      for(i in 1:(length(GList))){
        gli = as.character(GList[[i]][,1])
        if(length(gli)>0) DTa[1:nrow(GList[[i]]),i]= gli
      }
      
      colnames(DTa) = names(GList)
      
      if(is.null(DTa)){
        gVars$DATA = NULL
      }else{
        gVars$DATA = DTa
      }
      
      gVars$toPlot <- NULL # refresh plot map in the Plot Maps tab
      gVars$clust_mat <- NULL
      gVars$KEGG_MAT <- NULL
      
      DAT = gVars$DATA
      if(input$organism == "Mouse"){
        org_enrich = "mmusculus"
      }
      if(input$organism == "Human"){
        org_enrich = "hsapiens"
      }
      if(input$organism == "Rat"){
        org_enrich = "rnorvegicus"
      }
      
      annType = input$EnrichType #e.g. KEGG
      GOType = input$GOType # e.g. BP
      if(annType == "GO") annType = paste(annType, GOType,sep = ":")
      
      pval = as.numeric(input$pvalueTh)
      adjust_method = input$pcorrection
      
      # print("prova")
      enrichedList = gene_set_enrichment_analysis(gene_sets = GList,
                                                  organism=org_enrich,
                                                  pathway_database=annType,
                                                  only_annotated_genes=input$only_annotated,
                                                  pvalue_correction=adjust_method,
                                                  pvalue_threshold=pval,
                                                  return_only_significant=input$only_significant,
                                                  min_intersection_size = as.numeric(input$min_intersection),
                                                  output_map_type = input$MapValueType,
                                                  aggregation_function = input$aggregation
      )
      
      gVars$EnrichDatList = enrichedList$enriched_data_list
      gVars$hierarchy = enrichedList$hierarchy
      gVars$KEGG_MAT = enrichedList$KEGG_MAT
      gVars$KEGG_MAT_GENES = enrichedList$KEGG_MAT_GENES
      hier = enrichedList$hierarchy
      
      gVars$lev1_h = enrichedList$lvl1_h
      gVars$lev2_h = enrichedList$lvl2_h
      gVars$lev3_h = enrichedList$lvl3_h
      gVars$pheno_funmappone = pheno
      gVars$GList = GList
      
      
      #LOGGING here, does not seem to have been seperated from server
      logVars$enrichedList = enrichedList
      logVars$GList = GList
      if(gVars$in_glp_mode){
        
        #GLP MODE
        shinyalert(
          title = "GLP Mode Justification",
          text = "Please provide a justification for the Functional Annotation Parameters, p-value and aggregation Function.",
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
          callbackR =function(x) {logVars$enrichmentReason = x}
          
        )
        
        #save for report
        ts = Sys.time()
        logVars$enrichmentTimestamp = ts
        logVars$enrichmentOrganisms = input$organism
        logVars$enrichmentGeneID = input$idtype
        logVars$enrichmentFunctionalAnnotation = input$EnrichType
        logVars$enrichmentGO = input$GOType
        logVars$enrichmentcorrectionM = input$pcorrection
        logVars$enrichmentPvalTh = input$pvalueTh
        logVars$enrichmentSigRes = input$only_significant
        logVars$enrichmentAnnGenes = input$only_annotated
        logVars$enrichmentMinNGenes = input$min_intersection
        logVars$enrichmenAggFunction = input$aggregation
        logVars$enrichmentValT = input$MapValueType
        logVars$enrichmentPlotM = input$continuous
        
      }
      if(gVars$in_glp_mode){
        print("writing log file obj")
        line="-----------------------------------------------------"
        write(line,file=logVars$file,append=TRUE)
        
        line="ENRICHMENT"
        write(line,file=logVars$file,append=TRUE)
        
        line="RESULTS"
        write(line,file=logVars$file,append=TRUE)
        
        
        
        ts = Sys.time()
        tsn = as.numeric(ts)
        # fname = paste(logVars$location, "/tables/", toString(tsn), "_enrichedList.RData", sep="")
        # save(enrichedList, file = fname)
        # line=paste("The Current R Data Object is saved:", fname, sep=" ")
        # write(line,file=logVars$file,append=TRUE)
        # line=paste("You can access the enriched data table with $enriched_data_list, which is the table plotted in the Pathways Table tab", "", sep=" ")
        # write(line,file=logVars$file,append=TRUE)
        # 
        
        # fname = paste(logVars$location, "/tables/", toString(tsn), "_enrichedGList.RData", sep="")
        # save(GList, file = fname)
        # line=paste("The Current R Data Object is saved:", fname, sep=" ")
        # write(line,file=logVars$file,append=TRUE)
        # 
      }
      
      
      on.exit({
        print("inside on exit 733")
        shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
      })
      
      
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      shinyjs::info(e$message)
      shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
    })
  
  
  return(list(gVars,logVars))  
}


# funmappone_perform_enrichment = function(gVars, logVars, input){
#   time_variable = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
#   
#   BMD_TAB = gVars$bmdlatestTable
#   
#   unique_combo = unique(BMD_TAB[,c("Experiment",time_variable)])
#   
#   organism = input$organism
#   annType = input$idtype
#   
#   tryCatch(
#     {
#       GList = list()
#       grouping_experiment = c()
#       grouping_TP = c()
#       grouping_unique = c()
#       index = 1
#       
#       for(i in 1:nrow(unique_combo)){
#         experiment_id = unique_combo[i,1]
#         time_point_id = unique_combo[i,2]
#         idx = which(BMD_TAB[,"Experiment"] == experiment_id & BMD_TAB[,time_variable] == time_point_id)
#         
#         genelist = BMD_TAB[idx,c("Feature","BMD")]
#         genelist = as.data.frame(genelist)
#         genelist$BMD = as.numeric(genelist$BMD)
#         genelist = genelist[!is.na(genelist[,2]),]
#         
#         genelist = genelist %>%  dplyr::group_by(Feature) %>% dplyr::summarise( dplyr::across(everything(), list(mean)))
#         genelist = as.data.frame(genelist)
#         colnames(genelist) = c("Feature","BMD")
#         
#         GList[[paste(experiment_id,time_point_id,sep="_")]] = genelist
#         grouping_experiment = c(grouping_experiment,experiment_id)
#         grouping_TP = c(grouping_TP,time_point_id)
#         grouping_unique = c(grouping_unique,index)
#         index = index + 1
#       }
#     
#       
#       GList = convert_genes(organism = input$organism, GList=GList, annType = input$idtype)
#       
#       # TODO: consider removing Mp
#       Mp = cbind(names(GList),1:length(GList),grouping_experiment,grouping_TP)
#       colnames(Mp) = c("Chemical","Grouping", "Experiment","Timepoint")
#       pheno = cbind(Mp[,4],Mp[,1])
#       
#       DTa = matrix("",ncol = length(GList),nrow = max(unlist(lapply(GList, FUN = nrow))))
#       for(i in 1:(length(GList))){
#         gli = as.character(GList[[i]][,1])
#         if(length(gli)>0) DTa[1:nrow(GList[[i]]),i]= gli
#       }
#       
#       colnames(DTa) = names(GList)
#       
#       if(is.null(DTa)){
#         gVars$DATA = NULL
#       }else{
#         gVars$DATA = DTa
#       }
#       
#       gVars$toPlot <- NULL # refresh plot map in the Plot Maps tab
#       gVars$clust_mat <- NULL
#       gVars$KEGG_MAT <- NULL
#       
#       DAT = gVars$DATA
#       if(input$organism == "Mouse"){
#         org_enrich = "mmusculus"
#       }
#       if(input$organism == "Human"){
#         org_enrich = "hsapiens"
#       }
#       if(input$organism == "Rat"){
#         org_enrich = "rnorvegicus"
#       }
#       
#       annType = input$EnrichType #e.g. KEGG
#       GOType = input$GOType # e.g. BP
#       if(annType == "GO") annType = paste(annType, GOType,sep = ":")
#       
#       pval = as.numeric(input$pvalueTh)
#       adjust_method = input$pcorrection
#       
#       # print("prova")
#       enrichedList = gene_set_enrichment_analysis(gene_sets = GList,
#                                                   organism=org_enrich,
#                                                   pathway_database=annType,
#                                                   only_annotated_genes=input$only_annotated,
#                                                   pvalue_correction=adjust_method,
#                                                   pvalue_threshold=pval,
#                                                   return_only_significant=input$only_significant,
#                                                   min_intersection_size = as.numeric(input$min_intersection),
#                                                   output_map_type = input$MapValueType,
#                                                   aggregation_function = input$aggregation
#       )
#       
#       gVars$EnrichDatList = enrichedList$enriched_data_list
#       gVars$hierarchy = enrichedList$hierarchy
#       gVars$KEGG_MAT = enrichedList$KEGG_MAT
#       gVars$KEGG_MAT_GENES = enrichedList$KEGG_MAT_GENES
#       hier = enrichedList$hierarchy
#       
#       gVars$lev1_h = enrichedList$lvl1_h
#       gVars$lev2_h = enrichedList$lvl2_h
#       gVars$lev3_h = enrichedList$lvl3_h
#       gVars$pheno_funmappone = pheno
#       gVars$GList = GList
#       
#       
#       #LOGGING here, does not seem to have been seperated from server
#       logVars$enrichedList = enrichedList
#       logVars$GList = GList
#       if(gVars$in_glp_mode){
#         
#         #GLP MODE
#         shinyalert(
#           title = "GLP Mode Justification",
#           text = "Please provide a justification for the Functional Annotation Parameters, p-value and aggregation Function.",
#           size = "s",
#           closeOnEsc = TRUE,
#           closeOnClickOutside = FALSE,
#           html = FALSE,
#           type = "input",
#           inputType = "text",
#           inputValue = "",
#           inputPlaceholder = "",
#           showConfirmButton = TRUE,
#           showCancelButton = FALSE,
#           confirmButtonText = "Submit",
#           confirmButtonCol = "#AEDEF4",
#           timer = 0,
#           imageUrl = "",
#           animation = TRUE,
#           callbackR =function(x) {logVars$enrichmentReason = x}
#           
#         )
#         
#         #save for report
#         ts = Sys.time()
#         logVars$enrichmentTimestamp = ts
#         logVars$enrichmentOrganisms = input$organism
#         logVars$enrichmentGeneID = input$idtype
#         logVars$enrichmentFunctionalAnnotation = input$EnrichType
#         logVars$enrichmentGO = input$GOType
#         logVars$enrichmentcorrectionM = input$pcorrection
#         logVars$enrichmentPvalTh = input$pvalueTh
#         logVars$enrichmentSigRes = input$only_significant
#         logVars$enrichmentAnnGenes = input$only_annotated
#         logVars$enrichmentMinNGenes = input$min_intersection
#         logVars$enrichmenAggFunction = input$aggregation
#         logVars$enrichmentValT = input$MapValueType
#         logVars$enrichmentPlotM = input$continuous
#         
#       }
#       if(gVars$in_glp_mode){
#         print("writing log file obj")
#         line="-----------------------------------------------------"
#         write(line,file=logVars$file,append=TRUE)
#         
#         line="ENRICHMENT"
#         write(line,file=logVars$file,append=TRUE)
#         
#         line="RESULTS"
#         write(line,file=logVars$file,append=TRUE)
#         
#         
#         
#         ts = Sys.time()
#         tsn = as.numeric(ts)
#         fname = paste(logVars$location, "/tables/", toString(tsn), "_enrichedList.RData", sep="")
#         save(enrichedList, file = fname)
#         line=paste("The Current R Data Object is saved:", fname, sep=" ")
#         write(line,file=logVars$file,append=TRUE)
#         line=paste("You can access the enriched data table with $enriched_data_list, which is the table plotted in the Pathways Table tab", "", sep=" ")
#         write(line,file=logVars$file,append=TRUE)
#         
#         
#         fname = paste(logVars$location, "/tables/", toString(tsn), "_enrichedGList.RData", sep="")
#         save(GList, file = fname)
#         line=paste("The Current R Data Object is saved:", fname, sep=" ")
#         write(line,file=logVars$file,append=TRUE)
#         
#       }
#       
#       
#       on.exit({
#         print("inside on exit 733")
#         shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
#       })
# 
#       
#     },
#     error = function(e) {
#       # return a safeError if a parsing error occurs
#       shinyjs::info(e$message)
#       shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
#     })
#   
#   
#     return(list(gVars,logVars))  
# }

build_funmappone_heatmap = function(gVars, logVars, input){
  shiny::validate(need(expr = !is.null(gVars$EnrichDatList),message = "No data to plot in plot funmappone heatmap"))
  
  shiny::validate(need(expr = !is.null(gVars$KEGG_MAT),message = "No data to plot in build funmappone heatmap"))
  
  shiny::validate(need(expr = !is.null(input$colID),message = "Please select samples"))
  
  condition_to_pass = input$colID=="All" | (input$colID!="All" & length(input$colID)>=2)
  shiny::validate(need(expr = condition_to_pass,message = "Please select at least two samples"))
  shiny::validate(need(expr = !is.null(input$level),message = "Please select a level for the plot"))
  
  shiny::validate(need(expr = !is.null(input$lev1),message = "Please select a level 1 object"))
  shiny::validate(need(expr = !is.null(input$lev2),message = "Please select a level 2 object"))
  shiny::validate(need(expr = !is.null(input$lev3),message = "Please select a level 3 object"))
  
  shinyjs::html(id="loadingText", "Rendering Map")
  shinyjs::show(id="loading-content")
  
  on.exit({
    print("inside on exit 886")
    shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
  })
  
  if(ncol(gVars$KEGG_MAT)>0){
    p <- create_map_plot(
      lev1_content = input$lev1,
      lev2_content = input$lev2,
      lev3_content = input$lev3,
      experiment_ann =  as.data.frame(gVars$pheno_funmappone),
      kegg_hierarchy = gVars$hierarchy,
      toPlotMap = gVars$KEGG_MAT,
      samplesID = input$colID,
      collapse_level = as.numeric(input$level),
      doGrouping = input$doGrouping,
      aspectRatio = F,
      continuous = input$continuous=="continuous")
  }else{
    p = make_empty_plot()
  }
 
  gVars$toPlot <- p
  
  if(gVars$in_glp_mode){
    line="-----------------------------------------------------"
    write(line,file=logVars$file,append=TRUE)
    
    line="ENRICHMENT"
    write(line,file=logVars$file,append=TRUE)
    
    line="HEATMAP"
    write(line,file=logVars$file,append=TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    # fname = paste(logVars$location, "/plots/", toString(tsn), "_enrichmentHeatMap.RData", sep="")
    # save(p, file = fname)
    # line=paste("The Current R Data Object is saved:", fname, sep=" ")
    # write(line,file=logVars$file,append=TRUE)
    # 
    ts = Sys.time()
    logVars$buildFunmmapponeHeatmapTimestamp = ts
    logVars$buildFunmmapponeHeatmapLev1 = input$lev1
    logVars$buildFunmmapponeHeatmapLev2 = input$lev2
    logVars$buildFunmmapponeHeatmapLev3 = input$lev3
    logVars$buildFunmmapponeHeatmapSamplesID = input$colID
    logVars$buildFunmmapponeHeatmapCollapseLevel = input$level
    logVars$buildFunmmapponeHeatmapDoGrouping = input$doGrouping
    logVars$buildFunmmapponeHeatmapCountinous = input$continous
  }
  

  
  #check for display size, pop up message if too large
  if(!is.null(gVars$KEGG_MAT) && ((ncol(gVars$KEGG_MAT)* 20 ) + 10 * max(sapply(gVars$pheno_funmappone,nchar))) > 30e3){
    print("exceeding dimensions")
    shinyjs::info("Warning: too many functional categories, the map might not be readable. Download the PDF for better resolution image.")
  }
  
  return(gVars)
}

plot_gene_heatmap = function(gVars, input){
  shiny::validate(need(expr = !is.null(gVars$heatmap_params), message = "No data to plot in plot gene heatmap"))
  
  grid_draw_plot = grid_draw(toPlotGenes = gVars$heatmap_params$toPlotGenes,
                             kegg_nano_1 = gVars$heatmap_params$res_collapsed,
                             mat_to_Plot = gVars$heatmap_params$mat_to_Plot,
                             exp_ann = gVars$pheno_funmappone)#gVars$plotting_params$exp_ann)
  
  
  gVars$grid_draw_plot = grid_draw_plot
  if(gVars$in_glp_mode){
    print("writing log file obj")
    line="-----------------------------------------------------"
    write(line,file=logVars$file,append=TRUE)
    
    line="ENRICHMENT"
    write(line,file=logVars$file,append=TRUE)
    
    line="GENE HEATMAP"
    write(line,file=logVars$file,append=TRUE)
    
    line=paste("Hierarchy Level:", input$levelGene , sep=" ")
    write(line,file=logVars$file,append=TRUE)
    
    line=paste("Term:", input$selPath , sep=" ")
    write(line,file=logVars$file,append=TRUE)
    
    line=paste("Values:", input$selScoreType , sep=" ")
    write(line,file=logVars$file,append=TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    fname = paste(logVars$location, "/plots/", toString(tsn), "_enrichmentGeneHeatMap.RData", sep="")
    save(grid_draw_plot, file = fname)
    line=paste("The Current R Data Object is saved:", fname, sep=" ")
    write(line,file=logVars$file,append=TRUE)
    
  }
  
  return(gVars)
}

download_funmappone_input = function(gVars, file){
    
    if(is.null(gVars$GList) || is.null(gVars$pheno)){
      return(NULL)
    }
    
    shinyjs::html(id="loadingText", "Saving tables")
    shinyjs::show(id="loading-content")
    on.exit({
      # print("inside on exit")
      shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
    })
    
    write.xlsx(gVars$GList[[1]], file, sheetName = names(gVars$GList)[1], row.names = FALSE) 
    
    if(length(gVars$GList)>1){
      for(i in 2:length(gVars$GList)){
        write.xlsx(gVars$GList[[i]], file, sheetName = names(gVars$GList)[i], append = TRUE, row.names = FALSE) 
      }
    }
    
    write.xlsx(gVars$pheno, file, sheetName = "Groups", append = TRUE, row.names = FALSE) 

}

download_enriched_pathways_table = function(gVars, file){
  if(length(gVars$EnrichDatList)==0){ 
    print("No enrichment tables to save!")
    return(NULL)
  }
  
  shinyjs::html(id="loadingText", "Saving tables")
  shinyjs::show(id="loading-content")
  on.exit({
    print("inside on exit")
    shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")    
  })
  
  write_pathways_to_excel(filePath = file,
                          enriched_data_list = gVars$EnrichDatList)
}
