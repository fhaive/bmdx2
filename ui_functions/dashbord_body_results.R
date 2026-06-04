filtering_results = function(){
  tabPanel(value="AnovaTab", title="Pre-filtering",
           fluidRow(
             column(4,uiOutput("ExptimeSelAnova")),
             column(4,uiOutput("timePointSel")),
             column(4,downloadButton("downloadAnovaData", "Download All Filtered Data"))
           ),
           fluidRow(
             column(12,DT::dataTableOutput("Anova_table")),
           ),
           fluidRow(
             column(6,fluidRow(shinycssloaders::withSpinner(plotOutput("anovaPlot"), type = 6)))
             
           )
           
  )
}

aop_results = function(){
  tabPanel(value = "aop", title = "Adverse Outcome Pathway",

     tabsetPanel(
       tabPanel("Key events table",
                tabsetPanel(
                  tabPanel("Enrichment table",
                           fluidRow(
                             column(4,uiOutput("ExptimeSelKE")),
                             column(4,uiOutput("timePointSelKE")),
                             column(3, checkboxInput(inputId = "KEr_enrichment_table_convert_to_symbol",label = "Convert to symbol",value = TRUE)),
                             
                           ),
                           fluidRow(
                             column(4,downloadButton("downloadKE_enrich_res", "Download KEs enrichment results"))
                           ),
                           fluidRow(
                             column(12,DT::dataTableOutput("KE_enrichment_stat_dataframe"))
                           ),
                           fluidRow(
                             column(12,
                                    plotlyOutput("KE_bmd_dist"))
                           ),
                           fluidRow(
                             column(12,
                                    plotlyOutput("KE_bmd_dist_by_ppi_degree"))
                           )
                  ),
                  tabPanel("POD distribution across KEs",
                           
                           bsCollapse(id = "collapseGeneComparison", open = "Panel 2",
                              bsCollapsePanel("BML by KE Type",  style = "primary",
                                  fluidRow(
                                    column(12,plotlyOutput("BMD_boxplot_ke_type_bmdl"))
                                  )
                              ),
                              bsCollapsePanel("BMD by KE Type",  style = "primary",
                                  fluidRow(
                                    column(12,plotlyOutput("BMD_boxplot_ke_type_bmd"))
                                  )
                              ),
                              bsCollapsePanel("BMDU by KE Type",  style = "primary",
                                  fluidRow(
                                    column(12,plotlyOutput("BMD_boxplot_ke_type_bmdu"))
                                  )
                              ),
                              bsCollapsePanel("BMD by KE Level",  style = "primary",
                                  fluidRow(
                                    column(12,plotlyOutput("BMD_boxplot_ke_level"))
                                  )
                              ),
                              bsCollapsePanel("BMD by KE annotation to Systems",  style = "primary",
                                        fluidRow(
                                          column(12,plotlyOutput("BMD_boxplot_by_system"))
                                        )
                              )
                           )
                  )
                )
       ),
       tabPanel("KEr",
                tabsetPanel(
                  tabPanel("Individual KEr",
                           fluidRow(
                             column(4,uiOutput("ExptimeSelKEr")),
                             column(4,uiOutput("timePointSelKEr")),
                           ),
                           fluidRow(
                             column(3, checkboxInput(inputId = "KEr_filter_ke_by_aop_fingerprint",label = "Filter only AOP fingerprint",value = TRUE)),
                             shinyBS::bsTooltip("KEr_filter_ke_by_aop_fingerprint", "If selected only the KEs of the AOP present in the fingerprint will be visualized in the network", placement = "bottom"),
                           ),
                           fluidRow(
                             column(3, checkboxInput(inputId = "KEr_enlarge_ke_selection",label = "Enlarge KEs",value = TRUE)),
                             shinyBS::bsTooltip(id = "KEr_enlarge_ke_selection", 
                                                title = "If selected also most likely to reach MIEs and AOs will be added to the network", 
                                                placement = "bottom",trigger = "hover"),
                             
                             column(3, sliderInput("max_path_length", "Max path length", min=0, max=20, step=1,value = 3)),
                             shinyBS::bsTooltip(id = "max_path_length", 
                                                title = "The maximum distance (from the enriched KEs) to which MIEs and AOs can be looked for", 
                                                placement = "bottom",trigger = "hover"),
                             
                             column(3, sliderInput("n_AOs", "Number of adverse outcomes", min=0, max=20, step=1,value = 2)),
                             shinyBS::bsTooltip(id = "n_AOs", 
                                                title = "Number of adverse outcomes (AO) for each enriched KE", 
                                                placement = "bottom",trigger = "hover"),
                             
                             column(3, sliderInput("n_MIEs", "Number of molecular initiating events", min=0, max=20, step=1,value = 2)),
                             shinyBS::bsTooltip(id = "n_MIEs", 
                                                title = "Number of molecular initiating events (MIE) for each enriched KE", 
                                                placement = "bottom",trigger = "hover")
                           ),
                           
                           fluidRow(
                             column(3, checkboxInput(inputId = "convert_to_gene_symbol",label = "Use gene symbols",value = TRUE)),
                             shinyBS::bsTooltip(id = "convert_to_gene_symbol", 
                                                title = "If selected genes in the network will be converted to gene symbols", 
                                                placement = "bottom",trigger = "hover"),
                             column(4, checkboxInput(inputId = "KEr_group_by_ssbd",label = "Group by SSbD categories",value = TRUE)),
                             shinyBS::bsTooltip("KEr_group_by_ssbd", "If selected only the KEs of the AOP network are grouped by the SSbD categories", placement = "bottom"),
                           ),
                           fluidRow(
                             column(4, actionButton(inputId = "Create_KEr_network", label = "Create KE-KE relationship network")),
                             column(4, downloadButton("downloadvisNetworkOutput", "Download Network as HTML"))
                           ),
                           fluidRow(
                             column(12, visNetwork::visNetworkOutput(outputId = "display_make_visNetwork",height = "1000px"))
                           )  
                  ),
                  tabPanel("Comparison between KEr",
                           fluidRow(
                             column(4,uiOutput("PlotKErList_A")),
                             column(4,uiOutput("PlotKErList_B"))
                           ),
                           fluidRow(
                             column(6, "Network A intersect Network B"),
                             column(6, "Network A minus Network B")
                             
                           ),
                           fluidRow(
                             tabsetPanel(
                               tabPanel("Intersect",
                                        column(6, visNetwork::visNetworkOutput(outputId = "display_intersect",height = "1000px"))
                               ),
                               tabPanel("Difference",
                                        column(6, visNetwork::visNetworkOutput(outputId = "display_difference",height = "1000px"))
                               ),
                               tabPanel("Union",
                                        column(6, visNetwork::visNetworkOutput(outputId = "display_union",height = "1000px"))
                               )
                             )
                           )
                           
                  )
                )
                
                
                
       ),
       tabPanel("AOP table",
                tabsetPanel(
                  tabPanel("AOPs Visnetwork",  
                       fluidRow(
                         column(4,uiOutput("ExptimeSelAOP_visplot")),
                         column(4,uiOutput("timePointSelAOP_visplot"))
                       ),
                       fluidRow(
                         column(3,uiOutput("AOP_selection_for_visnet")),
                         column(3, checkboxInput(inputId = "AOP_visnet_only_direct",label = "Only directed edges",value = TRUE)),
                         column(3, sliderInput("aop_ppi_genegene_net_top_n_genes", "TopGenes", min=0, max=100, step=1,value = 5)),
                         
                       ),
                       fluidRow(
                         column(12,
                         tabsetPanel(
                           tabPanel("AOP enriched with molecular information", 
                                    fluidRow(
                                      column(12,visNetwork::visNetworkOutput("aop_net_visnetwork",height = "800px"))
                                    )
                           ),
                           tabPanel("AOP related gene subnetwork", 
                                    fluidRow(
                                      column(12,visNetwork::visNetworkOutput("aop_based_ppi_genegene_network", height = "800px"))
                                    )
                           )
                         )
                       ))
                       
                  ),
                  tabPanel("AOPs by SSbD categories",    
                           fluidRow(
                             sliderInput("ssbd_text_size", "Text size:", min = 8, max = 24, value = 14),
                           ),
                           fluidRow(
                             column(12,plotlyOutput("AOP_by_ssbdcategory", height = "90vh"))
                           )
                  ),
                  tabPanel("Enrichment table",     
                           fluidRow(
                             column(4,uiOutput("ExptimeSelAOP")),
                             column(4,uiOutput("timePointSelAOP"))
                           ),
                           fluidRow(
                             column(4,downloadButton("downloadAop_enrichment_results", "Download AOPs enrichment results"))
                           ),
                           fluidRow(
                             column(12,DT::dataTableOutput("AOP_enrichment_stat_dataframe"))
                           )
                  )
                )
       ),
       tabPanel("AOP fingerprint",
          tabsetPanel(
            tabPanel("Bubble plot",
                     fluidRow(
                       column(3,uiOutput("range_AOP_fingerprint_bubble_plot_groupby")),
                       shinyBS::bsTooltip("range_AOP_fingerprint_bubble_plot_groupby", "Main variable used for the plot! It defines which colums the AOP fingerprint will have", placement = "bottom"),
                       
                       # column(3,uiOutput("range_AOP_fingerprint_bubble_plot_groupby2")),
                       # shinyBS::bsTooltip("range_AOP_fingerprint_bubble_plot_groupby2", "plot group by", placement = "bottom"),
                       
                       column(3,checkboxInput("range_AOP_fingerprint_bubble_plot_is_group_by_numeric", "numeric grouping variable", value = FALSE)),
                       shinyBS::bsTooltip("range_AOP_fingerprint_bubble_plot_is_group_by_numeric", "if selected the group by variable is treated as numeric, and the column of the AOP fingerprint will order accordingly", placement = "bottom"),
                       
                       column(3,uiOutput("range_AOP_fingerprint_bubble_plot_filtercol")),
                       shinyBS::bsTooltip("range_AOP_fingerprint_bubble_plot_filtercol", "Select variable name to filter the data in the plot", placement = "bottom"),
                       
                       column(3,uiOutput("range_AOP_fingerprint_bubble_plot_filterby")),
                       shinyBS::bsTooltip("range_AOP_fingerprint_bubble_plot_filterby", "Select variable value to filter the data in the plot", placement = "bottom")
                       
                     ),
                     
                     fluidRow(
                       column(3,  sliderInput("min_proportion_AOP_fingerprint", "Minimum proportion of KE enriched in AOP:", min = 0, max = 1, value = 0)),
                       shinyBS::bsTooltip("min_proportion_AOP_fingerprint", "AOP with proportion of KE lower that the selected treshold will not be included in the plot", placement = "bottom"),
                       
                       column(3, checkboxInput("range_AOP_fingerprint_bubble_plot_is_group_by_SSbD", "Group AOP by SSbD Category", value = TRUE)),
                       shinyBS::bsTooltip("range_AOP_fingerprint_bubble_plot_is_group_by_SSbD", "If TRUE AOPs are grouped by their SSbD Category. This groups the rows of the AOP fingerprint plot.")
                     ),
                  
                     
                     fluidRow(
                        column(3, uiOutput("range_AOP_fingerprint_bubble_plot_x_axis")),
                        shinyBS::bsTooltip("range_AOP_fingerprint_bubble_plot_x_axis", "variable for the x axis"),
                        column(3, selectInput("range_AOP_fingerprint_bubble_plot_y_axis_input", "Y Axis", choices = c("AOP names"="a.name", "AOP IDs"="TermID"), selected = "a.name")),
                        shinyBS::bsTooltip("range_AOP_fingerprint_bubble_plot_y_axis_input", "variable for the y axis, either the AOP names or AOP ids can be used"),
                        column(3, sliderInput("AOP_fingerprint_font_size","Font size", min = 1, max = 20, value = 12)),
                        shinyBS::bsTooltip("AOP_fingerprint_font_size", "Smaller values correspond to lower font size in the plot", placement = "bottom")
                        
                     ),

                     
                    
                     fluidRow(
                       column(3, actionButton(inputId = "AOP_fingerprint_bubble_plot", label = "Bubble plot")),
                       shinyBS::bsTooltip("AOP_fingerprint_bubble_plot", "Plot bunnle plot for AOP fingerprint", placement = "bottom")
                       
                     ),
                     fluidRow(
                       column(12,plotlyOutput("bubble_plot_AOP_fingerprint")),
                     ),
                     fluidRow(
                       column(3, sliderInput("AOP_fingerprint_bubble_plot_height","Plot Height (cm)", min = 1, max = 3000, value = 300)),
                       column(3, sliderInput("AOP_fingerprint_bubble_plot_width","Plot Width (cm)", min = 1, max = 3000, value = 700)),
                       column(3,downloadButton("downloadbubble_plot_AOP_fingerprint", "Download bubble plot as a pdf")),
                       column(3,downloadButton("downloadAOP_fingerprint_results", "Download AOP fingerprint enrichment results"))
                     )
            ),
            tabPanel("Range plot",
                     fluidRow(
                       column(3,uiOutput("range_AOP_fingerprint_groupby")),
                       shinyBS::bsTooltip("range_AOP_fingerprint_groupby", "main variable used for the plot", placement = "bottom"),
                       column(3,uiOutput("range_AOP_fingerprint_groupby2")),
                       shinyBS::bsTooltip("range_AOP_fingerprint_groupby2", "plot group by", placement = "bottom"),
                       column(3,checkboxInput("range_AOP_fingerprint_is_group_by_numeric", "numeric grouping variable", value = FALSE)),
                       shinyBS::bsTooltip("range_AOP_fingerprint_is_group_by_numeric", "if selected the group by variable is treated as numeric", placement = "bottom")
                     ),
                     fluidRow(
                       column(3,uiOutput("range_AOP_fingerprint_filtercol")),
                       shinyBS::bsTooltip("range_AOP_fingerprint_filtercol", "Select variable name to filter the data in the plot", placement = "bottom"),
                       column(3,uiOutput("range_AOP_fingerprint_filterby")),
                       shinyBS::bsTooltip("range_AOP_fingerprint_filterby", "Select variable value to filter the data in the plot", placement = "bottom")
                     ),
                     fluidRow(
                       actionButton(inputId = "range_plot_AOP_fingerprint", label = "Range plot")
                     ),
                     fluidRow(
                       column(3, sliderInput("range_AOP_fingerprint_font_size","Font size", min = 1, max = 20, value = 12)),
                       column(12, plotlyOutput("range_AOP_fingerprint_plotly"))
                     )
            ),
          )
       )
       # tabPanel("KE by tissue",
       #    fluidRow(
       #      column(4, uiOutput("KE_tissue_selection")),
       # 
       #     column(4, checkboxInput(inputId = "KE_tissue_group_by_time",label = "Group by time",value = TRUE)),
       #     shinyBS::bsTooltip("KE_tissue_group_by_time", "If selected KEs will be grouped by time in the plot", placement = "bottom"),
       # 
       #      column(4,uiOutput("KE_tissue_experiment_selection"))
       #    ),
       #    fluidRow(
       #      column(3, actionButton(inputId = "KE_tissue_selection_action_button", label = "Plot KE by Tissue")),
       #      shinyBS::bsTooltip("KE_tissue_selection_action_button", "Plot barplot for BMD by KE by tissue", placement = "bottom")
       #    ),
       #    fluidRow(
       #      column(12,plotlyOutput("KE_tissue"))
       #    ),
       #    fluidRow(
       #      column(12, plotlyOutput("KE_by_tissue_distribution"))
       #    )
       # ),
       # tabPanel("KEs range plot",
       #          fluidRow(
       #            column(3,uiOutput("range_ke_groupby")),
       #            shinyBS::bsTooltip("range_ke_groupby", "main variable used for the plot", placement = "bottom"),
       #            column(3,uiOutput("range_ke_groupby2")),
       #            shinyBS::bsTooltip("range_ke_groupby2", "plot group by", placement = "bottom"),
       #            column(3,checkboxInput("range_ke_is_group_by_numeric", "numeric grouping variable", value = FALSE)),
       #            shinyBS::bsTooltip("range_ke_is_group_by_numeric", "if selected the group by variable is treated as numeric", placement = "bottom")
       #          ),
       #          fluidRow(
       #            column(3,uiOutput("range_ke_filtercol")),
       #            shinyBS::bsTooltip("range_ke_filtercol", "Select variable name to filter the data in the plot", placement = "bottom"),
       #            column(3,uiOutput("range_ke_filterby")),
       #            shinyBS::bsTooltip("range_ke_filterby", "Select variable value to filter the data in the plot", placement = "bottom")
       #          ),
       #          fluidRow(
       #            actionButton(inputId = "range_plot_ke", label = "Range plot")
       #          ),
       #          fluidRow(
       #            column(12, plotlyOutput("range_ke_plotly"))
       #          )
       # ),
       # tabPanel("KEs range plot",
       #          fluidRow(
       #            column(3,uiOutput("range_ke_groupby")),
       #            shinyBS::bsTooltip("range_ke_groupby", "main variable used for the plot", placement = "bottom"),
       #            column(3,uiOutput("range_ke_groupby2")),
       #            shinyBS::bsTooltip("range_ke_groupby2", "plot group by", placement = "bottom"),
       #            column(3,checkboxInput("range_ke_is_group_by_numeric", "numeric grouping variable", value = FALSE)),
       #            shinyBS::bsTooltip("range_ke_is_group_by_numeric", "if selected the group by variable is treated as numeric", placement = "bottom")
       #          ),
       #          fluidRow(
       #            column(3,uiOutput("range_ke_filtercol")),
       #            shinyBS::bsTooltip("range_ke_filtercol", "Select variable name to filter the data in the plot", placement = "bottom"),
       #            column(3,uiOutput("range_ke_filterby")),
       #            shinyBS::bsTooltip("range_ke_filterby", "Select variable value to filter the data in the plot", placement = "bottom")
       #          ),
       #          fluidRow(
       #            actionButton(inputId = "range_plot_ke", label = "Range plot")
       #          ),
       #          fluidRow(
       #            column(12, plotlyOutput("range_ke_plotly"))
       #          )
       # ),
       # tabPanel("KEKE-network",
       #          fluidRow(
       #            
       #          )    
       # )
     )
      
    

  )
}

gene_comparison_results = function(){
  tabPanel(value = "GenePairsTab", title = "Pairwise gene analysis",

           tabsetPanel(
             tabPanel("Gene pairs",
              
                      fluidRow(
                        bsCollapse(id = "collapseGeneComparison", open = "Panel 2",
                                   bsCollapsePanel("Gene Pairs",  style = "primary",
                                       fluidRow(
                                         column(3, column(3, checkboxInput("convert_to_gene_symbol", "Convert to gene symbol", value =  TRUE))),
                                         column(6, downloadButton("download_gene_pairs", "Download Gene Pairs Table"))
                                       ),
                                       fluidRow(
                                         column(6,DT::dataTableOutput("gene_pairs_table")),
                                         column(6,fluidRow(shinycssloaders::withSpinner(plotlyOutput("plot_gene_pairs"), type = 6)))
                                       )
                                   ),
                                   bsCollapsePanel("Clustering",  style = "primary",
                                       fluidRow(
                                         column(3,sliderInput(inputId = "nclust",label = "Number of clusters",min = 2,max = 10,value = 2)),
                                         column(3, selectInput(inputId = "clustering_distance",
                                                               label = "Select metric for clustering",
                                                               choices = c("Pearson Correlation"= "correlation" ,
                                                                           "Euclidean Distance" = "euclidean",
                                                                           "Combination of Both"="combination" ),selected = "euclidean")),
                                         column(3, actionButton(inputId = "perform_clustering_gene_pairs",label = "Run Analysis"))
                                        ), 
                                       fluidRow(
                                         column(3, column(3, checkboxInput("ppi_data_complex_heatmap", "Enrich with PPI info", value =  TRUE))),
                                         column(3,selectInput("cluster_organism", "Select organism:",
                                                     choices=c("Human"="human","Mouse"="mouse","Rat"="rat"), selected = "Human")),
                                         column(3,selectInput("cluster_gene_identifier", "Select gene identifier:",
                                                     choices=c("Ensembl" = "ENSEMBL","Symbol"="SYMBOL","EntrezID" = "ENTREZID"), selected = "ENSEMBL"))
                                       ),
                                       fluidRow(
                                         # plotlyOutput("plot_heatmap_gene_pairs")
                                         InteractiveComplexHeatmapOutput("complex_heatmap")
                                       )
                                   ),
                                   bsCollapsePanel("Genes in Cluster",  style = "primary",
                                     fluidRow(DT::dataTableOutput("GSEA_over_corr_clust_query_data_frame"))
                                   ),
                                   bsCollapsePanel("Enrichment",  style = "primary",
                                                   fluidRow(
                                                     column(6,sliderInput(inputId = "n_min_element_in_cluster",label = "Minimum number in a cluster to perform enrichment",min = 2,max = 50,value = 5)),
                                                     column(6, selectInput(inputId = "gostclustcorrection",
                                                                 label = "Correction method",
                                                                 choices = p.adjust.methods[p.adjust.methods != "none"],selected = "fdr"))
                                                     ),
                                                   
                                                   bsCollapsePanel("Gost Plot",  style = "primary",
                                                    fluidRow(
                                                      column(12, downloadButton("download_gost_clust_plot", "Download GSEA Cluster Plot (HTML)"))
                                                    ),
                                                    fluidRow(column(12, plotlyOutput("gost_clust_plot")))
                                                   ),
                                                   bsCollapsePanel("Gost Table",  style = "primary",
                                                     fluidRow(
                                                       column(12, downloadButton("download_gost_clust", "Download GSEA Cluster Table"))
                                                     ),
                                                     fluidRow(
                                                       column(12, DT::dataTableOutput("gost_clust_table")),
                                                     )
                                                   ),
                                                   bsCollapsePanel("Bubble Plot", style = "primary",
                                                       fluidRow(
                                                         column(6,uiOutput("sourceSelectUIGenePairs")),  # Dynamic UI output for select box),
                                                         column(6,uiOutput("sliderUIGenePairs"))
                                                       ),
                                                       fluidRow(
                                                         column(12, plotlyOutput("bubble_enrichment_gene_pairs"))
                                                       )
                                                   )
                                   )
                        )
                      ),        

                # fluidRow(
                #   plotOutput("plot_enrichment")
                # )
             ),
             tabPanel("Gene sets",
                      
                      tabsetPanel(
                        tabPanel("Funmappone Pathways",
                                 
                                 fluidRow(
                                   column(3,uiOutput("pathway_genes_dose_dependent_patterns_comparison"))
                                 ),
                                 fluidRow(
                                   column(6,plotlyOutput("plot_pathways_patterns"))
                                 ),
                                 fluidRow(
                                   column(3, checkboxInput("funmappone_plot_ppi", "Add PPI info", value =  TRUE)),
                                   column(3,sliderInput(inputId = "Funmappone_nclust",label = "Number of clusters",min = 1,max = 10,value = 1)),
                                   column(3, selectInput(inputId = "Funmappone_clustering_distance",
                                                         label = "Select metric for clustering",
                                                         choices = c("Pearson Correlation"= "correlation" ,
                                                                     "Euclidean Distance" = "euclidean",
                                                                     "Combination of Both"="combination" ),selected = "euclidean"))
                                 ),
                                 fluidRow(
                                   column(6,InteractiveComplexHeatmapOutput("complex_heatmap_gene_pathways"))
                                 )
                        ),
                        tabPanel("KEs",
                                 
                                 fluidRow(
                                   column(3,uiOutput("KE_genes_dose_dependent_patterns_comparison"))
                                 ),
                                 fluidRow(
                                   column(6,plotlyOutput("plot_KE_patterns"))
                                 ),
                                 fluidRow(
                                   column(3, checkboxInput("KE_plot_ppi", "Add PPI info", value =  TRUE)),
                                   column(3,sliderInput(inputId = "KE_nclust",label = "Number of clusters",min = 1,max = 10,value = 1)),
                                   column(3, selectInput(inputId = "KE_clustering_distance",
                                                         label = "Select metric for clustering",
                                                         choices = c("Pearson Correlation"= "correlation" ,
                                                                     "Euclidean Distance" = "euclidean",
                                                                     "Combination of Both"="combination" ),selected = "euclidean"))
                                 ),
                                 fluidRow(
                                   column(6,InteractiveComplexHeatmapOutput("complex_heatmap_gene_KE"))
                                 )
                        )
                      )
             ),
             tabPanel("PPI",
                fluidRow(
                  column(6, sliderInput("th_PPI", "Correlation Interval:", min = 0, max = 1, value = 0)),
                  column(6, actionButton("PPIButton", "Show genes on PPI")),
                ),
                fluidRow(
                  column(6, networkD3::forceNetworkOutput("plot_PPI")),
                  column(6, DT::dataTableOutput("PPI_stats"))
                ),
                fluidRow(
                  selectInput(inputId = "gostPPIcorrection",
                              label = "Correction method",
                              choices = p.adjust.methods[p.adjust.methods != "none"],selected = "fdr"),
                ),
                fluidRow(
                  column(12, plotlyOutput("gost_ppi_plot")),
                ),
               fluidRow(
                 column(12, DT::dataTableOutput("gost_ppi_table")),
               )
            )
            )
          
  )
}

bmd_results_gene_level = function(){
  tabPanel("Gene Level",
           
           
           # fluidRow(
           #   # Button with question mark icon
           #   actionButton("info_btn", label = "?",
           #                style = "font-size: 10px; font-weight: bold; color: white; background-color: #007BFF; 
           #              border: none; width: 20px; height: 20px;"),
           #   
           #   # Modal window triggered by button
           #   bsModal("info_modal", "Information", "info_btn",
           #           p("The steps below should be executed in the order indicated."),
           #           p("It is recommended to filter the genes before selecting the optimal model or computing the model average."),
           #           p("The optimal model for each gene should be computed before proceeding with the pathways or KE/AOP enrichment."),
           #           size = "medium")  # Can be "small", "medium", or "large"
           # ),


           fluidRow(
           bsCollapse(id = "collapseExample", open = "Panel 2",
                      bsCollapsePanel("Filtering",  style = "primary",
                          fluidRow(
                            column(3, selectInput("BMDN_filter_Cores", "Number of cores:", choices = c(1:25), selected = 1)),
                            shinyBS::bsTooltip("BMDN_filter_Cores", "Number of cores to be used for the filtering process", placement = "top")
                            
                          ),
                          fluidRow(
                            column(3, checkboxInput("filter_bmdl_na", "Filter NAs (BMDL)", value =  TRUE)),
                            shinyBS::bsTooltip("filter_bmdl_na", "If selected all the models with missing predicted BMDL are removed", placement = "bottom"),
                            
                            column(3, checkboxInput("filter_bmd_na", "Filter NAs (BMD)", value =  TRUE)),
                            shinyBS::bsTooltip("filter_bmd_na", "If selected all the models with missing predicted BMD are removed", placement = "bottom"),
                            
                            column(3, checkboxInput("filter_bmdu_na", "Filter NAs (BMDU)", value =  TRUE)),
                            shinyBS::bsTooltip("filter_bmdu_na", "If selected all the models with missing predicted BMDU are removed", placement = "bottom"),
                            
                            column(3, checkboxInput("filter_ic50_na", "Filter NAs (IC50)", value =  TRUE)),
                            shinyBS::bsTooltip("filter_ic50_na", "If selected all the models with missing predicted IC50 are removed", placement = "bottom")
                            
                          ),
                          fluidRow(
                            column(3, checkboxInput("filter_by_R2", "Filter models with R2<th", value =  TRUE)),
                            shinyBS::bsTooltip("filter_by_R2", "If selected all the models with R2 below the selected threshold will be removed", placement = "bottom"),
                            column(3, sliderInput("BMD_filter_r2", "Threshold R2:", min = 0, max = 1, value = 0.6)), 
                          ),
                          fluidRow(
                            column(3, checkboxInput("filter_bmdl_loof", "Filter by Lack of Fit", value =  FALSE)),
                            shinyBS::bsTooltip("filter_bmdl_loof", "If selected all the models with lack-of-fit p-value less than the threshold will be removed", placement = "bottom"),
                            column(3,sliderInput("bmdloofth", "Threshold for the Lack of Fit Pvalue:", min = 0, max = 1, value = 0.1)), 
                          ),
                          fluidRow(
                            column(3, checkboxInput("filter_by_monotonicity", "Only strictly monotonic models allowed", value =  FALSE)),
                            shinyBS::bsTooltip("filter_by_monotonicity", "If selected all the models with non strictly monotonic behavior will be removed", placement = "bottom"),
                          ),
                          fluidRow(
                            column(3, checkboxInput("ratio_filter2", "Apply Ratio filters", value = FALSE)),
                            shinyBS::bsTooltip("ratio_filter2", "If selected all the models with BMD/BMDL, BMDU/BMD and BMDU/BMDL ratios lower than the selected thresholds will be removed", placement = "bottom"),
                            column(3, sliderInput("bmd_bmdl_th2", "BMD/BMDL ratio:", min = 0, max = 100, value = 20)),
                            column(3, sliderInput("bmdu_bmd_th2", "BMDU/BMD ratio:", min = 0, max = 100, value = 20)),
                            column(3, sliderInput("bmdu_bmdl_th2", "BMDU/BMDL ratio:", min = 0, max = 100, value = 40))
                          ),
                          fluidRow(
                            column(3, checkboxInput("filter_bounds_bmdl_bmdu2_low", "Filter by Lower Boundary", value =  FALSE)),
                            shinyBS::bsTooltip("filter_bounds_bmdl_bmdu2_low", "If selected all the models with predicted BMD, BMDL and BMDU values lower than x% of the minimum dose will be removed", placement = "bottom"),
                            column(3, selectInput("min_dose_perc2", "Lowest dose filter:", choices = c(0,0.1,0.2,0.3),selected = 0.1)),
                          ),
                          fluidRow(
                            column(3, checkboxInput("filter_bounds_bmdl_bmdu2_high", "Filter by Upper Boundary", value =  FALSE)),
                            shinyBS::bsTooltip("filter_bounds_bmdl_bmdu2_high", "If selected all the models with predicted BMD, BMDL and BMDU values higher than x% of the maximum dose will be removed", placement = "bottom"),
                            column(3,selectInput("max_dose_perc2", "Highest dose filter:", choices = c(0,0.1,0.2,0.3),selected = 0.1))
                          ),
                          fluidRow(
                            column(3, checkboxInput("filter_by_negative_values", "No negative effective doses", value =  TRUE)),
                            shinyBS::bsTooltip("filter_by_negative_values", "If selected all the models with predicted effective doses with negative values will be removed", placement = "bottom"),
                            column(3, checkboxInput("filter_by_unordered_values", "No unordered effective doses", value =  TRUE)),
                            shinyBS::bsTooltip("filter_by_unordered_values", "If selected all the models whose effective doses do not respect the following relationship will be removed (BMDL<= BMD <= BMDU)", placement = "bottom")
                          ),
                          fluidRow(
                            column(6, actionButton("Apply_filter", "Apply Filters")),
                            shinyBS::bsTooltip("Apply_filter", "Applies all the filtering previously selected", placement = "bottom"),
                            
                          )
                      ),
                      bsCollapsePanel("Model Average",  style = "primary",
                          fluidRow(
                            column(6, actionButton("modelaverage", "Compute Average Models")),
                            shinyBS::bsTooltip("modelaverage", "For each feature with more than one fitted model, the average model will be computed", placement = "bottom"),
                            
                            column(4, selectInput("ModelAverageNCores", "Number of cores:", choices=c(1:25), selected = 1)),
                            shinyBS::bsTooltip("ModelAverageNCores", "Number of cores", placement = "bottom")
                            
                          )
                      ),
                      bsCollapsePanel("Select Best Model",  style = "primary",
                          fluidRow(
                            column(3, selectInput("select_best_model_options", "Methods", choices = c("AIC","Model average"),selected = "AIC")),
                            shinyBS::bsTooltip("select_best_model_options", "For each feature with more than one fitted model, choose the method to use to select the optimal one", placement = "top"),
                            column(6, actionButton("select_best_model", "Select the best model for each gene")),
                            shinyBS::bsTooltip("select_best_model", "For each feature with more than one fitted model, the optimal model is identified by using the selected method", placement = "bottom"),
                            
                          )
                      ),
                      bsCollapsePanel("Reset",  style = "primary",
                          fluidRow(
                            column(6, actionButton("resetmodelfilteringaverage", "Reset All")),
                            shinyBS::bsTooltip("resetmodelfilteringaverage", "Return to the original list of models. All the filtering performed and the computed average models will be removed from the list", placement = "bottom"),
                            
                          )
                      ),
                      bsCollapsePanel("Download",  style = "primary",
                        fluidRow(
                          column(6,downloadButton("downloadBMDData", "Download Latest BMD Table"))
                        )
                      )
           )
           ),
           fluidRow(
             fluidRow(
               column(6,uiOutput("ExptimeSelBMD")),
               column(6,uiOutput("timePointSel2"))
             ),
             fluidRow(
               tags$h4("Click on the rows of the table to show the corresponding fitted models", 
                       style = "text-align: center;")
             ),
             fluidRow(
               column(12, DT::dataTableOutput("BMD_table"))
             ),
             wellPanel(fluidRow(
               column(12,fluidRow(shinycssloaders::withSpinner(verbatimTextOutput("model_formula"), type = 6)))
             )),
             fluidRow(
               column(4, numericInput("xlim_u", "Upper limit for x-axis:", value = NULL, min = 0)),
               shinyBS::bsTooltip("xlim_u", "Numerical value to limit the x-axis. When set, the plot is limited in the range 0 - upper limit", placement = "top"),
               column(4, checkboxInput("plot_ic50", "Plot IC50", value = FALSE)),
               shinyBS::bsTooltip("plot_ic50", "If selected, IC50 is plotted", placement = "top")
               
             ), 
             fluidRow(
               column(12,fluidRow(shinycssloaders::withSpinner(plotOutput("bmd_fitting"), type = 6)))
             )
           )
           
           # sidebarLayout(
           #   sidebarPanel(width = 5, 
           #                fluidRow(
           #                  column(6,uiOutput("ExptimeSelBMD")),
           #                  column(6,uiOutput("timePointSel2"))
           #                ),
           #                fluidRow(
           #                  column(4, sliderInput("bmd_bmdl_th2", "BMD/BMDL ratio:", min = 0, max = 100, value = 20)),
           #                  column(4, sliderInput("bmdu_bmd_th2", "BMDU/BMD ratio:", min = 0, max = 100, value = 20)),
           #                  column(4, sliderInput("bmdu_bmdl_th2", "BMDU/BMDL ratio:", min = 0, max = 100, value = 40))
           #                  #column(3, checkboxInput("ratio_filter2", "Ratio filters", value = FALSE))
           #                ),
           #                fluidRow(
           #                  column(4,sliderInput("bmdloofth", "Threshold for the Lack of Fit Pvalue:", min = 0, max = 1, value = 0.1)), #NOT SURE IF CORRECT
           #                  column(4,selectInput("min_dose_perc2", "Lowest dose filter:", choices=c(0,0.1,0.2,0.3),selected=0.1)),
           #                  column(4,selectInput("max_dose_perc2", "Highest dose filter:", choices=c(0,0.1,0.2,0.3),selected=0.1))
           #                  #column(3, checkboxInput("filter_bounds_bmdl_bmdu2", "Filter by boundary (BMDL=0 or BMDU=maxDose)", value = FALSE))
           #                  # column(4, checkboxInput("Apply_filter", "Apply Filters", value = FALSE))
           #                  
           #                ),
           #                fluidRow(
           #                  column(4, checkboxInput("ratio_filter2", "Apply Ratio filters", value = FALSE)),
           #                  column(4, checkboxInput("filter_bounds_bmdl_bmdu2_low", "Filter by Lower Boundary", value =  FALSE)),
           #                  column(4, checkboxInput("filter_bounds_bmdl_bmdu2_high", "Filter by Upper Boundary", value =  FALSE))
           #                ),
           #                fluidRow(
           #                  column(4, checkboxInput("filter_bmdl_loof", "Filter by Lack of Fit", value =  TRUE)),
           #                  column(4, checkboxInput("filter_bmdl_na", "Filter NAs", value =  TRUE)),
           #                  column(4, selectInput("BMDN_filter_Cores", "Number of cores:", choices=c(1:25), selected = 1))
           #                ),
           #                fluidRow(
           #                  column(6, actionButton("Apply_filter", "Apply Filters")),
           #                  column(6, actionButton("modelaverage", "Compute Average Models"))
           #                ),
           #                fluidRow(
           #                  column(6, actionButton("resetmodelfilteringaverage", "Reset Filtering & Average"))
           #                ),
           #                fluidRow(
           #                  column(6,downloadButton("downloadBMDData", "Download Latest BMD Table"))
           #                )
           #   ),  
           #   mainPanel(width = 7, 
           #             fluidRow(
           #               column(12, DT::dataTableOutput("BMD_table"))
           #             ),
           #             wellPanel(fluidRow(
           #               column(12,fluidRow(shinycssloaders::withSpinner(verbatimTextOutput("model_formula"), type = 6)))
           #             )),
           #             fluidRow(
           #               column(12,fluidRow(shinycssloaders::withSpinner(plotOutput("bmd_fitting"), type = 6)))
           #             )
           #  ))
           
           # fluidRow(
           #   column(3,uiOutput("ExptimeSelBMD")),
           #   column(3,uiOutput("timePointSel2"))
           # ),
           # fluidRow(
           #   column(9, DT::dataTableOutput("BMD_table"))
           # ),
           # fluidRow(
           #   column(3, sliderInput("bmd_bmdl_th2", "BMD/BMDL ratio:", min = 0, max = 100, value = 20)),
           #   column(3, sliderInput("bmdu_bmd_th2", "BMDU/BMD ratio:", min = 0, max = 100, value = 20)),
           #   column(3, sliderInput("bmdu_bmdl_th2", "BMDU/BMDL ratio:", min = 0, max = 100, value = 40))
           #   #column(3, checkboxInput("ratio_filter2", "Ratio filters", value = FALSE))
           # ),
           # fluidRow(
           #   column(3,sliderInput("bmdloofth", "Threshold for the Lack of Fit Pvalue:", min = 0, max = 1, value = 0.1)), #NOT SURE IF CORRECT
           #   column(3,selectInput("min_dose_perc2", "Lowest dose filter:", choices=c(0,0.1,0.2,0.3),selected=0.1)),
           #   column(3,selectInput("max_dose_perc2", "Highest dose filter:", choices=c(0,0.1,0.2,0.3),selected=0.1))
           #   #column(3, checkboxInput("filter_bounds_bmdl_bmdu2", "Filter by boundary (BMDL=0 or BMDU=maxDose)", value = FALSE))
           #   # column(4, checkboxInput("Apply_filter", "Apply Filters", value = FALSE))
           # 
           # ),
           # fluidRow(
           #   column(3, checkboxInput("ratio_filter2", "Apply Ratio filters", value = FALSE)),
           #   column(3, checkboxInput("filter_bounds_bmdl_bmdu2_low", "Filter by Lower Boundary", value =  FALSE)),
           #   column(3, checkboxInput("filter_bounds_bmdl_bmdu2_high", "Filter by Upper Boundary", value =  FALSE))
           # ),
           # 
           # 
           # fluidRow(
           #   column(3, checkboxInput("filter_bmdl_loof", "Filter by Lack of Fit", value =  TRUE)),
           #   column(3, checkboxInput("filter_bmdl_na", "Filter NAs", value =  TRUE))
           # 
           # ),
           # fluidRow(
           #   column(3, selectInput("BMDN_filter_Cores", "Number of cores:", choices=c(1:25), selected = 1))
           # 
           # ),
           # fluidRow(
           # 
           #   column(3, actionButton("Apply_filter", "Apply Filters")),
           #   column(3, actionButton("modelaverage", "Compute Average Models")),
           #   column(3, actionButton("resetmodelfilteringaverage", "Reset Filtering & Average"))
           # ),
           # fluidRow(
           #   column(3,downloadButton("downloadBMDData", "Download Latest BMD Table"))
           # ),
           # 
           # fluidRow(
           #   column(12,fluidRow(shinycssloaders::withSpinner(plotOutput("bmd_fitting"), type = 6)))
           # )
           #torem
           # fluidRow(
           #   column(9, DT::dataTableOutput('x3')),
           #   column(3, verbatimTextOutput('x4'))
           # )
  )
}

bmd_results_time_point_comparison = function(){
  tabPanel("Compare different Experiments",
           tabPanel("Compare Experiments",
                    fluidRow(column(12,
                                    bsCollapse(id="TPSidebar", open="BMD",
                                               bsCollapsePanel("Number of dose dependent genes", style = "primary",
                                                               fluidRow(
                                                                 column(3,uiOutput("tp_groupby1")),
                                                                 column(3,actionButton("plottp", "Plot"))
                                                               ),
                                                               fluidRow(
                                                                 column(3,uiOutput("tp_filtercol")),
                                                                 column(3,uiOutput("tp_filterby"))
                                                               ),
                                                               shinycssloaders::withSpinner(plotlyOutput("NGTime"), type = 6)
                                               ),
                                               bsCollapsePanel("Histogram BMD/BMDL/BMDU, BMR, and IC50 values", style="primary",
                                                               fluidRow(

                                                                 column(3,uiOutput("BMDVals_colorby")),
                                                                 column(3,uiOutput("BMDVals_groupby1")),
                                                                 column(3,uiOutput("BMDVals_groupby2"))
                                                               ),
                                                               fluidRow(

                                                                 column(3,uiOutput("BMDVals_filtercol")),
                                                                 column(3,uiOutput("BMDVals_filterby")),
                                                                 column(3,actionButton("plotBMDVals", "Plot"))

                                                               ),

                                                               #"Here i will print the BMD value distribution",
                                                               shinycssloaders::withSpinner(plotlyOutput("BMD_dist_TP"), type = 6),
                                                               shinycssloaders::withSpinner(plotlyOutput("BMDL_dist_TP"), type = 6),
                                                               shinycssloaders::withSpinner(plotlyOutput("BMDU_dist_TP"), type = 6),
                                                               shinycssloaders::withSpinner(plotlyOutput("BMR_dist_TP"), type = 6),
                                                               shinycssloaders::withSpinner(plotlyOutput("IC50_dist_TP"), type = 6)
                                                               
                                                               
                                                               
                                                               
                                               ),
                                               bsCollapsePanel("Scatterplot BMD/BMDL relationship", style="primary",
                                                               
                                                               fluidRow(
                                                                 
                                                                 column(3,uiOutput("BMDBMDL_colorby")),
                                                                 column(3,uiOutput("BMDBMDL_groupby1")),
                                                                 column(3,uiOutput("BMDBMDL_groupby2"))
                                                               ),
                                                               fluidRow(
                                                                 
                                                                 column(3,uiOutput("BMDBMDL_filtercol")),
                                                                 column(3,uiOutput("BMDBMDL_filterby")),
                                                                 column(3,actionButton("plotBMDBMDL", "Plot"))
                                                                 
                                                               ),
                                                               #"Here i will print the BMD value distribution",
                                                               shinycssloaders::withSpinner(plotlyOutput("BMD_BMDL"), type = 6)
                                                               
                                               ),
                                               # bsCollapsePanel("R-squared (r2) values", style="primary",
                                               # 
                                               #                 fluidRow(
                                               # 
                                               #                   column(3,uiOutput("R2_colorby")),
                                               #                   column(3,uiOutput("R2_groupby1")),
                                               #                   column(3,uiOutput("R2_groupby2"))
                                               #                 ),
                                               #                 fluidRow(
                                               # 
                                               #                   column(3,uiOutput("R2_filtercol")),
                                               #                   column(3,uiOutput("R2_filterby")),
                                               #                   column(3,actionButton("plotR2", "Plot"))
                                               # 
                                               #                 ),
                                               #                 #"Here i will print the BMD value distribution",
                                               #                 shinycssloaders::withSpinner(plotlyOutput("BMD_R2_fitting"), type = 6)
                                               # 
                                               # ),
                                               bsCollapsePanel("Fitting statistics (R2,AIC,p-value)", style="primary",
                                                               
                                                               fluidRow(
                                                                 
                                                                 column(3,uiOutput("LOOF_colorby")),
                                                                 column(3,uiOutput("LOOF_groupby1")),
                                                                 column(3,uiOutput("LOOF_groupby2"))
                                                               ),
                                                               fluidRow(
                                                                 
                                                                 column(3,uiOutput("LOOF_filtercol")),
                                                                 column(3,uiOutput("LOOF_filterby")),
                                                                 column(3,actionButton("plotLOOF", "Plot"))
                                                                 
                                                               ),
                                                               #"Here i will print the BMD value distribution",
                                                               shinycssloaders::withSpinner(plotlyOutput("BMD_r2_fitting"), type = 6),
                                                               shinycssloaders::withSpinner(plotlyOutput("BMD_aic_fitting"), type = 6),
                                                               shinycssloaders::withSpinner(plotlyOutput("BMD_pval_fitting"), type = 6)
                                                               
                                               ),
                                               bsCollapsePanel("Fitted models", style="primary",

                                                               fluidRow(


                                                                 column(3,uiOutput("Fittedmodels_groupby1")),
                                                                 column(3,uiOutput("Fittedmodels_groupby2"))
                                                               ),
                                                               fluidRow(

                                                                 column(3,uiOutput("Fittedmodels_filtercol")),
                                                                 column(3,uiOutput("Fittedmodels_filterby")),
                                                                 column(3,actionButton("plotFittedmodels", "Plot"))

                                                               ),
                                                               fluidRow(column(12,shinycssloaders::withSpinner(plotOutput("BMD_dist_models"), type = 6)))
                                               ),
                                               bsCollapsePanel("ECDF", style="primary",
                                                               fluidRow(
                                                                 column(3,uiOutput("BMDComExp_relvar")),
                                                                 shinyBS::bsTooltip("BMDComExp_relvar", "main variable used for the plot", placement = "top"),
                                                                 column(3,uiOutput("BMDComExp_groupby2")),
                                                                 shinyBS::bsTooltip("BMDComExp_groupby2", "plot group by", placement = "top"),

                                                                 column(3,checkboxInput("BMD_is_group_by_numeric", "numeric grouping variable", value = FALSE)),
                                                                 shinyBS::bsTooltip("BMD_is_group_by_numeric", "if selected the group by variable is treated as numeric", placement = "bottom"),

                                                                 column(3,uiOutput("BMDComExp_othervars")),
                                                                 shinyBS::bsTooltip("BMDComExp_othervars", "Other variable used for grouping", placement="top"),

                                                               ),
                                                               fluidRow(
                                                                 column(3,uiOutput("compare_exp_filtercol")),
                                                                 shinyBS::bsTooltip("compare_exp_filtercol", "Select variable name to filter the data in the plot", placement = "top"),

                                                                 column(3,uiOutput("compare_exp_filterby")),
                                                                 shinyBS::bsTooltip("compare_exp_filterby", "Select variable value to filter the data in the plot", placement = "top"),

                                                               ),
                                                               fluidRow(
                                                                 # shinyBS::bsTooltip("BMD_compare_plot_type", "ecdf or histogram plot", placement="top"),

                                                                 column(3, checkboxInput("BMD_compare_scale", "Scale bmd", value = FALSE)),
                                                                 shinyBS::bsTooltip("BMD_compare_scale", "If selected the bmd values are scaled in 0-1. Useful when comparing experiments with different ranges of concentrations.", placement="bottom"),

                                                               ),
                                                               fluidRow(
                                                                 actionButton("plot_compare_experiments_ecdf", "Plot")
                                                               ),
                                                               fluidRow(
                                                                 column(12, plotlyOutput("cumulative_plot")) 
                                                                )
                                               ),
                                               bsCollapsePanel("UpSet Plot", style="primary",
                                                               fluidRow(
                                                                 column(3,uiOutput("BMDComExp_relvar_upset")),
                                                                 shinyBS::bsTooltip("BMDComExp_relvar_upset", "main variable used for the plot", placement = "top"),
                                                                 column(3,uiOutput("BMDComExp_groupby2_upset")),
                                                                 shinyBS::bsTooltip("BMDComExp_groupby2_upset", "plot group by", placement = "top"),
                                                                 column(3,uiOutput("BMDComExp_othervars_upset")),
                                                                 shinyBS::bsTooltip("BMDComExp_othervars_upset", "Other variable used for grouping", placement = "top"),

                                                               ),
                                                               fluidRow(
                                                                 column(3,uiOutput("compare_exp_filtercol_upset")),
                                                                 shinyBS::bsTooltip("compare_exp_filtercol_upset", "Select variable name to filter the data in the plot", placement = "top"),
                                                                 column(3,uiOutput("compare_exp_filterby_upset")),
                                                                 shinyBS::bsTooltip("compare_exp_filterby_upset", "Select variable value to filter the data in the plot", placement = "top"),

                                                               ),
                                                               fluidRow(
                                                                 column(3,sliderInput("maxInt", "Maximum intersection:", min = 1, max = 100, value = 10)),
                                                                 column(3,selectInput("groupby", "GroupBy:", c("degree" = "degree", "sets" = "sets"))),
                                                                 column(3,selectInput("orderby", "OrderBy:", c("frequency" = "freq", "degree" = "degree", "both" = "both"))),
                                                               ),
                                                               fluidRow(
                                                                 actionButton("plot_compare_experiments_upset", "Plot")
                                                               ),
                                                               fluidRow(
                                                                 column(12,plotOutput("upsetplot"))
                                                               )
                                               )
                                    )
                    )

                    ))

  )
}


bmd_results_gene_frequency = function(){
  tabPanel("Gene Frequency",
     fluidRow(
       column(4,uiOutput("geneFrequency_groupby")),
       column(4,uiOutput("geneFrequency_splitby"))
     ),
     fluidRow(
       column(4,  sliderInput("percentage_th_gene_freq", "Threshold:", min = 0, max = 1, value = 0)),
       shinyBS::bsTooltip(id = "percentage_th_gene_freq",title = "This threshold is used to subset the genes based on their frequency. E.g. if 0.6 is selected only genes with frequency higher than 0.6 are considered",placement = "top"),
       column(4, column(3, actionButton("GeneFrequency", "Compute gene frequency")))
     ),
     fluidRow(
       column(3,  selectInput(inputId = "organism_gene_freq", "Organism", choices=c("Human"="hsapiens", "Mouse"="mmusculus","Rat"="rnorvegicus"), selected="Human")),
       column(3,  selectInput(inputId = "correction_gene_freq", "Correction method", choices=c("Bonferroni"="bonferroni", "FDR"="fdr","gSCS"="gSCS"), selected="FDR"))
     ),
     tabsetPanel(
        tabPanel("Gene frequency",
           fluidRow(
             column(4, uiOutput("select_exp"))
           ),
           fluidRow(
             # column(8, shinycssloaders::withSpinner(plotlyOutput("gene_freq_lollipol_plot_list"), type = 6)),
             column(8, shinycssloaders::withSpinner(plotlyOutput("gene_freq_lineplot_plot_list"), type = 6)),

           ),
           fluidRow(
             column(12, shinycssloaders::withSpinner(DT::dataTableOutput("gene_freq_m_list"), type = 6))
           )
        ),
        tabPanel("GSEA",
           fluidRow(
             column(6,downloadButton("download_gostplot", "Download GSEA Plot as HTML")),
             column(6,downloadButton("download_gosttable", "Download GSEA Table"))
           ),
           
           bsCollapsePanel("GOST Plot",  style = "primary",
             fluidRow(
               column(12, shinycssloaders::withSpinner(plotlyOutput("gene_freq_gsea_most_freq_genes"), type = 6)),
             )
           ),
           bsCollapsePanel("GOST Table",  style = "primary",
             fluidRow(
               column(12, DT::dataTableOutput("gene_freq_gsea_most_freq_genes_table"))
             )
           ),
           bsCollapsePanel("Bubble Plot",  style = "primary",
             fluidRow(
               column(6,uiOutput("sourceSelectUIGeneFreq")),  # Dynamic UI output for select box),
               column(6,uiOutput("sliderUIGeneFreq"))
             ),
             fluidRow(
               column(12, plotlyOutput("bubble_enrichment_gene_freq"))
             )
           )
        )
     )

  )
}

bmd_results = function(){
  tabPanel(value="BMDTab", title="BMD Analysis",
           fluidRow(column(12,
                     tabBox(id = "bmdgenes", title = "", width = 12,
                            bmd_results_gene_level(),
                            bmd_results_time_point_comparison(),
                            # bmd_results_experiment_comparison(),
                            bmd_results_gene_frequency()
                     )
                   )
           )
  )
}

# funmappone_results = function(){
#   tabPanel(value = "enrTab",title = "Enrichment",
#            fluidRow(
#              column(7,
#                wellPanel(
#                  tags$h5("1. Data Selection"),
#                  fluidRow(
#                    column(2,selectInput(inputId = "level", label = "Hierarchy level",choices = list(1,2,3))),
#                    column(2,uiOutput("chose_lev1")), shinyBS::bsTooltip(id = "chose_lev1",title = "Note: remove ALL from the list for specific selection.",placement = "top"),
#                    column(2,uiOutput("chose_lev2")), shinyBS::bsTooltip(id = "chose_lev2",title = "Note: remove ALL from the list for specific selection.",placement = "top"),
#                    column(2,uiOutput("chose_lev3")), shinyBS::bsTooltip(id = "chose_lev3",title = "Note: remove ALL from the list for specific selection.",placement = "top"),
#                    column(2,uiOutput("selectColumn"), shinyBS::bsTooltip(id = "selectColumn",title = "Note: remove ALL from the list for specific selection.",placement = "top"))
#                  )
#                )),
#                column(2,
#                wellPanel(
#                  tags$h5("2. Plot section"),
#                  fluidRow(
#                    column(12,checkboxInput("doGrouping", "Show categories", value = TRUE))
#                    # column(4,checkboxInput("aspectRatio", "Keep aspect ratio", value = FALSE)),
#                  ),
#                  fluidRow(
#                    column(12,actionButton("do", "Plot Map")), shinyBS::bsTooltip(id = "do",title ="NOTE: press the Plot Mat button every time you update the map!",placement = "bottom")
#                  )
#                  
#                )),
#                column(2,wellPanel(
#                  tags$h5("3. Download Selection"),
#                  fluidRow(
#                    column(6,textInput(inputId ="img_width", value = 15,label = "Width")), #width
#                    column(6,textInput(inputId ="img_height", value = 30,label = "Height"))
#                  ),
#                  fluidRow(
#                    column(12,downloadButton('downloadData')), shinyBS::bsTooltip(id = "downloadData",title ="NOTE: when downloading, specify image size in inches ",placement = "bottom")
#                  )
#                )
#              )),
#            fluidRow(
#                tags$h5("Use scrollbars to navigate and see the whole map"),
#                
#                tabsetPanel(
#                  
#                  tabPanel("Heatmap",fluidRow(column(12,align="left",shinycssloaders::withSpinner(plotOutput(outputId="heatmap"), type=6)))),
#                  tabPanel("Cluster Bubble Plot", plotlyOutput("pathway_bubble")),
#                  tabPanel("Mean BMD for Time Point", plotlyOutput("meanBMD_timepoint")),
#                  tabPanel("Gene BMD in pathway",
#                           fluidRow(
#                             column(6, uiOutput("timePointSelPat"))
#                           ),
#                           fluidRow(
#                             column(12,
#                                    DT::dataTableOutput("PAT_table")
#                             )),
#                           fluidRow(
#                             column(12,
#                                    plotlyOutput("path_bmd_dist"))
#                           )
#                  ),
#                  tabPanel("Pathways Table",
#                           #"Here I will display the enriched pathway in tabular form",
#                           fluidRow(uiOutput("timePointSelTab")),
#                           fluidRow(downloadButton("downloadEnrichedPathwayTables", "Download")),
#                           fluidRow(DT::dataTableOutput("PatTable"))
#                  ),
#                  #tabPanel("Genes Heatmap", "Here I will display the gene heatmap")
#                  tabPanel("Heatmap Genes",
#                           fluidRow(column(4,
#                                           selectInput(inputId="levelGene", label="Choose a hierarchy level", choices=list(1,2,3))
#                           ),column(4,
#                                    uiOutput("choosePath")
#                           ),column(4,
#                                    selectInput(inputId="selScoreType", label="Show Values", choices=list("BMD"="lfc", "P-Value"="pval", "Combined"="comb"), selected="lfc")
#                           )),fluidRow(column(4,
#                                              #shinyBS::bsButton("doGeneHeatMap", label="Plot", style="danger", icon=icon("exclamation-circle"))
#                                              actionButton("doGeneHeatMap", "Plot")
#                           )),fluidRow(column(12,align="center",
#                                              shinycssloaders::withSpinner(plotOutput(outputId="heatmapGenes"), type=6)
#                           ))
#                  )
#                )
#              )
#           
#            
#            
#   )
# }

funmappone_results = function(){
  tabPanel(value = "enrTab",title = "FunMappONE",
           sidebarLayout(
             sidebarPanel(width = 3,
               wellPanel(
                 tags$h5("1. Data Selection"),
                 fluidRow(selectInput(inputId = "level", label = "Browse hierarchy: choose a level",choices = list(1,2,3))),
                 fluidRow(
                   column(4,uiOutput("chose_lev1")), shinyBS::bsTooltip(id = "chose_lev1",title = "Note: remove ALL from the list for specific selection.",placement = "top"),
                   column(4,uiOutput("chose_lev2")), shinyBS::bsTooltip(id = "chose_lev2",title = "Note: remove ALL from the list for specific selection.",placement = "top"),
                   column(4,uiOutput("chose_lev3")), shinyBS::bsTooltip(id = "chose_lev3",title = "Note: remove ALL from the list for specific selection.",placement = "top")
                 ),
                 fluidRow(
                   uiOutput("selectColumn"), shinyBS::bsTooltip(id = "selectColumn",title = "Note: remove ALL from the list for specific selection.",placement = "top")
                 )
               ),
               wellPanel(
                 tags$h5("2. Plot section"),
                 fluidRow(
                   column(4,checkboxInput("doGrouping", "Show categories", value = TRUE)),
                   # column(4,checkboxInput("aspectRatio", "Keep aspect ratio", value = FALSE)),
                   column(4,actionButton("do", "Plot Map")), shinyBS::bsTooltip(id = "do",title ="NOTE: press the Plot Mat button every time you update the map!",placement = "bottom")
                 )
               ),
               wellPanel(
                 tags$h5("3. Download Selection"),
                 fluidRow(
                   column(6,textInput(inputId ="img_width", value = 15,label = "Width")), #width
                   column(6,textInput(inputId ="img_height", value = 30,label = "Height"))
                   # column(4,downloadButton('downloadData')), shinyBS::bsTooltip(id = "downloadData",title ="NOTE: when downloading, specify image size in inches ",placement = "bottom")
                 ),
                 fluidRow(
                   downloadButton('downloadData'), shinyBS::bsTooltip(id = "downloadData",title ="NOTE: when downloading, specify image size in inches ",placement = "bottom")
                   
                 )
               )
             ),
             mainPanel(width = 9,
               tags$h5("Use scrollbars to navigate and see the whole map"),

               tabsetPanel(

                 tabPanel("Heatmap",
                          fluidRow(
                            column(12,align="left",shinycssloaders::withSpinner(plotOutput(outputId="heatmap"), type=6))
                                   # box(style='width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
                                   #     shinycssloaders::withSpinner(plotOutput(outputId="heatmap"), type=6))
                                   # )
                            )
                          ),
                 tabPanel("Range Plot",
                          fluidRow(
                            column(3,uiOutput("range_funmappone_groupby")),
                            shinyBS::bsTooltip("range_funmappone_groupby", "main variable used for the plot", placement="bottom"),
                            column(3,uiOutput("range_funmappone_groupby2")),
                            shinyBS::bsTooltip("range_funmappone_groupby2", "plot group by", placement="bottom"),
                            
                            column(3,checkboxInput("range_funmappone_is_group_by_numeric", "numeric grouping variable", value = FALSE)),
                            shinyBS::bsTooltip("range_funmappone_is_group_by_numeric", "if selected the group by variable is treated as numeric", placement="bottom"),
                            
                          ),
                          fluidRow(
                            column(3,uiOutput("range_funmappone_filtercol")),
                            shinyBS::bsTooltip("range_funmappone_filtercol", "Select variable name to filter the data in the plot", placement="bottom"),
                            
                            column(3,uiOutput("range_funmappone_filterby")),
                            shinyBS::bsTooltip("range_funmappone_filterby", "Select variable value to filter the data in the plot", placement="bottom"),
                            
                          ),
                          fluidRow(
                            actionButton(inputId = "range_plot_funmappone", label = "Range plot")
                          ),
                          shinycssloaders::withSpinner(plotlyOutput("pathways_range"), type=6)
                          
                    # fluidRow(
                    #   column(6, uiOutput("timePointSelPatRangePlot"))
                    # ),
                ),
                 # tabPanel("Cluster Bubble Plot", plotlyOutput("pathway_bubble")),
                 # tabPanel("Mean BMD for Time Point", plotlyOutput("meanBMD_timepoint")),
                 tabPanel("Gene BMD in pathway",
                          fluidRow(
                            column(6, uiOutput("timePointSelPat"))
                          ),
                          fluidRow(
                            column(12,
                                   DT::dataTableOutput("PAT_table")
                            )),
                          fluidRow(
                            column(12,
                                   plotlyOutput("path_bmd_dist"))
                          ),
                          fluidRow(
                            column(12,
                                   plotlyOutput("path_bmd_dist_by_ppi_degree"))
                          )
                 ),
                 tabPanel("Pathways Table",
                          #"Here I will display the enriched pathway in tabular form",
                          fluidRow(uiOutput("timePointSelTab")),
                          fluidRow(downloadButton("downloadEnrichedPathwayTables", "Download")),
                          fluidRow(DT::dataTableOutput("PatTable"))
                 ),
                 #tabPanel("Genes Heatmap", "Here I will display the gene heatmap")
                 tabPanel("Heatmap Genes",
                          fluidRow(column(4,
                                          selectInput(inputId="levelGene", label="Choose a hierarchy level", choices=list(1,2,3))
                          ),column(4,
                                   uiOutput("choosePath")
                          ),column(4,
                                   selectInput(inputId="selScoreType", label="Show Values", choices=list("BMD"="lfc", "P-Value"="pval", "Combined"="comb"), selected="lfc")
                          )),fluidRow(column(4,
                                             #shinyBS::bsButton("doGeneHeatMap", label="Plot", style="danger", icon=icon("exclamation-circle"))
                                             actionButton("doGeneHeatMap", "Plot")
                          )),
                          fluidRow(column(12,align="left",
                                             shinycssloaders::withSpinner(plotOutput(outputId="heatmapGenes"), type=6)
                          ))
                 )
               )
             )
           )


  )
}


tPOD_results = function(){
  tabPanel(value = "tpod", title = "Transcription wide POD",
           tabsetPanel(
             tabPanel("tPOD",
                      # fluidRow(
                      #   column(6, selectInput("tpod_pod_value", "TPOD options", choices = c("BMDL", "BMD","BMDU"), selected = "BMD"))
                      # ),
                      # fluidRow(
                      #   column(3, actionButton(inputId = "run_tpod", label = "Run tPOD")),
                      #   shinyBS::bsTooltip("run_tpod", "Compute tPOD", placement = "bottom")
                      # ),
                      fluidRow(
                        column(3,
                               selectInput("pod_value", 
                                           label = "POD Value", 
                                           choices = c("BMD", "BMDL", "BMDU"),
                                           selected = "BMD")
                        ),
                        column(3,
                               numericInput("percentile", 
                                            label = "Percentile", 
                                            value = 0.20, 
                                            min = 0, 
                                            max = 1, 
                                            step = 0.01)
                        ),
                        column(3,
                               selectInput("lowest_method", 
                                           label = "Lowest Method", 
                                           choices = c("lowest", "LCRD"),
                                           selected = "lowest")
                        ),
                        column(3,
                               actionButton(inputId = "run_tpod", label = "Run tPOD"),
                               shinyBS::bsTooltip("run_tpod", "Compute tPOD", placement = "bottom")
                        )
                      ),
                      fluidRow(
                        column(6,uiOutput("tpod_select_experiment_pheno")),
                        column(6,uiOutput("tpod_select_time_pheno"))
                      ),
                      fluidRow(
                        column(6, plotlyOutput("tpod_accumulation_plot")),
                        column(6, plotlyOutput("tpod_density_plot"))
                      ),
                      fluidRow(
                        column(12, DT::dataTableOutput("tpod_statistics"))
                      )
             )
             
           )
  )
}

bmdx_manual_tab = function(){
  tabPanel(value="Manual", title="Manual",
           uiOutput('markdown'),
           includeMarkdown("manual/manual.Rmd")
  )
}
