set_mode_panel = function(id, style = "danger"){
  bsCollapsePanel(id, style=style, 
                  fluidRow(
                    column(12, align="center",
                           selectInput(inputId = "mode", 
                                       label = "Run in GLP MODE:",
                                       choices = list("TRUE", "FALSE"),
                                       selected = "FALSE"),
                           shinyBS::bsTooltip("mode", "If TRUE is selected the all the choices performed during the analysis must be justified by the user", placement="top"),
                           
                    )
                  )
  )
}

load_phenodata_panel = function(id, style = "danger"){
  bsCollapsePanel(id, style=style,
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("import_pheno_submit", label="Import Phenotype Data", style="danger",icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("import_pheno_submit", "Launch a graphical window, to configure import of phenotype data from a file!", placement="bottom")
                    )
                  )
  )
}

load_expression_panel = function(id, style = "danger"){
  bsCollapsePanel(id, style=style,
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("import_expr_submit", label="Import Expression Matrix", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("import_expr_submit", "Launch a graphical window, to configure import of expression matrix from a file!", placement="bottom")
                           
                    )
                  )
  )
}

gene_filtering_panel = function(id, style = "danger"){
  bsCollapsePanel(id, style=style,
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("anova_filtering_button", label="Anova Filtering", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("anova_filtering_button", "Launch a graphical window, to configure the ANOVA test parameters! You can repeat this step with different parameters or choose any of the other filtering options. The latest configuration will be used in later steps!", placement="bottom")
                    )
                  ),
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("trend_filtering_button", label="Trend Filtering", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("trend_filtering_button", "Launch a graphical window, to configure the trend test parameters! You can repeat this step with different parameters or choose any of the other filtering options. The latest configuration will be used in later steps!", placement="bottom")
                    )
                  ),
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("fc_filtering_button", label="Filtering by Fold-change", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("fc_filtering_button", "Launch a graphical window, to configure the parameters for the fold-change calculation and filtering! You can repeat this step with different parameters or choose any of the other filtering options. The latest configuration will be used in later steps!", placement="bottom")
                    )
                  ),
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("skip_anova_filtering_button", label="Skip Filtering", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("skip_anova_filtering_button", "Skip Filtering and continue with all importet data. You can repeat this step with different paramters or choose any of the other filtering options. The latest configuration will be used in later steps!", placement="bottom")
                           
                    )
                  )       
  )
}

compute_bmd_panel = function(id, style = "danger"){
  bsCollapsePanel(id, style=style,
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("bmd_button", label="Compute BMD", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("bmd_button", "Launch a graphical window to configure BMD analysis!", placement="bottom")
                    )
                  )
  )
}

enrichment_panel = function(id, style = "danger"){
  bsCollapsePanel(id, style=style,
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("enrich_button", label="Enrichment", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("enrich_button", "Launch a graphical window to configure enrichment analysis!", placement="bottom")
                    )
                  )
  )
}

gene_comparison = function(id, style = "danger"){
  bsCollapsePanel(id, style=style,
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("gene_comparison_button", label="Gene pairs", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("gene_comparison_button", "Launch a graphical window to perform gene pair comparison analysis!", placement="bottom")
                    )
                  )
  )
}


aop_ke_analysis = function(id, style = "danger"){
  bsCollapsePanel(id, style=style,
                  fluidRow(
                    column(12, align="center",
                           shinyBS::bsButton("KE_enrichment_button", label="Enrich KEs", style="danger", icon=icon("exclamation-circle")),
                           shinyBS::bsTooltip("KE_enrichment_button", "Launch a graphical window to perform KEs enrichment!", placement="bottom")
                    )
                  )
  )
}


download_panel = function(id, style = "sample"){
  bsCollapsePanel(id,style=style,
                  fluidRow(
                    column(12, align="center",
                           downloadButton("exportRpt", "Analysis Report")
                    )
                  )            
  )
}

info_panel = function(id, style = "sample"){
  bsCollapsePanel(id, style=style,
                  fluidRow(
                    HTML(" <a style=color:blue;  target=\"_blank\"; href=\"https://github.com/Greco-Lab/BMDx\">GitHub</a>")
                  ),
                  fluidRow(
                    # HTML("<a style=color:blue; target=\"_blank\";  href=\"manual/Manual.pdf\">Manual</a>")
                    HTML('<a href="manual.html" target="_blank" style="color:blue;">Manual</a>')
                  ),
                  fluidRow(
                    HTML("<a style=color:blue;  target=\"_blank\"; href=\"https://github.com/Greco-Lab/BMDx/blob/master/pheno_list_2_exp_4_TP.xlsx\">Sample pheno data</a>")
                  ),
                  fluidRow(
                    HTML("<a style=color:blue;  target=\"_blank\"; href=\"https://github.com/Greco-Lab/BMDx/blob/master/exp_mat_file_2_exp_4_tp.xlsx\">Sample expression data</a>")
                  )
  )
}
