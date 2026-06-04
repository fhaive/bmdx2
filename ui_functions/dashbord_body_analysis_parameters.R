
funmappone_parameters = function(){
  shinyBS::bsModal("enrichPathways", "Enrichment", "enrich_button", size="large",
                   tags$h5("1. Input gene lists"),
                   wellPanel(
                     fluidRow(
                       column(6,
                              radioButtons("organism","1) Organisms",
                                           choices = c(human = "Human", mouse = "Mouse", rat = "Rat"),selected = "Human"),
                              shinyBS::bsTooltip(id = "organism",title = "Note: select organism and gene ID before uploading the file",placement = "bottom")
                              
                       ) ,
                       column(6,radioButtons("idtype","2) GeneID",
                                             choices = c(symbols = "SYMBOL", ensemble = "ENSEMBL",entrez = "ENTREZID"),
                                             selected = "SYMBOL"),
                              shinyBS::bsTooltip(id = "idtype",title = "Note: select organism and gene ID before uploading the file",placement = "bottom")
                       )
                     )
                   ),
                   tags$h5("2. Functional annotation parameters"),
                   wellPanel(
                     fluidRow(
                       column(6,radioButtons("EnrichType","Select Functional Annotation",
                                             choices = c(KEGG = "KEGG", REACTOME="REACTOME",GO = "GO"),
                                             selected = "KEGG")
                       ),
                       column(6,radioButtons("GOType","Select GO",
                                             choices = c(BP = "BP", CC="CC",MF = "MF"),
                                             selected = "BP")
                       )),
                     fluidRow(
                       column(4,radioButtons("pcorrection","Correction Method",
                                             choices = c(gSCS = "gSCS", fdr = "fdr", bonferroni = "bonferroni", Nominal = "none"),
                                             selected = "gSCS"),
                              shinyBS::bsTooltip(id = "pcorrection",
                                                 title = "Default is g:SCS. Check g:Profiler web page for more info",
                                                 placement = "top")
                       ),
                       column(4,selectInput(inputId = "pvalueTh", label = "P-value threshold:",choices = list(0.001,0.005,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09),selected = 0.05),
                       column(4, checkboxInput("only_significant", "Significant Results only", value = TRUE)),
                              
                              
                              
                       )),
                     fluidRow(
                       column(6, checkboxInput("only_annotated", "Annotated genes only", value = TRUE)),
                       column(6, sliderInput("min_intersection", "Minumum number of genes in the intersection:",
                                             min = 0, max = 100, value = 0))
                     )
                   ),
                   tags$h5("3. Display parameters"),
                   wellPanel(
                     fluidRow(
                       
                       column(6,radioButtons("aggregation","Aggregation Function",
                                             choices = c(min = "min", max = "max",mean = "mean",
                                                         median = "median"),
                                             selected = "mean")  
                       ),
                       
                       column(6,
                              radioButtons("MapValueType","Choose Values to be used in the Heatmap",
                                           choices = c(Pvalue = "PVAL", BMD="FC"), #,GenesModifications_PValue  = "FCPV"),
                                           selected = "FC")
                       )
                     ),
                     
                     fluidRow(column(6,
                              radioButtons("continuous","Plot modification for the Heatmap",
                                                      choices = c(values = "continuous",
                                                                  binary = "discrete"),
                                                      selected = "continuous")))
                   ),
                   tags$h5("4. Perform Enrichment"),
                   wellPanel(
                     fluidRow(
                       column(6,  shinyBS::bsButton("enrichment_analysis", label="Run Enrichment", style="info", icon=icon("hand-o-right")))
                     )
                   ),
                   tags$h5("5. Download gene lists as excel file"),
                   downloadButton("downloadFunmapponeInput", "Download")
  )
}

models_help = function(){
  bsCollapse(id="BMDModHelp",
             bsCollapsePanel("Linear Model", style="primary",
                             withMathJax(),
                             helpText("The formula for the linear model is
                                                                    $$ f(dose) = \\beta_0 + \\beta_1 dose $$
                                                                    The linear model is a special case of the polynomial model, with $$n=1$$")
             ),
             bsCollapsePanel("Polynomial Model", style = "primary",
                             withMathJax(),
                             
                             helpText("The polynomial model is a flexible regression approach that describes the dose-response relationship using a polynomial equation:"),
                             
                             HTML("$$ f(dose) = \\beta_0 + \\beta_1 \\cdot dose + \\beta_2 \\cdot dose^2 + \\beta_3 \\cdot dose^3 + \\ldots + \\beta_n \\cdot dose^n $$"),
                             
                             helpText("where \\(n\\) is the degree of the polynomial, and \\(\\beta_0, \\beta_1, \\beta_2, \\ldots, \\beta_n\\) are the model coefficients."),
                             
                             helpText(strong("Polynomial Model (n = 2) - Quadratic Model:")),
                             
                             HTML("$$ f(dose) = \\beta_0 + \\beta_1 \\cdot dose + \\beta_2 \\cdot dose^2 $$"),
                             
                             helpText("This model captures simple curvature in the dose-response relationship and is often used when there is a single turning point."),
                             
                             helpText(strong("Polynomial Model (n = 3) - Cubic Model:")),
                             
                             HTML("$$ f(dose) = \\beta_0 + \\beta_1 \\cdot dose + \\beta_2 \\cdot dose^2 + \\beta_3 \\cdot dose^3 $$"),
                             
                             helpText("A cubic model allows for two inflection points, making it useful when the response has more complex curvature."),
                             
                             helpText(strong("Polynomial Model (n = 4) - Quartic Model:")),
                             
                             HTML("$$ f(dose) = \\beta_0 + \\beta_1 \\cdot dose + \\beta_2 \\cdot dose^2 + \\beta_3 \\cdot dose^3 + \\beta_4 \\cdot dose^4 $$"),
                             
                             helpText("The quartic model adds an additional term, enabling more flexibility in capturing intricate dose-response relationships."),
                             
                             helpText(strong("Polynomial Model (n = 5) - Quintic Model:")),
                             
                             HTML("$$ f(dose) = \\beta_0 + \\beta_1 \\cdot dose + \\beta_2 \\cdot dose^2 + \\beta_3 \\cdot dose^3 + \\beta_4 \\cdot dose^4 + \\beta_5 \\cdot dose^5 $$"),
                             
                             helpText("The quintic model is the most flexible polynomial model, allowing for multiple inflection points, which may be necessary for highly nonlinear dose-response curves.")
             ),
             
             
             
             bsCollapsePanel("Power Model", style="primary",
                             withMathJax(),
                             helpText("The formula for the power model is
                                                                    $$ f(dose) = \\beta_0 + (dose)^\\delta $$
                                                                    The user can choose between $$\\delta = 2, 3, 4$$")                                           
             ),
             bsCollapsePanel("Exponential Model", style="primary",
                             withMathJax(),
                             helpText("The formula for the exponential model is
                                                                    $$ f(dose) = \\beta_0 + exp(dose) $$")                                           
             ),
             bsCollapsePanel("Hill Model", style="primary",
                             withMathJax(),
                             helpText("The formula for the hill model is 
                                                                    $$ f(dose) = \\beta_0 + \\dfrac{dose^n}{Kd + dose^n} $$
                                                                    The user can choose between $$n = 0.5,1,2,3,4,5$$ while Kd is fixed to 10.")
                             
             ),
             bsCollapsePanel("Log-logistic Model", style = "primary",
                             withMathJax(),
                             helpText("The Log-logistic 5 model is defined by the five-parameter model function
                                                                    $$f(dose, (b, c, d, e, f)) = c + \\dfrac{d-c}{(1+\\exp(b(\\log(dose)-\\log(e))))^f}$$
                                                                    If the parameter f differs from 1 then the function is asymmetric; otherwise it is symmetric (on log scale).$$\n$$",
                                      
                                      "The Log-logistic 4 model is defined by the four-parameter model function
                                                                    $$f(dose, (b,c,d,e)) = c + \\dfrac{d-c}{1+\\exp(b(\\log(dose)-\\log(e)))}$$
                                                                    The function is symmetric about the inflection point (e)$$\n$$",
                                      
                                      "The Log-logistic 3 model is defined by the three-parameter model function
                                                                    $$f(dose, (b,c,e)) =  c + \\dfrac{1-c}{1+\\exp(b(\\log(dose)-\\log(e)))}$$
                                                                    The function is symmetric about the inflection point (e)$$\n$$",
                                      
                                      "The Log-logistic 2 model is defined by the two-parameter model function
                                                                    $$f(dose, (b,e)) = \\dfrac{1}{1+\\exp(b(\\log(dose)-\\log(e)))}$$
                                                                    The function is symmetric about the inflection point (e)"
                             )
             ),
             bsCollapsePanel("Weibull Models", style = "primary",
                             withMathJax(),
                             
                             helpText("The Weibull models describe dose-response relationships using asymmetric sigmoidal functions. Different parameterizations allow for varying flexibility in curve fitting."),
                             
                             helpText(strong("Weibull 1.2 Model:")),
                             HTML("$$ f(dose, (b, e)) = 1 - \\exp(-b \\cdot dose^e) $$"),
                             helpText("This two-parameter model is used for growth curves and describes an asymmetric sigmoidal relationship."),
                             
                             helpText(strong("Weibull 1.3 Model:")),
                             HTML("$$ f(dose, (b, d, e)) = d - d \\cdot \\exp(-b \\cdot dose^e) $$"),
                             helpText("This three-parameter model extends Weibull 1.2 by adding a scaling parameter \\(d\\) that adjusts the upper asymptote."),
                             
                             helpText(strong("Weibull 1.4 Model:")),
                             HTML("$$ f(dose, (b, c, d, e)) = c + (d - c) \\cdot (1 - \\exp(-b \\cdot dose^e)) $$"),
                             helpText("This four-parameter model allows both upper (\\(d\\)) and lower (\\(c\\)) asymptotes, providing greater flexibility in curve fitting."),
                             
                             helpText(strong("Weibull 2.2 Model:")),
                             HTML("$$ f(dose, (b, e)) = \\exp(-\\exp(b \\cdot (\\log(dose) - e))) $$"),
                             helpText("This two-parameter model describes a sigmoidal decay function, where the inflection point is given by \\(e\\)."),
                             
                             helpText(strong("Weibull 2.3 Model:")),
                             HTML("$$ f(dose, (b, d, e)) = d \\cdot \\exp(-\\exp(b \\cdot (\\log(dose) - e))) $$"),
                             helpText("This three-parameter model extends Weibull 2.2 by introducing a scaling parameter \\(d\\), which modifies the maximum response."),
                             
                             helpText(strong("Weibull 2.4 Model:")),
                             HTML("$$ f(dose, (b, c, d, e)) = c + (d - c) \\cdot \\exp(-\\exp(b \\cdot (\\log(dose) - e))) $$"),
                             helpText("This four-parameter model generalizes Weibull 2.3 by incorporating both lower (\\(c\\)) and upper (\\(d\\)) asymptotes, making it the most flexible variant.")
             ),
             
             
             bsCollapsePanel("Michaelis-Menten Model", style = "primary",
                             withMathJax(),
                             helpText("The model is defined by the three-parameter model (MM.3) function
                                                                    $$f(dose, (c, d, e)) = c + \\dfrac{d-c}{1+(e/dose)}$$
                                                                    It is increasing as a function of the dose, attaining the lower limit c at dose 0 (x=0) and the upper limit d for infinitely large doses. 
                                                                    The parameter e corresponds to the dose yielding a response halfway between c and d. 
                                                                    The common two-parameter Michaelis-Menten model (MM.2) is obtained by setting c equal to 0.")
             )
             
             
             
  )
}
compute_bmd_parameters = function(){
  shinyBS::bsModal("computeBMD", "Compute BMD Value", "bmd_button", size="large",
                   fluidRow(
                     column(4, selectInput("BMDNCores", "Number of cores:", choices=c(1:25), selected = 1)),
                     shinyBS::bsTooltip("BMDNCores", "Number of cores to be used during the analysis", placement="top"),
                     column(4, textInput("BMDMaxIter", "Maximum iterations:", value=1024)),
                     shinyBS::bsTooltip("BMDMaxIter", "Convergence criteria for model fitting", placement="bottom")
                   ),
                   fluidRow(
                     column(4,selectInput("BMDdataType", "Data type:", choices=c("continuous", "binominal"),selected="continuous")),
                     shinyBS::bsTooltip("BMDdataType", "Type of data used in the modelling", placement="top"),
                     
                     column(4,selectInput("BMDdeviationType", "Deviation type:", choices=c("relative", "standard", "absolute"),selected="standard")),
                     shinyBS::bsTooltip("BMDdeviationType", "How the deviation is computed", placement="top"),
                     
                     column(4, selectInput("variance_type", "Variance type", choices=c("constant"="constant",
                                                                                       "non constant"="nonconstant",
                                                                                       "inferred"="infer"), selected = "Constant")),
                     
                   ),
                   fluidRow(
                    column(4,), 
                    column(4, selectInput("RespLev", label = "BMR factor", 
                                          choices=c("1 (0.522)" = "0.01", 
                                                    "5 (1.021)" = "0.05", 
                                                    "10 (1.349)" = "0.1", 
                                                    "15 (1.581)" = "0.15", 
                                                    "25 (1.932484)" = "0.25", 
                                                    "50 (2.600898)" = "0.5", 
                                                    "60 (2.855148)" = "0.6") , 
                                          selected ="0.1")),
                    shinyBS::bsTooltip("RespLev", "The number of standard deviations at which the BMD is defined.",
                                       # The BMR is computed by considering both the response at control and the BMR factor, 
                                       # so the BMR may change when the model used to fit the data changes.", 
                                       placement="top"),
                     # column(4,selectInput(inputId = "BMDsignificance", label = "Significance threshold for levene test:",
                     #                      choices = list(0.001,0.005,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09),selected = 0.05)),
                     column(4, sliderInput("BMDsignificance", "Significance threshold for levene test:", min = 0, max = 1, value = 0.05)),
                     
                     
                     # column(4, checkboxInput("constantVar", "Assumption of Constant Variance", value = TRUE)),
                     # shinyBS::bsTooltip("constantVar", "If selected constant variance is assumed during modelling", placement="bottom"),
                     # 
                     # column(4, checkboxInput("BMDmodelVar", "Model Variance", value = FALSE)), 
                     # shinyBS::bsTooltip("BMDmodelVar", "If selected and constant variance is not assumed,
                     #                                    the modelling is performed by considering the variability 
                     #                                    of the fitted model.", placement="bottom"),
                     
                    ),
                   fluidRow(
                     
                     column(4,), 
                     
                     column(4, sliderInput("conf_interval", "Confidece interval:", min = 0, max = 1, value = 0.95)),
                     shinyBS::bsTooltip("conf_interval", "The statistical confidence limit applied to the BMD estimated by the model", placement="bottom"),
                     
                   ),
                  
                   fluidRow(
                     column(4, sliderInput("LOOF", "Lack-of-fit p-value:", min = 0, max = 1, value = 0.1)),
                     # column(4,selectInput("LOOF", "Lack-of-fit PValue Th:", choices=c(0.3,0.2,0.1,0.05),selected=0.1)), 
                     shinyBS::bsTooltip("LOOF", "Threshold for the lack-of-fit p-value. Suggested value is 0.1", placement="top"),
                     
                     column(4,selectInput("BMDSettings", "Select models", choices=c("All","Regulatory","Degree of Freedom", "Custom"),selected="Custom")),
                     shinyBS::bsTooltip("BMDSettings", "Which set of models should be used", placement="top")
                     
                                          
                   ),
                   # TODO: the two parameters fist_model_AIC and strictly_monotonic are currently not included into the library! 
                   # TODO: consider adding the behaviour. 
                   #column(4, checkboxInput("first_model_AIC", "Only the optimal model (min AIC) is considered", value = TRUE)),
                   #column(4, checkboxInput("strictly_monotonic", "Only strictly monotonic models allowed", value = FALSE))
                   fluidRow(
                     column(12,offset = 0, 
                            wellPanel(uiOutput("bmd_checkbox"))
                     ) ),
                   fluidRow(
                     column(12, align="right",shinyBS::bsButton("bmd_analysis", label="Run BMD", style="info", icon=icon("hand-o-right")))
                   ),
                   fluidRow(
                     column(12,
                            h5("Models Description"),
                            models_help()
                     )
                   )
  )  
}

compare_gene_pairs_parameters = function(){
  shinyBS::bsModal("compareGenes", "Compare genes", "gene_comparison_button", size="large",
                   fluidRow(
                     column(6, uiOutput("gene_pair_experiments_filtercol")),
                     column(6, uiOutput("gene_pair_experiments_filterby"))
                   ),
                   fluidRow(
                     column(6, uiOutput("gene_pair_time_filtercol")),
                     column(6, uiOutput("gene_pair_time_filterby"))
                   ),
                   fluidRow(
                     column(3, selectInput("gene_comparison_organism", "Organism:", choices=c("human","mouse","rat"), selected = "human")),
                     column(3, selectInput("gene_comparison_gene_id_type", "Gene ID type:", choices=c("Ensembl"="ENSEMBL",
                                                                                                      "Symbol"="SYMBOL",
                                                                                                      "EntrezID"="ENTREZID"), selected = "ENSEMBL")),
                     column(3, selectInput("n_new_data", "Number of predictions:", choices=c(1000))), 
                     column(3, selectInput("gene_comparison_NCores", "Number of cores:", choices=c(1:25), selected = 1))
                   ),
                   fluidRow(
                     
                     column(12, align="right",shinyBS::bsButton("gene_pairs_comparison_analysis", label="Run gene pair comparison", style="info", icon=icon("hand-o-right")))
                   )                       
  )
}

ke_enrichment_parameters = function(){
  shinyBS::bsModal("KEenrichment", "Enrich KEs and AOPs", "KE_enrichment_button", size="large",
                   fluidRow(
                     selectInput("ke_organism", "Select organism:",
                                 choices=c("Human","Mouse","Rat"), selected = "Human"),
                     selectInput("ke_gene_identifier", "Select gene identifier:",
                                 choices=c("Ensembl"="ENSEMBL","Symbol"="SYMBOL","Entrez"="ENTREZID"), selected = "Ensembl")
                   ),
                   fluidRow(
                     column(4, sliderInput("ke_sig_pval", "(KEs) Enrichment Pval:", min = 0, max = 1, value = 0.05)),
                     column(4, selectInput("ke_correction_method", "(KEs) Correction method:",
                                           choices=stats::p.adjust.methods, selected = "fdr")),
                     column(4, checkboxInput("ke_only_significant", "(KEs) Significant Results only", value = TRUE)),
                     column(4, selectInput("KE_aggregation_function", "Select aggregation function:",
                                           choices=c("Median"="median","Mean"="mean","Min"="min","5th-quantile"="quantile_05"), selected = "median"))

                   ),

                   fluidRow(
                     column(4, sliderInput("aop_sig_pval", "(AOPs) Enrichment Pval:", min = 0, max = 1, value = 0.05)),
                     column(4, selectInput("aop_correction_method", "(AOPs) Correction method:",
                                           choices=stats::p.adjust.methods, selected = "fdr")),
                     column(4, checkboxInput("aop_only_significant", "(AOPs) Significant Results only", value = TRUE)),
                     
                   ),
                   
                   fluidRow(
                     column(4, sliderInput("min_aop_length_fingerprint", "Minimum length AOP:", min = 0, max = 10, value = 5)),
                     column(4, sliderInput("percentage_enriched_ke", "Percentage of enriched KE in AOP:",min = 0, max = 1, value = 0.33))
                   ),
                   
                   fluidRow(
                     column(4, checkboxInput("annotated_background", "Use annotate genes as background", value = FALSE)),
                     column(3, fileInput("background", label="Choose background file")),
                   ),
                   
                   fluidRow(
                     column(12, align="right",shinyBS::bsButton("ke_enrichment_analysis",
                                                                label="Enrich KE", style="info", icon=icon("hand-o-right")))
                   )
                   

  )
}

compute_anova_parameters = function(){
  shinyBS::bsModal("computeAnova", "Filter Genes by Anova", "anova_filtering_button", size="large",
                   fluidRow(
                     column(3, selectInput("anovaNCores", "Number of cores:", choices=c(1:25), selected = 1)),
                     column(3, selectInput("anovaPvalAdj", "Pvalue Adjustment Method:", choices=c("Nominal", "fdr", "bonferroni", "BH"),selected="fdr")),
                     column(3, sliderInput("anovaPvalTh", "P-value threshold:", min = 0, max = 1, value = 0.05))
                     # column(3, selectInput("anovaPvalTh", "Anova PValue Threshold:", choices=c(0.05,0.04,0.03,0.02,0.01))), #FOR TESTING ONLY
                   ),
                   # fluidRow(
                   #   textOutput("anova_update")
                   # ),
                   fluidRow(
                     column(12, align="right",shinyBS::bsButton("anova_analysis", label="Run Anova", style="info", icon=icon("hand-o-right")))
                   )                       
  )
}

compute_trend_parameters = function(){
  shinyBS::bsModal("computeTrend", "Filter Genes by Trend Test", "trend_filtering_button", size="large",
                   fluidRow(
                     column(3, selectInput("trendNCores", "Number of cores:", choices=c(1:25), selected = 1)),
                     column(3, selectInput("TrendPvalAdj", "Pvalue Adjustment Method:", choices=c("Nominal", "fdr", "bonferroni", "BH"),selected="fdr")),
                     column(3, sliderInput("trendPvalTh", "P-value threshold:", min = 0, max = 1, value = 0.05))
                     # column(3, selectInput("trendPvalTh", "Trend PValue Th:", choices=c(0.5, 0.05,0.04,0.03,0.02,0.01))), #FOR TESTING ONLY
                   ),
                   fluidRow(
                     column(12, align="right",shinyBS::bsButton("trend_analysis", label="Run Trend Test", style="info", icon=icon("hand-o-right")))
                   )                       
  )
}

compute_fc_parameters = function(){
  shinyBS::bsModal("computeFoldChange", "Filter Genes by Fold change", "fc_filtering_button", size="large",
                   fluidRow(
                     column(3, selectInput("fcPvalAdj", "Pvalue Adjustment Method:", choices=c("Nominal", "fdr", "bonferroni", "BH"), selected="fdr")),
                     column(3, selectInput("fcPvalTh", "PValue Threshold:", choices=c(0.05,0.04,0.03,0.02,0.01))), 
                     # column(3, selectInput("fcTh", "Fold-change Threshold:", choices=c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2), selected = 1.5)), 
                     column(3, sliderInput("fcTh", "Fold-change Threshold:", min=0, max=10, step=0.1,value = 1.5)), 
                     
                     column(3, selectInput("fcNCores", "Number of cores:", choices=c(1:25), selected = 1))
                   ),
                   fluidRow(
                     column(12, align="right",shinyBS::bsButton("fc_analysis", label="Run Fold-change", style="info", icon=icon("hand-o-right")))
                   )                       
  )
}

import_gene_expression_parameters = function(){
  shinyBS::bsModal("importGxModal", "Import Gene Expression Table", "import_expr_submit", size="large",
                   fluidRow(
                     column(3,fileInput("gx", label="File")),
                     column(3, textInput("x_ID", "add name for x variable", value = "dose", width = NULL, placeholder = NULL)),
                     column(3, textInput("y_ID", "add name for y variable", value = "expr", width = NULL, placeholder = NULL))
                   ),
                   fluidRow(
                     column(3, checkboxInput(inputId = "is_rnaseq",label = "RNASeq raw counts", value = FALSE)),
                     column(3, checkboxInput(inputId = "first_col_as_rownames_exp",label = "First columns as rownames", value = TRUE)),
                     column(3, checkboxInput(inputId = "check_numeric_exp",label = "Force columns to be numerical", value = TRUE)),
                     
                   ),
                   fluidRow(
                     column(12, align="right",shinyBS::bsButton("upload_gx_submit", label="Import", style="info", icon=icon("hand-o-right")))
                   )
  )
}

import_pheno_parameters = function(){
  shinyBS::bsModal("importPhenoModal", "Import Phenotype Data", "import_pheno_submit", size="large",
                   fluidRow(column(3, fileInput("fPheno", label="Choose phenodata file")),
                            column(3,actionButton("load_pheno_submit", "Load Phenodata File", class = "btn-info", style = "margin-top: 45px;"))
                   ),fluidRow(
                     column(2, align="left", textOutput("phRowsText"),textOutput("phColsText"))
                   ),hr(),
                   fluidRow(
                     column(12,
                            hidden(div(id="phenoPreviewDiv",
                                       fluidRow(
                                         column(12,
                                                rhandsontable::rHandsontableOutput("phenoTypesRH")
                                         )
                                       ),hr(),
                                       fluidRow(
                                         column(4, uiOutput("selSampleIDCol")),
                                         column(4, uiOutput("selDoseCol")),
                                         column(4, uiOutput("selTPCol"))
                                       ),
                                       # fluidRow(column(4, checkboxInput(inputId = "check_numeric_pheno",label = "Force columns to be numerical", value = FALSE))),
                                       fluidRow(column(3,actionButton("set_opt_vars", "Set Optional Variables of Interest")))
                                       
                            ))
                     )
                   ),
                   fluidRow(
                     column(12,
                            hidden(div(id="phenoOptVarsDiv",
                                       fluidRow(
                                         column(12,offset = 0, 
                                                wellPanel(uiOutput("phenoOptVars"))
                                                
                                         )
                                       ) ))
                     )
                   ), 
                   fluidRow(column(12, align="right",shinyBS::bsButton("upload_pheno_submit", label="Next", style="info", icon=icon("hand-o-right"))))
  )
}
