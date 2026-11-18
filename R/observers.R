#' Observers
#' 
#' \code{.create_observers} and \code{.create_launch_observers} define the
#' server to import and build TreeSE objects and track the state of the Build
#' and Launch buttons.
#'
#' @param input The Shiny input object from the server function.
#' @param rObjects A reactive list of values generated in the landing page.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @name create_observers
#' @keywords internal

#' @rdname create_observers
#' @importFrom utils read.table
#' @importFrom ape read.tree
#' @importFrom S4Vectors DataFrame
#' @importFrom mia importHUMAnN importMetaPhlAn importQIIME2 importMothur
#'   addAlpha
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
.create_import_observers <- function(input, rObjects, pObjects){

    # nocov start
    observeEvent(input$import, {
        
        pObjects$commands <- .setupCommand
        
        if( input$format == "dataset" ){
        
            rObjects$tse <- isolate(get(input$data))
            pObjects$commands <- c(pObjects$commands, .dataCommand(input$data))
        
        }else if( input$format == "rds" ){
        
            isolate({
                req(input$file)
                rObjects$tse <- readRDS(input$file$datapath)
                
                pObjects$commands <- c(
                    pObjects$commands, .rdsCommand(input$file)
                )
                
                if( input$rds_format == "phyloseq" ){
                    rObjects$tse <- .update_tse(rObjects, pObjects,
                        "convertFromPhyloseq", list(rObjects$tse))
                }else if( input$rds_format == "BIOM" ){
                    rObjects$tse <- .update_tse(rObjects, pObjects,
                        "convertFromBIOM", list(rObjects$tse))
                }else if( input$rds_format == "DADA2" ){
                    rObjects$tse <- .update_tse(rObjects, pObjects,
                        "convertFromDADA2", list(rObjects$tse))
                }
            })
        
        }else if( input$format == "raw" ){
        
            isolate({
                req(input$assay)
                
                assay_list <- lapply(input$assay$datapath,
                    function(x) as.matrix(read.table(x, row.names = 1,
                        header = TRUE, sep = "\t")))
                
                names(assay_list) <- gsub(".tsv", "", input$assay$name)
                
                coldata <- .set_optarg(input$coldata$datapath,
                    alternative = DataFrame(row.names = colnames(assay_list[[1]])),
                    loader = read.table, row.names = 1, header = TRUE, sep = "\t")
                
                rowdata <- .set_optarg(input$rowdata$datapath,
                    loader = read.table, row.names = 1,
                    header = TRUE, sep = "\t")
                
                row.tree <- .set_optarg(input$row.tree$datapath,
                    loader = read.tree)
                
                col.tree <- .set_optarg(input$col.tree$datapath,
                    loader = read.tree)
                
                fun_args <- list(assays = assay_list, colData = coldata,
                    rowData = rowdata, rowTree = row.tree, colTree = col.tree)
                
                rObjects$tse <- .update_tse(
                     rObjects, pObjects, "TreeSummarizedExperiment", fun_args)
                
                if( input$taxa.from.rownames ){
                    
                    rObjects$tse <- .update_tse(rObjects, pObjects,
                        "miaDash:::.rownames2taxa", list(rObjects$tse))
                }
            })
      
        }else if( input$format == "foreign" ){
          
            isolate({
                req(input$main.file)
              
                coldata <- .set_optarg(input$col.data$datapath,
                    alternative = input$col.data$datapath)
                
                treefile <- .set_optarg(input$tree.file$datapath,
                    alternative = input$tree.file$datapath)
                
                if( input$ftype == "BIOM" ){

                    fun_args <- list(file = input$main.file$datapath,
                        col.data = coldata, tree.file = treefile,
                        removeTaxaPrefixes = input$rm.tax.pref,
                        rankFromPrefix = input$rank.from.pref)
              
                }else if( input$ftype == "HUMAnN" ){
                
                    fun_args <- list(file = input$main.file$datapath,
                        col.data = coldata,
                        prefix.rm = input$rm.tax.pref,
                        remove.suffix = input$rm.hum.suf)

                }else if( input$ftype == "MetaPhlAn" ){

                    fun_args <- list(file = input$main.file$datapath,
                        col.data = coldata, tree.file = treefile)
                
                }else if( input$ftype %in% c("Mothur", "QIIME2") ){
                
                    rowdata <- .set_optarg(input$f.rowdata$datapath,
                        alternative = input$f.rowdata$datapath)
                    
                    fun_args <- list(assay.file = input$main.file$datapath,
                        row.file = input, col.file = rowdata)
                
                }
                
                imp_fun <- paste0("import", input$ftype)
                rObjects$tse <- .update_tse(
                    rObjects, pObjects, imp_fun, fun_args)
            })
            
        }
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    # nocov end
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom SummarizedExperiment assay
#' @importFrom mia subsetByPrevalent subsetByRare agglomerateByRank
#'   transformAssay
.create_manipulate_observers <- function(input, rObjects, pObjects) {
  
    # nocov start
    observeEvent(input$apply, {
      
        if( input$manipulate == "subset" ){
          
            isolate({
                req(input$subassay)
              
                if( input$subkeep == "prevalent" ){
                    subset_fun <- "subsetByPrevalent"
                } else if( input$subkeep == "rare" ){
                    subset_fun <- "subsetByRare"
                }
            
                fun_args <- list(rObjects$tse, assay.type = input$subassay,
                    prevalence = input$prevalence, detection = input$detection)
                
                rObjects$tse <- .update_tse(
                    rObjects, pObjects, subset_fun, fun_args)
              
            })
          
        }
        
        else if( input$manipulate == "agglomerate" ){
          
            isolate({
                
                fun_args <- list(rObjects$tse, rank = input$taxrank)
                rObjects$tse <- .update_tse(
                    rObjects, pObjects, "agglomerateByRank", fun_args)
              
            })
          
        } else if( input$manipulate == "transform" ){
          
            isolate({
                req(input$assay.type)
              
                if( input$assay.name != "" ){
                    name <- input$assay.name
                } else {
                    name <- input$trans_method
                }
              
                fun_args <- list(rObjects$tse, name = name,
                    method = input$trans_method, assay.type = input$assay.type,
                    MARGIN = input$margin, pseudocount = input$pseudocount)
                
                if( input$trans_method == "philr" ){
                    fun_args <- c(fun_args, tree.name = input$trans.tree)
                }
                
                rObjects$tse <- .update_tse(
                    rObjects, pObjects, "transformAssay", fun_args)
            })
          
        }
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom stats as.formula
#' @importFrom mia addAlpha addMDS addNMDS addRDA addHierarchyTree
#'   addPrevalence addPrevalentAbundance addCluster
#' @importFrom TreeSummarizedExperiment rowTree
#' @importFrom scater runMDS runPCA
#' @importFrom scuttle addPerCellQC
#' @importFrom bluster KmeansParam DmmParam HclustParam NNGraphParam
.create_estimate_observers <- function(input, rObjects, pObjects) {
    
    # nocov start
    observeEvent(input$compute, {
        
        if( input$estimate == "quality" ){
        
            isolate({
                req(input$estimate.assay)
        
                for( qmetric in input$quality.metrics ){
                
                    qfun <- paste0("add", qmetric)
                    
                    qfun_args <- list(rObjects$tse,
                        assay.type = input$estimate.assay)
                    
                    rObjects$tse <- .update_tse(
                        rObjects, pObjects, qfun, qfun_args)
                }
        
            })
        
        }else if( input$estimate == "alpha" ){
        
            if( is.null(input$alpha.index) ){
                .print_message("Please select one or more metrics.")
                return()
            }
        
            isolate({
                req(input$estimate.assay)
            
                if( input$estimate.name != "" ){
                    name <- input$estimate.name
                } else {
                    name <- input$alpha.index
                }
            
                fun_args <- list(rObjects$tse, name = name,
                    assay.type = input$estimate.assay, index = input$alpha.index)
                
                rObjects$tse <- .update_tse(
                    rObjects, pObjects, "addAlpha", fun_args)
            })
        
        }else if( input$estimate == "beta" ){
            
            if( input$ncomponents > nrow(rObjects$tse) - 1 ){
              
                .print_message(
                    "Please use a number of components smaller than the number",
                    "of features in the assay."
                )
              
                return()
            }
          
            isolate({
                req(input$estimate.assay)
                
                if( input$estimate.name != "" ){
                    name <- input$estimate.name
                }else{
                    name <- input$bmethod
                }
                
                beta_args <- list(rObjects$tse,
                    assay.type = input$estimate.assay,
                    ncomponents = input$ncomponents, name = name)
                
                if( input$bmethod %in% c("MDS", "NMDS", "RDA") ){
                    
                    beta_args <- c(beta_args, method = input$beta.index)
                    
                }else if( input$bmethod == "RDA" ){
                    
                    if( input$rda.formula == ""){
                        rda_formula <- NULL
                    }else{
                        rda_formula <- as.formula(input$rda.formula)
                    }
                    
                    beta_args <- c(beta_args, formula = rda_formula)
                }
                
                # if( input$beta.index == "unifrac" ){
                # Add beta.tree input
                # beta_args <- c(beta_args, tree = input$beta.tree)
                
                beta_fun <- paste0("run", input$bmethod)
                rObjects$tse <- .update_tse(
                    rObjects, pObjects, beta_fun, beta_args)
            })
        
        }else if( input$estimate == "cluster" ){
            
            isolate({
                req(input$estimate.assay)
                
                if( input$estimate.name != "" ){
                    name <- input$estimate.name
                }else{
                    name <- "clusters"
                }
                
                if( input$cmethod == "Dmm" ){
                
                    blus_params <- list(k = input$kclusters,
                        type = deparse(input$dmm.type))#, seed = input$dmm.seed)
                
                }else if( input$cmethod == "Hclust" ){
                
                    blus_params <- list()
                
                }else if( input$cmethod == "Kmeans" ){
                
                    blus_params <- list(centers = input$kclusters)
                
                }else if( input$cmethod == "NNGraph" ){
                
                    blus_params <- list(shared = input$nn.shared,
                        k = input$kneighbours)
                
                }
                
                blus_params <- sprintf("%s=%s", names(blus_params), blus_params)
                blus_params <- paste(blus_params, collapse = ", ")
                blus_fun <- sprintf("%sParam(%s)", input$cmethod, blus_params)
                
                clust_args <- list(rObjects$tse,
                    assay.type = input$estimate.assay,
                    by = input$clust.margin, full = input$clust.full, 
                    BLUSPARAM = eval(parse(text = blus_fun)),
                    name = name, clust.col = name)
                
                rObjects$tse <- .update_tse(
                    rObjects, pObjects, "addCluster", clust_args)
            })
            
        }
        rObjects$tse
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom SummarizedExperiment assayNames
#' @importFrom TreeSummarizedExperiment rowTreeNames colTreeNames
#' @importFrom mia taxonomyRanks
#' @importFrom rintrojs introjs
.update_observers <- function(input, session, rObjects){
    
    # nocov start
    observe({
      
        if( isS4(rObjects$tse) ){
        
            updateSelectInput(session, inputId = "subassay",
                choices = assayNames(rObjects$tse))
            
            updateSelectInput(session, inputId = "taxrank",
                choices = rev(taxonomyRanks(rObjects$tse)))
            
            updateSelectInput(session, inputId = "assay.type",
                choices = assayNames(rObjects$tse))
            
            updateSelectInput(session, inputId = "trans.tree",
                choices = switch(input$margin,
                features = rowTreeNames(rObjects$tse),
                samples = colTreeNames(rObjects$tse)))
            
            updateSelectInput(session, inputId = "estimate.assay",
                choices = assayNames(rObjects$tse))
            
            updateNumericInput(session, inputId = "ncomponents",
                max = nrow(rObjects$tse) - 1)
        }
    })
    # nocov end
    invisible(NULL)
}

#' @importFrom shiny stopApp
#' @importFrom shinyAce aceEditor
.create_general_observers <- function(input, session, rObjects, pObjects){
    
    observeEvent(input$launch, {
        rObjects$appMode <- "visualisation"
    }, ignoreInit = TRUE)
    
    observeEvent(input[["iSEE_INTERNAL_export_content"]], {
        req(rObjects$appMode == "analysis")
        
        .print_message(title = "Export dataset", size = "l",
            
            radioButtons(inputId = "export_format", label = "Format:",
               inline = TRUE, choices = .exportFormats),
            
            conditionalPanel(condition = "input.export_format != 'TreeSE'",
                 
                selectInput(inputId = "export.assay", label = "Assay:",
                    choices = NULL)),
            
            downloadButton(outputId = "download", label = "Download",
                class = "btn-primary"))
           
        updateSelectInput(session, inputId = "export.assay",
            choices = assayNames(rObjects$tse))
            
    }, ignoreInit = TRUE)
    
    observeEvent(input[["iSEE_INTERNAL_tracked_code"]], {
        req(rObjects$appMode == "analysis")
        
        #all_cmds <- .track_it_all(pObjects, se_name, ecm_name, mod_commands)
        commands <- paste(pObjects$commands, collapse = "\n\n")
        
        .print_message(title = "Generate reproducible R script", size = "l",
                
                "The script below reproduces your miaDash analysis. You can ",
                "use it to continue the analysis programmatically, to share a ",
                "reproducible script with your collaborators, or just to ",
                "learn code-based microbiome analysis.", HTML("<br/><br/>"),
                
                aceEditor("iSEE_INTERNAL_tracked_code", mode = "r",
                    theme = "chrome", readOnly = TRUE, value = commands,
                    height = "600px", wordWrap = TRUE))
    
    }, ignoreInit = TRUE)
    
    observeEvent(input[["iSEE_INTERNAL_tour_steps"]], {
        req(rObjects$appMode == "analysis")
        introjs(session, options = list(steps = .landing_page_tour))
    }, ignoreInit = TRUE)
    
    observeEvent(input[["iSEE_INTERNAL_open_vignette"]], {
        req(rObjects$appMode == "analysis")
        browseURL("https://microbiome.github.io/miaDash/articles/miaDash.html")
    }, ignoreInit = TRUE)
    
    observeEvent(input[["iSEE_INTERNAL_session_info"]], {
        req(rObjects$appMode == "analysis")
        .print_message(title = "Session information", size = "l",
            pre(paste(capture.output(sessionInfo()), collapse = "\n")))
    }, ignoreInit = TRUE)
    
    observeEvent(input[["iSEE_INTERNAL_citation_info"]], {
        req(rObjects$appMode == "analysis")
        .print_welcome_message()
    }, ignoreInit = TRUE)
    
    observeEvent(input[["iSEE_INTERNAL_metadata_info"]], {
        req(rObjects$appMode == "analysis" )
        browseURL(paste0("https://microbiome.github.io/mia/reference/",
            input$data, ".html"))
    }, ignoreInit = TRUE)
    
    observeEvent(input[["iSEE_INTERNAL_app_control"]], {
        req(rObjects$appMode == "analysis")
        stopApp(returnValue = invisible(NULL))
    }, ignoreInit = TRUE)
}

#' @rdname create_observers
.create_launch_observers <- function(FUN, input, session, rObjects) {
  
    # nocov start
    observeEvent(input$launch, {
        
        .launch_isee(FUN, input$panels, session, rObjects)
        
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
  
    invisible(NULL)
}

.print_welcome_message <- function(){
    .print_message(
        title = "Welcome to the Microbiome Analysis Dashboard! \U0001f9a0",
        tags$img(src = "www/mia_logo.png", height = "180px",
            style = paste("display: block; margin-left: auto;",
            "margin-right: auto; margin-bottom: 10px;")),
        "miaDash is actively maintained by the",
        tags$a(href = "https://datascience.utu.fi/",
        "Turku Data Science Group", target = "_blank", .noWS = "after"),
        ", so we are happy to receive feedback from you. Feature requests,",
        "bug reports and other comments can be submitted",
        tags$a(href = "https://github.com/microbiome/miaDash/issues",
        "here", target = "_blank", .noWS = "after"), HTML(".<br/><br/>"),
        "If you are new to this app, you can learn how to use it with",
        tags$a(href = "https://microbiome.github.io/miaDash/articles/miaDash.html",
        "this short tutorial", target = "_blank", .noWS = "after"),
        ". Technical support can be obtained on",
        tags$a(href = "https://app.gitter.im/#/room/#microbiome_miaverse:gitter.im",
        "our Gitter channel", target = "_blank", .noWS = "after"),
        HTML(".<br/><br/>"), "If you use this package, please cite it with ",
        "the following information:", HTML("<br/><br/>"),
        pre(paste(capture.output(citation("miaDash")), collapse = "\n"))
    )
}