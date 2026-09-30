#' miaDash utilities
#' 
#' Helper functions to support the app functionality.
#' 
#' @param selection \code{Numeric vector}. A list of indices for the mia
#'   datasets to return.
#' 
#' @param tse a
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' object.
#' 
#' @param fun \code{Function scalar}. Function to apply to \code{tse}.
#' 
#' @param fun.args \code{Named list}. A list of arguments to pass to \code{fun}.
#' 
#' @param title \code{Character scalar}. The title of the error message to
#'   print. (Default: \code{"Invalid input:"})
#' 
#' @param item \code{Character scalar}. The file path from which the object
#'   should be loaded.
#' 
#' @param loader \code{Function scalar}. The function to load \code{item}.
#'   (Default: \code{NULL})
#' 
#' @param alternative an alternative output to return when at least one
#'   \code{item} and \code{loader} are not defined. (Default: \code{NULL})
#' 
#' @param form \code{Character scalar} The formula to be checked.
#' 
#' @param ... Either a series of strings to form the message returned by
#'   \code{.print_message} or named arguments for \code{loader}.
#'
#' @return
#' \itemize{
#' \item \code{.import_datasets}: returns the list of available mia datasets.
#' \item \code{.update_tse}: returns a TreeSE object after applying \code{fun}
#'   with arguments \code{fun.args}. Eventual messages and errors are also
#'   printed.
#' \item \code{.print_message}: returns a modalDialog with the error message
#'   specified with \code{...} and titled \code{title}.
#' \item \code{.set_optarg}: returns an optional element for a TreeSE
#'   constructor. The output is either an object located at the file path
#'   \code{item} and loaded with \code{loader} or \code{alternative}.
#' \item \code{.check_formula}: returns \code{TRUE} or \code{FALSE} depending
#'   whether or not all variables included in \code{form} are present in
#'   \code{se} colData.
#' }
#'
#' @keywords internal
#' @name utils
NULL

#' @rdname utils
.import_datasets <- function() {
    
    mia_datasets <- data(package = "mia")
    mia_datasets <- mia_datasets$results[ , "Item"]
    data(list = mia_datasets, package = "mia")
    
    classes <- vapply(mia_datasets, function(x) class(get(x)), character(1L))
    mia_datasets <- mia_datasets[classes == "TreeSummarizedExperiment"]
    
    return(mia_datasets)
}

#' @rdname utils
.update_tse <- function(rObjects, pObjects, fun, fun.args = list()) {
    
    fun_name <- as.name(fun)
    
    fun_call <- fun.args |>
        append(fun_name, after = 0L) |>
        as.call()
    
    tse <- tryCatch({withCallingHandlers({
        
        tse <- eval(fun_call)
        
        fun_call[[2L]] <- as.name("tse")
        fun_call <- call("<-", as.name("tse"), fun_call)
        pObjects$commands <- c(pObjects$commands, fun_call)
        
        return(tse)
        
        # nocov start
        }, message = function(m) {
        
            showNotification(conditionMessage(m), type = "message")
            invokeRestart("muffleMessage")
        
        }, warning = function(w) {
            
            showNotification(conditionMessage(w), type = "warning")
            invokeRestart("muffleWarning")

        })}, error = function(e) {
            # Remove eventual calls to prevent breaking the session
            class(e) <- setdiff(class(e), "call")
            attr(e, "call") <- NULL
            
            .print_message(paste(e, collapse = " "))
            return(rObjects$tse)
        })
        # nocov end
    
    return(tse)
}

#' @rdname utils
.print_message <- function(..., title = "Invalid input:") {

    # nocov start
    showModal(modalDialog(
        title = title, ...,
        easyClose = TRUE, footer = NULL
    ))
    # nocov end
}

#' @rdname utils
.set_optarg <- function(item, loader = NULL, alternative = NULL, ...){
    
    if( !(is.null(item) || is.null(loader)) ){
        out <- loader(item, ...)
    } else {
        out <- alternative
    }
    
    return(out)
}

#' @rdname utils
#' @importFrom SummarizedExperiment colData
.check_formula <- function(form, tse){
    
    form <- gsub("data ~\\s*", "", form)
    vars <- unlist(strsplit(form, "\\s*[\\+|\\*]\\s*"))
    
    cond <- all(vars %in% names(colData(tse)))
    return(cond)
}

#' @rdname utils
#' @importFrom mia importBIOM
#' @importFrom SummarizedExperiment colData
#' @importFrom TreeSummarizedExperiment rowTree
#' @importFrom S4Vectors DataFrame
#' @importFrom ape read.tree
#' @importFrom utils read.table
.importBIOM <- function(file, col.data = NULL, tree.file = NULL, ...){
  
    tse <- importBIOM(file, ...)
  
    if( !is.null(col.data) ){
        coldata <- read.table(file = col.data, header = TRUE, sep = "\t")
        rownames(coldata) <- colnames(tse)
        colData(tse) <- DataFrame(coldata)
    }

    if( !is.null(tree.file) ){
        rowTree(tse) <- read.tree(tree.file)
    }
  
    return(tse)
}

#' @rdname utils
#' @importFrom SummarizedExperiment rowData
#' @importFrom S4Vectors DataFrame
#' @importFrom mia getTaxonomyLabels
.rownames2taxa <- function(x){

    tax_df <- data.frame(Taxonomy = rownames(x))
      
    rowdata <- mia:::.parse_taxonomy(
        tax_df,
        col.name = "Taxonomy",
        removeTaxaPrefixes = TRUE
    )

    rowData(x) <- DataFrame(rowdata)
    rownames(x) <- getTaxonomyLabels(x, make.unique = TRUE)
    
    return(x)
}
