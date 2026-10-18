#' miaDash
#'
#' miaDash is a web app that provides an interface to build and explore
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' (TreeSE) objects by means of \link[iSEE:iSEE]{iSEE}.
#'
#' @return An \code{\link[iSEE:iSEE]{iSEE}} app with a custom landing page to
#'   build TreeSE objects and explore \link[mia:mia-datasets]{mia datasets}.
#'
#' @examples
#' app <- miaDash()
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#' 
#' @seealso \link[iSEE:iSEE]{iSEE} \link[mia:mia]{mia}
#'   \link[miaViz:miaViz]{miaViz}
#'
#' @name miaDash

#' @export
#' @rdname miaDash
#' @importFrom iSEE iSEE
#' @importFrom utils packageVersion
#' @importFrom htmltools tags
miaDash <- function() {
    
    addResourcePath("www", system.file("man/figures", package = "miaDash"))
    
    iSEE(landingPage = .landing_page, tabTitle = "miaDash", appTitle = tags$div(
        paste0("Microbiome Analysis Dashboard - v", packageVersion("miaDash")),
        tags$img(src = "www/mia_logo.png", height = "40px", style = "margin-left: 10px"),
        style = "cursor: pointer; font-weight: 500",
        onclick = "window.location='https://miadash-microbiome.2.rahtiapp.fi/'"))
}

#' @importFrom shinyjs enable
#' @importFrom iSEEtree .check_all_panels RowTreePlot RowTreePlot AbundancePlot
#'   AbundanceDensityPlot RDAPlot ScreePlot LoadingPlot ColumnTreePlot
#'   RowGraphPlot ColumnGraphPlot PrevalencePlot
.launch_isee <- function(FUN, initial, session, rObjects) {
    
    # nocov start
    tse <- rObjects$tse
    
    initial <- lapply(initial, function(x) eval(parse(text = paste0(x, "()"))))
    initial <- .check_all_panels(tse, initial)
    FUN(SE = tse, INIT = initial)
    
    enable("iSEE_INTERNAL_organize_panels")  # organize panels
    enable("iSEE_INTERNAL_link_graph")       # link graph
    enable("iSEE_INTERNAL_panel_settings")   # panel settings
    enable("iSEE_INTERNAL_draft_tour")       # tour draft
    
    invisible(NULL)
    # nocov end
}