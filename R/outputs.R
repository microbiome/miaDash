#' Outputs
#'
#' \code{.render_overview} and \code{.render_download} create the output of the
#'   UI, which consists of the overview of the TreeSE object and the download
#'   object, respectively.
#'
#' @param output The Shiny output object from the server function.
#'
#' @return Adds a rendered item to \code{output}.
#'   A \code{NULL} value is invisibly returned.
#'
#' @name render_output
#' @keywords internal

#' @rdname render_output
.render_overview <- function(output, rObjects) {
  
    # nocov start
    output$object <- renderPrint({
        rObjects$tse
    })
    # nocov end

    invisible(NULL)
}

#' @rdname render_output
#' @importFrom mia convertToPhyloseq exportRaw exportQIIME2 exportMothur
.render_download <- function(input, output, rObjects){
    # nocov start
    output$download <- downloadHandler(
        filename = function(){
            suffix <- switch(input$export_format,
                TreeSE = "rds", phyloseq = "rds", BIOM = "biom",
                Mothur = "zip", QIIME2 = "zip", Raw = "zip")
            
            paste0(tolower(input$export_format), "-", Sys.Date(), ".", suffix)
        },
        content = function(file) switch(
            input$export_format,
            TreeSE = saveRDS(rObjects$tse, file),
            phyloseq = {
                pseq <- convertToPhyloseq(rObjects$tse, assay.type = input$export.assay)
                saveRDS(pseq, file)
            },
            BIOM = .write_biom(rObjects$tse, file, input$export.assay),
            .write_foreign(file, rObjects$tse, input$export.format, input$export.assay)
        )
    )
    # nocov end
    invisible(NULL)
}

#' @importFrom SummarizedExperiment assay assay<-
#' @importFrom rbiom as_rbiom write_biom
.write_biom <- function(tse, file, assay.type){
    # Select main assay
    assay(tse) <- assay(tse, assay.type = assay.type)
    # Convert tse to biom
    biom <- as_rbiom(tse)
    # Write biom to file
    write_biom(biom, file)
    return(invisible)
}

#' @importFrom utils zip
.write_foreign <- function(file, tse, as, assay.type){
    # Select exporter by format
    FUN <- switch(
        as, Raw = exportRaw, QIIME2 = exportQIIME2, Mothur = exportMothur
    )
    # Write object to directory
    dpath <- tempfile()
    FUN(rObjects$tse, dpath, assay.type)
    # Compress directory
    zip(file, files = list.files(dpath, full.names = TRUE), flags = "-r9Xj")
    return(invisible)
}
