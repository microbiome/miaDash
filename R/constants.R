#' Constants
#'
#' Constant values used throughout the miaDash app.
#' 
#' @section Panel layout:
#' \describe{
#' \item{\code{.miaDashDefaultPanels}}{List of panel names in the default layout of miaDash.}
#' \item{\code{.miaDashOtherPanels}}{List of panel names not in the default layout of miaDash.}
#' \item{\code{.rdsFormats}}{List of readable RDS object formats.}
#' \item{\code{.foreignFormats}}{List of readable foreign object formats.}
#' \item{\code{.exportFormats}}{List of exportable object formats.}
#' \item{\code{.transformMethods}}{List of transformations applicable to assays.}
#' \item{\code{.alphaMetrics}}{List of alpha diversity metrics.}
#' \item{\code{.betaMetrics}}{List of beta diversity metrics.}
#' \item{\code{.betaMethods}}{List of beta diversity methods.}
#' \item{\code{.qualityMetrics}}{List of metrics for quality control.}
#' }
#'
#' @author Giulio Benedetti
#' 
#' @keywords internal
#' @name constants
#' @aliases .miaDashDefaultPanels .miaDashOtherPanels .rdsFormats
#' .foreignFormats .exportFormats .transformMethods .qualityMetrics
#' .alphaMetrics .betaMetrics .betaMethods .clustMethods .DmmCriteria
NULL

#' @rdname constants
.miaDashDefaultPanels <- c("RowDataTable", "ColumnDataTable", "RowTreePlot",
    "AbundancePlot", "AbundanceDensityPlot", "ReducedDimensionPlot",
    "ComplexHeatmapPlot")

#' @rdname constants
.miaDashOtherPanels <- c("PrevalencePlot", "RDAPlot", "ScreePlot",
    "LoadingPlot", "ColumnTreePlot", "RowGraphPlot", "ColumnGraphPlot",
    "RowDataPlot", "ColumnDataPlot")

#' @rdname constants
.rdsFormats <- c("TreeSE", "phyloseq", "BIOM", "DADA2")

#' @rdname constants
.foreignFormats <- c("BIOM", "HUMAnN", "MetaPhlAn", "Mothur", "QIIME2")

#' @rdname constants
.exportFormats <- c("TreeSE", "phyloseq", "BIOM", "QIIME2", "Mothur", "Raw")

#' @rdname constants
.transformMethods <- c("alr", "chi.square", "clr", "css", "frequency",
    "hellinger", "log", "log10", "log2", "max", "normalize", "pa", "philr",
    "range", "rank", "rclr", "relabundance", "rrank", "standardize", "total")

#' @rdname constants
.qualityMetrics <- list("Library size" = "PerCellQC",
    "Prevalence" = "Prevalence", "Prevalent abundance" = "PrevalentAbundance",
    "Hierarchy tree" = "HierarchyTree")

#' @rdname constants
.alphaMetrics <- c("coverage_diversity", "fisher_diversity", "faith_diversity",
    "gini_simpson_diversity", "inverse_simpson_diversity",
    "log_modulo_skewness_diversity", "shannon_diversity", "absolute_dominance",
    "dbp_dominance", "core_abundance_dominance", "gini_dominance",
    "dmn_dominance", "relative_dominance", "simpson_lambda_dominance",
    "camargo_evenness", "pielou_evenness", "simpson_evenness", "evar_evenness",
    "bulla_evenness", "ace_richness", "chao1_richness", "hill_richness",
    "observed_richness")

#' @rdname constants
.betaMetrics <- c("euclidean", "bray", "jaccard", "unifrac")

#' @rdname constants
.betaMethods <- c("MDS", "NMDS", "PCA", "RDA")
#"TSNE", "UMAP")

#' @rdname constants
.clustMethods <- c("Dmm", "Hclust", "Kmeans", "NNGraph")

#' @rdname constants
.DmmCriteria <- c("laplace", "AIC", "BIC")
