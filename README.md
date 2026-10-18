# miaDash <img src="man/figures/mia_logo.png" align="right" width="120" />

[![issues](https://img.shields.io/github/issues/microbiome/miaDash)](https://github.com/microbiome/miaDash/issues)
[![pulls](https://img.shields.io/github/issues-pr/microbiome/miaDash)](https://github.com/microbiome/miaDash/pulls)
[![R-CMD-check](https://github.com/microbiome/miaDash/workflows/build/badge.svg)](https://github.com/microbiome/miaDash/actions)
[![codecov](https://codecov.io/gh/microbiome/miaDash/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/microbiome/miaDash?branch=devel)
[![codefactor](https://www.codefactor.io/repository/github/microbiome/miadash/badge)](https://www.codefactor.io/repository/github/microbiome/miadash)

The goal of miaDash is to provide a user-friendly interface to import,
manipulate, analyse and visualise TreeSummarizedExperiment objects.

## Usage

miaDash is available online at [this address](https://miadash-microbiome.2.rahtiapp.fi/).
While suitable for small and medium datasets, the online version may slow down
when very large datasets are analysed. In this case, the app can be installed
and run locally. Either way, functionality to subset and agglomerate the data is
also provided in the app.

## Installation instructions

The release version can be installed from Bioconductor as follows:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("miaDash")
```

Contributors or users interested in the latest functionality can install the
devel version as follows:

```
remotes::install_github("microbiome/miaDash")
```

## Example

The basic functionality of miaDash can be explored as follows:

```
# Import miaDash
library(miaDash)

# Launch miaDash
if (interactive()) {
  miaDash()
}
```

## Code of Conduct

Please note that the miaDash project is released with a
[Contributor Code of Conduct](https://bioconductor.org/about/code-of-conduct/).
By contributing to this project, you agree to abide by its terms. Contributions
are welcome in the form of feedback, issues and pull requests. You can find the
contributor guidelines of the miaverse
[here](https://github.com/microbiome/mia/blob/devel/CONTRIBUTING.md).

## Acknowledgements

miaDash results from the joint effort of the larger R/Bioconductor community. In
particular, this software mainly depends on the following packages:

- [_mia_](https://bioconductor.org/packages/release/bioc/html/mia.html)
- [_iSEEtree_](https://bioconductor.org/packages/devel/bioc/html/iSEEtree.html)
- [_iSEE_](https://www.bioconductor.org/packages/release/bioc/html/iSEE.html)
- [TreeSummarizedExperiment](https://www.bioconductor.org/packages/release/bioc/html/TreeSummarizedExperiment.html)
- [_shiny_](https://cran.r-project.org/web/packages/shiny/)
