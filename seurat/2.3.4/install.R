install.packages(c("devtools", "BiocManager"))
BiocManager::install(c("S4Vectors", "made4", "SummarizedExperiment", "SingleCellExperiment", "MAST", "DESeq2", "destiny", "multtest"))
devtools::install_github("mojaveazure/loomR", ref = "develop")
devtools::install_version("Seurat", version = "2.3.4", repos = "http://cran.r-project.org", dependencies = TRUE, upgrade = "never")