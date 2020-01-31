library(Seurat)
library(R.utils)
library(Matrix)
library(hash)

seed <- 0
setOption('mc.cores', 8)

adata <- Read10X_h5('./inputs/MantonBM_nonmix_10x.h5')

n.tags <- adata@Dim[2]
subset.idx.hashmap <- hash()

##Split raw matrix into matrices due to channel.
for (j in 1:n.tags) {
    name <- adata@Dimnames[[2]][j]
    chan <- strsplit(name, '-')[[1]][1]
    if (has.key(chan, subset.idx.hashmap)) {
        subset.idx.hashmap[[chan]] <- append(subset.idx.hashmap[[chan]], j)
    } else {
        subset.idx.hashmap[[chan]] <- c(j)
    }
}

dsets <- list()
for (k in keys(subset.idx.hashmap)) {
    print(paste("Processing", k))
    dsets[[k]] <- adata[, subset.idx.hashmap[[k]]]
}

## Create Seurat objects
seurat.obj.list <- list()
hvg.common <- c()
for (i in 1:length(dsets)) {
    name <- names(dsets)[i]
    print(paste(i, ". Processing", name))
    ica <- CreateSeuratObject(raw.data = dsets[[i]], project = name, min.cells = 138)
    mito.features <- grep(pattern = "^MT-", rownames(ica@data), value = TRUE)
    percent.mito <- Matrix::colSums(ica@raw.data[mito.features, ]) / Matrix::colSums(ica@raw.data)
    ica <- AddMetaData(object = ica, metadata = percent.mito, col.name = "percent.mito")
    ica <- FilterCells(object = ica, subset.names = c("nGene", "percent.mito"), low.threshold = c(499, -Inf), high.threshold = c(6000, 0.1))
    ica <- NormalizeData(ica, normalization.method = "LogNormalize", scale.factor = 1e5)
    ica <- ScaleData(ica)
    ica <- FindVariableGenes(ica, mean.function = ExpMean, dispersion.function = LogVMR, x.low.cutoff = 0.0125, x.high.cutoff = 7, y.cutoff = 0.5, do.plot = FALSE)
    hvg.ica <- rownames(x = ica@hvg.info)
    if (length(hvg.common) == 0) {
        hvg.common <- hvg.ica
    } else {
        hvg.common <- intersect(hvg.common, hvg.ica)
    }
    seurat.obj.list[[name]] <- ica
}
save(seurat.obj.list, hvg.common, file = 'seurat_obj_list.RData')
print("Seurat object list saved!")


## Integration using MultiCCA
##ica.cca <- RunMultiCCA(object.list = seurat.obj.list, genes.use = hvg.common, num.ccs = 20)

##save(ica.cca, file = 'ica_cca.RData')
##print("CCA result saved!")
