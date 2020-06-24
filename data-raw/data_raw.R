#' We select HALLMARK_HYPOXIA pathway genes from TCGA pan-cancer dataset to illustrate examples
exprSet <- readRDS(file = "G:/TCGA_genomedata/PANCAN/Toil recompute data/TCGA_toil_pancan_tumor_expr_genecor.Rds")
require(msigdbr)
require(dplyr)
m_df = msigdbr(species = "Homo sapiens", category = "H")
m_df = m_df %>%
  filter(gs_name == "HALLMARK_HYPOXIA")
s <- c("patient","tissue",m_df$human_gene_symbol)
exprSet = exprSet[, colnames(exprSet) %in% s]
usethis::use_data(exprSet, overwrite = TRUE)

