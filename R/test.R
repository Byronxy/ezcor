# ##https://github.com/Byronxy/ezcor
## options
#options(future.globals.maxSize= 5*1024*1024^2)

## load data
# exprSet <- readRDS(file = "G:/TCGA_genomedata/PANCAN/Toil recompute data/TCGA_toil_pancan_tumor_expr_genecor.Rds")

#
## test ezcor
# system.time(res <- ezcor::ezcor(data= exprSet,
#                       split = T,
#                       split_var = "tissue",
#                       var1 = "TMED3",
#                       var2 = "HIF1A",
#                       Cor_method = "spearman",
#                       adjust_method = "none",
#                       sig_label = TRUE,
#                       verbose = TRUE))

# system.time(res <- ezcor::ezcor(data= exprSet,
#                                     split = F,
#                                     split_var = "tissue",
#                                     var1 = "TMED3",
#                                     var2 = "HIF1A",
#                                     Cor_method = "spearman",
#                                     adjust_method = "none",
#                                     sig_label = TRUE,
#                                     verbose = TRUE))


## test ezcor_batch
# target_variable <- c("NT5E")
# genelist <- colnames(exprSet)
# genelist <-  setdiff(genelist,c("patient","tissue",target_variable))
#
# genelist1 =genelist[1:100]
# ## split = T
#
# system.time(res <- ezcor::ezcor_batch(data = exprSet,
#                                          var1 = target_variable,
#                                          var2 = genelist1,
#                                          split = T,
#                                          split_var = "tissue",
#                                          Cor_method = "pearson",
#                                          adjust_method = "none",
#                                          use = "complete",
#                                          sig_label = TRUE,
#                                          parallel = F,
#                                          verbose = T))
## split = F
# system.time(res <- ezcor::ezcor_batch(data = exprSet,
#                                          var1 = target_variable,
#                                          var2 = genelist1,
#                                          split = F,
#                                          split_var = "tissue",
#                                          Cor_method = "pearson",
#                                          adjust_method = "none",
#                                          sig_label = TRUE,
#                                          parallel = F,
#                                          verbose = T))
# ##partial correlation
# system.time(res <- ezcor::ezcor_partial_cor(data = exprSet,
#                                             var1 = "TMED3",
#                                             var2 = "HIF1A",
#                                             var3 = c("TP53","NT5E","CD28"),
#                                          split = T,
#                                          split_var = "tissue",
#                                          Cor_method = "pearson",
#                                          sig_label = TRUE))

# system.time(res <- ezcor::ezcor_partial_cor_batch(data = exprSet,
#                                             var1 = "TMED3",
#                                             var2 = genelist1,
#                                             var3 = c("TP53","NT5E","CD28"),
#                                             split = T,
#                                             split_var = "tissue",
#                                             Cor_method = "pearson",
#                                             sig_label = TRUE))


# ##bicor
# res <- WGCNA::bicor(x = exprSet[, "TMED3"], y = exprSet[, "HIF1A"])
# res <- ezcor::ezcor_bicor(data = exprSet,"TMED3","HIF1A")
# res <- ezcor::ezcor_bicor_batch(data = exprSet,"TMED3",genelist1)
