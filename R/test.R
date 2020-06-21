# ##https://github.com/Byronxy/ezcor
# options(future.globals.maxSize= 5*1024*1024^2)
# exprSet <- readRDS(file = "G:/TCGA_genomedata/PANCAN/Toil recompute data/TCGA_toil_pancan_tumor_expr_genecor.Rds")
#
#
# res <- ezcor::ezcor2var(data= exprSet,
#                       split = T,
#                       split_var = "tissue",
#                       var1 = "TMED3",
#                       var2 = "HIF1A",
#                       Cor_method = "pearson",
#                       adjust_method = "none",
#                       sig_label = TRUE,
#                       verbose = TRUE)
#
# target_variable <- c("NT5E")
# genelist <- colnames(exprSet)
# genelist <-  setdiff(genelist,c("patient","tissue",target_variable))
#
# genelist1 =genelist[1:1000]
# system.time(res <- ezcor::ezcor_parallel(data = exprSet,
#                                          target_variable = target_variable,
#                                          covariates = genelist1,
#                                          split = T,
#                                          split_var = "tissue",
#                                          batch_size = 100,
#                                          Cor_method = "pearson",
#                                          adjust_method = "none",
#                                          sig_label = TRUE,
#                                          parallel = T,
#                                          verbose = T))
#
# system.time(res <- ezcor::ezcor_parallel(data = exprSet,
#                                          target_variable = target_variable,
#                                          covariates = genelist1,
#                                          split = F,
#                                          split_var = "tissue",
#                                          batch_size = 100,
#                                          Cor_method = "pearson",
#                                          adjust_method = "none",
#                                          sig_label = TRUE,
#                                          parallel = T,
#                                          verbose = T))
#
# system.time(res <- ezcor::ezcor_partial_cor(data = exprSet,
#                                             var1 = "TMED3",
#                                             var2 = "HIF1A",
#                                             var3 = c("TP53","NT5E","CD28"),
#                                          split = F,
#                                          split_var = "tissue",
#                                          Cor_method = "pearson",
#                                          sig_label = TRUE))
# res[1,]$z = var3
# var3 = paste(var3,collapse = ",")
