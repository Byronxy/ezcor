# # ##https://github.com/Byronxy/ezcor
# ## options
# #options(future.globals.maxSize= 5*1024*1024^2)
#
# rm(list = ls())
#
# ## load data
# data("exprSet", package = "ezcor", envir = environment())
#
# ##
# g1 <- colnames(exprSet)[3]
# g2 <- colnames(exprSet)[4]
# ##--------------------------------test ezcor---------------------------------
# res <- ezcor::ezcor(data= exprSet,
#                       split = T,
#                       split_var = "tissue",
#                       var1 = g1,
#                       var2 = g2,
#                       cor_method = "spearman",
#                       adjust_method = "none",
#                       sig_label = TRUE,
#                       verbose = TRUE)
#
# res <- ezcor::ezcor(data= exprSet,
#                                     split = F,
#                                     split_var = "tissue",
#                                     var1 = g1,
#                                     var2 = g2,
#                                     cor_method = "spearman",
#                                     adjust_method = "none",
#                                     sig_label = TRUE,
#                                     verbose = TRUE)
#
#
# ##---------------------test ezcor_batch--------------------------------------------
# data("exprSet", package = "ezcor", envir = environment())
# target_variable = colnames(exprSet)[3]
# genelist <- colnames(exprSet)
# genelist <-  setdiff(genelist,c("patient","tissue",target_variable))
#
# ## split = T
#
# res <- ezcor::ezcor_batch(data = exprSet,
#                                          var1 = target_variable,
#                                          var2 = genelist,
#                                          split = T,
#                                          split_var = "tissue",
#                                          cor_method = "pearson",
#                                          adjust_method = "none",
#                                          use = "complete",
#                                          sig_label = TRUE,
#                                          parallel = F,
#                                          verbose = T)
# ## split = F
# res <- ezcor::ezcor_batch(data = exprSet,
#                                          var1 = target_variable,
#                                          var2 = genelist,
#                                          split = F,
#                                          split_var = "tissue",
#                                          cor_method = "pearson",
#                                          adjust_method = "none",
#                                          sig_label = TRUE,
#                                          parallel = F,
#                                          verbose = T)
# ##-------------------partial correlation----------------------------------
# data("exprSet", package = "ezcor", envir = environment())
# g1 <- colnames(exprSet)[3]
# g2 <- colnames(exprSet)[4]
# g3 <- colnames(exprSet)[c(5,6)]
# res <- ezcor::ezcor_partial_cor(data = exprSet,
#                                             var1 = g1,
#                                             var2 = g2,
#                                             var3 = g3,
#                                          split = T,
#                                          split_var = "tissue",
#                                          cor_method = "pearson",
#                                          sig_label = TRUE)
#
#
#
# ##---------------------bicor-----------------------------------------------
# g1 <- colnames(exprSet)[3]
# g2 <- colnames(exprSet)[4]
# genelist <- colnames(exprSet)
# genelist <-  setdiff(genelist,c("patient","tissue",g1))
# res <- ezcor::ezcor_bicor(data = exprSet,g1,g2)
# res <- ezcor::ezcor_bicor_batch(data = exprSet,g1,genelist)
