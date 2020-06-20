#' Run correlation between two variables and support group by a variable
#'
#' @param data a `data.frame` containing variables
#' @param split whether perform correlation grouped by a variable, default is 'FALSE'
#' @param split_var a `character`, the group variable
#' @param var1 a `character, the first variable in correlation
#' @param var2 a `character, the second variable in correlation
#' @param Cor_method method="pearson" is the default value. The alternatives to be passed to cor are "spearman" and "kendall"
#' @param adjust_method What adjustment for multiple tests should be used? ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#' @param sig_label whether add symbal of significance. P < 0.001,"***"; P < 0.01,"**"; P < 0.05,"*"; P >=0.05,""
#' @param verbose if `TRUE`, print extra info.

#' @import psych
#' @import purrr
#' @importFrom magrittr set_names
#'
#' @return a `data.frame`
#' @author Yi Xiong
#' @export

ezcor2var <- function(data= NULL,
                      split = FALSE,
                      split_var = NULL,
                      var1 = NULL,
                      var2 = NULL,
                      Cor_method = "pearson",
                      adjust_method = "none",
                      sig_label = TRUE,
                      verbose = TRUE){
  stopifnot(is.data.frame(data))
  ss <- data
  if (!var1 %in% colnames(ss)){stop("the first variable is unavailable in the dataset!")}
  if (!var2 %in% colnames(ss)){stop("the second variable is unavailable in the dataset!")}
  if (split == TRUE){
    if (!split_var %in% colnames(ss)){stop("split variable is unavailable in the dataset!")}
    #index
    n = which(colnames(ss) %in% split_var)
    sss <- with(ss,split(ss, ss[,n]))
    s <- names(sss)
    ##calculate correlation
    cor2var <- purrr::map(s, purrr::safely(function(x) {
      #x = s[1]
      sss_sub<- sss[[x]]
      dd <- psych::corr.test(as.numeric(sss_sub[,var1]),as.numeric(sss_sub[,var2]), method = Cor_method,adjust = adjust_method)
      #dd <- stats::cor.test(as.numeric(sss_can[,var1]),as.numeric(sss_can[,var2]), type = Cor_method)
      ddd <- data.frame(cor = dd$r, p.value = dd$p, method= Cor_method, adjust = adjust_method, v1 = var1, v2 = var2 ,stringsAsFactors = F)
      ddd$group <- x
      return(ddd)
    })) %>% magrittr::set_names(s)

    cor2var <- cor2var %>%
      purrr::map(~ .x$result) %>%
      purrr::compact()
    cor2var_df <- do.call(rbind.data.frame, cor2var)

    if (sig_label == TRUE) {
      cor2var_df$pstar <- ifelse(cor2var_df$p.value < 0.05,
                                 ifelse(cor2var_df$p.value < 0.001, "***", ifelse(cor2var_df$p.value < 0.01,"**","*")),
                                 "")
    }
    return(cor2var_df)
  }

  if (split == FALSE){
    sss <- ss
    dd <- psych::corr.test(as.numeric(sss[,var1]),as.numeric(sss[,var2]), method = Cor_method,adjust = adjust_method)
    ddd <- data.frame(cor = dd$r, p.value = dd$p, method= Cor_method, adjust = adjust_method, v1 = var1,  v2 = var2, stringsAsFactors = F)

    cor2var_df <- ddd

    if (sig_label == TRUE) {
      cor2var_df$pstar <- ifelse(cor2var_df$p.value < 0.05,
                                 ifelse(cor2var_df$p.value < 0.001, "***", ifelse(cor2var_df$p.value < 0.01,"**","*")),
                                 "")

    }
    return(cor2var_df)
  }
}




