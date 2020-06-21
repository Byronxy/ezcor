#' Run correlation between two variables in a batch mode and support group by a variable
#'
#' @param data a `data.frame` containing variables
#' @param split whether perform correlation grouped by a variable, default is 'FALSE'
#' @param split_var a `character`, the group variable
#' @param var1 a `character`, the first variable in correlation
#' @param var2 a `vector` containing variables in a batch mode
#' @param Cor_method method="pearson" is the default value. The alternatives to be passed to cor are "spearman" and "kendall"
#' @param adjust_method What adjustment for multiple tests should be used? ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#' @param sig_label whether add symbal of significance. P < 0.001,"***"; P < 0.01,"\\**"; P < 0.05,"*"; P >=0.05,""
#' @param parallel if `TRUE`, do parallel computation by **furrr** package.
#' @param verbose if `TRUE`, print extra info. If `parallel` is `TRUE`,
#' set `verbose` to `FALSE` may speed up.
#'
#' @import dplyr
#' @import psych
#' @import purrr
#' @importFrom purrr set_names
#'
#' @return a `data.frame`
#' @author Yi Xiong, Shixiang Wang
#' @export
#'
#'

ezcor_parallel <- function(data,
                           var1,
                           var2,
                           split = FALSE,
                           split_var = NULL,
                           Cor_method = "pearson",
                           adjust_method = "none",
                           sig_label = TRUE,
                           parallel = FALSE,
                           verbose = FALSE){

  stopifnot(is.data.frame(data))

  ss <- data

  if (!var1 %in% colnames(ss)){stop("the first variable is unavailable in the dataset!")}
  if (!var2 %in% colnames(ss)){stop("the second variable is unavailable in the dataset!")}
  if (length(var1) != 1){stop("only one element is needed in the var1 variable!")}
  if (split == TRUE){
    if (!split_var %in% colnames(ss)){stop("split variable is unavailable in the dataset!")}
    all_cols <- unique(c(var1, var2, split_var))
    #ss <- data
    ss <- ss[, all_cols]
    if (parallel) {
      if (!requireNamespace("furrr")) {
        stop("Please install 'furrr' package firstly!")
      }
      if (length(var2) < 200) {
        if (verbose) message("Warning: variable < 200, parallel computation is not recommended!")
      }

      if (!requireNamespace("furrr")) {
        stop("Please install 'furrr' package firstly!")
      }
      oplan <- future::plan()
      future::plan("multiprocess")
      on.exit(future::plan(oplan), add = TRUE)
      res <- furrr::future_map(var2,
                               ezcor_caller,
                               data = ss,
                               split_var = split_var,
                               split = split,
                               var1 = var1,
                               Cor_method = Cor_method,
                               adjust_method = adjust_method,
                               sig_label = sig_label,
                               verbose = verbose)
    } else {
      all_cols <- unique(c(var1, var2))
      #ss <- data
      ss <- ss[, all_cols]
      res <- purrr::map(var2,
                        ezcor_caller,
                        data = ss,
                        split_var = split_var,
                        split = split,
                        var1 = var1,
                        Cor_method = Cor_method,
                        adjust_method = adjust_method,
                        sig_label = sig_label,
                        verbose = verbose) %>% purrr::set_names(var2)
    }
    res2 <- dplyr::bind_rows(res)
    return(res2)
  }
  else{
    res <- purrr::map(var2,
                      ezcor_caller,
                      data = ss,
                      split_var = split_var,
                      split = split,
                      var1 = var1,
                      Cor_method = Cor_method,
                      adjust_method = adjust_method,
                      sig_label = sig_label,
                      verbose = verbose) %>% purrr::set_names(var2)
    res2 <- dplyr::bind_rows(res)
    return(res2)
  }

  }

ezcor_caller <- function(var2,
                         data,
                         split_var = split_var,
                         split = split,
                         var1 = var1,
                         Cor_method = Cor_method,
                         adjust_method = adjust_method,
                         sig_label = sig_label,
                         verbose = TRUE){
  ezcor2var(
    data = data,
    split_var = split_var,
    split = split,
    var1 = var1,
    var2 = var2,
    Cor_method = Cor_method,
    adjust_method = adjust_method,
    sig_label = sig_label,
    verbose = verbose
  )
}
