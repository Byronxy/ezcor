#' Run partial correlation
#'
#' @inheritParams ppcor::pcor.test
#' @seealso [ppcor::pcor.test()] which this function wraps.
#' @param data a `data.frame` containing variables
#' @param split whether perform correlation grouped by a variable, default is 'FALSE'
#' @param split_var a `character`, the group variable
#' @param var1 a `character`, the first variable in correlation
#' @param var2 a `character`, the second variable in correlation
#' @param var3 a `character` or `character vector`, the third variable in correlation
#' @param Cor_method method="pearson" is the default value. The alternatives to be passed to cor are "spearman" and "kendall"
#' @param sig_label whether add symbal of significance. P < 0.001,"***"; P < 0.01,"**"; P < 0.05,"*"; P >=0.05,""
#' @import purrr
#' @export
#' @return a `data.frame`
#' @author Yi Xiong
ezcor_partial_cor <- function(data = NULL,
                              split = FALSE,
                              split_var = NULL,
                              var1 = NULL,
                              var2 = NULL,
                              var3 = NULL,
                              Cor_method = "pearson",
                              sig_label = TRUE,
                              ...) {
  if (!requireNamespace("ppcor")) {
    stop("Please install 'ppcor' package firstly!")
  }
  stopifnot(is.data.frame(data))
  ss <- data
  #var1 = x; var2 = y; var3 = z
  if (!var1 %in% colnames(ss)) {
    stop("the first variable is unavailable in the dataset!")
  }
  if (!var2 %in% colnames(ss)) {
    stop("the second variable is unavailable in the dataset!")
  }
  if (length(var1) != 1) {
    stop("only one element is needed in the first variable!")
  }
  if (length(var2) != 1) {
    stop("only one element is needed in the second variable!")
  }
  if (split == TRUE) {
    if (!split_var %in% colnames(ss)) {
      stop("split variable is unavailable in the dataset!")
    }
    #index
    n = which(colnames(ss) %in% split_var)
    sss <- with(ss, split(ss, ss[, n]))
    s <- names(sss)
    ##calculate correlation
    cor2var <- purrr::map(s, purrr::safely(function(x) {
      #x = s[1]
      sss_sub <- sss[[x]]
      if (length(var3) > 1) {
        dd <-
          ppcor::pcor.test(
            as.numeric(sss_sub[, var1]),
            as.numeric(sss_sub[, var2]),
            as.matrix(sss_sub[, var3]),
            method = Cor_method,
            ...
          )
        ddd <-
          data.frame(
            cor_partial = dd$estimate,
            p.value = dd$p.value,
            method = Cor_method,
            x = var1,
            y = var2,
            z = paste(var3, collapse = ",") ,
            stringsAsFactors = F
          )
        ddd$group <- x
        return(ddd)
      } else{
        dd <-
          ppcor::pcor.test(as.numeric(sss_sub[, var1]),
                           as.numeric(sss_sub[, var2]),
                           as.numeric(sss_sub[, var3]),
                           method = Cor_method....)
        ddd <-
          data.frame(
            cor_partial = dd$estimate,
            p.value = dd$p.value,
            method = Cor_method,
            x = var1,
            y = var2,
            z = var3,
            stringsAsFactors = F
          )
        ddd$group <- x
        return(ddd)
      }

    })) %>% purrr::set_names(s)

    cor2var <- cor2var %>%
      purrr::map( ~ .x$result) %>%
      purrr::compact()
    cor2var_df <- do.call(rbind.data.frame, cor2var)

    if (sig_label == TRUE) {
      cor2var_df$pstar <- ifelse(
        cor2var_df$p.value < 0.05,
        ifelse(
          cor2var_df$p.value < 0.001,
          "***",
          ifelse(cor2var_df$p.value < 0.01, "**", "*")
        ),
        ""
      )
    }
    return(cor2var_df)
  }

  if (split == FALSE) {
    sss <- ss
    if (length(var3) > 1) {
      dd <-
        ppcor::pcor.test(as.numeric(sss[, var1]),
                         as.numeric(sss[, var2]),
                         as.matrix(sss[, var3]),
                         method = Cor_method,
                         ...)
      ddd <-
        data.frame(
          cor_partial = dd$estimate,
          p.value = dd$p.value,
          method = Cor_method,
          x = var1,
          y = var2,
          z = paste(var3, collapse = ",") ,
          stringsAsFactors = F
        )
      #ddd$group <- x
      return(ddd)
    } else{
      dd <-
        ppcor::pcor.test(as.numeric(sss[, var1]),
                         as.numeric(sss[, var2]),
                         as.numeric(sss[, var3]),
                         method = Cor_method)
      ddd <-
        data.frame(
          cor_partial = dd$estimate,
          p.value = dd$p.value,
          method = Cor_method,
          x = var1,
          y = var2,
          z = var3,
          stringsAsFactors = F
        )
      #ddd$group <- x
      return(ddd)
    }

    cor2var_df <- ddd

    if (sig_label == TRUE) {
      cor2var_df$pstar <- ifelse(
        cor2var_df$p.value < 0.05,
        ifelse(
          cor2var_df$p.value < 0.001,
          "***",
          ifelse(cor2var_df$p.value < 0.01, "**", "*")
        ),
        ""
      )

    }
    return(cor2var_df)
  }
}


#' Run parital correlation in a batch mode
#'
#' @inheritParams ppcor::pcor.test
#' @seealso [ppcor::pcor.test()] which this function wraps.
#' @param data a `data.frame` containing variables
#' @param split whether perform correlation grouped by a variable, default is 'FALSE'
#' @param split_var a `character`, the group variable
#' @param var1 a `character`, the first variable in correlation
#' @param var2 a `character` or `vector`, the second variable in correlation, and support batch mode
#' @param var3 a `character` or `character vector`, the third variable in correlation
#' @param Cor_method method="pearson" is the default value. The alternatives to be passed to cor are "spearman" and "kendall"
#' @param sig_label whether add symbal of significance. P < 0.001,"***"; P < 0.01,"**"; P < 0.05,"*"; P >=0.05,""
#' @import purrr
#' @export
#' @return a `data.frame`
#' @author Yi Xiong

ezcor_partial_cor_batch <- function(data = NULL,
                              split = FALSE,
                              split_var = NULL,
                              var1 = NULL,
                              var2 = NULL,
                              var3 = NULL,
                              Cor_method = "pearson",
                              sig_label = TRUE,
                              ...) {
  if (!requireNamespace("ppcor")) {
    stop("Please install 'ppcor' package firstly!")
  }
  stopifnot(is.data.frame(data))
  ss <- data
  #var1 = x; var2 = y; var3 = z
  if (!var1 %in% colnames(ss)) {
    stop("the first variable is unavailable in the dataset!")
  }
  if (!var2 %in% colnames(ss)) {
    stop("the second variable is unavailable in the dataset!")
  }
  if (length(var1) != 1) {
    stop("only one element is needed in the first variable!")
  }
  # if (length(var2) != 1){stop("only one element is needed in the second variable!")}
  if (split == TRUE) {
    if (!split_var %in% colnames(ss)) {
      stop("split variable is unavailable in the dataset!")
    }
    all_cols <- unique(c(var1, var2, var3, split_var))
    ss <- ss[, all_cols]
    res <- purrr::map(
      var2,
      ezcor_partial_cor_caller,
      data = ss,
      split = split,
      split_var = split_var,
      var1 = var1,
      var3 = var3,
      Cor_method = Cor_method,
      sig_label = sig_label,
      ...
    ) %>% purrr::set_names(var2)
    res2 <- dplyr::bind_rows(res)
    return(res2)
  } else{
    all_cols <- unique(c(var1, var2,var3))
    ss <- ss[, all_cols]
    res <- purrr::map(
      var2,
      ezcor_partial_cor_caller,
      data = ss,
      split = split,
      split_var = split_var,
      var1 = var1,
      var3 = var3,
      Cor_method = Cor_method,
      sig_label = sig_label,
      ...
    ) %>% purrr::set_names(var2)
    res2 <- dplyr::bind_rows(res)
    return(res2)
  }

}

ezcor_partial_cor_caller <- function(var2,
                                     data,
                                     split = split,
                                     split_var = split_var,
                                     var1 = var1,
                                     var3 = var3,
                                     Cor_method = Cor_method,
                                     sig_label = sig_label,
                                     ...) {
  ezcor_partial_cor(
    data = data,
    split = split,
    split_var = split_var,
    var1 = var1,
    var2 = var2,
    var3 = var3,
    Cor_method = Cor_method,
    sig_label = sig_label,
    ...
  )
}
