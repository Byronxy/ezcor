#' Run Biweight Midcorrelation
#'
#' @inheritParams WGCNA::bicor
#' @seealso [WGCNA::bicor()] which this function wraps.
#' @param data a `data.frame` containing variables
#' @param var1 a `character`, the first variable in correlation
#' @param var2 a `character`, the second variable in correlation
#' @param split `logic value`,whether perform correlation grouped by a variable, default is 'FALSE'
#' @param split_var a `character`, the group variable
#' @import purrr
#' @import dplyr
#' @return a `data.frame`
#' @author Yi Xiong
#' @export
#'
ezcor_bicor <- function(data, var1,var2,split = FALSE,split_var,...){
  if (!requireNamespace("WGCNA")) {
    stop("Please install 'WGCNA' package firstly!")
  }
  stopifnot(is.data.frame(data))
  if (split == FALSE){
    res <- WGCNA::bicor(x = as.numeric(data[,var1]), y = as.numeric(data[,var2]),...)
    bicor2var_df <- data.frame(bicor = res, v1 = var1, v2 = var2 ,stringsAsFactors = F)
    return(bicor2var_df)
  }else{
    ss <- data;rm(data)
    if (!split_var %in% colnames(ss)){stop("split variable is unavailable in the dataset!")}
    #index
    n = which(colnames(ss) %in% split_var)
    sss <- with(ss,split(ss, ss[,n]))
    s <- names(sss)
    bicor2var <- purrr::map(s, purrr::safely(function(x) {
      sss_sub<- sss[[x]]
      res_bicor <- WGCNA::bicor(x = as.numeric(sss_sub[,var1]), y = as.numeric(sss_sub[,var2]),...)
      res_bicor <- data.frame(bicor = res_bicor, v1 = var1, v2 = var2 ,stringsAsFactors = F)
      res_bicor$group = x
      return(res_bicor)
    })) %>% purrr::set_names(s)
    bicor2var <- bicor2var %>%
      purrr::map(~ .x$result) %>%
      purrr::compact()
    bicor2var_df <- do.call(rbind.data.frame, bicor2var)
    return(bicor2var_df)
  }
}

#' Run Biweight Midcorrelation in a batch mode
#'
#' @inheritParams WGCNA::bicor
#' @seealso [WGCNA::bicor()] which this function wraps.
#' @param data a `data.frame` containing variables
#' @param var1 a `character`, the first variable in correlation
#' @param var2 a `vector` containing variables in a batch mode
#' @param split `logic value`, whether perform correlation grouped by a variable, default is 'FALSE'
#' @param split_var a `character`, the group variable
#' @import purrr
#' @import dplyr
#' @return a `data.frame`
#' @author Yi Xiong
#' @export
#'
ezcor_bicor_parallel <- function(data,var1,var2,split = FALSE,split_var,...){
  stopifnot(is.data.frame(data))
  if (!var1 %in% colnames(data)){stop("the first variable is unavailable in the dataset!")}
  if (!var2 %in% colnames(data)){stop("the second variable is unavailable in the dataset!")}
  if (length(var1) != 1){stop("only one element is needed in the var1 variable!")}
  if (split == TRUE){
    if (!split_var %in% colnames(data)){stop("split variable is unavailable in the dataset!")}
    all_cols <- unique(c(var1, var2,split_var))
    data <- data[, all_cols]
    res <- purrr::map(var2,
                      ezcor_bicor_caller,
                      data = data,
                      split_var = split_var,
                      split = split,
                      var1 = var1,
                      ...) %>% purrr::set_names(var2)
    res2 <- dplyr::bind_rows(res)
    return(res2)

  }else{
    all_cols <- unique(c(var1, var2))
    data <- data[, all_cols]
    res <- purrr::map(var2,
                      ezcor_bicor_caller,
                      data = data,
                      split_var = split_var,
                      split = split,
                      var1 = var1,
                      ...) %>% purrr::set_names(var2)
    res2 <- dplyr::bind_rows(res)
    return(res2)
  }


}


ezcor_bicor_caller <- function(var2,
                               data,
                               split_var = split_var,
                               split = split,
                               var1 = var1,
                               ...){
  ezcor_bicor(
    data = data,
    split_var = split_var,
    split = split,
    var1 = var1,
    var2 = var2,
    ...
  )
}
