# TO DO: add the residuals
#' @export
read_rbya_sam <- function(fit, ibya) {

  nay <-
    stockassessment::ntable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer()) %>%
    tidyr::gather(age, n, -year, convert = TRUE)
  fay <-
    stockassessment::faytable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer()) %>%
    tidyr::gather(age, f, -year, convert = TRUE)
  res <-
    nay %>%
    dplyr::full_join(fay, by = c("year", "age")) %>%
    dplyr::as_tibble()

  if(!missing(ibya)) {
    res %>%
      dplyr::full_join(ibya, by = c("year", "age")) %>%
      return()
  } else {
    res %>% return()
  }

}
