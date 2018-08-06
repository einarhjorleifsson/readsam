# TO DO: add the residuals
#' @export
read_rbya_sam <- function(fit, ibya) {

  nay <-
    stockassessment::ntable(fit) %>%
    as.data.frame() %>%
    mutate(year = rownames(.) %>% as.integer()) %>%
    gather(age, n, -year, convert = TRUE)
  fay <-
    stockassessment::faytable(fit) %>%
    as.data.frame() %>%
    mutate(year = rownames(.) %>% as.integer()) %>%
    gather(age, f, -year, convert = TRUE)
  res <-
    nay %>%
    full_join(fay, by = c("year", "age")) %>%
    as_tibble()

  if(!missing(ibya)) {
    res %>%
      full_join(ibya, by = c("year", "age")) %>%
      return()
  } else {
    res %>% return()
  }

}
