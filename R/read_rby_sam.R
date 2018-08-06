#' @export
read_rby_sam <- function(fit, ibya) {

  pY <-
    stockassessment::catchtable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer(),
           variable = "catch")
  rec <-
    stockassessment::rectable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer(),
           variable = "rec")
  ssb <-
    stockassessment::ssbtable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer(),
           variable = "ssb")
  tsb <-
    stockassessment::tsbtable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer(),
           variable = "tsb")
  fbar <-
    stockassessment::fbartable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer(),
           variable = "fbar")

  pY %>%
    dplyr::bind_rows(rec) %>%
    dplyr::bind_rows(ssb) %>%
    dplyr::bind_rows(tsb) %>%
    dplyr::bind_rows(fbar) %>%
    dplyr::as_tibble()
}
