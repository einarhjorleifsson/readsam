#' @export
read_retro_sam <- function(retro, ibya) {
  out <- list()
  for(i in 1:length(retro)) {
    out[[i]] <-
      read_rby_sam(retro[[i]]) %>%
      dplyr::mutate(assyear = i)
    bio <-
      stockassessment::ntable(retro[[i]]) %>%
      as.data.frame() %>%
      dplyr::mutate(year = rownames(.) %>% as.integer()) %>%
      tidyr::gather(age, n, -year, convert = TRUE) %>%
      dplyr::left_join(ibya %>% select(year, age, cW)) %>%
      dplyr::filter(age %in% 4:14) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(bio = sum(n * cW) / 1e3)
    out[[i]] <-
      out[[i]] %>%
      dplyr::bind_rows(bio %>%
                         dplyr::select(year = year, Estimate = bio) %>%
                         dplyr::mutate(variable = "bio",
                         assyear = i))

  }
  out %>% dplyr::bind_rows() %>% dplyr::as_tibble()
}