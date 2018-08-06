#' @export
read_retro_sam <- function(retro, ibya) {
  out <- list()
  for(i in 1:length(retro)) {
    out[[i]] <-
      sam_rby(retro[[i]]) %>%
      mutate(assyear = i)
    bio <-
      ntable(retro[[i]]) %>%
      as.data.frame() %>%
      mutate(year = rownames(.) %>% as.integer()) %>%
      gather(age, n, -year, convert = TRUE) %>%
      left_join(ibya %>% select(year, age, cW)) %>%
      filter(age %in% 4:14) %>%
      group_by(year) %>%
      summarise(bio = sum(n * cW) / 1e3)
    out[[i]] <-
      out[[i]] %>%
      bind_rows(bio %>%
                  select(year = year, Estimate = bio) %>%
                  mutate(variable = "bio",
                         assyear = i))

  }
  out %>% bind_rows() %>% as_tibble()
}