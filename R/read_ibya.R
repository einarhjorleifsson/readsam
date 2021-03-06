#' @export
read_ibya <- function(ass, web = TRUE, user = "user3") {

  files <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat",
             "mo.dat", "nm.dat", "pf.dat", "pm.dat", "sw.dat")
  var <- c("oC", "cW", "dW", "lF", "lW",
           "mat", "m", "pF", "pM", "sW")
  res <- list()

  for(j in 1:length(files)) {

    if(web) {
      url <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",
                   user,
                   ass,
                   "data",
                   sep="/")
      fil <- get_file(url, files[j])
    } else {
      fil <- paste0(ass,
                   "/data/",
                   files[j])
    }

    res[[j]] <-
      stockassessment::read.ices(fil) %>%
      as.data.frame() %>%
      dplyr::mutate(year = row.names(.) %>% as.integer()) %>%
      tidyr::gather(age, value, -year, convert = TRUE) %>%
      dplyr::mutate(variable = var[j]) %>%
      dplyr::as_tibble()
  }

  if(web) {
    url <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",
                 user,
                 ass,
                 "data",
                 sep="/")
    fil <- get_file(url, "survey.dat")
  } else {
    fil <- paste0(ass,
                  "/data/survey.dat")
  }

  x <- stockassessment::read.ices(fil)

  sur <- list()
  for(i in 1:length(x)) {
    sur[[i]] <-
      x[[i]] %>%
      as.data.frame() %>%
      dplyr::mutate(year = row.names(.) %>% as.integer()) %>%
      tidyr::gather(age, value, -year, convert = TRUE) %>%
      dplyr::mutate(variable = paste0("oU", i)) %>%
      dplyr::as_tibble()
  }

  sur <-
    sur %>%
    dplyr::bind_rows()

  res %>%
    dplyr::bind_rows() %>%
    dplyr::bind_rows(sur) %>%
    tidyr::spread(variable, value) %>%
    dplyr::as_tibble() %>%
    return()

}
