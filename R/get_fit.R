#' @export
get_fit <- function(ass, web = TRUE, user = "user3") {

  url <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",
               user,
               ass,
               "run",
               sep="/")

  fil <- get_file(url, "model.RData")
  attach(fil, pos = 2)
  fit <- fit
  detach(pos = 2)
  return(fit)

}
