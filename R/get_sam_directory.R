#' @title Get a sam directory from stockassessment.org
#'
#' @description The function copies the whole directory of an assessment run from
#' stockassessment.org to a local directory
#'
#' @export
#'
#' @param assessment Name of the assessment
#' @param user Name of the user
#'

get_sam_directory <- function(assessment, user = "user3") {

  message("Function will be dropped from this package. See package 'ramsam'")

  path <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",user,assessment,sep="/")
  cmd <- paste0("wget --recursive --reject=png,html --level=0 --no-parent ",path,"/")
  system(cmd)

  # cleanup
  Path <- "www.stockassessment.org/datadisk/stockassessment/userdirs"
  Path <- paste(Path,user,assessment,sep="/")
  cmd <- paste("mv", Path, ".")
  system(cmd)
  system("rm -r www.stockassessment.org")

}



get_sam <- function(assessment, user="user3") {

  path <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",user,assessment,sep="/")

  # get input files
  res <- list()

  files <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat",
             "mo.dat", "nm.dat", "pf.dat", "pm.dat", "sw.dat")
  var <- c("oC", "cW", "dW", "lF", "lW",
           "mat", "m", "pF", "pM", "sW")

  tmpfile <- tempfile()

  for(i in 1:length(files)) {

    download.file(paste0(path, "/data/", files[i]), destfile = tmpfile)
    res[[i]] <-
      stockassessment::read.ices(tmpfile) %>%
      as.data.frame() %>%
      mutate(year = row.names(.) %>% as.integer()) %>%
      gather(age, value, -year, convert = TRUE) %>%
      mutate(variable = var[i]) %>%
      as_tibble()
  }

  ibya <-
    res %>%
    bind_rows()

  # Get fit

  download.file(paste0(path, "/run/model.RData"), destfile = tmpfile)
  load(tmpfile)


}






sam_process_error <- function(rbya, plus_group=TRUE, plot_it=FALSE) {

  # dummy
  n.d <- cW <- year <- z.d <- age <- b <- 0

  # ----------------------------------------------------------------------------
  # align the year-classes
  x <- rbya[,c("year","age","n")]
  x$year <- x$year - 1
  x$age <- x$age - 1
  names(x)[3] <- "n.end"
  d <- plyr::join(rbya[,c("year", "age", "n", "m", "f", "cW")], x, by = c("year", "age"))
  d <- d[!is.na(d$n.end),]

  # ----------------------------------------------------------------------------
  # exclude plus-group
  if(plus_group) d <- d[d$age < max(d$age),]

  # ----------------------------------------------------------------------------
  # process error expressed as mortality
  d$z.n <- log(d$n/d$n.end)
  d$z.f <- d$f + d$m
  d$z.d  <- d$z.n - d$z.f

  # ----------------------------------------------------------------------------
  # process error expressed as numbers
  d$n.end2 <- d$n * exp(-(d$f + d$m))
  # Calculate the difference
  d$n.d <- d$n.end - d$n.end2

  # ----------------------------------------------------------------------------
  # process errror expressed as biomass
  x <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(n.d * cW,na.rm=TRUE))
  x <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(n.d * cW,na.rm=TRUE))

  if(plot_it) {
    mort <- ggplot2::ggplot(d,ggplot2::aes(year,z.d)) +
      ggplot2::theme_bw() +
      ggplot2::geom_text(ggplot2::aes(label=age)) +
      ggplot2::stat_smooth(span=0.1) +
      ggplot2::labs(x="",y="",title="Process error expressed as deviations in mortality")

    abun <- ggplot2::ggplot(d,ggplot2::aes(year,n.d)) +
      ggplot2::theme_bw() +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::facet_wrap(~ age, scale="free_y") +
      ggplot2::labs(x="", y="",title="Process error expressed as deviations in number of fish")

    mass <- ggplot2::ggplot(x[x$year < max(x$year),],ggplot2::aes(year,b)) +
      ggplot2::theme_bw() +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::labs(x="",y="Mass",title="Process error expressed as deviation in mass")

    return(list(rbya=d,rby=x,mort=mort,abun=abun,mass=mass))
  }

  return(list(rbya=d, rby=x))

}