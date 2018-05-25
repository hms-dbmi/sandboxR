#' @author Gregoire Versmee, Laura Versmee
#' @export
#' @import parallel
#' @import nhanesA
#' @import data.table


dl.nhanes <- function(destination = getwd()) {
  ncores <- getOption("mc.cores", parallel::detectCores())

  tables <- nhanesA::nhanesSearchTableNames("")

  test <- parallel::mclapply(tables, function(e) {
    df <- nhanesA::nhanes(e)
    if (is.null(df)) return(NULL)
    return(df)
  }, mc.cores = ncores)

  names <- sapply(sub(".csv", "", basename(tables)) , function(e) strsplit(e, "_")[[1]][1])
  names(test) <- names
  test <- test[!sapply(test, is.null)]

  # Now we combine the tables per questionnaires
  unique_names <- unique(names(test))

  test2 <- lapply(unique_names, function(e) {
    l <- test[names(test) == e]
    df <- data.table::rbindlist(l, fill = TRUE)
    return(df)
  })
  names(test2) <- unique_names

  destination <- paste0(destination, "/NHANES_tables_per_questionnaire")
  dir.create(destination, showWarnings = FALSE)

  parallel::mcmapply(function(e,f) {
    data.table::fwrite(e, paste0(destination, "/", f, ".csv"))
  }, test2, names(test2), mc.cores = ncores)
}
