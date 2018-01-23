#' @title Gets you the full name of the study on dbGap
#'
#' @param phs dbGap study ID (phs00xxxx, or 00xxxx, or xxx)
#'
#' @return Returns the full name of the study
#'
#' @author Gregoire Versmee, Laura Versmee

#' @export


study.name <- function(phs)  {

  phs <- phs.version(phs)

  content <- RCurl::getURL(paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=", phs))

  start <- as.numeric(regexpr("<span id=\"study-name\" name=\"study-name\">\n", content)) + nchar("<span id=\"study-name\" name=\"study-name\">\n")
  stop <- as.numeric(regexpr("\n</span>", content)) - 1
  study_name <-substr(content, start, stop)


  #  unlist(strsplit(phs, "\\."))[1]
#  gapexchange <- paste0("ftp://anonymous:anonymous@ftp.ncbi.nlm.nih.gov/dbgap/studies/", unlist(strsplit(phs, "\\."))[1], "/", phs, "/", "GapExchange_", phs, ".xml")
#  xmllist <- XML::xmlToList(RCurl::getURLContent(gapexchange))
#  study_name <- xmllist[["Studies"]][["Study"]][["Configuration"]][["StudyNameEntrez"]]
   return(study_name)

}

