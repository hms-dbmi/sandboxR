#' @title Gets you the list of the consent groups of the study
#'
#' @param phs dbGap study ID (phs00xxxx, or 00xxxx, or xxx)
#'
#' @return Returns a data frame with 3 cols : "consent group number", "cg short name" and "cg long name"
#'
#' @author Gregoire Versmee, Laura Versmee

#' @export


consent.groups <- function(phs)  {

  if (!is.parent(phs))  phs <- parent.study(phs)[1]
  phs <- phs.version(phs)
  unlist(strsplit(phs, "\\."))[1]
  gapexchange <- paste0("ftp://anonymous:anonymous@ftp.ncbi.nlm.nih.gov/dbgap/studies/", unlist(strsplit(phs, "\\."))[1], "/", phs, "/", "GapExchange_", phs, ".xml")
  xmllist <- XML::xmlToList(RCurl::getURLContent(gapexchange))
  cg <- t(data.frame(xmllist[["Studies"]][["Study"]][["Configuration"]][["ConsentGroups"]]))
  cg <- data.frame(cg)
  row.names(cg) <- cg[,1]
  cg <- cg[,-1]
  cg

}

