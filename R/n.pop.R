#' @title Gets the population number of the study
#'
#' @param phs dbGap study ID (phs00xxxx, or 00xxxx, or xxx)
#' @param consentgroup if false, will return only the total number of participants
#'
#' @return a data.frame with 2 cols : name of the consent group and n total. Possibility to add the breakdown by gender
#'
#' @description This function extracts informations from data.dict.xml files from the dbgap ftp server to get the population characteristics. Works only for parents studies.
#' @import XML
#' @import RCurl
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export


n.pop <- function(phs, consentgroups = TRUE, gender = FALSE)  {

  phs <- phs.version(phs)
  url<- paste0("ftp://anonymous:anonymous@ftp.ncbi.nlm.nih.gov/dbgap/studies/", unlist(strsplit(phs, "\\."))[1], "/", phs, "/")
  filenames2 <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filenames2 <- paste(url, "/", strsplit(filenames2, "\r*\n")[[1]], sep = "")
  ind <- grepl("pheno", filenames2)
  phenodir <- filenames2[ind]
  filelist <- RCurl::getURL(paste0(phenodir, "/"), ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filelist <- paste(phenodir, "/", strsplit(filelist, "\r*\n")[[1]], sep = "")
  ind <- (grepl("Subject.var_report.xml", filelist))
  subjdict <- filelist[ind]

  #Extract xml
  xmlfile <- XML::xmlParse(RCurl::getURLContent(subjdict))
  xmllist <- XML::xmlToList(xmlfile)

  #Create the data.frame
  temp <- data.frame()

  cg <- c()
  for (i in 1:(length(xmllist)-1))  {
    if (is.null(xmllist[[i]][[".attrs"]][["var_name"]]))  next
    if (xmllist[[i]][[".attrs"]][["var_name"]] == "CONSENT")  cg <- c(cg, i)
  }

  for (j in 2:length(cg))  {
    name <- unlist(strsplit(xmllist[[cg[j]]][["total"]][["stats"]][["enum"]][["text"]], " "))
    name <- name[length(name)]
    temp[j-1,1] <- substr(name, 2, nchar(name) -1)
    temp[j-1, 2] <- xmllist[[cg[j]]][["total"]][["subject_profile"]][["sex"]][["male"]]
    temp[j-1, 3] <- xmllist[[cg[j]]][["total"]][["subject_profile"]][["sex"]][["female"]]
    temp[j-1, 4] <- xmllist[[cg[j]]][["total"]][["stats"]][["stat"]][["n"]]
  }

  temp[j,1] <- "TOTAL"
  for (h in 2:4)  {
    temp[j,h] <- sum(as.numeric(temp[1:(nrow(temp)-1),h]))
  }

  colnames(temp) <- c("consent_group", "male", "female", "total")

  if (gender == FALSE)  temp <- temp[,c(1,4)]
  if (consentgroups == TRUE)  return(temp)
  if (consentgroups == FALSE) return(as.numeric(temp[nrow(temp), ncol(temp)]))

}

