#' @title Gets the number of phenotypic variables in the study
#'
#' @param phs dbGap study ID (phs00xxxx, or 00xxxx, or xxx)
#'
#' @return Return the number of phenotypic datatables in the study
#'
#' @description This function extracts informations from data.dict.xml files from the dbgap ftp server to get the study characteristics. Works only for a parent study.
#' @import RCurl
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export


n.variables <- function(phs)  {

  phs <- phs.version(phs)
  url<- paste0("ftp://anonymous:anonymous@ftp.ncbi.nlm.nih.gov/dbgap/studies/", unlist(strsplit(phs, "\\."))[1], "/", phs, "/")
  filenames <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filenames <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")

  phenodir <- filenames[grepl("pheno", filenames)]

  filelist <- RCurl::getURL(paste0(phenodir, "/"), ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filelist <- paste(phenodir, "/", strsplit(filelist, "\r*\n")[[1]], sep = "")

  ind <- (grepl(".data_dict.xml", filelist)) & (!grepl("henotypes.data_dict.xml", filelist)) & (!grepl("ample_Attributes.data_dict.xml", filelist)) &
         (!grepl("Subject.data_dict", filelist)) & (!grepl("Sample.data_dict", filelist)) & (!grepl("Pedigree.data_dict", filelist))
  temp <- filelist[ind]

  temp <- lapply(temp, function(x) {
    XML::xmlSize(XML::xmlRoot(XML::xmlParse(RCurl::getURLContent(x)))) - 1
  })

  return(Reduce(sum, temp))
}
