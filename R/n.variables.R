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
  filenames2 <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filenames2 <- paste(url, "/", strsplit(filenames2, "\r*\n")[[1]], sep = "")
  ind <- grepl("pheno", filenames2)
  phenodir <- filenames2[ind]
  filelist <- RCurl::getURL(paste0(phenodir, "/"), ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filelist <- paste(phenodir, "/", strsplit(filelist, "\r*\n")[[1]], sep = "")
  
  ind <- (grepl(".data_dict.xml", filelist)) & (!grepl("henotypes.data_dict.xml", filelist)) & (!grepl("ample_Attributes.data_dict.xml", filelist)) &
    (!grepl("Subject.data_dict", filelist)) & (!grepl("Sample.data_dict", filelist)) & (!grepl("Pedigree.data_dict", filelist))
  temp <- filelist[ind]
  
  total <- 0
  
  #Looping!!
  for (i in 1:length(temp))  {
    
    #Extract xml
    xmllist <- XML::xmlToList(RCurl::getURLContent(temp[i]))
    xmlfile <- XML::xmlParse(RCurl::getURLContent(temp[i]))
    xmltop <- XML::xmlRoot(xmlfile)
    
    total <- total + ((XML::xmlSize(xmltop)) -1)
  }
  
  total
  
}

