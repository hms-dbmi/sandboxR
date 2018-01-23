#' @title Create a variables dictionnary of your study
#'
#' @param phs dbGap study ID (phs00xxxx, or 00xxxx, or xxx)
#'
#' @return a data.frame with 4 cols : variable identifier (dbGap), table name, variable name, variable description
#'
#' @description This function extracts informations from data.dict.xml files from the dbgap ftp server to create a variable dictionnary.
#' @import XML
#' @import RCurl
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export


variables.dict <-function (phs)  {

  wd <- getwd()

  phs <- phs.version(phs)

  #selecting all xml files except for "Subject", "Sample", "Pedigree", and phenotypics data from substudies
  url<- paste0("ftp://anonymous:anonymous@ftp.ncbi.nlm.nih.gov/dbgap/studies/", unlist(strsplit(phs, "\\."))[1], "/")

  filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filenames <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
  filenames2 <- getURL(paste0(filenames[length(filenames)], "/"), ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filenames2 <- paste(filenames[length(filenames)], "/", strsplit(filenames2, "\r*\n")[[1]], sep = "")
  ind <- grepl("pheno", filenames2)
  phenodir <- filenames2[ind]
  filelist <- getURL(paste0(phenodir, "/"), ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  filelist <- paste(phenodir, "/", strsplit(filelist, "\r*\n")[[1]], sep = "")
  ind <- (grepl(".data_dict.xml", filelist)) & (!grepl("henotypes.data_dict.xml", filelist)) & (!grepl("ample_Attributes.data_dict.xml", filelist)) &
    (!grepl("Subject.data_dict", filelist)) & (!grepl("Sample.data_dict", filelist)) & (!grepl("Pedigree.data_dict", filelist))
  temp <- filelist[ind]

  #Create the data.frame
  variablesdict <- data.frame()

  #Create column names
  cnamesvt <- c("phv", "dt_study_name", "var_name", "var_desc")

  #Looping!!
  for (i in 1:length(temp))  {

    #Extract xml
    vt <- data.frame()
    xmllist <- XML::xmlToList(RCurl::getURLContent(temp[i]))
    xmlfile <- XML::xmlParse(RCurl::getURLContent(temp[i]))
    xmltop <- XML::xmlRoot(xmlfile)

    #Get dt dbgap name + version + study name
    dt_name <- xmllist[[".attrs"]][["id"]]
    dt_sn <- substr(temp[i], regexpr(dt_name, temp[i]) + nchar(dt_name)+1, regexpr(".data_dict", temp[i])-1)

    #Create vt
    for (j in 2:XML::xmlSize(xmltop))  {
      if (XML::xmlName(xmltop[[j]]) == "variable") {
        vt[j,1] <- xmllist[[j]]$.attrs
        vt[j,2] <- dt_sn
        vt[j,3] <- xmllist[[j]]$name
        vt[j,4] <- xmllist[[j]]$description
      }
    }

    #Append to the final tables
    colnames(vt) <- cnamesvt
    variablesdict <- rbind(variablesdict, vt)
  }

  ## Remove empty rows
  emrow <- apply(variablesdict, 1, function(x) all(is.na(x)))
  variablesdict <- variablesdict[!emrow, ]

  return(variablesdict)

  setwd(wd)
}
