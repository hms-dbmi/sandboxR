#' @title Create a tables dictionnary of your study
#'
#' @param phs dbGap ID of your study "phs000000". No version please, as it will automatically return the latest one.
#'
#' @return a data.frame with 3 cols : XXXXX
#'
#' @description This function extracts informations from data.dict.xml files from the dbgap ftp server to create a table dictionnary.
#' @import XML
#' @import RCurl
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export


datatables.dict <-function (phs)  {

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


  #Create the data.frames
  datatablesdict <- data.frame()

  #Create column names
  cnamesdt <- c("pht", "dt_study_name", "dt_label")

  #Looping!!
  for (i in 1:length(temp))  {

    #Extract xml
    xmllist <- XML::xmlToList(RCurl::getURLContent(temp[i]))
    xmlfile <- XML::xmlParse(RCurl::getURLContent(temp[i]))
    xmltop <- XML::xmlRoot(xmlfile)

    #Get dt dbgap name + version + study name
    dt_name <- xmllist[[".attrs"]][["id"]]
    dt_sn <- substr(temp[i], regexpr(dt_name, temp[i]) + nchar(dt_name)+1, regexpr(".data_dict", temp[i])-1)
    dt_label <- xmllist[["description"]]
    if (is.null(dt_label)) dt_label <- dt_sn

    #Create datatablesdict
    dt <- data.frame(dt_name, dt_sn, dt_label)

    #Append to the final tables
    colnames(dt) <- cnamesdt
    datatablesdict <- rbind(datatablesdict, dt)
  }

  ## Remove empty rows
  emrow <- apply(datatablesdict, 1, function(x) all(is.na(x)))
  datatablesdict <- datatablesdict[!emrow, ]

  return(datatablesdict)

  setwd(wd)
}
