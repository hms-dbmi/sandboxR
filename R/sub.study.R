#' @title Gives the substudies associated with the one selected
#'
#' @param phs dbGap study ID (phs00xxxx, or 00xxxx, or xxx)
#'
#' @return Returns a dataframe with 2 cols : the phs id and the name of the substudy
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export
 

sub.study <- function(phs)  {

  if (!is.parent(phs))  warning("Your study is not a parent study")
  phs <- phs.version(phs)
  substudies <- paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetFolderView.cgi?current_study_id=", phs, "&current_type=101&current_object_id=1&current_folder_type=101")
  sub1 <- RCurl::getURLContent(substudies)
  sub1 <- strsplit(sub1, "\r*\n")[[1]]
  sub1 <- XML::xmlToList(sub1)
  sub1 <- sub1[[3]]
  
  subst <- data.frame(matrix(ncol = 2))
  colnames(subst) <- c("phs", "name")
  for (i in 1:length(sub1))  {
    phssub <- sub1[[i]][["a"]][[".attrs"]][["onclick"]]
    subst[i,1] <- substr(phssub, regexpr("phs", phssub), regexpr("return", phssub)-4)
    subst[i,2] <- sub1[[i]][["a"]][["text"]]
  }
  
  subst
}
