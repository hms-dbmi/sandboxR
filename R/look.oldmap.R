#' @title View an old mapping file on R
#'
#' @param mappath Pathway to the folder where your mapping file and your tree are located
#' @param oldcsv Name of the csv file that you want to use as a map. Old csv files can be found with the list.oldmaps function
#'
#' @return Return a view of your old mapping file as a data.frame
#'
#'
#' @author Gregoire Versmee, Laura Versmee

#' @export



look.oldmap <- function(mappath, old)  {
  
  wd <- getwd()
  
  oldmap <- read.csv(paste0(mappath, "/.oldmaps/",old), header = TRUE, stringsAsFactors = FALSE)
  View(oldmap)
  
  setwd(wd)
}

