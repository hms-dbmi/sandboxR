#' @title Get the list of the saved old maps
#'
#' @param mappath Pathway to the folder containing the mapping file
#'
#' @return Return a vector listing all the csv files containing saved maps
#'
#' @description This function gets the list of the old maps saved in the ".oldmaps" folder.
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export


list.oldmaps <- function(mappath)  {

wd <- getwd()

setwd(paste0(mappath, "/.oldmaps"))
list <- list.files(all.files = TRUE, recursive = FALSE)
ind <- grepl("map_", list)
list <- list[ind]

return(list)

setwd(wd)
}

