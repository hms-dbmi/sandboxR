#' @title Change the structure of your tree folder using an old mapping file
#'
#' @param mappath Pathway to the folder where your mapping file and your tree are located
#' @param oldcsv Name of the csv file that you want to use as a map. Old csv files can be found with the list.oldmaps function
#'
#' @return A new tree structure, according to the old map that you selected. The 0_map.csv is also changed for the one that you chose, and the old map is saved in the .oldmap folder
#'
#'
#' @author Gregoire Versmee, Laura Versmee

#' @export



recover.map <- function(oldcsv, mappath)  {

  wd <- getwd()

  ## save the old map file, with date and time
  oldmap <- paste0(mappath, "/0_map.csv")
  if (dir.exists(paste0(mappath, "/.oldmaps")) == FALSE)  dir.create(paste0(mappath, "/.oldmaps"))
  file.copy(oldmap, paste0(mappath, "/.oldmaps/map_", format(Sys.time(), format = "%Y-%m-%j %H%M %Z"), ".csv"))

  ## Remove empty directories
  setwd(mappath)
  system("find . -empty -type d -delete")

  ## Create the data.frame
  pathway <- data.frame(matrix(ncol = 2))
  cnames <- c("phv", "pathway")
  colnames(pathway) <- cnames

  ## Get thhe pathways for each file in the tree
  foo <- grepl("_tree", list.dirs(mappath, recursive = FALSE, full.names = FALSE))
  treepath <- list.dirs(mappath, recursive = FALSE)[foo]
  a <- list.files(treepath, full.names = TRUE, recursive = TRUE)
  a <- gsub(paste0(treepath, "/"), "", a)
  b <- strsplit(a, "/")

  ## Create a dataframe with 2 columns: phv and pathways
  for (i in 1:length(b))  {
    file1 <- unlist(strsplit(b[[i]][[length(b[[i]])]], " "))
    file2 <- file1[length(file1)]
    phv <- substr(file2, 1, regexpr(".csv", file2)-1)
    pathway[i,1] <- phv
    pathway[i,2] <- a[i]
  }

  ## open the map you want to recover
  map <- read.csv(paste0(mappath, "/.oldmaps/", oldcsv), header = TRUE)

  ## merge it with the old pathways
  map <- map[1:19]
  map <- merge(map, pathway)

  ## write the new 0_map.csv file
  write.csv(map, paste0(mappath, "/0_map.csv"), row.names = FALSE, na = "")

  ## MapToTree
  MapToTree(mappath)

  setwd(wd)
}

