#' @title Change the structure of your tree folder using your mapping file
#'
#' @param mappath Pathway to the folder where your mapping file and your tree are located
#'
#' @return Move and rename the files in the tree folder according to your mapping file
#'
#'
#' @author Gregoire Versmee, Laura Versmee

#' @export


MapToTree <- function(mappath)  {

wd <- getwd()

## Find the old pathways
map <- read.csv(paste0(mappath, "/0_map.csv"), stringsAsFactors = FALSE, na.strings = "")

foo <- grepl("_tree", list.dirs(mappath, recursive = FALSE, full.names = FALSE))
treepath <- list.dirs(mappath, recursive = FALSE)[foo]
setwd(treepath)

for (i in 1:nrow(map))  {
  newpath <- treepath
  for (j in 6:19)  {
    if (is.na(map[i,j])) break
    else {
      newpath <- paste0(newpath, "/", gsub("/", "|", map[i,j]))
      if (dir.exists(newpath) == FALSE)  dir.create(newpath)
    }
  }
  newpath <- paste0(newpath, "/", gsub("/", "|", map[i,5]), " ", map[i,1], ".csv")
  if (gsub(paste0(treepath, "/"), "", newpath) != map[i,20])  {
    file.copy(paste0(treepath, "/", map[i, 20]), newpath)
    file.remove(map[i,20])
    map[i, 20] <- as.character(gsub(paste0(treepath, "/"), "", newpath))
  }
}

## Remove empty directories in the tree
system("find . -empty -type d -delete")

write.csv(map, paste0(mappath, "/0_map.csv"), row.names = FALSE, na = "")

setwd(wd)

}
