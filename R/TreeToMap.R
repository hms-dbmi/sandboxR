#' @title Change your mapping file according to the tree structure
#'
#' @param mappath Pathway to the folder where your mapping file and your tree are located
#'
#' @return A new mapping file "0_map.csv" according to your tree structure. The old map is moved to a hidden folder ".oldmaps"
#'
#' @author Gregoire Versmee, Laura Versmee

#' @export

TreeToMap <- function(mappath)  {

  ## save the old map file, with date and time
  map <- read.csv(paste0(mappath, "/0_map.csv"), header = TRUE, na.strings = "")
  dir.create(paste0(mappath, "/.oldmaps"), showWarnings = FALSE)
  write.csv(map, paste0(mappath, "/.oldmaps/map_", format(Sys.time(), format = "%Y-%m-%d_%H%M_%Z"), ".csv"), row.names = FALSE, na ="")

  ## Remove empty directories
  system(paste("find", mappath,"-empty -type d -delete"))

  ## Get the pathways for each file in the tree
  treepath <- list.dirs(mappath, recursive = FALSE)
  treepath <- treepath[grepl("_tree", treepath)]
  a <- list.files(treepath, full.names = TRUE, recursive = TRUE)
  a <- trimws(sub(paste0(treepath, "/") ,"", a))
  pathways <- a

  ## Get the phv
  regexpr <- regexpr("phv", a)
  phv <- substr(a, regexpr, nchar(a)-4)

  ## Get the data label
  a <- trimws(substr(a, 1, regexpr-1))
  a <- strsplit(a, "/")
  label <- sapply(a, function(e) e[length(e)])

  ## Get the sub-dirs
  a <- sapply(a, function(e) e[-length(e)])
  newmap <- sapply(1:9, function(e) sapply(a, "[", e))

  ## Create the new map
  newmap <- data.frame(cbind(phv, label, newmap, pathways), stringsAsFactors = FALSE)
  map <- merge(map[,1:5], newmap, by.x = "phv", by.y = 1, all = FALSE)
  write.csv(map, paste0(mappath, "/0_map.csv"), row.names = FALSE, na = "")
}
