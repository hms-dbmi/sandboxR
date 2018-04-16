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
  map <- read.csv(paste0(mappath, "/0_map.csv"), header = TRUE, na.strings = "", stringsAsFactors = FALSE)
  dir.create(paste0(mappath, "/.oldmaps"), showWarnings = FALSE)
  write.csv(map, paste0(mappath, "/.oldmaps/map_", format(Sys.time(), format = "%Y-%m-%d_%H%M_%Z"), ".csv"), row.names = FALSE, na ="")

  ## Remove empty directories
  system(paste("find", mappath, "-name .DS_Store -type f -delete"))
  system(paste("find", mappath,"-empty -type d -delete"))

  ## Get the pathways for each file in the tree
  treepath <- list.dirs(mappath, recursive = FALSE, full.names = TRUE)
  treepath <- treepath[grepl("_tree", treepath)]
  a <- list.files(treepath, full.names = TRUE, recursive = TRUE)

  a <- parallel::mclapply(list.files(treepath, full.names = TRUE, recursive = TRUE), function(e) {
    return(c(colnames(read.csv(e))[2], substr(e, 1, nchar(e)-4)))
  }, mc.cores = getOption("mc.cores", parallel::detectCores()))

  phv <- sapply(a, "[", 1)
  b <- sapply(a, "[", 2)
  label <- basename(b)

  ## Get the sub-dirs
  newmap <- sapply(1:9, function(e) sapply(strsplit(sub(paste0(treepath, "/"), "", dirname(b)), "/"), "[", e))

  ## Create the new map
  newmap <- data.frame(cbind(phv, label, newmap), stringsAsFactors = FALSE, row.names = NULL)
  map <- merge(map[,1:6], newmap, by.x = "phv", by.y = 1, all = FALSE)
  colnames(map) <- c("phv", "pht", "study_name", "var_desc", "var_study_name", "num_or_char", "data_label", paste0("sd",1:9))
  write.csv(map, paste0(mappath, "/0_map.csv"), row.names = FALSE, na = "")
}
