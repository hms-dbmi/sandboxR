#' @title Change the structure of your tree folder using your mapping file
#' @param mappath Pathway to the folder where your mapping file and your tree are located
#' @return Move and rename the files in the tree folder according to your mapping file
#' @author Gregoire Versmee, Laura Versmee
#' @export
#' @import parallel
#' @import stringi

MapToTree <- function(mappath)  {

  ncores <- parallel::detectCores()

  ## Read and save the old map
  map <- read.csv(paste0(mappath, "/0_map.csv"), stringsAsFactors = FALSE, na.strings = "")
  write.csv(map, paste0(mappath, "/.oldmaps/map_", format(Sys.time(), format = "%Y-%m-%d_%H%M_%Z"), ".csv"), row.names = FALSE, na ="")

  ## Replace missing label by old variable name
  map[,6][is.na(map[,6])] <- map[,3][is.na(map[,6])]

  ## Replace forbidden characters
  for(i in 6:15) {
    map[,i] <- gsub("/", "|", gsub("\\\\", "|", gsub("\\{", "(", gsub("\\}", ")", map[,i]))))
  }

  ## Get the actual paths
  treepath <- list.dirs(mappath, recursive = FALSE)
  treepath <- treepath[grepl("_tree", treepath)]

  a <- parallel::mclapply(list.files(treepath, full.names = TRUE, recursive = TRUE), function(e) {
    return(c(colnames(read.csv(e))[2], e))
  }, mc.cores = ncores)
  a <- cbind(sapply(a, "[", 1), sapply(a, "[", 2))

  ## Get the new paths
  newpath <- cbind(map[,1],
                   gsub("/NA", "",
                        file.path(treepath, apply(map[,7:15], 1, function(e) paste0(strtrim(e, 240), collapse = "/")))),
                   paste0(strtrim(map[,6], 240), " ",  map[,1], ".csv"))

  merge <- as.matrix(merge(a, newpath, by.x = 1, by.y =1))

  ## Create the new dirs
  parallel::mclapply(merge[,3], dir.create, showWarnings = FALSE, recursive = TRUE, mc.cores = ncores)

  ## rename the files
  lapply(1:nrow(merge), function(e) {
    file.rename(merge[e,2], file.path(merge[e,3], merge[e,4]))
    })

  ## remove the phv part of the title if possible
  lapply(list.files(treepath, recursive = TRUE, full.names = TRUE), function(e) {
    to <- paste0(substr(e, 1, nchar(e) - regexpr(" ", stringi::stri_reverse(e))), ".csv")
    if (!file.exists(to))  file.rename(e, to)
  })

  ## Remove empty directories in the tree
  system(paste("find", treepath, "-name .DS_Store -type f -delete"))
  system(paste("find", treepath, "-empty -type d -delete"))
}
