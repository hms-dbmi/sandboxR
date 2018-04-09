#' @title Change the structure of your tree folder using your mapping file
#' @param mappath Pathway to the folder where your mapping file and your tree are located
#' @return Move and rename the files in the tree folder according to your mapping file
#' @author Gregoire Versmee, Laura Versmee
#' @export
#' @import parallel

MapToTree <- function(mappath)  {

  ## Read and save the old map
  map <- read.csv(paste0(mappath, "/0_map.csv"), stringsAsFactors = FALSE, na.strings = "")
  write.csv(map, paste0(mappath, "/.oldmaps/map_", format(Sys.time(), format = "%Y-%m-%d_%H%M_%Z"), ".csv"), row.names = FALSE, na ="")

  ## Get the actual paths
  treepath <- list.dirs(mappath, recursive = FALSE)
  treepath <- treepath[grepl("_tree", treepath)]

  a <- parallel::mclapply(list.files(treepath, full.names = TRUE, recursive = TRUE), function(e) {
    return(c(colnames(read.csv(e))[2], e))
  }, mc.cores = getOption("mc.cores", parallel::detectCores()))
  a <- cbind(sapply(a, "[", 1), sapply(a, "[", 2))

  ## Get the new paths
  newpath <- cbind(map[,1],
                   paste0(treepath, "/",
                            apply(map[,8:16], 1, function(e) gsub("/NA", "", paste0(e, collapse = "/")))),
                   paste0(map[,7], " ",  map[,1], ".csv"))

  merge <- merge(a, newpath, by.x = 1, by.y =1)
  merge <- as.matrix(merge)

  ## Create the new dirs
  parallel::mclapply(merge[,3], dir.create, showWarnings = FALSE, recursive = TRUE, mc.cores = getOption("mc.cores", parallel::detectCores()))

  ## rename the files
  parallel::mclapply(1:nrow(merge), function(e) {
    file.rename(merge[e,2], paste0(merge[e,3], "/", merge[e,4]))
    }, mc.cores = getOption("mc.cores", parallel::detectCores()))

  ## remove the phv part of the title if possible
  lapply(list.files(mappath, recursive = TRUE, full.names = TRUE), function(e) {
    to <- paste0(substr(e, 1, regexpr(" phv", e) -1), ".csv")
    if (!file.exists(to))  file.rename(e, to)
  })

  ## Remove empty directories in the tree
  system(paste("find", treepath, "-name .DS_Store -type f -delete"))
  system(paste("find", treepath, "-empty -type d -delete"))
}
