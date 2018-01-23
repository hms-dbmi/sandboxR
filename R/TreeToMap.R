#' @title Change your mapping file according to the tree structure
#'
#' @param mappath Pathway to the folder where your mapping file and your tree are located
#'
#' @return A new mapping file "0_map.csv" according to your tree structure. The old map is moved to a hidden folder ".oldmaps"
#'
#' @author Gregoire Versmee, Laura Versmee

#' @export

TreeToMap <- function(mappath)  {

  wd <- getwd()

  ## save the old map file, with date and time
  oldmap <- paste0(mappath, "/0_map.csv")
  if (dir.exists(paste0(mappath, "/.oldmaps")) == FALSE)  dir.create(paste0(mappath, "/.oldmaps"))
  file.copy(oldmap, paste0(mappath, "/.oldmaps/map_", format(Sys.time(), format = "%Y-%m-%j %H%M %Z"), ".csv"))

  ## Remove empty directories
  setwd(mappath)
  system("find . -empty -type d -delete")

  ## Get thhe pathways for each file in the tree
  foo <- grepl("_tree", list.dirs(mappath, recursive = FALSE, full.names = FALSE))
  treepath <- list.dirs(mappath, recursive = FALSE)[foo]
  a <- list.files(treepath, full.names = TRUE, recursive = TRUE)
  a <- gsub(paste0(treepath, "/"), "", a)
  b <- strsplit(a, "/")

  ## Create the data.frame
  new <- data.frame(matrix(ncol = 17))
  cnames <- c("phv", "data_label", paste0("sd",1:14), "pathway")
  colnames(new) <- cnames

  ## Looping
  for (i in 1:length(b))  {
    dim <- length(b[[i]]) - 1
    end <- dim + 1
    nb_col <- 1:end
    file1 <- unlist(strsplit(b[[i]][[length(b[[i]])]], " "))
    file2 <- file1[length(file1)]
    phv <- substr(file2, 1, regexpr(".csv", file2)-1)
    data_label <- substr(b[[i]][[length(b[[i]])]], 1, regexpr(phv, b[[i]][[length(b[[i]])]]) - 2)
    new[i,1] <- phv
    new[i,2] <- data_label
    b[[i]][[end]] <- NA
    new[i,nb_col + 2] <- b[[i]]
    new[i,17] <- a[i]
  }

  old <- read.csv(oldmap, header = TRUE)
  old <- old[1:4]

  map <- merge(old, new, all.y = TRUE, by.x = "phv", by.y = "phv")

  write.csv(map, paste0(mappath, "/0_map.csv"), row.names = FALSE, na = "")

  setwd(wd)
}

