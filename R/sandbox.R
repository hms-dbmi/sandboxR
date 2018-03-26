#' @title De-aggregate the variable in the datatables and create the first mapping file
#'
#' @param phs dbGap study ID in the for of phsxxxxxx
#' @param study_name Nickname used for this study. Optional, otherwise will use the phs name
#' @param consent_groups Vector containing the pathways of the folders where the .txt files (or .txt.gz) are located for each consent groups that you want to extract
#' @param tree_dest Pathway to the folder where you want to create the mapping file
#'
#' @return a mapping file (0_map.csv), and a folder (study_tree) with the tree structure of your sandboxes. Also create a hidden file to copy oldmaps (.oldmaps)
#'
#' @description This function extracts informations from .txt.gz files ind dbgap. It will de-agregate the datatables to create one csv file per variable with 2 columns (dbgap_ID and variable_name), and sort them in one folder per datatable. It also creates the first mapping file ("0_map.csv).
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export

sandbox <- function(phs, consent_groups, tree_dest = consent_groups[1], study_name = phs)  {

  ## set some pathways
  wd <- getwd()
  mappath <- paste0(tree_dest, "/", study_name, "_map")
  treepath <- paste0(mappath, "/", study_name, "_tree")
  if (dir.exists(mappath) == FALSE)  dir.create(mappath)
  if (dir.exists(treepath) == FALSE)  dir.create(treepath)

  ##Write the first map
  if (dir.exists(paste0(mappath, "/.oldmaps")) == FALSE)  dir.create(paste0(mappath, "/.oldmaps"))
  map <- data.frame(matrix(ncol = 20))
  count <- 1
  cnames <- c("phv", "study_name", "var_desc", "var_study_name",  "data_label", paste0("sd",1:14), "pathway")
  colnames(map) <- cnames

  #read datatablesdict and variablesidct
  datatablesdict <- datatables.dict(phs)
  variablesdict <- variables.dict(phs)

  ## For each consent groups
  for (i in 1:length(consent_groups))  {

    ## List the text files with the variables data
    setwd(consent_groups[i])
    temp <- list.files(pattern = ".txt", recursive = TRUE)
    ind <- grepl("MULTI.txt", temp)
    g <- temp[!ind]

    # extract the datatables characteristics from the datatablesdict
    for (j in 1:length(g))  {
      v <- read.csv(file = g[j], header = TRUE, sep = "\t", comment.char = "#")
      if (ncol(v) < 2) next
      st_name <- (strsplit(g[j],"\\."))[[1]]
      st_name <- st_name[length(st_name)-3]
      inddt <- which(datatablesdict[2] == st_name)
      pht <- as.character(datatablesdict[inddt, 1])
      st_desc <- as.character(datatablesdict[inddt, 3])
      if (is.null(st_desc))  st_desc <- st_name
      if (nchar(as.character(st_desc)) > 255)  st_desc <- substr(st_desc, 1, 255)
      if (dir.exists(paste0(treepath, "/", st_desc)) == FALSE)  dir.create(paste0(treepath, "/", st_desc), recursive = TRUE)

      # make 1 csv file per variable, with 1st col = dbgapID, 2nd col = variable
      for (k in 3:ncol(v))  {
        namefile <- names(v[k])
        c <- c(1,k)
        output <- data.frame(v[c])
        indvart <- which((variablesdict[2] == st_name) & (variablesdict[3] == names(output[2])))
        phv <- variablesdict[indvart,1]
        data_label <- variablesdict[indvart,4]
        if (nchar(as.character(data_label)) > 230)  data_label <- substr(data_label, 1, 230)
        filepath <- paste0(treepath,"/", gsub("/", "|", st_desc), "/", gsub("/", "|", data_label), " ", phv, ".csv")
        map[count,1] <- as.character(phv)
        map[count,2] <- st_name
        map[count,3] <- as.character(data_label)
        map[count,4] <- namefile
        map[count,5] <- as.character(data_label)
        map[count,6] <- st_desc
        map[count,20] <- paste0("/", gsub("/", "|", st_desc), "/", gsub("/", "|", data_label), " ", phv, ".csv")
        count <- count + 1

        ## Append to an existing file (multiple consent groups)
        if (file.exists(filepath)) {
          output2 <- read.csv(file = filepath, header = TRUE)
          output <- rbind(output, output2)
          output <- unique(output)
        }
        write.csv(output, file = filepath, row.names = FALSE)
      }
    }
  }
  map <- unique(map)
  write.csv(map, paste0(mappath, "/0_map.csv"), row.names = FALSE, na = "")

  setwd(wd)
}
