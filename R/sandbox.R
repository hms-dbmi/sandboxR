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
#' @import parallel
#' @import XML
#' @import RCurl

sandbox <- function(phs, consent_groups, tree_dest = consent_groups[1], study_name = phs)  {

  ## set some pathways
  mappath <- paste0(tree_dest, "/", study_name, "_map")
  treepath <- paste0(mappath, "/", study_name, "_tree")
  dir.create(mappath, showWarnings = FALSE)
  dir.create(treepath, showWarnings = FALSE)

  dir.create(paste0(mappath, "/.oldmaps"), showWarnings = FALSE)

  phs <- phs.version(phs)


  #selecting all xml files except for "Subject", "Sample", "Pedigree", and phenotypics data from substudies
  url<- paste0("ftp://anonymous:anonymous@ftp.ncbi.nlm.nih.gov/dbgap/studies/", unlist(strsplit(phs, "\\."))[1], "/", phs, "/")

  # create the map frame
  filenames <- strsplit(RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n")[[1]]
  phenodir <- paste0(url, filenames[grep("pheno", filenames)], "/")
  filelist <- strsplit(RCurl::getURL(phenodir, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\n")[[1]]
  temp <- filelist[(grepl(".data_dict.xml", filelist)) & (!grepl("Sample_Attributes.data_dict.xml", filelist)) &
                     (!grepl("Subject.data_dict.xml", filelist)) & (!grepl("Sample.data_dict.xml", filelist)) & (!grepl("Pedigree.data_dict.xml", filelist))]

  mcl <- parallel::mclapply(temp, function(e) {
    message(e)
    xmllist <- XML::xmlToList(RCurl::getURLContent(paste0(phenodir, e)))
    pht <- strsplit(xmllist[[".attrs"]][["id"]], "\\.")[[1]][1]
    dt_sn <- substr(e, regexpr(pht, e) + nchar(pht)+4, regexpr(".data_dict", e)-1)
    st_desc <- xmllist[["description"]]
    if (is.null(st_desc)) st_desc <- dt_sn
    if (nchar(as.character(st_desc)) > 255)  st_desc <- substr(st_desc, 1, 255)
    dir.create(paste0(treepath, "/", st_desc), recursive = TRUE, showWarnings = FALSE)
    xmllist <- xmllist[names(xmllist) == "variable"][-1]
    l <- cbind(phv = sapply(xmllist, "[", ".attrs", USE.NAMES = FALSE), var_name = sapply(xmllist, "[", "name", USE.NAMES = FALSE), var_desc = sapply(xmllist, "[", "description", USE.NAMES = FALSE))
    l <- data.frame(cbind(pht = pht, dt_sn = dt_sn, st_desc = st_desc, l))

    return(l)

  }, mc.cores = getOption("mc.cores", parallel::detectCores()))


  # write the first map
  map <- data.table::rbindlist(mcl)[,c(4,1,2,6,5,1,6,3)]
  map <- data.frame(apply(map,2, as.character))
  map[,7] <- substr(map[,4], 1, 230)
  map[,c(6,9:16)] <- NA
  colnames(map) <- c("phv", "pht", "study_name", "var_desc", "var_study_name", "num_or_char", "data_label", paste0("sd",1:9))

  ## For each consent groups
  for (i in 1:length(consent_groups))  {
    g <- list.files(path = consent_groups[i], pattern = ".txt.gz", recursive = TRUE, full.names = TRUE)
    g <- g[(!grepl("Sample_Attributes", g)) & (!grepl("MULTI.txt.gz", g))]

    parallel::mclapply(g, function(e) {
      v <- read.csv(file = e, header = TRUE, sep = "\t", comment.char = "#")
      if (ncol(v) > 2) {
        g_pht <- regexpr("pht", e)
        g_pht <- substr(e, g_pht, g_pht+8)
        listmcl <- map[map[,2] == g_pht,]

        # make 1 csv file per variable, with 1st col = dbgapID, 2nd col = variable (with phv name)
        for (j in 3:ncol(v))  {
         df <- v[!is.na(v[,j]),c(1,j)]
         colnames(df) <- c("dbGaP_ID", as.character(listmcl[j-2, 1]))
         filepath <- paste0(treepath, "/", gsub("/", "|", listmcl[j-2,8]), "/", gsub("/", "|", listmcl[j-2,7]), " ", listmcl[j-2,1], ".csv")
         write.csv(df, file = filepath, row.names = FALSE, append = TRUE)
        }
      }
    }, mc.cores = getOption("mc.cores", parallel::detectCores()))
  }

  ## remove the phv part of the title if possible
  lapply(list.files(mappath, recursive = TRUE, full.names = TRUE), function(e) {
    to <- paste0(substr(e, 1, regexpr(" phv", e) -1), ".csv")
    if (!file.exists(to))  file.rename(e, to)
  })

  write.csv(map, paste0(mappath, "/0_map.csv"), row.names = FALSE, na = "")
}
