#' @title De-aggregate the variable in the datatables and create the first mapping file
#'
#' @param phs dbGap study ID in the for of phsxxxxxx
#' @param study_name Nickname used for this study. Optional, otherwise will use the phs name
#' @param files Vector containing the pathways of the folders where the .txt files (or .txt.gz) are located for each consent groups that you want to extract
#' @param destination Pathway to the folder where you want to create the mapping file
#'
#' @return a mapping file (0_map.csv), and a folder (study_tree) with the tree structure of your sandboxes. Also create a hidden file to copy oldmaps (.oldmaps)
#'
#' @description This function extracts informations from .txt.gz files ind dbgap. It will de-agregate the datatables to create one csv file per variable with 2 columns (dbgap_ID and variable_name), and sort them in one folder per datatable. It also creates the first mapping file ("0_map.csv).
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export
#' @import parallel
#' @import XML
#' @import rlist
#' @import RCurl

dbgap.expand <- function(phs, files, destination, study_name = phs)  {

  ### set some pathways
  ## Create the destination folder
  mappath <- paste0(destination, "/", study_name)
  dir.create(mappath, showWarnings = FALSE)

  ## Create the tree folder
  treepath <- paste0(mappath, "/", study_name, "_tree")
  dir.create(treepath, showWarnings = FALSE)

  ## Create the .oldmaps folder
  oldmappath <- paste0(mappath, "/.oldmaps")
  dir.create(paste0(mappath, "/.oldmaps"), showWarnings = FALSE)

  ## Get the last version of the study
  phs <- phs.version(phs)


  ### Get xml files
  url<- paste0("ftp://anonymous:anonymous@ftp.ncbi.nlm.nih.gov/dbgap/studies/", unlist(strsplit(phs, "\\."))[1], "/", phs, "/")

  ## selecting all xml files except for "Subject", "Sample", "Pedigree", and phenotypics data from substudies
  filenames <- strsplit(RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n")[[1]]
  phenodir <- paste0(url, filenames[grep("pheno", filenames)], "/")
  filelist <- strsplit(RCurl::getURL(phenodir, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\n")[[1]]
  filelist <- filelist[(grepl(".data_dict.xml", filelist)) & (!grepl("Sample_Attributes.data_dict.xml", filelist)) &
                     (!grepl("Subject.data_dict.xml", filelist)) & (!grepl("Sample.data_dict.xml", filelist)) & (!grepl("Subject_Images.data_dict.xml", filelist)) & (!grepl("Pedigree.data_dict.xml", filelist))]


  ### Get the number of cores of the instance
  ncores <- getOption("mc.cores", parallel::detectCores())

  #### Extract informations from the xml files
  mcl <- lapply(filelist, function(e) {
    xmllist <- XML::xmlToList(RCurl::getURLContent(paste0(phenodir, e)))

    ### Manage questionnaires
    ## Get the questionnaire id (pht) from the xml table
    pht <- strsplit(xmllist[[".attrs"]][["id"]], "\\.")[[1]][1]

    ## Get the questionnaire name (study id) from the name of the xml file
    dt_sn <- substr(e, regexpr(pht, e) + nchar(pht)+4, regexpr(".data_dict", e)-1)

    ## Get the questionnaire description from the xml table
    st_desc <- xmllist[["description"]]

    ## Map the description to the questionnaire id (if desc exists)
    if (is.null(st_desc)) st_desc <- dt_sn
    if (nchar(as.character(st_desc)) > 255)  st_desc <- substr(st_desc, 1, 255)

    ## Create one folder per questionnaire
    dir.create(paste0(treepath, "/", st_desc), recursive = TRUE, showWarnings = FALSE)

    ### Manage variables
    ## Get the variables list
    variables <- xmllist[names(xmllist) == "variable"][-1]

    ## Extract informations from each variable
    list_var <- cbind(phv = sapply(variables, "[", ".attrs", USE.NAMES = FALSE),
                      var_name = sapply(variables, "[", "name", USE.NAMES = FALSE),
                      var_desc = sapply(variables, "[", "description", USE.NAMES = FALSE),
                      key = sapply(variables, function(f) if (length(f[names(f) == "value"]) > 0) return("["(f, "name")) else return(NA)))
    list_var <- data.frame(cbind(pht = pht, dt_sn = dt_sn, st_desc = st_desc, list_var), row.names = NULL)
    list_var[,7] <- sub("NA", NA, list_var[,7])

    list_dict <- lapply(variables, function(f) {
      extract <- f[names(f) == "value"]
      if (length(extract) > 0) {
        dico <- data.frame(cbind(key = "["(f, "name"), code = sapply(extract, "[", ".attrs", USE.NAMES = FALSE), text = sapply(extract, "[", "text", USE.NAMES = FALSE)), row.names = NULL)
        return(dico)
      } else return(NULL)
    })

    list_dict <- list_dict[!sapply(list_dict, is.null)]

    return(list(list_var = list_var, list_dict = list_dict))
  })

  total_var <- unlist(lapply(mcl, "[", "list_var"), recursive = FALSE)
  total_dict <- unlist(lapply(mcl, "[", "list_dict"), recursive = FALSE)
  total_dict <- unlist(total_dict[sapply(total_dict, function(e) if (length(e) > 0) return(TRUE) else return(FALSE))], recursive = FALSE)

  ### write the first map
  map <- data.table::rbindlist(total_var)[,c(4,1,6,1,7,6,3)]
  map <- data.frame(apply(map,2, as.character))
  map[,6] <- substr(map[,6], 1, 230)
  map[,c(4, 8:15)] <- NA
  colnames(map) <- c("variable_id", "questionnaire_id", "variable_original_name", "num_or_char", "code_key", "data_label", paste0("sd",1:9))

  ### write the first dictionnary
  dict <- data.table::rbindlist(total_dict)

  ## Get all the txt.gz files fro the given study, and the subject file
  g <- as.character(sapply(files, list.files, pattern = ".txt.gz", recursive = TRUE, full.names = TRUE))
  cgfile <- g[grepl("Subject.MULTI.txt.gz", g)][1]
  g <- g[(!grepl("Sample_Attributes", g)) & (!grepl("MULTI.txt.gz", g)) & (!grepl("Subject_Images", g))]

  ## copy the original files to a hidden folder
  filespath <- paste0(mappath, "/.files")
  dir.create(filespath, showWarnings = FALSE)
  file.copy(g, paste0(filespath, "/", basename(g)))
  file.copy(cgfile, paste0(filespath, "/", basename(cgfile)))

  ## Define function expand
  expand <- function(j, v, listmcl) {
    df <- v[!(is.na(v[,j]) | v[,j] == "NA"),c(1,j)]
    colnames(df) <- c("dbGaP_ID", as.character(listmcl[j-2, 1]))
    filepath <- paste0(treepath, "/", gsub("/", "|", listmcl[j-2,6]), "/", gsub("/", "|", listmcl[j-2,5]), " ", listmcl[j-2,1], ".csv")
    list_df <- list(list(df))
    names(list_df) <- filepath
    return(list_df)
  }

  ### Expand everything
  total_exp <- parallel::mclapply(g, function(e) {
    v <- read.csv(file = e, header = TRUE, sep = "\t", comment.char = "#", na.strings = "")
    nvar <- ncol(v)
    if (nvar < 3) return(NULL)
    else {
      g_pht <- regexpr("pht", e)
      g_pht <- substr(e, g_pht, g_pht+8)
      listmcl <- map[map[,2] == g_pht,-4]
      exp <- lapply(3:nvar, expand, v=v, listmcl=listmcl)
    }
    return(unlist(exp, recursive = FALSE))
  }, mc.cores = ncores)

  total_exp <- unlist(unlist(total_exp, recursive = FALSE), recursive = FALSE)
  unames <- unique(names(total_exp))

  ## Merge the tables by name
  total_exp <- lapply(unames, function(e) {
    Reduce(rbind, total_exp[which(names(total_exp) == e)])
  })

  names(total_exp) <- unames

  ## Omit "phv" in the name if unique
  rename <- names(total_exp)
  rename <- substr(rename, 1, regexpr(" phv", rename) -1)
  c <- which(rename %in% duplicated(rename) == FALSE)
  names(total_exp)[c] <- paste0(rename[c], ".csv")

  #### nead to deal with duplicates and write the tree
  parallel::mcmapply(function(e, f) {
    f <- data.table::setDT(f)[, ENCOUNTER := seq_len(.N), by = c(colnames(f)[1])]

    lapply(2:(length(colnames(f))-1), function(x) {

      test <- data.table::data.table(cbind(f[,1], f[,..x], f[,"ENCOUNTER"]))
      test <- na.omit(test, 2)
      data.table::fwrite(test, e)
    })

  }, names(total_exp), total_exp, mc.cores = ncores)

  ## Remove empty directories in the tree
  system(paste("find", treepath, "-name .DS_Store -type f -delete"))
  system(paste("find", treepath, "-empty -type d -delete"))

  ## order the map and write it
  map <- map[with(map, order(sd1, sd2, sd3, sd4, sd5, sd6, sd7, sd8, sd9, data_label)),]
  data.table::fwrite(map, paste0(mappath, "/0_map.csv"))

  ## write the dictionnary table
  colnames(dict) <- c("key", "code", "value")
  data.table::fwrite(dict, paste0(mappath, "/1_dictionnary.csv"))

  ## write the population table
  pop <- read.csv(file = cgfile, header = TRUE, sep = "\t", comment.char = "#", na.strings = "")[,c(1,3)]
  pop <- pop[!(pop[,2] == 0),]
  pop <- cbind(pop, age = NA, gender = NA, race = NA)
  colnames(pop) <- c("dbGaP_Subject_ID", "consent_group", "age", "sex", "race")
  data.table::fwrite(pop, paste0(mappath, "/2_population.csv"))
}
