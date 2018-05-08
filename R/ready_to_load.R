#' @author Gregoire Versmee, Laura Versmee
#' @export

readytoload <- function(mappath, short_name, long_name, phs)  {

## Clean up the map
map <- read.csv(paste0(mappath, "/0_map.csv"), stringsAsFactors = FALSE)

## !!!!! Need also to remove the "*"
modif <- apply(map[,c(7:16)], 2, function(e) {
  trimws(gsub("\\*", "", gsub("  ", "", gsub("\\[", "(", gsub("\\]", ")", gsub(":", "-", gsub("%", "percent", gsub("/", "|", gsub("\\\\", "|", e)))))))))

  ## Remove the dots and question marks at the end
  sapply(e, function (f){
    end <- nchar(f)
    if (!is.na(f) & (substr(f, end, end) == "." | substr(f, end, end) == "?"))  return(trimws(gsub("\\*", "", gsub("  ", "", gsub("\\[", "(", gsub("\\]", ")", gsub(":", "-", gsub("%", "percent", gsub("/", "|", gsub("\\\\", "|", substr(f, 1, end-1)))))))))))  else  return(f)
  })
})

map <- cbind(map[,1:6], modif)
#write.csv(map, paste0(mappath, "/0_map.csv"), row.names = FALSE, na = "")

## read the tree
treepath <- list.dirs(mappath, recursive = FALSE)
treepath <- treepath[grepl("_tree", treepath)]

## Decode everything and Check if patients are in the study, and no duplicates
dico <- read.csv(paste0(mappath, "/1_dictionnary.csv"), na.strings = "NA", stringsAsFactors = FALSE)
dico <- dico[!is.na(dico[,3]),]
dico <- dico[!is.na(dico[,2]),]

consent <- read.csv(paste0(mappath, "/2_population.csv"), stringsAsFactors = FALSE)[,1:2]

test <- parallel::mclapply(list.files(treepath, recursive = TRUE, full.names = TRUE), function(e) {
  csv <- read.csv(e, stringsAsFactors = FALSE)
  phv <- colnames(csv)[2]
  code <- map[which(map[,1] == phv),6]
  if (!is.na(code) & !is.null(code) & nrow(csv) > 1) {
    code2 <- dico[which(dico[,1] == code),]
    csv[,2] <- sapply(as.character(csv[,2]), function(r) if (any(r == as.character(code2[,2])))  return(as.character(code2[which(as.character(code2[,2]) == f), 3]))  else  return(NA))
    csv <- csv[(!is.na(csv[,2])) | (sapply(csv[,1], function(f) any(f == consent[,1]))) | (!duplicated(csv[,1])),]
  }
    if (nrow(csv) > 0)  write.csv(csv, e, row.names = FALSE)  else {
      file.remove(e)
      return(phv)
  }
}, mc.cores = getOption("mc.cores", parallel::detectCores()))

flag_empty <- unlist(test)
if (!is.null(flag_empty))  newmap3 <- map[-sapply(flag_empty, function(e)  which(map[,1] == e)),]  else  newmap3 <- map

## Num or char
a <- parallel::mclapply(list.files(treepath, full.names = TRUE, recursive = TRUE), function(e) {
  csv <- read.csv(e, stringsAsFactors = FALSE)
  var <- csv[,2]
  if (length(levels(as.factor(var))) <= 10)  {
    struct <- "cat"
  }  else  {
    if (length(which(is.na(as.numeric(var)))) / length(var) < 0.05) {
      struct <- "num"
      } else  struct <- "cat"
  }
  return(c(colnames(read.csv(e))[2], struct))
}, mc.cores = getOption("mc.cores", parallel::detectCores()))

numORchar <- cbind(sapply(a, "[", 1), sapply(a, "[", 2))
newmap4 <- merge(newmap3, numORchar, by.x = 1, by.y = 1)


## If num, remove not-num character
test <- parallel::mclapply(list.files(treepath, recursive = TRUE, full.names = TRUE), function(e) {
  csv <- read.csv(e, stringsAsFactors = FALSE)
  phv <- colnames(csv)[2]
  struct <- newmap4[which(newmap4[,1] == phv),17]
  if (struct == "num") {
    csv <- csv[!is.na(as.numeric(csv[,2])),]
    if (nrow(csv) > 0)  write.csv(csv, e, row.names = FALSE)  else {
      file.remove(e)
      return(phv)
    }
  }
}, mc.cores = getOption("mc.cores", parallel::detectCores()))

flag_empty <- unlist(test)
if (!is.null(flag_empty))  newmap6 <- newmap4[-sapply(flag_empty, function(e)  which(newmap4[,1] == e)),]  else  newmap6 <- newmap4


## Remove empty directories
system(paste("find", treepath, "-name .DS_Store -type f -delete"))
system(paste("find", mappath,"-empty -type d -delete"))


## Create the final map
## append the population table
date <- as.character(Sys.Date())
population <- as.matrix(read.csv(paste0(mappath, "/2_population.csv")))
population[is.na(population)] <- "NA"
population <- data.frame(population)
phv <- paste0(short_name, "_", colnames(population[2:5]))
NAME_CHAR = c(levels(as.factor(population$CONSENT)), "age", levels(as.factor(population$sex)), levels(as.factor(population$race)))
population_cd <- cbind(phv = paste0(short_name, "_", NAME_CHAR),
                       CONCEPT_CD = paste0(1:length(NAME_CHAR), "_", short_name),
                       CONCEPT_PATH = paste0("\\", long_name, "\\00. population\\", c(paste0("consent_groups\\", levels(as.factor(population$CONSENT))), "age", paste0("gender\\", levels(as.factor(population$sex))), paste0("race\\", levels(as.factor(population$race))))),
                       NAME_CHAR = NAME_CHAR,
                       CONCEPT_BLOB = NA, UPDATE_DATE = date, DOWNLOAD_DATE = date, IMPORT_DATE = date, SOURCESYSTEM_CD = short_name, UPLOAD_ID = NA, TABLE_NAME = "CONCEPT_DIMENSION")

## Create the map
tree <- list.files(treepath, recursive = TRUE, full.names = TRUE)
blob <- parallel::mclapply(tree, function(e) {
  csv <- read.csv(e, stringsAsFactors = FALSE)
  phv <- colnames(csv)[2]
  rowmap <- which(newmap6[,1] == phv)
  concept_path <- paste0("\\", long_name, "\\", apply(newmap6[rowmap,c(8:16,7)], 1, function(f) gsub("\\\\\\\\", "", gsub("\\\\NA", "", paste0(f, collapse = "\\")))), "\\")
  if (newmap6[rowmap,17] == "cat") {
    concept_path <- paste0(concept_path, "\\", levels(as.factor(csv[,2])))
    return(data.frame(cbind(phv = phv, CONCEPT_PATH = concept_path, NAME_CHAR = levels(as.factor(csv[,2])))))
  } else {
    return(data.frame(cbind(phv = phv, CONCEPT_PATH = concept_path, NAME_CHAR = newmap6[rowmap,7])))
  }
}, mc.cores = getOption("mc.cores", parallel::detectCores()))

final_map <- data.table::rbindlist(blob)
final_map <- cbind(final_map[,1], CONCEPT_CD = paste0((length(NAME_CHAR)+1):(nrow(final_map)+(length(NAME_CHAR))), "_", short_name),
                   final_map[,2:3], CONCEPT_BLOB = NA, UPDATE_DATE = date, DOWNLOAD_DATE = date, IMPORT_DATE = date, SOURCESYSTEM_CD = short_name, UPLOAD_ID = NA, TABLE_NAME = "CONCEPT_DIMENSION")
#final_map <- rbind(population_cd, final_map)


## Create observation table
## append the population table
#glop <- lapply(c(2,4,5), function(e)  {
glop <- lapply(c(2), function(e)  {
  csv <- population[,c(1,e)]
  CONCEPT_CD <- sapply(1:nrow(csv), function (f) {
  return(population_cd[which(population_cd[,4] == csv[f,2]), 2])
  })
  return(data.frame(cbind(phv = paste0(short_name, "_", csv[,2]), PATIENT_NUM = paste0(csv[,1], phs), CONCEPT_CD = CONCEPT_CD, VALTYPE_CD = "T", TVAL_CHAR = csv[,2], NVAL_NUM = NA), row.names = NULL))
}
)

#csv <- population[,c(1,3)]
#CONCEPT_CD <- population_cd[which(population_cd[,4] == "age"), 2]
#paglop <- cbind(phv = paste0(short_name, "_age"), PATIENT_NUM = paste0(csv[,1], phs), CONCEPT_CD = CONCEPT_CD, VALTYPE_CD = "N", TVAL_CHAR = "E", NVAL_NUM = csv[,2])

#population_obs <- rbind(data.table::rbindlist(glop), paglop)
population_obs <- data.table::rbindlist(glop)

## Create table
mcl <- parallel::mclapply(list.files(treepath, recursive = TRUE, full.names = TRUE), function(e) {
  message(e)
  csv <- read.csv(e, stringsAsFactors = FALSE)
  phv <- colnames(csv)[2]
  rowmap <- which(newmap6[,1] == phv)
  if (newmap6[rowmap,17] == "cat") {
    rowind <- which(final_map[,1] == phv)
    ind <- final_map[rowind, c(2,4)]
    ind2 <- sapply(csv[,2], function (f)  which(ind[,2] == f))
    return(data.frame(phv = phv, PATIENT_NUM = paste0(csv[,1], phs), CONCEPT_CD = ind[ind2,1], VALTYPE_CD = "T", TVAL_CHAR = csv[,2], NVAL_NUM = NA))
  } else {
    rowind <- which(final_map[,1] == phv)
    return(data.frame(phv = phv, PATIENT_NUM = paste0(csv[,1], phs), CONCEPT_CD = final_map[rowind, 2], VALTYPE_CD = "N", TVAL_CHAR = "E", NVAL_NUM = csv[,2]))
  }
}, mc.cores = 8)

observations <- data.table::rbindlist(mcl)
#observations <- rbind(population_obs, data.table::rbindlist(mcl))


## Create the patient_dimension table
patient_dim <- cbind(PATIENT_NUM = paste0(population[,1], phs),
                     VITAL_STATUS_CD = NA,
                     BIRTH_DATE = NA,
                     DEATH_DATE = NA,
                     SEX_CD = as.character(population[,4]),
                     AGE_IN_YEARS_NUM = population[,3],
                     LANGUAGE_CD = NA,
                     RACE_CD = as.character(population[,5]),
                     MARITAL_STATUS_CD = NA, RELIGION_CD = NA, ZIP_CD = NA, STATECITYZIP_PATH = NA,
                     UPDATE_DATE = date, DOWNLOAD_DATE = date, IMPORT_DATE = date,
                     SOURCESYSTEM_CD = short_name,
                     UPLOAD_ID = NA,
                     PATIENT_BLOB = NA,
                     INCOME_CD = NA)

dir.create(paste0(mappath, "/.loading"), showWarnings = FALSE)
write.csv(final_map[,2:11], paste0(mappath, "/.loading/concept_dimension.csv"), row.names = FALSE, na = "")
write.csv(observations[,2:6], paste0(mappath, "/.loading/observation_fact.csv"), row.names = FALSE, na = "")
write.csv(patient_dim, paste0(mappath, "/.loading/patient_dimension.csv"), row.names = FALSE, na = "")



write.csv(final_map[,2:5], paste0(mappath, "/.loading/patient_mapping_table.csv"), row.names = FALSE, na = "")
write.csv(final_map[,2:3], paste0(mappath, "/.loading/patient_trial.csv"), row.names = FALSE, na = "")
write.csv(final_map[,2:8], paste0(mappath, "/.loading/i2b2_table.csv"), row.names = FALSE, na = "")
write.csv(final_map[,2:10], paste0(mappath, "/.loading/i2b2_secure.csv"), row.names = FALSE, na = "")
write.csv(final_map[,2:4], paste0(mappath, "/.loading/table_access.csv"), row.names = FALSE, na = "")
}

