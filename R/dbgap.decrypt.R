#' @title Decrypt ncbi_enc files
#'
#' @return Decrypt the file(s) and replace it in the same folder
#'
#' @description This function decrypts dbGap files (*ncbi_enc) using your personnal key. Be careful, it can replace the settings of your "vdb-info" file
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export

dbgap.decrypt <- function()  {

## escape regex symbols

message("Where is the file you want to decrypt?")
file <- file.choose()

message("Where is your key?")
key <- file.choose()

file <- gsub(" ", "\\\\ ", file)
key <- gsub(" ", "\\\\ ", key)

wd <- getwd()

# DL and untar sratoolkit for mac
download.file("ftp-trace.ncbi.nlm.nih.gov/sra/sdk/2.8.2-1/sratoolkit.2.8.2-1-mac64.tar.gz", "./sratoolkit.2.8.2-1-mac64.tar.gz")
untar("./sratoolkit.2.8.2-1-mac64.tar.gz")
file.remove("./sratoolkit.2.8.2-1-mac64.tar.gz")

#Reset the sra settings
if (file.exists("~/.ncbi/user-settings.mkfg"))  file.remove("~/.ncbi/user-settings.mkfg")

# import the key
system(paste("./sratoolkit.2.8.2-1-mac64/bin/vdb-config --import", key))
Sys.sleep(1)

#set the wd to the repository
p <- read.table("~/.ncbi/user-settings.mkfg")
repo <- as.character(p[which(grepl("root", p[ ,1])), 3])
setwd(repo)

# decrypt the files
g <- system(paste0(wd, "/sratoolkit.2.8.2-1-mac64/bin/vdb-decrypt ", file))

#clean up your mess!
setwd(wd)
file.remove(repo)
sra <- list.files(pattern = "sratoolkit*")
system(paste0("rm -r ./", sra))
}


