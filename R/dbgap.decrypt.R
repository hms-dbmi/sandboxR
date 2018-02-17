#' @title Decrypt ncbi_enc files
#'
#' @return Decrypt the file(s) and replace it in the same folder
#' @param file file or folder where your encrypted files are located
#'
#' @description This function decrypts dbGap files (*ncbi_enc) using your personnal key. Be careful, it can replace the settings of your "vdb-info" file
#'
#' @author Gregoire Versmee, Laura Versmee
#' @export

dbgap.decrypt <- function(file, key){
    ## file paths cleaning and quoting
    file = shQuote( normalizePath( file ))
    key  = shQuote( normalizePath( key ))

    ## get the R temp dir (for the decryption tool)
    config.dir = tempdir()
    
    ## import the key and tell vdb what config dir we want to use
    system2("vdb-config",c("--import", key, config.dir))

    ## this is required for the decryption program to run without error
    setwd(config.dir)

    ## decrypt the files
    g <- system2("vdb-decrypt", file)
}
