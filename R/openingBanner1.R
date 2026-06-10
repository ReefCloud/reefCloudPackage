
##############################################################################
## The following function generates an informative banner that displays the ##
## names of various files and paths                                         ##
##############################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
openingBanner1 <- function() {
  ## get the maximum string length
  currentTime = format(Sys.time(),'%d/%m/%Y %H:%M:%S')
    maxStringLength = max(nchar(c(
        AWS_PATH,
        INPUT_DATA,
        DATA_FROM,
        TIER_DATA,
        ## DATA_PROGRAM,
        ## DATA_METHOD,
        ## DATA_TYPE,
        DOMAIN_CATEGORY,
        DOMAIN_NAME,
        DATA_PATH,
        FILENAME,
        INPUT_FORMAT,
        OUTPUT_PATH,
        LOG_FILE,
        currentTime)))
  cat(paste0(
      paste0(rep('*',23 + maxStringLength),collapse=''),'*\n',
    '* Remote data source: ', DATA_FROM,       paste0(rep(' ', maxStringLength-nchar(DATA_FROM)), collapse=''),' *\n',
    '* Remote data path:   ', AWS_PATH,        paste0(rep(' ', maxStringLength-nchar(AWS_PATH)), collapse=''),' *\n',
    '* Domain category:    ', DOMAIN_CATEGORY, paste0(rep(' ', maxStringLength-nchar(DOMAIN_CATEGORY)), collapse=''),' *\n',
    '* Domain name:        ', DOMAIN_NAME,     paste0(rep(' ', maxStringLength-nchar(DOMAIN_NAME)), collapse=''),' *\n',
    '* By Tier:            ', BY_TIER,         paste0(rep(' ', maxStringLength-nchar(BY_TIER)), collapse=''),' *\n',
    '* Input format:       ', INPUT_FORMAT,    paste0(rep(' ', maxStringLength-nchar(INPUT_FORMAT)), collapse=''), ' *\n',
    '* Benthic data name:  ', INPUT_DATA,      paste0(rep(' ', maxStringLength-nchar(INPUT_DATA)), collapse=''),' *\n',
    '* Tier data name:     ', TIER_DATA,       paste0(rep(' ', maxStringLength-nchar(TIER_DATA)), collapse=''),' *\n',
    ## '* Program:         ', DATA_PROGRAM,    paste0(rep(' ', maxStringLength-nchar(DATA_PROGRAM)), collapse=''),' *\n',
    ## '* Method:          ', DATA_METHOD,     paste0(rep(' ', maxStringLength-nchar(DATA_METHOD)), collapse=''),' *\n',
    ## '* Data type:       ', DATA_TYPE,       paste0(rep(' ', maxStringLength-nchar(DATA_TYPE)), collapse=''),' *\n',
    ## '* Domain name:     ', DOMAIN_NAME,     paste0(rep(' ', maxStringLength-nchar(DOMAIN_NAME)), collapse=''),' *\n',
    '* Local data path:    ', DATA_PATH,       paste0(rep(' ', maxStringLength-nchar(DATA_PATH)), collapse=''),' *\n',
    ## '* Benthic filename:   ', FILENAME,        paste0(rep(' ', maxStringLength-nchar(FILENAME)), collapse=''), ' *\n',
    '* Outputs path:       ', OUTPUT_PATH,     paste0(rep(' ', maxStringLength-nchar(OUTPUT_PATH)), collapse=''), ' *\n',
    '* Local log file:     ', LOG_FILE,        paste0(rep(' ', maxStringLength-nchar(LOG_FILE)), collapse=''), ' *\n',
    '* Date:               ', currentTime,     paste0(rep(' ', maxStringLength-nchar(currentTime)), collapse=''), ' *\n',
    paste0(rep('*',23 + maxStringLength + 1),collapse=''),'\n',
    collapse=''
  ))
}
