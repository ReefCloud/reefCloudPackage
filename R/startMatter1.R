


#################################################################################
## The following function performs all of the openning procedures only for the ##
## parent script that is called from Rscripts (not subsequent scripts that are ##
## sourced from the script):                                                   ##
## - check that all the necessary packages can be loaded - if not quit         ##
## - perform any necessary configurations (mainly creating directories if they ##
##   are missing)                                                              ##
## - extract the REEF (reef name) from the commandline arguments and store it  ##
##   in the global environment                                                 ##
#################################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
startMatter1 <- function(){
    parentFile <- FALSE
    ## get the full set of command line arguments
    args <- commandArgs()
    ## determine whether any of them include the name of the R script file
    file <- grep('--file=.*', args)
    ## if they do (indicating that startMatter is being run from the parent script),
    ## extract the filename of the R script
    if (length(file)>0) {
        script <- gsub('--file=(.*)', '\\1', args[4])
        ## only consider it the parentFile if the number of system frames (previous environments)
        ## is equal to one.  if there are more than that, then this function must have been called from
        ## an environment that was itself called from the parent script
        if (sys.nframe()==1) parentFile <- TRUE
    }
    ## only perform the following for the parent script (no children)
    if (parentFile){
        ## Get the command line arguments
        args <- commandArgs(trailing=TRUE)
        if (length(args)==0) {
            cat(paste0('Sorry, you must supply an input path and filename as a commandline argument', '\n'))
            q(save = "no", status = 1)
        } else {
            ## get the full filename of the input file
            arg <- args[1]
            ## parse out the path and filename
            AWS_PATH <<- gsub('(.*)/raw$', '\\1/', dirname(arg))
            if (grepl('.*\\.csv$',args)) {  #if the commandline argument ends in a csv
                FILENAME <<- basename(arg)
                ## parse out the domain unit (e.g. reef, region etc)
                DOMAIN_CATEGORY <<- gsub('.*process/[^/]*/[^/]*/[^/]*/([^/]*)/.*','\\1', AWS_PATH)
                DOMAIN_NAME <<- gsub('.*process/[^/]*/[^/]*/[^/]*/[^/]*/([^/]*)/.*','\\1', AWS_PATH)
                DATA_FROM <<- 'FILE'
            } else {                     #we will have to go to the AIMS database
                ## generate a filename to use for data storage
                FILENAME <<- paste0(basename(arg), ".csv")
                ## Determine what sort of data, and what spatial domain
                DOMAIN_NAME <<- gsub('([a-zA-Z]*-)*(.*)$','\\2', basename(arg))
                DATA_FROM <<- 'DATABASE'
                if(!parseDomain()) {
                    stop('The supplied commandline argument domain name is not valid')
                }
            }
            ## cat(paste0('Working with: ',  DOMAIN_NAME, '\n'))
            ## Get the program (LTMP or MMP)
            DATA_PROGRAM <<- gsub('.*process/([^/]*).*','\\1', AWS_PATH)
            ## Get the method (photo-transect or manta)
            DATA_METHOD <<- gsub('[^/]*//[^/]*/[^/]*/([^/]*)/.*','\\1', AWS_PATH)
            ## Get the type (cover, fish, mpa)
            DATA_TYPE <<- DATA_TYPE #gsub('([^-]*)-([^-]*)-([^-]*)-.*','\\3', FILENAME)
            ## Get internal data path.expand
            DATA_PATH <<- DEFAULT_DATA_PATH

            ## Create input and output names
            CSV_FILE <<- FILENAME
            RDATA_FILE <<- gsub('\\.csv','\\.RData', FILENAME)
            FILENAME <<- gsub('\\.csv', '', FILENAME)

            LOG_FILE <<- paste0(DATA_PATH, 'log/', FILENAME, '.log')
            ## perform any configurations (mainly creating directories if they dont exist)
            source('config.R')

            ## reefCloudPackage::openingBanner()  # Disabled - uses legacy STATUS object

            reefCloudPackage::log('INFO',  logFile = LOG_FILE, '--Modelling routines--',
                          msg = paste0('Running script WITH command line args'))
            ## load all packages and quit if any are missing
            missing <- reefCloudPackage::checkPackages()
            reefCloudPackage::log(ifelse(missing=="", 'FAILURE', 'SUCCESS'), logFile = LOG_FILE,
                          Category = '--Starting matter--',
                          msg = 'Checking that all necessary packages are available')
            if(missing=="") {
                stop(paste('The following required packages are missing: ',paste(missing, collapse=', ')))
                ##q()
            }
        }
    }
}
