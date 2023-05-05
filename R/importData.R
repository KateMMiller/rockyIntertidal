#' @title importData: Import views directly from NETN rocky intertidal database
#'
#' @description This function imports flat files from the rocky intertidal database. Each view
#' is added to a ROCKY environment in your workspace, or to your global environment based on whether
#' new_env = TRUE or FALSE. You must have the latest ODBC SQL driver installed for this function to
#' work. It can be downloaded from: https://go.microsoft.com/fwlink/?linkid=2168524
#'
#' @param type Specify whether you are connecting to a DSN or file.
#' \describe{
#' \item{"DSN"}{Default. DSN database. If odbc argument is not specified, will default to rocky_BE.}
#' \item{"file"}{Connects to a backend based on the specified path}}
#'
#' @param DSN DSN name of the database when using type = DSN. If not specified will defaut to "rocky_BE".
#'
#' @param path Quoted path of database backend file, including the name of the backend.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in ROCKY environment. If \code{FALSE}, stores views in global environment
#'
#' @return Assigns database tables to global environment
#'
#' @examples
#' \dontrun{
#'
#' # Import views using DSN
#' importData(type='DSN', DSN="rocky_BE") # this is the same as importData()
#'
#' # Import database in specific folder:
#' importData(type='file', path='./Data/NETN_RockyIntertidal_Database_be_20230120.mdb')
#'
#' }
#'
#' @return Rocky intertidal database views in specified environment
#'
#' @export


importData <- function(type = c('DSN', 'file'), DSN = 'rocky_BE', path = NA, new_env = TRUE){

  # Bug handling
  type <- match.arg(type)
  stopifnot(class(new_env) == 'logical')

  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Check that DSN or specified file exist
  dsn_list <- odbc::odbcListDataSources()

  if(type == 'DSN' & !any(dsn_list$name %in% DSN)){
    stop(paste0("Specified DSN ", DSN, " is not a named database source." ))}

  if(type == "file"){
    if(is.na(path)){stop("Must specify a path to the database for type = file option.")
    } else {
    if(file.exists(path) == FALSE){stop("Specified path or database does not exist.")}}
   }

  if(new_env == TRUE){
    ROCKY <<- new.env()
    env = ROCKY
    } else { env = .GlobalEnv }

  pb = txtProgressBar(min = 0, max = 11, style = 3)

  db <- if (type == 'DSN'){
    db <- DBI::dbConnect(drv = odbc::odbc(), dsn = DSN)
  }
  else if (type == 'file'){
    db <- DBI::dbConnect(drv=odbc::odbc(),
                        .connection_string =
                          paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", path))
  }

  assign("Bolts", DBI::dbReadTable(db, "qryR_FlatFile_Bolts_All"), envir = env)
  setTxtProgressBar(pb, 1)
  assign("Echinoderm_Counts", DBI::dbReadTable(db, "qryR_FlatFile_Echinoderm_Counts_wide"), envir = env)
  setTxtProgressBar(pb, 2)
  assign("Echinoderm_Measurements", DBI::dbReadTable(db, "qryR_FlatFile_Echinoderm_Measurements"), envir = env)
  setTxtProgressBar(pb, 3)
  assign("MotileInvert_Counts", DBI::dbReadTable(db, "qryR_FlatFile_MotileInvert_Counts"), envir = env)
  setTxtProgressBar(pb, 4)
  assign("MotileInvert_Measurements", DBI::dbReadTable(db, "qryR_FlatFile_MotileInvert_Measurements"), envir = env)
  setTxtProgressBar(pb, 5)
  assign("PhotoQuadrats_Cover", DBI::dbReadTable(db, "qryR_FlatFile_PhotoQuadrats_Cover"), envir = env)
  setTxtProgressBar(pb, 6)
  assign("PointIntercept_BoltDist_A", DBI::dbReadTable(db, "qryR_FlatFile_PointIntercept_BoltDist_parta"), envir = env)
  setTxtProgressBar(pb, 7)
  assign("PointIntercept_BoltDist_B", DBI::dbReadTable(db, "qryR_FlatFile_PointIntercept_BoltDist_partb"), envir = env)
  setTxtProgressBar(pb, 8)
  assign("PointIntercept_BoltDist_C", DBI::dbReadTable(db, "qryR_FlatFile_PointIntercept_BoltDist_partc"), envir = env)
  setTxtProgressBar(pb, 9)
  assign("PointIntercept_SppDetections", DBI::dbReadTable(db, "qryR_FlatFile_PointIntercept_SppDetections"), envir = env)
  setTxtProgressBar(pb, 10)
  assign("Event_Notes", DBI::dbReadTable(db, "qryR_FlatFile_Event_Notes"), envir = env)
  setTxtProgressBar(pb, 11)
  # assign("Quadrat_Transect", DBI::dbReadTable(db, "qryR_Quadrat_Transect"), envir = env)
  # setTxtProgressBar(pb, 11)

  DBI::dbDisconnect(db)

  close(pb)

  noquote('database import complete')

}
