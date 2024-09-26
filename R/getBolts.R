#' @title getBolts: get bolt data
#'
#' @importFrom dplyr filter mutate select
#'
#' @description This function filters bolt data by park, site, plot type or plot name.
#'
#' @param park Include data from all parks, or choose one.
#' \describe{
#' \item{'all'}{Includes all parks monitored in the network}
#' \item{'ACAD'}{Includes only sites in Acadia National Park}
#' \item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
#' }
#'
#' @param site Include data from all sites, or choose specific sites based on site code.
#' \describe{
#' \item{'all'}{Includes all sites returned by other filter arguments in function}
#' \item{"BASHAR"}{Bass Harbor, ACAD}
#' \item{"LITHUN"}{Little Hunter, ACAD}
#' \item{"LITMOO"}{Little Moose, ACAD}
#' \item{"OTTPOI"}{Otter Point, ACAD}
#' \item{"SCHPOI"}{Schoodic Point, ACAD}
#' \item{"SHIHAR"}{Ship Harbor, ACAD}
#' \item{"CALISL"}{Calf Island, BOHA}
#' \item{"GREISL"}{Green Island, BOHA}
#' \item{"OUTBRE"}{Outer Brewster, BOHA}
#' }
#'
#' @param plotType Type of plot to filter. Options include:
#' c("all", "Band Transect", "Benchmark", "Photoplot", "Point Intercept Transect",
#' "Recruitment Plot", "Reference", "Temperature Logger")
#'
#' @param plotName Filter on plot name. Options include:
#'   c("A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5", "BM",
#'     "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
#'     "R1", "R2", "R3", "R4", "R5", "REF", "S1", "S2", "S3", "S4", "S5",
#'     "T1", "T2", "T3",
#'     "TEMP1", "TEMP1 ", "TEMP2", "TEMP3",
#'     "U1", "U2", "U3", "U4", "U5", "X1", "X2", "X3")
#'
#' @param community Filter on target community type. Note that "all_records" returns all options, whereas
#' "All" is part of the CommunityType lookup, and will return records where CommunityType == "All".
#'  Options include:
#' c("all_records", "All", "Ascophyllum", "Barnacle", "Echinoderms", "Fucus", "Mussel",
#' "Red Algae", "Not Applicable")
#'
#' @param coords Specify which GPS coordinates to return, Lat/Long, UTM Zone 19N, or both. Options:
#' c("latlong", "utm", "all")
#'
#' @param dropNA Logical. If TRUE (default), blank X/Y coordinates and elevations are removed.
#' If FALSE, all records are returned.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default, returns all records except blank X/Y, or elevations
#' bolts <- getBolts()
#'
#' # Return photo plot bolts for all sites in BOHA
#' boha_photo <- getBolts(park = "BOHA", plotType = "Photoplot")
#'
#' # Return bolts for algal only in ACAD's Ship Harbor and Bass Harbor
#' acad_alg <- getBolts(site = c("BASHAR", "SHIHAR"),
#'   species = c("Ascophyllum", "Fucus", "Red Algae"))
#'
#' # Return bolts for barnacle recruitment plots in ACAD
#' acad_barn <- getBolts(park = "ACAD")
#'
#' # Return bolts for summer barnacle recruitment photoplots at Little Hunter
#' lihu_sb <- getBolts(site = "LITHUN", plotName = c("S1", "S2", "S3", "S4", "S5"))
#'
#'
#' # Return bolts for point intercept transects in Outer Brewster
#' ob_tr <- getBolts(site = "OUTBRE", plotType = "Point Intercept Transect")
#'
#' }
#'
#'
#' @return Returns a data frame of bolt data filtered by function arguments
#' @export

getBolts <- function(park = "all", site = "all", plotType = "all",
                     plotName = "all", community = "all_records", coords = c("all"),
                     dropNA = TRUE){

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotType %in% c("all", "Band Transect", "Benchmark", "Photoplot", "Point Intercept Transect",
    "Recruitment Plot", "Reference", "Temperature logger"))

  stopifnot(community %in% c("all_records", "All", "Ascophyllum", "Barnacle", "Echinoderms", "Fucus",
                           "Mussel", "Red Algae", "Not Applicable"))
  stopifnot(plotName %in% c("all", "A1", "A2", "A3", "A4", "A5",
                            "B1", "B2", "B3", "B4", "B5", "BM",
                            "F1", "F2", "F3", "F4", "F5",
                            "M1", "M2", "M3", "M4", "M5",
                            "R1", "R2", "R3", "R4", "R5", "REF",
                            "S1", "S2", "S3", "S4", "S5",
                            "T1", "T2", "T3",
                            "TEMP1", "TEMP1 ", "TEMP2", "TEMP3",
                            "U1", "U2", "U3", "U4", "U5",
                            "X1", "X2", "X3"))
  stopifnot(class(dropNA) == "logical")

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(bolts <- get("Bolts", envir = env), #|>
             #dplyr::mutate(UnitCode = ifelse(Site_Name == "Acadia NP", "ACAD", "BOHA")),
           error = function(e){stop("Bolts data frame not found. Please import rocky intertidal data.")})

  bolts_park <- if(any(park %in% 'all')){ bolts
  } else {filter(bolts, UnitCode %in% park)}

  bolts_loc <- if(any(site %in% 'all')){ bolts_park
  } else {filter(bolts_park, SiteCode %in% site)}

  bolts_plottype <- if(any(plotType %in% 'all')){ bolts_loc
  } else {filter(bolts_loc, PlotType %in% plotType)}

  bolts_species <- if(any(community %in% 'all_records')){ bolts_plottype
  } else {filter(bolts_plottype, CommunityType %in% community)}

  bolts_pname <- if(any(plotName %in% 'all')){ bolts_species
  } else {filter(bolts_species, PlotName %in% plotName)}

  bolts_na <- if(dropNA == TRUE){
    bolts_pname |> filter(!is.na(BoltLatitude)) |> filter(!is.na(BoltLongitude)) |> filter(!is.na(Bolt_MLLW_Elev))
  } else {bolts_pname}

  bolts_final <-
    if(coords == "all"){
    bolts_na |> select(GroupCode, GroupName, UnitCode, UnitName, SiteName, SiteCode,
                       Label, PlotName, PlotType, CommunityType,
                       BoltLatitude, BoltLongitude, LatLong_Datum,
                       Bolt_UTM_E = BM_UTM_E, Bolt_UTM_N = BM_UTM_N,
                       Bolt_UTM_Zone = BM_UTM_Zone, Bolt_UTM_Datum = BM_UTM_Datum,
                       Bolt_NAVD88_Elev, Bolt_MLLW_Elev, Notes, IsBoltCUI)
  } else if(coords == "latlong"){
    bolts_na |> select(GroupCode, GroupName, UnitCode, UnitName, SiteName, SiteCode,
                       Label, PlotName, PlotType, CommunityType, LatLong_Datum,
                       BoltLatitude, BoltLongitude, Bolt_NAVD88_Elev,
                       Bolt_MLLW_Elev, Notes, IsBoltCUI)
  } else if(coords == "utm"){
    bolts_na |> select(GroupCode, GroupName, UnitCode, UnitName, SiteName, SiteCode,
                       Label, PlotName, PlotType, CommunityType, LatLong_Datum,
                       Bolt_UTM_E = BM_UTM_E, Bolt_UTM_N = BM_UTM_N,
                       Bolt_UTM_Zone = BM_UTM_Zone, Bolt_UTM_Datum = BM_UTM_Datum,
                       Bolt_NAVD88_Elev, Bolt_MLLW_Elev, Notes, IsBoltCUI)

  }

  if(nrow(bolts_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(bolts_final)

}
