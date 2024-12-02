#' @title getBarnacleRecruitment: get Barnacle recruitment count data
#'
#' @importFrom dplyr filter group_by select summarize
#'
#' @description This function filters barnacle recruitment count data by park, site, and plot name.
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
#' @param plotName Filter on plot name. Options include:
#' c("all", "summer", "winter", "S1", "S2", "S3", "S4", "S5", "U1", "U2", "U3", "U4", "U5")
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
#' Can specify a vector of years.
#'
#' @param QAQC Logical. If FALSE (Default) does not return QAQC records. If TRUE,
#' returns all records, including QAQC scoring records. This differs from other functions in that
#' QAQC is determined at the record level, not the visit level.
#'
#' @param dropNA Logical. If TRUE (default), blank counts are removed.
#' If FALSE, all records are returned.
#'
#' @param timeTaken Filter on whether barnacle counts are from early (summer) or late (winter) photos from S-labeled plots.
#' Note that this filter is only valid for years 2019 and later. Options are "all", "early", and "late" (default).
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns all records except QAQC records and blank counts
#' barn <- getBarnacleRecruitment()
#'
#' # Barnacle counts for ACAD only sites
#' barn_acad <- getBarnacleRecruitment(park = "ACAD")
#'
#' # Barnacle counts for specific sites, plots, and years
#'
#' barn_summer <- getBarnacleRecruitment(park = "ACAD", plotName = "summer")
#' barn_BOHA <- getBarnacleRecruitment(site = c("CALISL", "GREISL"))
#' barn_5yr <- getBarnacleRecruitment(years = 2016:2021)
#' barn_first_last <- getBarnacleRecruitment(years = c(2013, 2021))
#' barn21_qaqc <- getBarnacleRecruitment(years = 2024, QAQC = TRUE)
#'
#' # Barnacle counts for winter-only photos taken of S plots.
#' barn_late <- getBarnacleRecruitment(years = 2019:2024, timeTaken = 'late')
#'
#' }
#'
#'
#' @return Returns a data frame of barnacle recruitment count data.
#' @export

getBarnacleRecruitment <- function(park = "all", site = "all", plotName = "all",
                                   QAQC = FALSE, years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                                   dropNA = TRUE, timeTaken = "late"){

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "summer", "winter",
                            "S1", "S2", "S3", "S4", "S5",
                            "U1", "U2", "U3", "U4", "U5"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(class(dropNA) == "logical")
  timeTaken <- match.arg(timeTaken, c("all", "early", "late"))

  # set up plot name list and catch if summer and winter are both specified when should be 'all' instead
  if(any(plotName %in% "summer" & any(plotName %in% "winter"))){plotName = "all"}
  if(any(plotName %in% "summer")){plotName = c("S1", "S2", "S3", "S4", "S5")}
  if(any(plotName %in% "winter")){plotName = c("U1", "U2", "U3", "U4", "U5")}

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(barn <- get("Barnacle_Recruitment", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(StartDate, "%Y"))),
           error = function(e){
             stop("Barnacle_Recruitment data frame not found. Please import rocky intertidal data.")})

  # Handle summer plot photos with a early and late count starting in 2019
  # Note: Have to wait until Date_Taken is populated.
  # barn_sum2 <- barn |> group_by(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName,
  #                               PlotName, QAQCType, QAQC, Year) |>
  #   summarize(num_samps = sum(!is.na(Count)),
  #             num_dates = length(unique(StartDate)),
  #             first_date = first(StartDate),
  #             last_date = last(StartDate),
  #             .groups = 'drop') |>
  #   filter(Year >= 2019)

  # Until DateTaken is populated, use notes field
  barn2 <- barn |> mutate(time_taken =
                            ifelse(grepl("S", PlotName) & Year >= 2019 & grepl("winter", Notes, ignore.case = T),
                                   "late",
                              ifelse(grepl("S", PlotName) & Year >= 2019 & !grepl("winter", Notes, ignore.case = T),
                                   "early", NA_character_)))

  barn_park <- if(any(park %in% 'all')){ barn2
  } else {filter(barn2, UnitCode %in% park)}

  barn_loc <- if(any(site %in% 'all')){ barn_park
  } else {filter(barn_park, SiteCode %in% site)}

  barn_pname <- if(any(plotName %in% 'all')){barn_loc
  } else {filter(barn_loc, PlotName %in% plotName)}

  barn_year <- filter(barn_pname, Year %in% years)

  barn_qaqc <- if(QAQC == TRUE){barn_year
  } else {barn_year |> filter(QAQCType == "NA") |> filter(QAQC == FALSE)}

  barn_na <- if(dropNA == TRUE){barn_qaqc |> filter(!is.na(Count))} else {barn_qaqc}

  barn_time <- if(any(timeTaken %in% 'all')){barn_na
    } else {barn_na |> filter(time_taken %in% timeTaken)}

  barn_final <- barn_time |>
    select(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year, QAQC, QAQCType,
           PlotName, Count, Notes, DateScored, time_taken, Scorer, IsPointCUI)

  if(nrow(barn_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(barn_final)
}
