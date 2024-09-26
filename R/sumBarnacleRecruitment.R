#' @title sumBarnacleRecruitment: sum Barnacle recruitment count data
#'
#' @include getBarnacleRecruitment.R
#'
#' @importFrom dplyr filter group_by select slice summarize
#'
#' @description This function filters barnacle recruitment count data by park, site, and plot name and summarizes counts for summer and winter settlement periods.
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
#' \item{"OTTPOI"}{Otter Point, ACAD}ed+56
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
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns all records
#' barn <- sumBarnacleRecruitment()
#'
#' # sum barnacle counts for ACAD only sites
#' barn_acad <- sumBarnacleRecruitment(park = "ACAD")
#'
#' # sum barnacle counts for specific sites, plots,and years
#'
#' barn_summer <- sumBarnacleRecruitment(park = "ACAD", plotName = "summer")
#' barn_BOHA <- sumBarnacleRecruitment(site = c("CALISL", "GREISL"))
#' barn_5yr <- sumBarnacleRecruitment(years = 2016:2021)
#' barn_first_last <- sumBarnacleRecruitment(years = c(2013, 2021))
#' barn21_qaqc <- sumBarnacleRecruitment(years = 2024, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of barnacle recruitment count data.
#' @export

sumBarnacleRecruitment <- function(park = "all", site = "all", plotName = "all",
                                   QAQC = FALSE, years = 2013:as.numeric(format(Sys.Date(), "%Y"))){

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "summer", "winter",
                            "S1", "S2", "S3", "S4", "S5",
                            "U1", "U2", "U3", "U4", "U5"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  # set up plot name list and catch if summer and winter are both specified when should be 'all' instead
  if(any(plotName %in% "summer" & any(plotName %in% "winter"))){plotName = "all"}
  if(any(plotName %in% "summer")){plotName = c("S1", "S2", "S3", "S4", "S5")}
  if(any(plotName %in% "winter")){plotName = c("U1", "U2", "U3", "U4", "U5")}

  barn <- getBarnacleRecruitment(park = park, site = site, plotName = plotName, QAQC = QAQC,
                                 years = years, dropNA = TRUE)

  barn$plot_type <- ifelse(barn$PlotName %in% c("S1", "S2", "S3", "S4", "S5"), "summer", "winter")

  #++++ Update when there's a logical way to select the correct count per plot/visit +++++
  # Check for and handle duplicate plot counts
  dup_check <- barn |>
    group_by(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate,
             Year, QAQC, QAQCType, plot_type) |>
    summarize(num_plots = sum(!is.na(PlotName)), .groups = 'drop') |>
    filter(num_plots > 5)

  if(nrow(dup_check) > 0){warning(paste0("There were ", nrow(dup_check), " plots with more than one count per visit. ",
                                         "Only taking the first count, and/or dropping records with 'Winter' in the notes field."))}

  # Drop duplicate counts in 2023
  barn1 <- barn |> filter(!grepl("Winter", Notes))

  # Drop other duplicates by slicing only the first record
  barn2 <- barn1 |> group_by(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName,
                             StartDate, Year, QAQC, QAQCType, PlotName, IsPointCUI, plot_type) |>
    slice(1)

  barnsum <- barn2 |>
    group_by(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate,
             Year, QAQC, QAQCType, plot_type) |>
    summarize(median_count = median(Count),
              max_count = max(Count),
              min_count = min(Count),
              num_plots = sum(!is.na(PlotName)),
              .groups = 'drop')

  return(barnsum)
}
