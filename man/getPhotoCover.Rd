% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPhotoCover.R
\name{getPhotoCover}
\alias{getPhotoCover}
\title{getPhotoCover: get percent cover data from photo quadrats}
\usage{
getPhotoCover(
  park = "all",
  site = "all",
  plotName = "all",
  species = "all",
  category = "all",
  community = "all",
  years = 2013:as.numeric(format(Sys.Date(), "\%Y")),
  QAQC = FALSE,
  dropNA = T
)
}
\arguments{
\item{park}{Include data from all parks, or choose one.
\describe{
\item{'all'}{Includes all parks monitored in the network}
\item{'ACAD'}{Includes only sites in Acadia National Park}
\item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
}}

\item{site}{Include data from all sites, or choose specific sites based on site code.
\describe{
\item{'all'}{Includes all sites returned by other filter arguments in function}
\item{"BASHAR"}{Bass Harbor, ACAD}
\item{"LITHUN"}{Little Hunter, ACAD}
\item{"LITMOO"}{Little Moose, ACAD}
\item{"OTTPOI"}{Otter Point, ACAD}
\item{"SCHPOI"}{Schoodic Point, ACAD}
\item{"SHIHAR"}{Ship Harbor, ACAD}
\item{"CALISL"}{Calf Island, BOHA}
\item{"GREISL"}{Green Island, BOHA}
\item{"OUTBRE"}{Outer Brewster, BOHA}
}}

\item{plotName}{Filter on plot name. Options include:
c("all", A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
"F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
"R1", "R2", "R3", "R4", "R5")}

\item{species}{Filter on species/cover code. Options include:
c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
"CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR", "NOSAMP",
"OTHINV", "OTHPLA", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "SAND", "TAR",
"ULVINT", "ULVLAC", "UNIDEN"). If a new species is added, the function will warn the user
that an unrecognized species was specified in case it was an error.}

\item{category}{Filter on category. Options include:
c("all", "Genus", "Species", "Species Group", and "Substrate")}

\item{community}{Filter on target community. Options include:
c("Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae")}

\item{years}{Filter on year of data collected. Default is 2013 to current year.
Can specify a vector of years.}

\item{QAQC}{Logical. If FALSE (Default) does not return QAQC events. If TRUE,
returns all events, including QAQC events. If TRUE, also returns "Same Grid" and "New Grid" fields.}

\item{dropNA}{Logical. If TRUE (default), blank percent cover records are removed.
If FALSE, all records are returned.}
}
\value{
Returns a data frame of photo plot percent cover data filtered by function arguments
}
\description{
This function filters photo quadrat percent cover data by park,
site, plot name, and species.
}
\examples{
\dontrun{

importData()

# Default filter returns all records except QAQC visits and blank cover records
cov <- getPhotoCover()

# Photoplot cover for ACAD only sites
cov <- getPhotoCover(park = "ACAD")

# Species detections for specific sites, plots, species, and years

cov_t3 <- getPhotoCover(park = "ACAD", plotName = "T3")
cov_BOHA2 <- getPhotoCover(site = c("CALISL", "GREISL"))
cov_ab <- getPhotoCover(park = "BOHA", species = c("ALGBRO"))
cov_5yr <- getPhotoCover(years = 2016:2021)
cov_first_last <- getPhotoCover(years = c(2013, 2021))
cov21_with_qaqc <- getPhotoCover(years = 2021, QAQC = TRUE)

}


}
