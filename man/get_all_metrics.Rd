% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all-metrics.R
\name{get_all_metrics}
\alias{get_all_metrics}
\title{Get all metrics}
\usage{
get_all_metrics(
  df,
  geo,
  subsector,
  resource,
  resource.name,
  normalizer,
  fips,
  dat.year,
  resource.smaller.than,
  resource.greater.than,
  fundraising.cols
)
}
\arguments{
\item{df}{A dataframe containing the required fields for computing the metric grouped by geographical level and sub sector.}

\item{geo}{A character string indicating the column name for geographical area unique code.}

\item{subsector}{A character string indicating the column name for sub sectors.}

\item{resource}{A character string indicating the column name for resource based on which HHI is calculated. Example: Revenue, assets etc.}

\item{resource.name}{A character string indicating the column name for the new column generated with aggregated resource.}

\item{normalizer}{A character string indicating the column name for the column used to normalize the HHI index.}

\item{resource.greater.than}{A numeric value indicating that it is the lower limit of the resource for density evaluation. All organizations having resource value greater than that would be counted for evaluation.}

\item{fundraising.cols}{A vector containing list of column names that have the fundraising fees for the nonprofits.#'}

\item{number}{A numeric value that is used to calculate concentration ratio for top that number of firms. Value ranges from 1-8. Example, 4 if concentration ratio for top 4 firms.}

\item{smaller.than}{A numeric value indicating that it is the upper limit of the resource for density evaluation. All organizations having resource value less than that would be counted for evaluation.}
}
\value{
A new dataframe with all metrics evaluated for each year.
}
\description{
Calculate all competition metrics and return in a single dataframe and return a dataframe.
}
\examples{
head( dat.all )

}
