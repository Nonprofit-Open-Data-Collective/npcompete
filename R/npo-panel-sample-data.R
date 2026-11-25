#' Sample of the SOI panel sample frame (Vermont, 2015-2019)
#'
#' Every 501(c)(3) organization in Vermont that filed a Form 990 or 990-EZ in
#' tax years 2015 to 2019, taken from the NCCS SOI panel sample frame with
#' county and metro populations merged on. Whole counties and metros are
#' kept, so the extract works with the density models. Use it for examples
#' and testing; built by `data-raw/create-npo-panel-sample.R`.
#'
#' @format A data frame with 9,165 rows (organization-years) and 18 columns:
#' \describe{
#'   \item{EIN2}{Employer identification number, formatted `EIN-XX-XXXXXXX`}
#'   \item{TAXYEAR}{Tax year}
#'   \item{COUNTY_FIPS}{Five-digit county FIPS code}
#'   \item{CENSUS_CBSA_FIPS}{Core-based statistical area code; `NA` outside metro and micro areas}
#'   \item{V501C}{501(c) subsection (all 3)}
#'   \item{NTEEV2}{NTEE code in NCCS version 2 format, such as `HMS-N67-RG`}
#'   \item{TOTREV, TOTEXP, NETASS, TOTASS}{Total revenue, total expenses, net assets and total assets}
#'   \item{REVPROG, REVCONT}{Program service revenue and contributions}
#'   \item{FNDFEE}{Professional fundraising fees}
#'   \item{ORG_RULING_YEAR}{Year of the IRS ruling granting exemption}
#'   \item{REVOKED_YEAR_X}{1 if the organization's exemption was revoked in that year}
#'   \item{MISSING_YEAR}{1 for rows filled in for a gap year rather than taken from a filing}
#'   \item{POP_COUNTY, POP_METRO}{County and metro population for the year}
#' }
#'
#' @source NCCS SOI panel sample frame, <https://nccs.urban.org/>
"npo_panel_sample"
