###---------------------------------------------------
###   Dynamics: entry, exit, churn, graduation
###---------------------------------------------------

# Tax years of the mass automatic revocations under the Pension Protection
# Act of 2006: organizations that had not filed for three years lost their
# exemption in 2010-2012, most of them long dormant.
ppa_revocation_years <- 2010:2012


#' Entry, exit and churn rates by market
#'
#' Follows each organization across years and counts, for every market and
#' year `t`, the organizations that entered (present in `t`, not in `t - 1`)
#' and exited (present in `t - 1`, then absent for at least `absent_years`
#' years). Rates use the stock in `t - 1` as the denominator, with the
#' market and subsector the organization had in `t - 1` for exits and in `t`
#' for entries. Organizations that move between markets count as neither.
#'
#' @section What an exit means:
#' In the filers universe an exit is an **exit from filing**: an
#' organization that stops filing a Form 990 or 990-EZ may have dropped
#' below the filing threshold and switched to the 990-N postcard, so an exit
#' is not a death. In the registered universe an exit is removal from the
#' IRS registry: revocation, termination or merger.
#'
#' @section Gaps and censoring:
#' Absences shorter than `absent_years` between two appearances are filled:
#' the organization counts as present, so a late or missed filing is neither
#' an exit nor a re-entry. A longer absence ends with a re-entry. Exits in
#' the last `absent_years - 1` observed years cannot be confirmed yet and are
#' reported as `NA`, as are all rates for the first observed year, which has
#' no prior year.
#'
#' @section Mass revocations of 2010-2012:
#' The Pension Protection Act revoked the exemptions of hundreds of
#' thousands of organizations that had not filed for three years, almost all
#' of them in 2010-2012. `EXITS_PPA` counts exits in those years that
#' coincide with a revocation. `drop_ppa_revocations = TRUE` leaves them out
#' of `EXITS` and the rates.
#'
#' @inheritParams npcompete-params
#' @param id Name of the organization id column.
#' @param filer_type Optional filer type column (`"990"`, `"990EZ"`,
#'   `"990N"`, `"NONFILER"`, ...). When given, graduation and downgrade
#'   rates are added (see Value).
#' @param revocation Name of the revocation year column, if any; used for
#'   `EXITS_PPA`.
#' @param absent_years Years of absence that confirm an exit. Use 2 or more
#'   for filers, whose filings can lapse a year; 1 for the registered
#'   universe, where absence is observed directly.
#' @param observed_years The years `df` covers, which must be consecutive.
#'   Defaults to the years present in `df`. Pass it when some observed years
#'   are not reported, so that their absences still count.
#' @param drop_ppa_revocations Leave revocation exits of 2010-2012 out of
#'   `EXITS` and the rates.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR` and:
#' \describe{
#'   \item{N_PREV, N_CUR}{Organizations in `t - 1` and in `t`}
#'   \item{ENTRIES, EXITS, EXITS_PPA}{Counts; `EXITS_PPA` are exits in
#'     2010-2012 that coincide with a revocation}
#'   \item{ENTRY_RATE, EXIT_RATE}{`ENTRIES / N_PREV`, `EXITS / N_PREV`}
#'   \item{NET_GROWTH}{`(N_CUR - N_PREV) / N_PREV`, which also reflects moves
#'     between markets}
#'   \item{CHURN_RATE}{`(ENTRIES + EXITS) / N_PREV`}
#'   \item{N_PREV_GRASSROOTS, GRADUATIONS, GRADUATION_RATE}{With
#'     `filer_type`: 990-N filers and non-filers in `t - 1`, how many filed a
#'     990 or 990-EZ in `t`, and the share}
#'   \item{N_PREV_FILERS, DOWNGRADES, DOWNGRADE_RATE}{With `filer_type`:
#'     990 and 990-EZ filers in `t - 1`, how many were 990-N filers or
#'     non-filers in `t`, and the share}
#' }
#' Rates are `NA` where `N_PREV` is 0.
#'
#' @examples
#' d <- data.frame(
#'   EIN2 = c( "a", "a", "a", "a", "b", "b", "c", "c", "d", "d" ),
#'   GEO  = "X", SUB = "ART",
#'   YEAR = c( 2018:2021, 2018, 2019, 2020, 2021, 2018, 2020 ) )
#' # a: present throughout; b: exits after 2019; c: enters in 2020;
#' # d: misses 2019 (filled as present with absent_years = 2), then gone
#' # after 2020, which cannot be confirmed as an exit until 2022
#' get_dynamics( d, geography = "GEO", subsector = "SUB", year = "YEAR",
#'               absent_years = 2 )
#'
#' @export
get_dynamics <- function(df, geography = "CBSA_FIPS", subsector = "NTEEV2",
                         year = "YEAR", id = "EIN2", filer_type = NULL,
                         revocation = "REVOCATION_YEAR", absent_years = 2,
                         observed_years = NULL, drop_ppa_revocations = FALSE) {
  k <- as.integer(absent_years)
  if (length(k) != 1 || is.na(k) || k < 1)
    stop("`absent_years` must be a whole number, 1 or more.", call. = FALSE)
  need <- c(id, geography, subsector, year, filer_type)
  missing_cols <- setdiff(need, names(df))
  if (length(missing_cols) > 0)
    stop("Column(s) not found in df: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)

  geo <- as.character(df[[geography]])
  geo[geo %in% ""] <- NA
  d <- data.table::data.table(
    ID  = as.character(df[[id]]),
    Y   = suppressWarnings(as.integer(as.character(df[[year]]))),
    GEO = geo,
    SUB = make_subsector(df[[subsector]], subsector),
    FT  = if (is.null(filer_type)) NA_character_ else as.character(df[[filer_type]]),
    REV = if (revocation %in% names(df)) suppressWarnings(as.integer(df[[revocation]]))
          else NA_integer_)
  d <- d[!is.na(Y) & !is.na(ID)]

  obs <- sort(unique(as.integer(if (is.null(observed_years)) d$Y else observed_years)))
  if (length(obs) < 2 || any(diff(obs) != 1))
    stop("Dynamics need two or more consecutive observed years; got ",
         paste(obs, collapse = ", "), ".", call. = FALSE)
  y0 <- min(obs); y1 <- max(obs)
  d <- d[Y %in% obs]
  d <- unique(d, by = c("ID", "Y"))
  data.table::setorderv(d, c("ID", "Y"))

  # Fill absences shorter than k with the organization's prior attributes.
  d[, NEXT := data.table::shift(Y, type = "lead"), by = "ID"]
  g <- d[!is.na(NEXT) & NEXT - Y - 1L >= 1L & NEXT - Y - 1L < k]
  if (nrow(g) > 0) {
    n <- g$NEXT - g$Y - 1L
    f <- g[rep(seq_len(nrow(g)), n)]
    f$Y <- f$Y + sequence(n)
    f$REV <- NA_integer_
    d <- data.table::rbindlist(list(d, f))
    data.table::setorderv(d, c("ID", "Y"))
  }
  d[, PREV_Y := data.table::shift(Y), by = "ID"]
  d[, NEXT := data.table::shift(Y, type = "lead"), by = "ID"]
  d[, NEXT_FT := data.table::shift(FT, type = "lead"), by = "ID"]

  filer_types <- c("990", "990EZ")
  entry <- is.na(d$PREV_Y) | d$PREV_Y < d$Y - 1L
  exit  <- is.na(d$NEXT) | d$NEXT > d$Y + 1L
  ppa   <- exit & !is.na(d$REV) & d$REV == d$Y + 1L & (d$Y + 1L) %in% ppa_revocation_years
  cont  <- !is.na(d$NEXT) & d$NEXT == d$Y + 1L
  gr    <- d$FT %in% grassroots_types
  fil   <- d$FT %in% filer_types

  zero <- rep(0, nrow(d))
  cur <- data.frame(GEO = d$GEO, SUB = d$SUB, YEAR = d$Y,
                    CUR = 1, PREV = zero, ENTRY = as.numeric(entry),
                    EXIT = zero, PPA = zero, PGR = zero, GRAD = zero,
                    PFIL = zero, DOWN = zero, stringsAsFactors = FALSE)
  prv <- data.frame(GEO = d$GEO, SUB = d$SUB, YEAR = d$Y + 1L,
                    CUR = zero, PREV = 1, ENTRY = zero,
                    EXIT = as.numeric(exit), PPA = as.numeric(ppa),
                    PGR = as.numeric(gr), GRAD = as.numeric(gr & cont & d$NEXT_FT %in% filer_types),
                    PFIL = as.numeric(fil), DOWN = as.numeric(fil & cont & d$NEXT_FT %in% grassroots_types),
                    stringsAsFactors = FALSE)
  ev <- rbind(cur, prv)
  ev <- ev[ev$YEAR > y0 & ev$YEAR <= y1, ]

  cols <- c("CUR", "PREV", "ENTRY", "EXIT", "PPA", "PGR", "GRAD", "PFIL", "DOWN")
  out <- summarize_levels(ev, function(cur, prev, entry, exit, ppa, pgr, grad, pfil, down)
    list(N_PREV = sum(prev), N_CUR = sum(cur), ENTRIES = sum(entry),
         EXITS = sum(exit), EXITS_PPA = sum(ppa), N_PREV_GRASSROOTS = sum(pgr),
         GRADUATIONS = sum(grad), N_PREV_FILERS = sum(pfil), DOWNGRADES = sum(down)),
    cols = cols)

  if (drop_ppa_revocations) out$EXITS <- out$EXITS - out$EXITS_PPA
  censored <- out$YEAR > y1 - k + 1L          # absence not yet long enough to confirm
  out$EXITS[censored] <- NA
  out$EXITS_PPA[censored] <- NA
  rate <- function(x, n) ifelse(n > 0, x / n, NA_real_)
  out$ENTRY_RATE <- rate(out$ENTRIES, out$N_PREV)
  out$EXIT_RATE  <- rate(out$EXITS, out$N_PREV)
  out$NET_GROWTH <- rate(out$N_CUR - out$N_PREV, out$N_PREV)
  out$CHURN_RATE <- rate(out$ENTRIES + out$EXITS, out$N_PREV)
  out$GRADUATION_RATE <- rate(out$GRADUATIONS, out$N_PREV_GRASSROOTS)
  out$DOWNGRADE_RATE  <- rate(out$DOWNGRADES, out$N_PREV_FILERS)

  keep <- c("GEO", "SUB", "YEAR", "N_PREV", "N_CUR", "ENTRIES", "EXITS", "EXITS_PPA",
            "ENTRY_RATE", "EXIT_RATE", "NET_GROWTH", "CHURN_RATE")
  if (!is.null(filer_type))
    keep <- c(keep, "N_PREV_GRASSROOTS", "GRADUATIONS", "GRADUATION_RATE",
              "N_PREV_FILERS", "DOWNGRADES", "DOWNGRADE_RATE")
  out[keep]
}
