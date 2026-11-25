# Small org-year panel with hand-checked metric values.
#
# 2020:
#   A / ART : 60, 30, 10  (+ one org with missing revenue)
#   A / EDU : 50, 50
#   B / ART : 100
#   C / EDU : 0, 0        (all-zero market)
#   NA / ART: 25          (missing geography; national rollups only)
# 2021: market A / ART only : 80, 20
# Market D has population but no organizations (only via pop_table).

toy_panel <- function() {
  data.frame(
    GEO    = c("A", "A", "A", "A", "A", "A", "B", "C", "C", NA, "A", "A"),
    SUB    = c("ART", "ART", "ART", "ART", "EDU", "EDU", "ART", "EDU", "EDU",
               "ART", "ART", "ART"),
    YEAR   = c(rep(2020, 10), 2021, 2021),
    TOTREV = c(60, 30, 10, NA, 50, 50, 100, 0, 0, 25, 80, 20),
    POP    = c(rep(1e5, 6), 5e4, 2e4, 2e4, NA, 1e5, 1e5),
    stringsAsFactors = FALSE
  )
}

toy_pop <- function() {
  data.frame(GEO = c("A", "B", "C", "D", "A", "B", "C", "D"),
             YEAR = rep(c(2020, 2021), each = 4),
             POP = c(1e5, 5e4, 2e4, 1e4, 1e5, 5e4, 2e4, 1e4))
}

# Run a concentration metric on the toy panel with its column names.
run_toy <- function(f, ...) {
  f(toy_panel(), geography = "GEO", subsector = "SUB", resource = "TOTREV",
    year = "YEAR", ...)
}

# Scale-free density on the toy panel.
run_density <- function(...) {
  get_scale_free_density(toy_panel(), geography = "GEO", population = "POP",
                         subsector = "SUB", year = "YEAR",
                         resource = "TOTREV", ...)
}

cell <- function(out, geo, sub, year = 2020) {
  out[out$GEO == geo & out$SUB == sub & out$YEAR == year, , drop = FALSE]
}
