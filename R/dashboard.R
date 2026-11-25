###---------------------------------------------------
###   Dashboard: explore npc_build_metrics() output in Shiny
###---------------------------------------------------

#' Explore metrics in an interactive dashboard
#'
#' Opens a Shiny app for browsing the output of [npc_build_metrics()]: pick
#' a universe, geography, metric, resource, subsector, year and market, and
#' see the market against the distribution of all markets, over time,
#' against a second metric, and across subsectors. The table tab downloads
#' the selection as CSV.
#'
#' @param data Metrics to show. Any of:
#'   \itemize{
#'     \item the long output of [npc_build_metrics()] (or several, combined
#'       with `rbind()`, for example one per universe), or a list of them;
#'     \item its wide output (`format = "wide"`), a named list of tables;
#'     \item a folder written with `out_dir`, or a path to a CSV written
#'       there;
#'     \item `NULL` (default), the bundled [cbsa_metrics] for 2019.
#'   }
#' @param launch Start the app. With `FALSE`, return the app object, for
#'   example to deploy it with `shiny::runApp()` or on a Shiny server.
#' @param ... Passed to [shiny::runApp()].
#'
#' @return The Shiny app object, invisibly when launched.
#' @examples
#' \dontrun{
#' npc_dashboard()      # bundled 2019 CBSA metrics
#'
#' f <- npc_build_metrics( 2015:2021, geo = c( "cbsa", "county" ), source = "nccs" )
#' r <- npc_build_metrics( 2015:2021, geo = c( "cbsa", "county" ), source = "nccs",
#'                         universe = "registered" )
#' npc_dashboard( rbind( f, r ) )
#' }
#' @export
npc_dashboard <- function(data = NULL, launch = interactive(), ...) {
  need_pkg("shiny", "run the dashboard")
  need_pkg("bslib", "run the dashboard")
  need_pkg("ggplot2", "draw the dashboard's charts")
  d <- dashboard_data(data)
  app <- shiny::shinyApp(dashboard_ui(d), dashboard_server(d))
  if (!launch) return(app)
  shiny::runApp(app, ...)
  invisible(app)
}


# Normalize any supported input to the long layout, with market names.
dashboard_data <- function(data = NULL) {
  long_cols <- c("GEO_LEVEL", "GEO", "SUB", "YEAR", "UNIVERSE", "RESOURCE",
                 "METRIC", "VALUE")
  is_long <- function(x) is.data.frame(x) && all(c("METRIC", "VALUE") %in% names(x))
  from_wide <- function(tables, universe) {
    tables <- lapply(tables, function(t) as.data.frame(t[, !grepl("^EXPECTED_", names(t))]))
    by_res <- any(grepl("_", names(tables)))
    metrics_to_long(tables, universe, by_res)
  }

  if (is.null(data)) {
    data <- from_wide(list(cbsa_TOTREV = npcompete::cbsa_metrics), "filers")
  } else if (is.character(data)) {
    files <- if (dir.exists(data)) list.files(data, "\\.csv$", ignore.case = TRUE,
                                              full.names = TRUE) else data
    files <- files[!grepl("manifest", basename(files))]
    if (length(files) == 0) stop("No metric CSV files found in ", data, call. = FALSE)
    mf <- file.path(dirname(files[1]), "manifest.csv")
    universe <- if (file.exists(mf)) {
      m <- utils::read.csv(mf, stringsAsFactors = FALSE)
      m$value[m$key == "universe"]
    } else "filers"
    read <- function(f) utils::read.csv(f, colClasses = c(GEO = "character"),
                                        stringsAsFactors = FALSE)
    long <- files[grepl("long", basename(files))]
    data <- if (length(long) > 0) {
      do.call(rbind, lapply(long, read))
    } else {
      tabs <- lapply(files, read)
      nm <- toupper(sub("^NPCOMPETE-METRICS-BY-(.*)\\.CSV$", "\\1", basename(files),
                        ignore.case = TRUE))
      geo <- tolower(sub("-.*$", "", nm))
      res <- ifelse(grepl("-", nm), sub("^[^-]*-", "", nm), "")
      names(tabs) <- ifelse(res == "", geo, paste(geo, res, sep = "_"))
      from_wide(tabs, universe)
    }
  } else if (is_long(data)) {
    data <- as.data.frame(data)
  } else if (is.list(data) && length(data) > 0 && all(vapply(data, is_long, logical(1)))) {
    data <- do.call(rbind, lapply(data, as.data.frame))
  } else if (is.list(data) && length(data) > 0 && all(vapply(data, is.data.frame, logical(1)))) {
    mf <- attr(data, "manifest")
    universe <- if (!is.null(mf)) mf$value[mf$key == "universe"] else "filers"
    data <- from_wide(data, universe)
  } else {
    stop("`data` must be output of npc_build_metrics(), a folder or CSV it ",
         "wrote, or NULL.", call. = FALSE)
  }

  missing_cols <- setdiff(long_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Metric data is missing columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  data <- as.data.frame(data)[long_cols]
  data$GEO <- as.character(data$GEO)
  data$YEAR <- as.integer(data$YEAR)
  data$RESOURCE[data$RESOURCE %in% ""] <- NA
  data <- data[!is.na(data$VALUE) | data$METRIC %in% c("N", "POP"), ]
  data$NAME <- market_names(data$GEO, data$GEO_LEVEL)
  rownames(data) <- NULL
  data
}

# Readable market names for CBSA and county codes.
market_names <- function(geo, level) {
  cb <- unique(npcompete::pop_cbsa[c("CBSA_FIPS", "CBSA_NAME")])
  ct <- unique(npcompete::pop_county[c("COUNTY_FIPS", "STATE_FIPS", "COUNTY_NAME")])
  abbr <- names(state_fips)[match(ct$STATE_FIPS, state_fips)]
  ct_name <- paste0(ct$COUNTY_NAME, ", ", abbr)
  out <- ifelse(level == "cbsa", cb$CBSA_NAME[match(geo, cb$CBSA_FIPS)],
                ct_name[match(geo, ct$COUNTY_FIPS)])
  out[geo == "ALL"] <- "United States"
  out[is.na(out)] <- geo[is.na(out)]
  out
}

# Metric choices labeled with their registry descriptions.
metric_choices <- function(ids) {
  reg <- metric_registry()
  extra <- c(N = "Number of organizations", POP = "Population",
             RESOURCE_TOTAL = "Resource total", N_PREV = "Organizations in t - 1",
             ENTRIES = "Entries", EXITS = "Exits", N_GRASSROOTS = "Grassroots organizations")
  known <- reg$id[reg$id %in% ids]
  others <- setdiff(ids, known)
  lab <- c(stats::setNames(reg$description, reg$id), extra)
  ids <- c(known, others)
  stats::setNames(ids, ifelse(is.na(lab[ids]), ids, paste0(ids, ": ", lab[ids])))
}

dashboard_ui <- function(d) {
  first <- function(x) sort(unique(x))[1]
  years <- sort(unique(d$YEAR))
  bslib::page_sidebar(
    title = "npcompete: nonprofit competition and density",
    theme = bslib::bs_theme(version = 5, primary = "#1696d2"),
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput("universe", "Universe", sort(unique(d$UNIVERSE))),
      shiny::selectInput("level", "Geography", sort(unique(d$GEO_LEVEL))),
      shiny::selectInput("metric", "Metric", metric_choices(sort(unique(d$METRIC)))),
      shiny::selectInput("resource", "Resource", character()),
      shiny::selectInput("sub", "Subsector", c("ALL", setdiff(sort(unique(d$SUB)), "ALL"))),
      shiny::selectInput("year", "Year", rev(years), selected = max(years)),
      shiny::selectizeInput("market", "Market", choices = NULL,
                            options = list(placeholder = "Type a name")),
      shiny::selectInput("metric2", "Compare with", metric_choices(sort(unique(d$METRIC))))
    ),
    bslib::navset_card_tab(
      bslib::nav_panel("Distribution", shiny::plotOutput("dist", height = 420),
                       shiny::uiOutput("dist_note")),
      bslib::nav_panel("Over time", shiny::plotOutput("trend", height = 420)),
      bslib::nav_panel("Compare", shiny::plotOutput("scatter", height = 480)),
      bslib::nav_panel("Subsectors", shiny::plotOutput("subs", height = 420)),
      bslib::nav_panel("Table",
                       shiny::downloadButton("download", "Download CSV"),
                       shiny::tableOutput("table")),
      bslib::nav_panel("About", shiny::uiOutput("about"))
    )
  )
}

dashboard_server <- function(d) {
  # Index rows once by universe, geography level and metric, so each chart
  # reads only its slice of what can be tens of millions of rows.
  scope_key <- paste(d$UNIVERSE, d$GEO_LEVEL, sep = "|")
  idx <- split(seq_len(nrow(d)), paste(scope_key, d$METRIC, sep = "|"))
  scopes <- lapply(split(seq_len(nrow(d)), scope_key), function(i) {
    ids <- sort(unique(d$METRIC[i]))
    res <- sort(unique(d$RESOURCE[i][!is.na(d$RESOURCE[i])]))
    n <- i[d$METRIC[i] %in% c("N", ids[1]) & d$GEO[i] != "ALL"]
    m <- unique(d[n, c("GEO", "NAME")])
    list(ids = ids, res = res, markets = m[order(m$NAME), ])
  })

  function(input, output, session) {
    gg <- function() ggplot2::theme_minimal(base_size = 13)
    key <- shiny::reactive(paste(input$universe, input$level, sep = "|"))

    shiny::observeEvent(key(), {
      s <- scopes[[key()]]
      if (is.null(s)) return()
      shiny::updateSelectInput(session, "resource",
                               choices = if (length(s$res)) s$res else c("(none)" = ""))
      keep <- if (isTRUE(input$metric %in% s$ids)) input$metric else NULL
      shiny::updateSelectInput(session, "metric", choices = metric_choices(s$ids), selected = keep)
      shiny::updateSelectInput(session, "metric2", choices = metric_choices(s$ids),
                               selected = if (isTRUE(input$metric2 %in% s$ids)) input$metric2 else "N")
      shiny::updateSelectizeInput(session, "market", server = TRUE,
                                  choices = c(stats::setNames("ALL", "United States"),
                                              stats::setNames(s$markets$GEO, s$markets$NAME)))
    })

    # Values of one metric: the resource-specific rows, or the shared ones
    values <- function(metric) {
      i <- idx[[paste(key(), metric, sep = "|")]]
      if (is.null(i)) return(d[0, ])
      v <- d[i, ]
      if (any(!is.na(v$RESOURCE))) v <- v[v$RESOURCE %in% input$resource, ]
      v
    }
    selected <- shiny::reactive({
      v <- values(input$metric)
      v[v$SUB == input$sub & v$YEAR == as.integer(input$year), ]
    })
    label <- shiny::reactive(input$metric)
    describe <- shiny::reactive(sub("^[^:]*: ", "", names(metric_choices(input$metric))))
    market_label <- shiny::reactive({
      if (is.null(input$market) || input$market == "") "" else
        market_names(input$market, input$level)
    })

    output$dist <- shiny::renderPlot({
      v <- selected()
      mk <- v[v$GEO != "ALL", ]
      shiny::validate(shiny::need(nrow(mk) > 0, "No values for this selection."))
      p <- ggplot2::ggplot(mk, ggplot2::aes(VALUE)) +
        ggplot2::geom_histogram(bins = 40, fill = "#1696d2", colour = "white") +
        ggplot2::labs(x = label(), y = "Markets",
                      title = paste0(input$sub, ", ", input$year), subtitle = describe()) + gg()
      sel <- mk$VALUE[mk$GEO == input$market]
      if (length(sel) == 1 && !is.na(sel))
        p <- p + ggplot2::geom_vline(xintercept = sel, colour = "#ec008b", linewidth = 1) +
          ggplot2::annotate("label", x = sel, y = Inf, vjust = 1.2, label = market_label())
      p
    })

    output$dist_note <- shiny::renderUI({
      v <- selected()
      mk <- v[v$GEO != "ALL" & !is.na(v$VALUE), ]
      sel <- mk$VALUE[mk$GEO == input$market]
      if (length(sel) != 1) return(shiny::p("Pick a market to place it in the distribution."))
      shiny::p(sprintf("%s: %s, higher than %.0f%% of %d markets.", market_label(),
                       format(signif(sel, 4), big.mark = ","),
                       100 * mean(mk$VALUE < sel), nrow(mk)))
    })

    output$trend <- shiny::renderPlot({
      v <- values(input$metric)
      v <- v[v$SUB == input$sub & v$GEO %in% c("ALL", input$market), ]
      shiny::validate(shiny::need(length(unique(v$YEAR)) > 1,
                                  "These data hold a single year."))
      v$SERIES <- ifelse(v$GEO == "ALL", "United States", market_label())
      ggplot2::ggplot(v, ggplot2::aes(YEAR, VALUE, colour = SERIES)) +
        ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point() +
        ggplot2::scale_colour_manual(values = c("#1696d2", "#ec008b")) +
        ggplot2::labs(x = "Year", y = label(), colour = NULL, subtitle = describe()) + gg() +
        ggplot2::theme(legend.position = "top")
    })

    output$scatter <- shiny::renderPlot({
      a <- selected()
      b <- values(input$metric2)
      b <- b[b$SUB == input$sub & b$YEAR == as.integer(input$year), c("GEO", "VALUE")]
      m <- merge(a[a$GEO != "ALL", c("GEO", "NAME", "VALUE")], b, by = "GEO",
                 suffixes = c("", "_2"))
      shiny::validate(shiny::need(nrow(m) > 0, "No markets have both metrics."))
      p <- ggplot2::ggplot(m, ggplot2::aes(VALUE_2, VALUE)) +
        ggplot2::geom_point(alpha = 0.4, colour = "#1696d2") +
        ggplot2::labs(x = input$metric2, y = label()) + gg()
      hit <- m[m$GEO == input$market, ]
      if (nrow(hit) == 1)
        p <- p + ggplot2::geom_point(data = hit, colour = "#ec008b", size = 4) +
          ggplot2::geom_label(data = hit, ggplot2::aes(label = NAME), vjust = -0.6)
      p
    })

    output$subs <- shiny::renderPlot({
      v <- values(input$metric)
      g <- if (is.null(input$market) || input$market == "") "ALL" else input$market
      v <- v[v$GEO == g & v$YEAR == as.integer(input$year) & v$SUB != "ALL", ]
      shiny::validate(shiny::need(nrow(v) > 0, "No subsector values for this market."))
      ggplot2::ggplot(v, ggplot2::aes(stats::reorder(SUB, VALUE), VALUE)) +
        ggplot2::geom_col(fill = "#1696d2") + ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = label(),
                      title = paste0(market_names(g, input$level), ", ", input$year)) + gg()
    })

    table_rows <- shiny::reactive({
      v <- selected()
      v <- v[order(-v$VALUE), c("GEO", "NAME", "SUB", "YEAR", "UNIVERSE", "RESOURCE",
                                "METRIC", "VALUE")]
      v
    })
    output$table <- shiny::renderTable(utils::head(table_rows(), 200), digits = 4)
    output$download <- shiny::downloadHandler(
      filename = function() paste0("npcompete-", input$metric, "-", input$level, "-",
                                   input$year, ".csv"),
      content = function(file) utils::write.csv(table_rows(), file, row.names = FALSE))

    output$about <- shiny::renderUI({
      reg <- metric_registry()
      r <- reg[reg$id == input$metric, ]
      shiny::tagList(
        shiny::h4(input$metric),
        shiny::p(if (nrow(r)) r$description else "Supporting count."),
        if (nrow(r)) shiny::p("Valid for universes: ", r$universes),
        shiny::p("Built with npcompete ", as.character(utils::packageVersion("npcompete")),
                 ". See ", shiny::a("the package site",
                                    href = "https://nonprofit-open-data-collective.github.io/npcompete/"),
                 " for definitions."))
    })
  }
}
