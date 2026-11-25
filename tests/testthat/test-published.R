# Published metric tables, read from a fake release folder.

fake_release <- function() {
  skip_if_not_installed("arrow")
  arch <- build_tmp_archive()
  root <- withr_tempdir()
  rel <- file.path(root, "releases", "metrics-2099.01")
  dir.create(rel, recursive = TRUE)
  w <- suppressMessages(npc_build_metrics(2019:2020, geo = "cbsa", resources = "TOTREV",
                                          metrics = c("HHI", "DENSITY_PER_100K"),
                                          dir = arch, format = "wide"))
  t <- as.data.frame(w$cbsa_TOTREV)
  arrow::write_parquet(t, file.path(rel, "metrics_filers_cbsa_TOTREV.parquet"))
  data.table::fwrite(t, file.path(rel, "metrics_filers_cbsa_TOTREV.csv.gz"))
  mf <- attr(w, "manifest")
  mf$table <- "filers_cbsa"
  utils::write.csv(mf, file.path(rel, "manifest.csv"), row.names = FALSE)
  url <- paste0("file:///", sub("^/", "", normalizePath(file.path(root, "releases"),
                                                        winslash = "/")))
  list(url = url, cache = file.path(root, "cache"), table = t)
}

test_that("npc_load_metrics reads and caches a published table", {
  fr <- fake_release()
  old <- options(npcompete.release_url = fr$url)
  on.exit(options(old))

  w <- npc_load_metrics("filers", "cbsa", "TOTREV", vintage = "metrics-2099.01",
                        dir = fr$cache)
  expect_s3_class(w, "tbl_df")
  expect_equal(nrow(w), nrow(fr$table))
  expect_type(w$GEO, "character")
  expect_equal(attr(w, "vintage"), "metrics-2099.01")
  expect_equal(attr(w, "manifest")$value[attr(w, "manifest")$key == "universe"], "filers")
  expect_true(file.exists(file.path(fr$cache, "published", "metrics-2099.01",
                                    "metrics_filers_cbsa_TOTREV.parquet")))

  l <- npc_load_metrics("filers", "cbsa", "TOTREV", vintage = "metrics-2099.01",
                        dir = fr$cache, format = "long")
  expect_named(l, c("GEO_LEVEL", "GEO", "SUB", "YEAR", "UNIVERSE", "RESOURCE",
                    "METRIC", "VALUE"))
  expect_true(all(c("HHI", "RESOURCE_TOTAL") %in% l$METRIC))
  expect_s3_class(npcompete:::dashboard_data(l), "data.frame")

  expect_error(npc_load_metrics("registered", "cbsa", vintage = "metrics-2099.01",
                                dir = fr$cache), "Could not download")
})
