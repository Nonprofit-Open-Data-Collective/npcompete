# npcompete development plan

**Goal:** turn npcompete into a package that can build competition and density metrics for **any requested set of years**. It should do this from two sources:

1. **Archive mode:** tables stored locally (the SOI panel frame and, later, BMF snapshots).
2. **Live mode:** fresh downloads from NCCS that are harmonized to the same schema.

It should also support two universes of organizations:

- **filers:** orgs that file a 990 or 990-EZ.
- **registered:** every exempt org on the IRS registry, which adds the grassroots orgs that file a 990-N or file nothing.

Metrics that need financial data are limited to filers.

*Written 2026-11-24 from a read of the repo at commit `a0a2beb`.*

---

## 1. Where the package stands today

### 1.1 What works

The newer, year-aware functions run when you source them. They take a panel with `GEO / SUB / YEAR` grouping and add `ALL` rollups:

- `get_gini()`, `get_nhhi()`, `get_kwoka()`, `get_cr_all()`
- `get_scale_free_density()`, a log-log residual density

`R/CREATE-METRICS-BY-CBSA-AND-COUNTY.R` uses these to produce the three `CONCENTRATION-METRICS-*.CSV` files. It reads `SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV`, which is 2.1 GB and has these properties:

- One row per EIN per tax year.
- Columns: `TAXYEAR`, `COUNTY_FIPS`, `CENSUS_CBSA_FIPS`, `V501C`, `TOTREV`, `TOTEXP`, `NETASS`, `NTEEV2`, `ORG_RULING_YEAR`, `REVOKED_YEAR_X`, `MISSING_YEAR`, `POP_COUNTY`, `POP_METRO`, among others.
- Sources: `SOI_20xx_PC` and `SOI_20xx_EZ`, with tax years 2010 to 2023.

### 1.2 Blockers: the package will not build or load as is

| Problem | Where |
|---|---|
| Script code runs at the top level inside `R/` (`fread()` calls, plots, `all_m_totrev %>% ...`, pasted console tables). `R CMD build` and `load_all()` fail on it. | `CREATE-METRICS-BY-CBSA-AND-COUNTY.R`, `all-metrics-v2.R`, `add-pop-soi-sample-frame-panel.R`, `scale-free-density.R`, `scale-free-SCRATCH.R`, `scale-free-density-robust.R`, `DESCRIPTIVE-ANALYSIS.R` |
| `get_all_metrics()` is defined three times with different signatures. Whichever file collates last wins. | `all-metrics.R`, `all-metrics-v2.R`, `CREATE-METRICS-...R` |
| Data files live in `R/`: a 2.1 GB CSV and three files of about 34 MB each. | `R/*.CSV` (untracked) |
| `NAMESPACE` is stale. It exports old functions (`get_kwoka_index`, `get_density_*`, `get_fundraising_efficiency`) and leaves out the ones actually used (`get_cr_all`, `get_kwoka`, `get_scale_free_density`). | `NAMESPACE` |
| `DESCRIPTION` is a placeholder and lists only `dplyr` under Imports. The code also uses DescTools, tidyr, purrr, data.table, rlang, magrittr and MASS. | `DESCRIPTION` |
| `bottomcode()` is defined three times. | `hhi.R`, `utils.R`, `gini.R` |
| `all-metrics-v2.R` is not valid UTF-8 (contains a stray CP-1252 en dash). | |

### 1.3 Methodological bugs to fix before any new metrics are added

1. **Density regressions omit markets with zero orgs.** `get_scale_free_density()` builds groups only from org rows. A county with no environmental orgs therefore never enters the `LOG_NPO ~ LOG_POP` model and gets no density value. The low-density tail is truncated, and residuals are biased upward. This matters even more for subsector × county cells.
   - **Fix:** build the full `GEO × SUB × YEAR` frame from the population table, with `N = 0` where no orgs exist.
2. **`GEO = "ALL"` rows contaminate the regressions.** The national rows (`dd3`) share `SUB` and `YEAR` with the market rows, so each national total enters its subsector's regression as one more "market". Its `POP` is also wrong: it is the mean of org-level metro populations, not the national population.
   - **Fix:** fit models on market rows only, and take national population from the population table.
3. **Population comes from org rows.** `POP_METRO` and `POP_COUNTY` are merged onto each org row and averaged within groups, so a market with no orgs has no population.
   - **Fix:** join population at the market level after aggregation.
4. **`MISSING_YEAR` gap-fill rows are counted** (about 3% of rows in a sample). Decide whether they count as present for density and dynamics. They should not contribute financials.
5. **The last tax year is incomplete.** TY2023 has roughly 60% of the rows in TY2022, and TY2024 is close to empty. The current build drops these years with a hard-coded filter (`TAXYEAR > 2010 & TAXYEAR < 2023`).
   - **Fix:** add a completeness check that warns about or drops partial years automatically.
6. **APIs are inconsistent across metrics:**
   - `get_hhi()` has no `ALL` rollups and allows `year = NULL`. The other metrics have rollups and require a year.
   - `get_cr()` takes its arguments in a different order and never calls `na.omit()`.
   - The subsector is always cut with `substr(NTEEV2, 1, 3)`, which is hard-coded.
7. **The old density functions** (`get_density`, `_small`, `_big`) depend on the bundled `county_population`, which uses the wide `POPESTIMATE{year}` layout, and on the old column names (`MSA_NECH`, `NTMAJ12`). They do not work with the current panel.

---

## 2. Target architecture

```
  Acquisition                 Standardize               Metrics                 Output
 ┌────────────────────┐      ┌──────────────────┐     ┌────────────────────┐   ┌─────────────────┐
 │ source="archive"   │      │ npc_standardize()│     │ metric registry    │   │ tidy long/wide  │
 │  local panel →     │─────▶│ canonical EIN×YR │────▶│ count metrics      │──▶│ + manifest       │
 │  parquet by year   │      │ schema + flags   │     │ resource metrics   │   │ (sources, vintage│
 │ source="nccs"      │      │ FILER_TYPE       │     │ dynamics (entry/   │   │  years, universe)│
 │  CORE + BMF + 990N │─────▶│ market frame     │     │  exit, churn)      │   └─────────────────┘
 │  + revocations     │      │ (GEO×SUB×YR+POP) │     └────────────────────┘
 └────────────────────┘      └──────────────────┘
```

### 2.1 Canonical org-year schema

Both sources produce the same table, one row per `EIN2 × YEAR`:

| Column | Notes |
|---|---|
| `EIN2`, `YEAR` | keys |
| `COUNTY_FIPS`, `CBSA_FIPS` | CBSA taken from a county→CBSA crosswalk with a declared delineation vintage |
| `NTEEV2`, `SUB` | `SUB` is derived by a configurable function (default: NTEEV2 major group) |
| `SUBSECTION` | 501(c) type; default filter is `3` |
| `FILER_TYPE` | `"990"`, `"990EZ"`, `"990N"`, `"NONFILER"` |
| `RULING_YEAR`, `REVOKED_YEAR`, `REINSTATED_YEAR` | used for dynamics |
| `TOTREV`, `TOTEXP`, `NETASS`, `TOTASS`, `REVPROG`, `REVCONT`, `FNDFEE` | `NA` for 990-N and non-filers |
| `IMPUTED_ROW` | renamed from `MISSING_YEAR` |
| `SOURCE` | provenance |

A separate **market frame** holds one row per `GEO × YEAR` with population. It is joined at the market level, which fixes bugs 1–3 in §1.3.

### 2.2 Acquisition layer (new: `R/source-archive.R`, `R/source-nccs.R`)

```r
npc_get_orgs(
  years,                                  # e.g. 2012:2022, validated against source coverage
  universe = c("filers", "registered"),
  source   = c("archive", "nccs"),
  path     = getOption("npcompete.archive_dir"),
  subsection = 3,
  cache    = TRUE
)
```

**Archive mode** works from local files:

- **Filers:** read the local SOI panel frame.
  - Add `npc_build_archive(csv)`. It converts the 2.1 GB CSV once into Parquet partitioned by `YEAR` (with arrow). After that, `years = 2015:2018` reads only those partitions instead of scanning 2 GB with `fread`.
- **Registered:** read locally stored annual BMF snapshots plus a revocation file, saved in the same archive directory. These can be downloaded once by live mode and then frozen.
- The archive directory lives **outside the repo**. Its location is set with `options(npcompete.archive_dir=)` or the `NPCOMPETE_ARCHIVE` environment variable.

**Live mode** downloads from NCCS and caches under `tools::R_user_dir("npcompete", "cache")`:

| Need | NCCS / IRS source (confirm field names against each data dictionary) |
|---|---|
| Filer financials by year | CORE series on `nccsdata.s3.us-east-1.amazonaws.com`, keys like `CORE-YYYY-CHARITIES-PC` and `-PZ`. Covers roughly 1989 to 2022–24 and excludes 990-N filers. |
| Geography, NTEE, ruling date | Unified BMF `geocoding/unified-bmf/latest/bmf_unified_geocoded.csv` (one row per EIN, 1989 to present), or the `nccsdata::nccs_read()` parquet |
| Registered universe in year *t* | Monthly BMF archives: `processed/bmf-legacy/YYYY_MM/` (1989–2022) and `processed/bmf/YYYY_MM/` (2023-06 onward). Use one snapshot per year (for example December). |
| 990-N status | NCCS `raw/e-postcard/YYYY-MM-E-POSTCARD.csv`, or IRS `data-download-epostcard.zip`. **Holds only the most recent filing per org** (see open question 2). |
| Exits | `raw/revocations/YYYY-MM-REVOCATIONS-ORGS.csv`, or IRS `data-download-revocation.zip`. Gives `revocation_date` and `exemption_reinstatement_date`. |
| Population | Bundled county and CBSA tables for 2000–2025 (currently in Dropbox `00 - DATA/population`). These move into `data/`. |

Each live build writes a **manifest**: URLs, download timestamp, file hashes, BMF month used, and package version. Two runs can then be compared, and a live build can be promoted to an archive.

Year validation: each source declares its coverage. A request outside that range fails with a clear message, for example "archive filer panel covers 2010–2023; TY2023 incomplete".

### 2.3 Universes and filer classification

Registered orgs in year *t* are those in the year-*t* BMF snapshot, or, as a fallback, those with `RULING_YEAR ≤ t`, not revoked by *t* (or reinstated), and a matching subsection.

Each registered org-year is then classified:

- **`990` / `990EZ`:** the EIN appears in the filer panel for year *t*. Allow ±1 year for fiscal-year slippage.
- **`990N`:** the EIN has a postcard filing for *t*. This depends on open question 2.
- **`NONFILER`:** everything else. This group includes:
  - churches that are not required to file (BMF filing requirement code `06`)
  - dormant orgs that have not yet been auto-revoked
  - unmatched EINs

Expose a `universe` argument: `"filers"` (current behavior), `"registered"` (all), or `"grassroots"` (registered minus filers, that is `990N` + `NONFILER`). Add `exclude_churches = TRUE` as an option.

### 2.4 Metric registry (refactor `R/metrics-*.R`)

Replace the copied three-block code in every metric (geo×sub, geo, sub) with one internal helper:

```r
summarize_levels(dat, .f, levels = c("geo_sub", "geo", "sub", "national"))
```

Each metric is then a small function plus a registry entry that declares what it needs:

| Metric | Needs a resource | Allowed universes |
|---|---|---|
| HHI, NHHI, CR1–8, Kwoka, Gini | yes | filers |
| Resource density residual (`DENSITY_PCTL_R`) | yes | filers |
| Commercial share, fundraising efficiency | yes (multiple columns) | filers |
| Large-org density (`TOTREV > x`) | yes | filers |
| **N, density per 100k, scale-free count density (`DENSITY_PCTL_N`)** | no | filers, registered, grassroots |
| **Small-org density** | no (filers: `TOTREV < x`; registered: `FILER_TYPE %in% c("990N","NONFILER")`) | all |
| **Grassroots share** (grassroots / registered) | no | registered |
| **Entry rate, exit rate, net growth, churn** | no | all (definitions differ, §2.5) |
| **Median org age** (from ruling year) | no | all |
| **Graduation rate** (990-N/non-filer → filer) | no | registered |

`npc_build_metrics()` checks each requested metric against the universe. If, say, `HHI` is requested with `universe = "grassroots"`, it errors with something like: "HHI requires financial data; grassroots orgs do not report revenue. Use `universe = 'filers'`."

### 2.5 Dynamics: entry and exit (new `R/dynamics.R`)

All rates use the stock at *t−1* as the denominator and are computed with `summarize_levels()`.

**Filers**

- *Entry into filing:* first year an EIN appears in the panel.
- *Exit from filing:* last year an EIN appears, followed by at least `k` absent years (default `k = 2`).
  - The last `k` years of the window are right-censored and reported as `NA`.
  - Imputed gap rows count as present.
- An exit from filing is **not** a death. The org may have dropped below the $50k threshold and switched to the 990-N. The documentation must say this.

**Registered**

- *Entry:* ruling year, or first appearance in a BMF snapshot.
- *Exit:* the revocation year, or removal from the BMF snapshot (termination or merger). Reinstatement reverses the exit.
- **Mass auto-revocations in 2010–2012** under the Pension Protection Act produce a large artificial spike. Flag those years, and offer `drop_ppa_revocations = TRUE` or a smoothed series.

**Cross-universe**

- Filer↔grassroots transitions per year: a graduation rate and a downgrade rate.

### 2.6 Front door

```r
npc_build_metrics(
  years     = 2012:2022,
  geo       = c("cbsa", "county"),
  universe  = c("filers", "registered", "grassroots"),
  metrics   = "default",          # or a character vector from npc_metrics()
  resources = c("TOTREV", "TOTEXP", "NETASS"),
  source    = c("archive", "nccs"),
  window    = 1,                  # >1 = pooled rolling windows to stabilize small markets
  format    = c("long", "wide"),
  out_dir   = NULL                # if set, writes CSV/Parquet + manifest.json
)
npc_metrics()                     # registry listing: name, needs_resource, universes, description
npc_load_metrics(vintage = "latest")  # precomputed output tables (optional, §3 phase 5)
```

Output is long by default, with columns `GEO, GEO_LEVEL, SUB, YEAR, UNIVERSE, RESOURCE, METRIC, VALUE, N`. The existing wide CSV layout is available through `format = "wide"`, so the Shiny app keeps working.

---

## 3. Phased work plan

### Phase 0: Make it a package again (about 1 day)

- Move scripts out of `R/`:
  - `CREATE-METRICS-*.R` and `add-pop-*.R` go to `data-raw/`.
  - `scale-free-*SCRATCH*` / `-robust` and `DESCRIPTIVE-ANALYSIS.R` go to `analysis/`, which is build-ignored.
  - `R/old/` is deleted or moved out.
- Move all `R/*.CSV` out of the repo. Add `*.CSV` / `*.csv` under `R/` to `.gitignore`.
- Un-ignore `.Rbuildignore` (it is currently listed in `.gitignore`) and add `^data-raw$`, `^analysis$`, `^DEV-PLAN\.md$`.
- Keep one `get_all_metrics()` and one `bottomcode()`. Convert `all-metrics-v2.R` to UTF-8.
- Fill in `DESCRIPTION`:
  - Imports: dplyr, tidyr, purrr, rlang, data.table, DescTools (or an internal Gini).
  - Suggests: arrow, testthat, nccsdata.
- Re-run roxygen.
- Add a GitHub Actions `R CMD check` workflow.
- **Done when:** `devtools::check()` passes with no errors.

### Phase 1: Tests and bug fixes (2–3 days)

- Write a `testthat` fixture: about 30 orgs across 3 markets and 2 years, with hand-computed values for HHI, NHHI, CR, Kwoka, Gini and density.
- Fix bugs 1–5 in §1.3. The zero-count market frame is the most important; tests should cover zero cells and that `ALL` rows are excluded from model fitting.
- Unify signatures (bug 6). Deprecate the old `get_density_*` and `get_kwoka_index` with `lifecycle` warnings.
- Rebuild `nonprofit_sample` on the canonical schema.

### Phase 2: Archive source and year selection (2 days)

- `npc_standardize()` maps the SOI panel to the canonical schema.
- `npc_build_archive()` converts the CSV to Parquet by year. `npc_get_orgs(source = "archive", universe = "filers")` reads it.
- Bundle county and CBSA population for 2000–2025 plus a county→CBSA crosswalk in `data/`. Build them with `data-raw/population.R`.
- Add the completeness check and coverage validation.
- `npc_build_metrics()` for filers.
- **Done when:** the output reproduces the current `CONCENTRATION-METRICS-*.CSV` within tolerance. The only expected differences are the documented ones from the bug fixes.

### Phase 3: Metric registry and count metrics (2 days)

- `summarize_levels()` and the registry; port the existing metrics onto them.
- Add count density, small-org density, median age and rolling windows.
- Universe/metric guard rails with informative errors.

### Phase 4: Live NCCS source (3–5 days; most of the unknowns are here)

- Download helpers with caching, retries and the manifest. Use `nccsdata` where it covers the need (BMF parquet). Use direct S3 GETs for CORE, the BMF archives, e-postcards and revocations.
- Harmonize CORE PC/PZ to the canonical schema: field crosswalk by year, since CORE variable names change across eras.
- Geocode with the unified BMF (county FIPS), then apply the CBSA crosswalk.
- **Validation:** for overlapping years, compare live filers against archive filers (counts by market, totals by resource) and report the differences.

**Status (2026-11-24): done.** `R/source-nccs.R`, `data-raw/validate-nccs-source.R`.

- Source used: the harmonized panel `processed_merged/core/{Y}/990combined/core_{Y}_990combined.parquet` (2010–2024, 990 + 990-EZ, all subsections, about 50 MB a year). It already resolves the CORE schema drift, so no per-era crosswalk was needed. Geography, NTEEV2 and ruling year come from `geocoding/unified-bmf/latest/bmf_unified_geocoded.parquet`.
- The BMF has no county FIPS, only geocoded county and state names; `npc_match_county()` matches them to `pop_county`; 0.18% of filers end up with no county.
- Tax year = year the fiscal period began (as in the SOI panel), so tax year *t* needs CORE files *t* and *t*+1.
- Validation, TY2018–2019, 501(c)(3): EIN sets identical except 3–4 archive-only EINs a year; TOTREV/TOTEXP/TOTASS equal for 100% of matched orgs (all but one in 2019), NETASS 99.9% (after using assets − liabilities for 990-EZ); national totals agree to within $1–2 million out of $2.4–2.5 trillion. Market counts correlate at 0.9999 (median absolute difference 2.2% for counties, 1.7% for CBSAs), mostly because live builds use each org's current BMF address.
- This answers open question 1 in practice: the SOI panel frame is the population of 990/990-EZ filers and matches the public NCCS files.
- **Found and fixed in the archive:** about 1,100–1,400 duplicate EIN-year rows per year (mostly short-period returns after accounting-period changes, some exact repeats), which inflated archive counts by about 0.3% and counted those orgs twice in concentration metrics. `npc_standardize()` and `npc_get_orgs()` now keep one row per EIN-year: a real filing over an imputed row, then the latest source file, then the latest period end. This picks the same filing as NCCS for all 2,966 duplicated EIN-years in TY2018–2019.
- Not covered: fundraising fees (no field in 990combined), revocations and e-postcards (Phase 5).

### Phase 5: Registered and grassroots universes plus dynamics (3–4 days)

- Build an annual registered universe from the BMF snapshots, with the revocation-list fallback.
- `FILER_TYPE` classification; churches option.
- `R/dynamics.R`: entry, exit, churn and graduation, with censoring and the 2010–2012 auto-revocation flag.
- Optional: publish versioned output tables as GitHub release assets (not committed to the repo). `npc_load_metrics()` fetches them, which replaces the 1990–2019 `data-share/np-compete.csv`.

### Phase 6: Docs and dashboard (1–2 days)

- Write a vignette on universes, explaining which metrics are valid where and why exits from filing are not deaths.
- Write a vignette on archive vs live builds and reproducibility through manifests.
- Point the Shiny app at `npc_build_metrics()` output and add a universe toggle.
- Rewrite the README: install, the quick start, and the bug in the current README example (a comma is missing after `resource='TOTREV'`).

---

## 4. Open questions (need answers before Phases 4–5)

1. **What exactly is the SOI panel frame?** It looks like the population of 990/990-EZ filers (the frame SOI samples from), not the weighted SOI sample. If it is the sample, every count metric needs weights. Can it be rebuilt from public NCCS files (CORE / efile)? Or is it only available internally? If only internally, live filer mode will be a CORE-based approximation, not an exact match.
2. **Historical 990-N status.** The e-postcard files hold only each org's latest filing. Do monthly e-postcard archives exist on the NCCS S3 bucket, or a 990-N panel? If not, `990N` and `NONFILER` can be told apart only for recent years. For earlier years the grassroots universe would be a single category: "registered, not filing a 990/EZ".
3. **Churches.** Should orgs not required to file (filing requirement `06`) count in grassroots density by default?
4. **Geography vintage.** Use one CBSA delineation for all years (comparable over time), or the delineation in force each year?
5. **Fiscal vs tax year.** Is `TAXYEAR` the right period for the filer panel? It mixes fiscal years that end in different months. For registered dynamics, use calendar years from the BMF snapshot month.

## 5. Risks

- **CORE schema drift** across 1989–2022 is the largest effort risk in Phase 4. Limit live mode to 2010 onward at first.
- **Data size:** the BMF archives and CORE run to several GB. Parquet with predicate pushdown (arrow) is required, and live mode should download only the requested years.
- **Small-market noise:** subsector × county cells are often 0–3 orgs. Rolling windows and minimum-N flags (`N < 5` → flag) should ship together with the new count metrics.
