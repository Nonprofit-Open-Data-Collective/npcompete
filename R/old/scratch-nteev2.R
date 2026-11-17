#' Convert legacy NTEE codes to the updated 3-part format
#'
#' These functions translate legacy NCCS NTEE codes into a standardized
#' three-part structure of the form `INDUSTRY-NTEE-ORGTYPE`.  
#' Each segment encodes a distinct conceptual layer:
#' 
#' - `INDUSTRY`: high-level field of activity (e.g., `ART`, `EDU`, `HEL`, `HMS`, etc.)
#' - `NTEE`: original or normalized NTEE code (e.g., `B29`, `A6A`, `Z99`)
#' - `ORGTYPE`: organization type indicator (`RG`, `AA`, `MT`, etc.)
#'
#' @section Conversion Rules:
#' \describe{
#'   \item{Industry (`get_industry()`)}{
#'     Based on the first letter of the NTEE code with special cases for
#'     universities and hospitals.
#'     \itemize{
#'       \item `B40–B43`, `B50` → `UNI`
#'       \item `E20–E24` → `HOS`
#'       \item See \url{https://nccs.urban.org/nccs/pubs/ntee-two-page-2005.pdf}
#'     }
#'   }
#'   \item{Org type (`get_org_type()`)}{
#'     Derived from the first two digits of the numeric portion:
#'     \itemize{
#'       \item `01` = AA (Advocacy)
#'       \item `02` = MT (Mgmt & Technical Assistance)
#'       \item `03` = PA (Professional Association)
#'       \item `05` = RP (Research/Public Policy)
#'       \item `11` = MS (Monetary Support - Single Org)
#'       \item `12` = MM (Monetary Support - Multiple Orgs)
#'       \item `19` = NS (Nonmonetary Support)
#'       \item Otherwise = RG (Regular Nonprofit)
#'     }
#'   }
#'   \item{Full code (`get_nteev2()`)}{
#'     Combines the two components:
#'     \itemize{
#'       \item Regular orgs → `[INDUSTRY]-[CODE]-RG`
#'       \item Specialty orgs (e.g. `B01`) → `[INDUSTRY]-B00-AA`
#'       \item Specialty with 4-digit code (e.g. `B0129`) → `[INDUSTRY]-B29-AA`
#'       \item Letter-number-letter codes (e.g. `A6A`) → `[INDUSTRY]-A6A-RG`
#'       \item Blanks → `UNU-Z99-RG`
#'     }
#'   }
#' }
#'
#' @param x Character vector of legacy NTEE codes.
#'
#' @return For `get_industry()` and `get_org_type()`, a character vector.
#'         For `get_nteev2()`, a fully formatted 3-part code.
#' @name ntee_conversion
#' @examples
#' codes <- c("B29", "B01", "B0129", "A6A", "", "E22", "B50")
#' convert_ntee(codes)
NULL


# ---------------------------------------------------------------------
#' @rdname ntee_conversion
#' @export
get_industry <- function(x) {
  x <- toupper(trimws(x))
  
  ifelse(x == "" | is.na(x), "UNU",
  ifelse(substr(x, 1, 3) %in% c("B40", "B41", "B42", "B43", "B50"), "UNI",
  ifelse(substr(x, 1, 3) %in% c("E20", "E21", "E22", "E24"), "HOS",
  switch(substr(x, 1, 1),
         "A" = "ART",
         "B" = "EDU",
         "C" = "ENV",
         "D" = "ENV",
         "E" = "HEL",
         "F" = "HEL",
         "G" = "HEL",
         "H" = "HEL",
         "I" = "HMS",
         "J" = "HMS",
         "K" = "HMS",
         "L" = "HMS",
         "M" = "HMS",
         "N" = "HMS",
         "O" = "HMS",
         "P" = "HMS",
         "Q" = "IFA",
         "R" = "PSB",
         "S" = "PSB",
         "T" = "PSB",
         "U" = "PSB",
         "V" = "PSB",
         "W" = "PSB",
         "X" = "REL",
         "Y" = "MMB",
         "Z" = "UNU",
         "UNU"))))
}


# ---------------------------------------------------------------------
#' @rdname ntee_conversion
#' @export
get_org_type <- function(x) {
  x <- toupper(trimws(x))
  
  ifelse(x == "" | is.na(x), "RG", {
    two <- suppressWarnings(as.numeric(substr(x, 2, 3)))
    if (is.na(two)) "RG" else
      dplyr::case_when(
        two == 1  ~ "AA",
        two == 2  ~ "MT",
        two == 3  ~ "PA",
        two == 5  ~ "RP",
        two == 11 ~ "MS",
        two == 12 ~ "MM",
        two == 19 ~ "NS",
        TRUE      ~ "RG"
      )
  })
}


# ---------------------------------------------------------------------
#' @rdname ntee_conversion
#' @export
get_nteev2 <- function(x) {
  x <- toupper(trimws(x))
  
  sapply(x, function(code_raw) {
    # Blank → UNU-Z99-RG
    if (is.na(code_raw) || code_raw == "") {
      return("UNU-Z99-RG")
    }
    
    # Right-pad 4-char codes to 5 chars (e.g., A115 -> A1150)
    code <- if (nchar(code_raw) == 4 && grepl("^[A-Z][0-9]{3}$", code_raw)) {
      stringr::str_pad(code_raw, width = 5, side = "right", pad = "0")
    } else {
      code_raw
    }
    
    industry <- get_industry(code)
    org_type <- get_org_type(code)
    
    # Letter-number-letter pattern (e.g., A6A) → pass through as the NTEE piece
    if (grepl("^[A-Z][0-9]+[A-Z]+$", code)) {
      return(paste0(industry, "-", code, "-", org_type))
    }
    
    # Regular nonprofits: keep the (possibly padded) code
    if (org_type == "RG") {
      return(paste0(industry, "-", code, "-", org_type))
    }
    
    # Specialty orgs with 5-char letter+4 digits (e.g., B0129, E1191)
    if (nchar(code) == 5 && grepl("^[A-Z][0-9]{4}$", code)) {
      # Example: B0129 -> take last two digits "29" -> B29
      letter   <- substr(code, 1, 1)
      last_two <- substr(code, 4, 5)
      return(paste0(industry, "-", letter, last_two, "-", org_type))
    }
    
    # Specialty orgs with 3-char codes like B01 → B00-<type>
    if (nchar(code) == 3 && grepl("^[A-Z][0-9]{2}$", code)) {
      letter <- substr(code, 1, 1)
      return(paste0(industry, "-", letter, "00-", org_type))
    }
    
    # Fallback: keep whatever code we have
    paste0(industry, "-", code, "-", org_type)
  })
}

# ---------------------------------------------------------------------
#' Vectorized wrapper for NTEE v2 conversion
#'
#' Convenience function that returns a tibble with the original codes,
#' derived industry, org_type, and full NTEE v2 code.
#'
#' @param x Character vector of legacy NTEE codes.
#' @return A tibble with columns: `ntee`, `industry`, `org_type`, `nteev2`.
#' @export
#' @examples
#' codes <- c("B29", "B01", "B0129", "A6A", "", "E22", "B50")
#' convert_ntee(codes)
convert_ntee <- function(x) {
  tibble::tibble(
    ntee      = x,
    industry  = get_industry(x),
    org_type  = get_org_type(x),
    nteev2    = get_nteev2(x)
  )
}