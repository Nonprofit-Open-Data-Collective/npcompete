get_clean_ntee <- function(x) {
  x <- toupper(trimws(x))
  x[is.na(x) | x == ""] <- "Z99"
  
  # Pad 4-character codes (A115 -> A1150)
  x[nchar(x) == 4] <- stringr::str_pad(x[nchar(x) == 4], width = 5, side = "right", pad = "0")
  
  letter    <- substr(x, 1, 1)
  first_two <- substr(x, 2, 3)
  last_two  <- substr(x, 4, 5)
  
  # Handle non-numeric first_two
  first_two_num <- suppressWarnings(as.numeric(first_two))
  first_two_num[is.na(first_two_num)] <- 99
  
  ntee <- substr(x, 1, 3)  # default (e.g. B29)
  ntee45 <- paste0(letter, last_two)
  ntee00 <- paste0(letter, "00")
  
  # Specialty nonprofits
  specialty3 <- first_two_num <= 19 & nchar(x) == 3
  specialty5 <- first_two_num <= 19 & nchar(x) == 5
  
  ntee[specialty3] <- ntee00[specialty3]
  ntee[specialty5] <- ntee45[specialty5]
  
  # Edge case: digits 4-5 encode industries UNI or HOS (B8443, E4721)
  # Still use default first three for ntee: B84, E47 
  # get_industry() function uses last two to code industry
  
  return( ntee )
}


get_industry <- function(x) {
  x <- toupper(trimws(x))
  x[is.na(x) | x == ""] <- "Z99"
  
  # Normalize code: pad and derive clean_ntee for industry lookup
  x[nchar(x) == 4] <- stringr::str_pad(x[nchar(x) == 4], width = 5, side = "right", pad = "0")
  letter   <- substr(x, 1, 1)
  last_two <- substr(x, 4, 5)
  ntee_ind <- substr(x, 1, 3)
  ntee_ind[nchar(x) == 5] <- paste0(letter[nchar(x) == 5], last_two[nchar(x) == 5])
  
  # Vectorized classification
  industry <- dplyr::case_when(
    ntee_ind %in% c("B40", "B41", "B42", "B43", "B50") ~ "UNI",
    ntee_ind %in% c("E20", "E21", "E22", "E24")       ~ "HOS",
    substr(ntee_ind, 1, 1) == "A" ~ "ART",
    substr(ntee_ind, 1, 1) == "B" ~ "EDU",
    substr(ntee_ind, 1, 1) %in% c("C", "D") ~ "ENV",
    substr(ntee_ind, 1, 1) %in% c("E", "F", "G", "H") ~ "HEL",
    substr(ntee_ind, 1, 1) %in% LETTERS[9:16] ~ "HMS", # I–P
    substr(ntee_ind, 1, 1) == "Q" ~ "IFA",
    substr(ntee_ind, 1, 1) %in% c("R","S","T","U","V","W") ~ "PSB",
    substr(ntee_ind, 1, 1) == "X" ~ "REL",
    substr(ntee_ind, 1, 1) == "Y" ~ "MMB",
    substr(ntee_ind, 1, 1) == "Z" ~ "UNU",
    TRUE ~ "UNU"
  )
  
  return(industry)
}

get_org_type <- function(x) {
  x <- toupper(trimws(x))
  x[ x == "" | is.na(x) ] <- "Z99"

  two <- suppressWarnings(as.numeric(substr(x, 2, 3)))

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
}

get_nteev2 <- function(x) {
  # Vectorized preprocessing
  ntee <- get_clean_ntee(x)
  industry   <- get_industry(x)
  org_type   <- get_org_type(x)
  
  # Default nteev2 composition
  out <- paste0(industry, "-", ntee, "-", org_type)
  
  # Handle letter-number-letter codes (e.g. A6A)
  # is_lnl <- grepl("^[A-Z][0-9]+[A-Z]+$", x)
  # out[is_lnl] <- paste0(get_industry(x[is_lnl]), "-", x[is_lnl], "-", org_type[is_lnl])
  
  # Handle blanks
  # out[x == "" | is.na(x)] <- "UNU-Z99-RG"
  
  return( out )
}


