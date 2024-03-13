#' Loads or downloads the necessary data for all modules
#'
#' Either downloads the necessary data for all modules from Eurostat using
#' dataset ids or loads the files provided in the user-specified input directory
#' and selects the required variables.
#'
#' @param specification A tibble or data.frame as returned by
#'   \code{\link{check_config_table}}.
#' @param dictionary A tibble or data.frame storing the Eurostat variable code
#'   in column 'eurostat_code'. If \code{download == TRUE} then the dictionary
#'   also requires column named 'dataset_id' that stores the Eurostat dataset
#'   id. When \code{NULL}, the \link[=dict]{default dictionary} is used.
#' @param primary_source A character of length 1 determining whether the
#' required variables are first obtained from \code{"download"} and then
#' \code{"local"}ly or vice versa.
#' @param inputdata_directory A path to .rds, .csv, or .xlsx input files in
#' which the data is stored. Can be \code{NULL}, in which case data is obtained
#' via download if possible.
#' @param save_to_disk A path to a directory where the final dataset will be
#'   saved, including file name and ending. Not saved when \code{NULL}.
#' @param quiet Logical with default = \code{FALSE}. Should messages be displayed?
#' These messages are intended to give more information about the estimation
#' and data retrieval process.
#' @param constrain.to.minimum.sample Logical. Should all data series be
#' constrained to the minimum data series? Default is \code{TRUE}.
#'

load_or_download_variables <- function(specification,
                                       dictionary = NULL,
                                       primary_source = c("download", "local"),
                                       inputdata_directory = NULL,
                                       save_to_disk = NULL,
                                       quiet = FALSE,
                                       constrain.to.minimum.sample = TRUE) {

  # input check
  # primary_source must be "download" or "local"
  primary_source <- match.arg(primary_source)
  # user dictionary or default (TODO: write a function that validates whether user dict is ok)
  if (is.null(dictionary)) {
    dictionary <- aggregate.model::dict
  }
  # determine whether user has added additional filters
  default_cols <- colnames(aggregate.model::dict)
  actual_cols <- colnames(dictionary)
  additional_filters <- setdiff(actual_cols, default_cols)

  # which variables need to be obtained (download or locally)
  to_obtain <- determine_variables(specification = specification,
                                   dictionary = dictionary)

  # if to_obtain$database contains "local" then must have specified directory
  if ("local" %in% unique(to_obtain$database) && is.null(inputdata_directory)) {
    stop("At least one of the variables comes from 'local' file but no inputdata_directory has been specified.")
  }

  if (is.character(inputdata_directory)) {
    if (file.exists(inputdata_directory) & !dir.exists(inputdata_directory)) {
      stop("The variable 'inputdata_directory' must be a character path to a directory, not to a file.")
    }
  }

  # initialise empty df to store the overall data
  full <- data.frame()

  # sources
  sources <- unique(to_obtain$database)
  if (length(setdiff(sources, c("eurostat", "edgar", "local"))) >= 1L) {
    stop("Currently, only allow data bases 'eurostat', 'edgar', or 'local' files.")
  }
  if ("edgar" %in% sources) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package \"readxl\" must be installed to read in .xlsx files from EDGAR.")
    }
  }

  # Save original timeout setting
  tmpmc <- options("timeout")
  on.exit(options(tmpmc)) # set the old timeout setting on exit
  options(timeout = 1000)

  # data input
  if (primary_source == "download") {

    # steps:
    # 1) download from eurostat
    # 2) download from edgar
    # 3) local loading
    # -> since download updates "found", local does not overwrite (download takes precedence)

    if ("eurostat" %in% sources) {
      step1 <- download_eurostat(to_obtain = to_obtain,
                                 additional_filters = additional_filters,
                                 quiet = quiet)
      to_obtain <- step1$to_obtain
      full <- dplyr::bind_rows(full, step1$df)
    }
    if ("edgar" %in% sources) {
      step2 <- download_edgar(to_obtain = to_obtain, quiet = quiet)
      to_obtain <- step2$to_obtain
      full <- dplyr::bind_rows(full, step2$df)
    }
    if ("local" %in% sources) {
      step3 <- load_locally(to_obtain = to_obtain,
                            inputdata_directory = inputdata_directory,
                            quiet = quiet)
      to_obtain <- step3$to_obtain
      full <- dplyr::bind_rows(full, step3$df)
    }
    if (any(to_obtain$found == FALSE)) {
      stop(paste0("The following variables could not be obtained: ",
                  paste(to_obtain %>% dplyr::filter(.data$found == FALSE) %>% dplyr::pull(.data$model_varname), collapse = ", ")))
    }

  } else { # (primary_source == "local")

    # steps:
    # 1) local loading
    # 2) download from eurostat
    # 3) download from edgar
    # -> since local loading updates "found", only download if not found locally (local loading takes precedence)

    # this might load data even if it has database == "eurostat" or "edgar" (b/c local first)
    # step1 only makes sense when inputdata_directory has been specified
    if (!is.null(inputdata_directory)) {
      step1 <- load_locally(to_obtain = to_obtain,
                            inputdata_directory = inputdata_directory,
                            quiet = quiet)
      to_obtain <- step1$to_obtain
      full <- dplyr::bind_rows(full, step1$df)
    }

    not_loaded_eurostat <- which(to_obtain$database == "eurostat" & to_obtain$found == FALSE)
    if (length(not_loaded_eurostat) > 0L) {
      step2 <- download_eurostat(to_obtain = to_obtain,
                                 additional_filters = additional_filters,
                                 quiet = quiet)
      to_obtain <- step2$to_obtain
      full <- dplyr::bind_rows(full, step2$df)
    }
    not_loaded_edgar <- which(to_obtain$database == "edgar" & to_obtain$found == FALSE)
    if (length(not_loaded_edgar) > 0L) {
      step3 <- download_edgar(to_obtain = to_obtain, quiet = quiet)
      to_obtain <- step3$to_obtain
      full <- dplyr::bind_rows(full, step3$df)
    }
    if (any(to_obtain$found == FALSE)) {
      stop(paste0("The following variables could not be obtained: ",
                  paste(to_obtain %>% dplyr::filter(.data$found == FALSE) %>% dplyr::pull(.data$model_varname), collapse = ", ")))
    }

  } # end primary_source == "local"

  # might have to deal with unbalanced data (though arx/isat might deal with it?)
  # quick solution for our present case, might not work for all cases
  availability <- full %>%
    dplyr::group_by(.data$na_item) %>%
    dplyr::summarise(
      min_date = min(.data$time),
      max_date = max(.data$time),
      n = dplyr::n()
    ) %>%
    dplyr::ungroup()



  if (!is.null(save_to_disk)) {

    if (!is.character(save_to_disk)) {stop("'save_to_disk' must be a character file path.")}

    if (!dir.exists(gsub("/[^/]*$","",save_to_disk))) {
      dir.create(gsub("/[^/]*$","",save_to_disk))
    }

    # which file ending was chosen
    ending <- stringr::str_extract(string = save_to_disk, pattern = "\\.[:alpha:]+$")
    if (ending %in% c(".RDS", ".rds", ".Rds")) {
      saveRDS(object = full, file = save_to_disk)
    } else if (ending == ".csv") {
      if (!requireNamespace("readr", quietly = TRUE)) {
        stop("Package \"readr\" must be installed to save .csv files.")
      }
      readr::write_csv(x = full, file = save_to_disk)
    } else if (ending %in% c(".xls", ".xlsx")) {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Package \"writexl\" must be installed to save .xls or .xlsx files.")
      }
      writexl::write_xlsx(x = full, path = save_to_disk)
    } else {
      warning(paste0("File ending currently chosen in 'save_to_disk' is ", ending, ", which is not yet implemented. Please choose one of RDS, rds, Rds, csv, xls, xlsx."))
    }

  }

  # This must come after saving
  if (constrain.to.minimum.sample) {
    if (max(stats::dist(availability$n, method = "maximum") / max(availability$n)) > 0.2) {
      warning("Unbalanced panel, will lose more than 20\\% of data when making balanced")
    }
    min_date <- max(availability$min_date) # highest minimum date
    max_date <- min(availability$max_date) # lowest maximum date
    full <- full %>%
      dplyr::filter(.data$time >= min_date & .data$time <= max_date)
    # might still not be balanced but beginning- & end-points are balanced
    # I believe zoo in gets deals with unbalanced inside time period (could be wrong)
  }

  return(full)

}


#' Download Eurostat
#'
#' Downloads Eurostat data given a data.frame of required variables
#'
#' @inheritParams load_or_download_variables
#' @param to_obtain A data.frame as returned by [determine_variables()].
#' @param additional_filters A character vector storing the column names of
#' additional filters if present. Length 0 if not present.
#'
#' @return Returns a list with two named elements: \code{$df} stores the
#' downloaded data and \code{$to_obtain} the updated data.frame tracking which
#' variables still need to be obtained.

download_eurostat <- function(to_obtain, additional_filters, quiet) {

  # note:
  # ideally wanted to set filters during download, so don't need to download all data series and all countries
  # eurostat::get_eurostat(id = "namq_10_gdp", filters = list(na_item = "B1GQ", geo = "AT"), cache = FALSE)
  # but eurostat JSON API cannot handle this, says "too many categories" (there are issues about this on GitHub)
  # so for now, need to download whole dataset and only then select subset

  # initialise empty df
  df_eurostat <- data.frame()

  # download Eurostat
  eurostat_dataset_ids <- to_obtain %>%
    dplyr::filter(.data$database == "eurostat" & .data$found == FALSE) %>%
    dplyr::pull(.data$dataset_id) %>%
    unique()

  # loop through required datasets
  for (i in seq_along(eurostat_dataset_ids)) {

    # download dataset
    if(quiet){
      suppressWarnings(suppressMessages(tmp <- eurostat::get_eurostat(id = eurostat_dataset_ids[i]) %>%
                                          dplyr::rename(time = "TIME_PERIOD") %>%
                                          dplyr::select(-dplyr::any_of("freq"))))
    } else {
      tmp <- eurostat::get_eurostat(id = eurostat_dataset_ids[i]) %>%
        dplyr::rename(time = "TIME_PERIOD") %>%
        dplyr::select(-dplyr::any_of("freq"))
    }
    # check whether download worked
    if(is.null(tmp)) {
      stop("Issue with automatic EUROSTAT download. Likely cause is a lack of/an unstable internet connection. Check your internet connection. Also consider saving the downloaded data to disk using 'save_to_disk' and 'inputdata_directory'.")
    }
    # extract each variable that we are looking for in this dataset and apply filters
    indices <- which(to_obtain$database == "eurostat" & to_obtain$dataset_id == eurostat_dataset_ids[i])
    for (j in indices) {
      # under which column is the variable stored?
      varcolname <- to_obtain$var_col[j]
      sub <- tmp %>%
        dplyr::filter(!!as.symbol(varcolname) == to_obtain$variable_code[j]) %>%
        {if(dplyr::select(., dplyr::any_of("geo")) %>% ncol == 1){dplyr::filter(., .data$geo == to_obtain$geo[j])}else{.}} %>%
        {if(dplyr::select(., dplyr::any_of("unit")) %>% ncol == 1){dplyr::filter(., .data$unit == to_obtain$unit[j])}else{.}} %>%
        {if(dplyr::select(., dplyr::any_of("s_adj")) %>% ncol == 1){dplyr::filter(., .data$s_adj == to_obtain$s_adj[j])}else{.}} %>%
        {if(dplyr::select(., dplyr::any_of("nace_r2")) %>% ncol == 1){dplyr::filter(., .data$nace_r2 == to_obtain$nace_r2[j])}else{.}} %>%
        {if(dplyr::select(., dplyr::any_of("ipcc_sector")) %>% ncol == 1){dplyr::filter(., .data$ipcc_sector == to_obtain$ipcc_sector[j])}else{.}} %>%
        {if(dplyr::select(., dplyr::any_of("cpa2_1")) %>% ncol == 1){dplyr::filter(., .data$cpa2_1 == to_obtain$cpa2_1[j])}else{.}} %>%
        {if(dplyr::select(., dplyr::any_of("siec")) %>% ncol == 1){dplyr::filter(., .data$siec == to_obtain$siec[j])}else{.}}
      # if user specified additional filters, apply them now
      for (k in seq_along(additional_filters)) {
        filtername <- additional_filters[k]
        sub <- sub %>%
          {if(dplyr::select(., dplyr::any_of(filtername)) %>% ncol == 1){dplyr::filter(., .data[[filtername]] == to_obtain[j, filtername])}else{.}}
      }
      # if after filtering "sub" is not empty, we found the variable and can mark it as such
      if (NROW(sub) == 0L) {
        stop(paste0("For model variable '", to_obtain$model_varname[j], "', the dataset is empty after applying filter. Check whether the dictionary and the data source for changes and errors (i.e. name of units, etc.)"))
      } else {
        to_obtain[j, "found"] <- TRUE
      }
      # rename the column storing the variable to "na_item", as most columns are called
      # replace the database variable code with the model variable name
      sub <- sub %>%
        dplyr::rename_with(.cols = dplyr::all_of(varcolname), .fn = ~paste0("na_item")) %>%
        dplyr::mutate(na_item = to_obtain$model_varname[j])
      # if have monthly data, need to aggregate to quarterly
      if (to_obtain$freq[j] == "m") {
        # need to aggregate across all filters
        columns <- colnames(sub)
        unique_columns <- setdiff(columns, "values") # should be unique across these
        stopifnot(sum(duplicated(sub[, unique_columns])) == 0L) # sanity check
        groupby_columns <- union(c("year", "quarter"), setdiff(unique_columns, "time")) # want to group_by year-quarter, so exclude time column
        sub <- sub %>%
          dplyr::mutate(year = lubridate::year(.data$time),
                        quarter = lubridate::quarter(.data$time)) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(groupby_columns))) %>%
          dplyr::summarise(values = sum(.data$values),
                           n = dplyr::n(), # record how many months are available in each quarter
                           time = min(.data$time)) %>%
          dplyr::ungroup() %>%
          # drop "incomplete" quarters
          dplyr::filter(.data$n == 3L) %>%
          dplyr::select(-"year", -"quarter", -"n")
      }
      # ensure column "time" is a Date variable (Moritz had this)
      sub <- sub %>%
        dplyr::mutate(time = as.Date(.data$time))
      # add subset to df_eurostat, final dataset
      df_eurostat <- dplyr::bind_rows(df_eurostat, sub)
    } # end for variables
  } # end for datasets

  return(list(df = df_eurostat, to_obtain = to_obtain))

}

#' Download Edgar
#'
#' Downloads Edgar data given a data.frame of required variables
#'
#' @inheritParams download_eurostat
#'
#' @return Returns a list with two named elements: \code{$df} stores the
#' downloaded data and \code{$to_obtain} the updated data.frame tracking which
#' variables still need to be obtained.

download_edgar <- function(to_obtain, quiet) {

  # initialise empty df
  df_edgar <- data.frame()

  # download EDGAR
  edgar_dataset_ids <- to_obtain %>%
    dplyr::filter(.data$database == "edgar" & .data$found == FALSE) %>%
    dplyr::pull(.data$dataset_id) %>%
    unique()
  # these should be links
  if (length(edgar_dataset_ids) != sum(grepl(pattern = "^http", x = edgar_dataset_ids))) {
    stop("Database 'edgar' should be a link and therefore start with 'http'.")
  }

  # loop through required datasets
  for (i in seq_along(edgar_dataset_ids)) {

    # download dataset
    # create temporary file to save zip file
    tmp_download <- tempfile(fileext = "zip")
    if(quiet){
      suppressWarnings(suppressMessages(
        utils::download.file(url = edgar_dataset_ids[i],
                             destfile = tmp_download,
                             mode = "wb")))
    } else {
      utils::download.file(url = edgar_dataset_ids[i],
                           destfile = tmp_download,
                           mode = "wb")
    }
    # create temporary directory to extract contents of zip file into
    tmp_extract <- tempdir()
    # determine .xlsx filename (should work in future if naming convention stays the same)
    # extract everything starting from one of the GHG abbreviations to the end, including .zip
    zipfilename <- stringr::str_extract(string = edgar_dataset_ids[i], pattern = "(CO2|CH4|N2O)(.)+(\\.zip$)")
    # extract GHG name
    ghg <- stringr::str_extract(string = zipfilename, pattern = "(CO2|CH4|N2O)")

    # assume that will continue to be able to remove "_m" and replace ".zip" by ".xlsx
    # commented out by Moritz 13.03.2024 due to update of file convention by EDGAR for v8
    #filename <- stringr::str_remove(string = zipfilename, pattern = "_m")
    # this now matches everything from the last /
    filename <- stringr::str_extract(string = edgar_dataset_ids[i], pattern = "([^/]+$)")
    filename <- stringr::str_replace(string = filename, pattern = ".zip", replacement = ".xlsx")

    # unzip the .xlsx file into temporary directory
    utils::unzip(zipfile = tmp_download, files = filename, exdir = tmp_extract)

    # read in the data
    columns <- c("Country_code_A3", "Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    # have different sheets depending on whether want by IPCC sector or TOTAL (different from nace_r2, where TOTAL is nested)
    # so depending on the filter, need to read in a different sheet
    # extract each variable that we are looking for in this dataset and apply filters; do this by sheet for efficiency
    indices_total <- which(to_obtain$database == "edgar" & to_obtain$dataset_id == edgar_dataset_ids[i] & to_obtain$ipcc_sector == "TOTAL")
    indices_sector <- which(to_obtain$database == "edgar" & to_obtain$dataset_id == edgar_dataset_ids[i] & to_obtain$ipcc_sector != "TOTAL")
    # load in sheet with total emissions
    if (length(indices_total) > 0L) {
      tmp <- readxl::read_excel(path = file.path(tmp_extract, filename),
                                sheet = "TOTALS BY COUNTRY",
                                skip = 9)
      # quick sanity check
      stopifnot(unique(tmp$Substance) == ghg)
      # extract each variable that we are looking for in this dataset and apply filters
      for (j in indices_total) {
        # filter the data
        sub <- tmp %>%
          dplyr::select(dplyr::all_of(columns)) %>%
          dplyr::filter(.data$Country_code_A3 == countrycode::countrycode(to_obtain$geo[j], "iso2c", "iso3c")) %>%
          dplyr::rename(geo = "Country_code_A3") %>%
          dplyr::mutate(geo = to_obtain$geo[j])
        # shape into long format
        sub <- sub %>%
          tidyr::pivot_longer(cols = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                              names_to = "Month",
                              values_to = "values") %>%
          dplyr::mutate(Month = dplyr::case_when(.data$Month == "Jan" ~ "01",
                                                 .data$Month == "Feb" ~ "02",
                                                 .data$Month == "Mar" ~ "03",
                                                 .data$Month == "Apr" ~ "04",
                                                 .data$Month == "May" ~ "05",
                                                 .data$Month == "Jun" ~ "06",
                                                 .data$Month == "Jul" ~ "07",
                                                 .data$Month == "Aug" ~ "08",
                                                 .data$Month == "Sep" ~ "09",
                                                 .data$Month == "Oct" ~ "10",
                                                 .data$Month == "Nov" ~ "11",
                                                 .data$Month == "Dec" ~ "12",
                                                 TRUE ~ "error")) %>%
          dplyr::mutate(time = as.Date(paste(.data$Year, .data$Month, "01", sep = "-"))) %>%
          dplyr::select(-"Year", -"Month") %>%
          dplyr::mutate(year = lubridate::year(.data$time),
                        quarter = lubridate::quarter(.data$time)) %>%
          dplyr::group_by(.data$geo, .data$year, .data$quarter) %>%
          dplyr::summarise(values = sum(.data$values),
                           n = dplyr::n(), # record how many months are available in each quarter
                           time = min(.data$time)) %>%
          dplyr::ungroup() %>%
          # drop "incomplete" quarters (should not occur b/c released when complete)
          dplyr::filter(.data$n == 3L) %>%
          dplyr::select(-"year", -"quarter", -"n") %>%
          dplyr::mutate(na_item = to_obtain$model_varname[j])
        # if after filtering "sub" is not empty, we found the variable and can mark it as such
        if (NROW(sub) == 0L) {
          stop(paste0("For model variable '", to_obtain$model_varname[j], "', the dataset is empty after applying filter."))
        } else {
          to_obtain[j, "found"] <- TRUE
        }
        # ensure column "time" is a Date variable (Moritz had this)
        sub <- sub %>%
          dplyr::mutate(time = as.Date(.data$time))
        # add subset to df_edgar, final dataset
        df_edgar <- dplyr::bind_rows(df_edgar, sub)
      } # end for variables (total)
    } # end if variables (total)

    if (length(indices_sector) > 0L) {
      tmp <- readxl::read_excel(path = file.path(tmp_extract, filename),
                                #sheet = paste0(ghg, "_IPCC2006"),
                                sheet = "IPCC 2006",
                                skip = 9)
      # quick sanity check
      stopifnot(unique(tmp$Substance) == ghg)
      # extract each variable that we are looking for in this dataset and apply filters
      for (j in indices_sector) {
        # filter the data
        sub <- tmp %>%
          dplyr::select(dplyr::all_of(c(columns, "ipcc_code_2006_for_standard_report"))) %>%
          dplyr::filter(.data$Country_code_A3 == countrycode::countrycode(to_obtain$geo[j], "iso2c", "iso3c")) %>%
          dplyr::rename(geo = "Country_code_A3") %>%
          dplyr::mutate(geo = to_obtain$geo[j])
        # shape into long format
        sub <- sub %>%
          tidyr::pivot_longer(cols = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                              names_to = "Month",
                              values_to = "values") %>%
          dplyr::mutate(Month = dplyr::case_when(.data$Month == "Jan" ~ "01",
                                                 .data$Month == "Feb" ~ "02",
                                                 .data$Month == "Mar" ~ "03",
                                                 .data$Month == "Apr" ~ "04",
                                                 .data$Month == "May" ~ "05",
                                                 .data$Month == "Jun" ~ "06",
                                                 .data$Month == "Jul" ~ "07",
                                                 .data$Month == "Aug" ~ "08",
                                                 .data$Month == "Sep" ~ "09",
                                                 .data$Month == "Oct" ~ "10",
                                                 .data$Month == "Nov" ~ "11",
                                                 .data$Month == "Dec" ~ "12",
                                                 TRUE ~ "error")) %>%
          dplyr::mutate(time = as.Date(paste(.data$Year, .data$Month, "01", sep = "-"))) %>%
          dplyr::select(-"Year", -"Month")
        # new filter step: IPCC sector
        # allow for upstream filter, e.g. if user sets "1.B":
        # (i) first check whether it exists already; if so, filter it
        # (ii) if not, then check for downstream such as "1.B.1" and "1.B.2" and aggregate by summing
        ipcc_codes_available <- unique(sub$ipcc_code_2006_for_standard_report)
        if (to_obtain$ipcc_sector[j] %in% ipcc_codes_available) {
          # code exists already, simply filter on it
          sub <- sub %>%
            dplyr::filter(.data$ipcc_code_2006_for_standard_report == to_obtain$ipcc_sector[j]) %>%
            dplyr::select(-"ipcc_code_2006_for_standard_report")
        } else {
          # code does not exist, check sub-codes
          ipcc_subcodes <- stringr::str_subset(string = ipcc_codes_available,
                                               pattern = paste0("^", to_obtain$ipcc_sector[j]))
          if (length(ipcc_subcodes) == 0L) {
            stop(paste0("Unable to detect matching IPCC codes or subcodes for variable '",  to_obtain$model_varname[j], "'."))
          }
          sub <- sub %>%
            dplyr::filter(.data$ipcc_code_2006_for_standard_report %in% ipcc_subcodes) %>%
            dplyr::group_by(.data$geo, .data$time) %>%
            dplyr::summarise(values = sum(.data$values)) %>%
            dplyr::ungroup()
          # don't necessarily enforce same number of IPCC codes per month (so don't calculate nobs)
        }
        # now aggregate to quarterly
        sub <- sub %>%
          dplyr::mutate(year = lubridate::year(.data$time),
                        quarter = lubridate::quarter(.data$time)) %>%
          dplyr::group_by(.data$geo, .data$year, .data$quarter) %>%
          dplyr::summarise(values = sum(.data$values),
                           n = dplyr::n(), # record how many months are available in each quarter
                           time = min(.data$time)) %>%
          dplyr::ungroup() %>%
          # drop "incomplete" quarters (should not occur b/c released when complete)
          dplyr::filter(.data$n == 3L) %>%
          dplyr::select(-"year", -"quarter", -"n") %>%
          dplyr::mutate(na_item = to_obtain$model_varname[j])
        # if after filtering "sub" is not empty, we found the variable and can mark it as such
        if (NROW(sub) == 0L) {
          stop(paste0("For model variable '", to_obtain$model_varname[j], "', the dataset is empty after applying filter."))
        } else {
          to_obtain[j, "found"] <- TRUE
        }
        # ensure column "time" is a Date variable (Moritz had this)
        sub <- sub %>%
          dplyr::mutate(time = as.Date(.data$time))
        # add subset to df_edgar, final dataset
        df_edgar <- dplyr::bind_rows(df_edgar, sub)
      } # end for variables (sector)
    } # end if variables (sector)
    # before move to next dataset, unlink to file location
    unlink(tmp_download)
    unlink(file.path(tmp_extract, filename))
  } # end for datasets

  return(list(df = df_edgar, to_obtain = to_obtain))

}

#' Load Locally
#'
#' Loads data from a local directory
#'
#' @inheritParams load_or_download_variables
#' @param to_obtain A data.frame as returned by [determine_variables()].
#'
#' @return Returns a list with two named elements: \code{$df} stores the
#' downloaded data and \code{$to_obtain} the updated data.frame tracking which
#' variables still need to be obtained.

load_locally <- function(to_obtain, inputdata_directory, quiet) {

  # initialise empty df
  df_local <- data.frame()

  if (is.data.frame(inputdata_directory)) { # Moritz allowed for direct input of df, keep this for now (but is not in doc?)

    if(!quiet){
      cat("Variable provided as 'inputdata_directory' seems to be a data.frame type. Used as data source.\n")
    }

    # determine which codes were found in the dataset
    codes.found <- unique(inputdata_directory$na_item)
    # since found those, can update to_obtain
    indices <- which(to_obtain$model_varname %in% codes.found & to_obtain$found == FALSE)
    to_obtain[indices, "found"] <- TRUE

    # subset the relevant data
    df_local <- inputdata_directory %>%
      dplyr::filter(.data$na_item %in% to_obtain$model_varname[indices]) # choose the relevant ones

  } else {

    # order different from the download_xxx() functions
    # here: loop over all files, add them to data if code detected in file & if not found before

    files <- list.files(path = inputdata_directory, pattern = "\\.(Rds|RDS|rds|csv|xlsx|xls)$")
    if(!quiet){
      cat("Local files are used.\n")
      cat("The following files are opened and scanned for relevant data for the model.\n")
      cat(paste0(files, collapse = " "))
      cat("\n")
      cat("Note: If these include non-data files (with a likely different structure and hence likely errors), it is recommended to move all data files to a dedicated directory or to save them there using the 'save_to_disk' argument in the first place:\n")
      cat("\n")
      cat("You can quiet this message with quiet = TRUE.")
      cat("\n")
    }

    # loop through local files
    for (i in seq_along(files)) {

      pth <- file.path(inputdata_directory, files[i])
      if(grepl("\\.(Rds|RDS|rds)$",pth)){
        tmp <- readRDS(file = pth)
      } else if (grepl("\\.(csv)$",pth)){
        if (!requireNamespace("readr", quietly = TRUE)) {
          stop("Package \"readr\" must be installed to read in .csv files.")
        }
        tmp <- readr::read_csv(pth, show_col_types = FALSE, guess_max = 1000000)
      } else if (grepl("\\.(xls|xlsx)$",pth)){
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package \"readxl\" must be installed to read in .xls or .xlsx files.")
        }
        tmp <- readxl::read_excel(path = pth, guess_max = 1000000)
      }

      # run a few checks on the loaded data
      if(!"na_item" %in% names(tmp)){stop(paste0("Locally loaded data not the right format. No 'na_item' column found in file: ",pth,". Set quiet = FALSE to see which files are identified to be loaded." ))}
      if(!"na_item" %in% names(tmp)){stop(paste0("Locally loaded data not the right format. No 'time' column found in file: ",pth,". Set quiet = FALSE to see which files are identified to be loaded." ))}

      # determine which codes were found in the dataset
      codes.found <- unique(tmp$na_item)

      # since found those, can update to_obtain
      indices <- which(to_obtain$model_varname %in% codes.found & to_obtain$found == FALSE)
      if (length(indices) == 0L) {next} # skip to next iteration if no relevant variables found
      to_obtain[indices, "found"] <- TRUE

      # subset the relevant data
      sub <- tmp %>%
        dplyr::filter(.data$na_item %in% to_obtain$model_varname[indices]) %>% # choose the relevant ones
        # ensure column "time" is a Date variable (Moritz had this)
        dplyr::mutate(time = as.Date(.data$time))

      # add the relevant data
      df_local <- dplyr::bind_rows(df_local, sub)

    } # end for files

  }

  return(list(df = df_local, to_obtain = to_obtain))

}


