# test that load_or_download_variables() works correctly
# also test the granular functions download_eurostat(), download_edgar(), load_locally()
# since downloading is involved, run these tests only locally

test_that("download_eurostat works correctly", {

  skip_on_cran()
  skip_on_ci()

  # setup
  specification <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "EmiCO2Combustion"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "HDD + CDD + HICP_Energy + GValueAdd"
    )
  )
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict
  additional_filters <- character() # no additional filters
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                   dictionary = dictionary)

  # basic functionality
  a <- aggregate.model:::download_eurostat(to_obtain = to_obtain,
                         additional_filters = additional_filters,
                         quiet = FALSE)
  # expected:
  # download 4 datasets from eurostat: namq_10_a10, namq_10_gdp, prc_hicp_midx, nrg_chdd_m
  # list with two entries: a df and updated "to_obtain" where the eurostat vars are found == TRUE
  # monthly data should have been converted to quarterly: HDD, CDD, HICP_Energy
  expect_length(a, 2)
  expect_type(a, "list")
  expect_named(a, c("df", "to_obtain"))
  expect_true(all(a$to_obtain[which(to_obtain$database == "eurostat"), "found"]))
  expect_false(all(a$to_obtain[which(to_obtain$database != "eurostat"), "found"]))
  expect_identical(a$df %>% dplyr::filter(na_item == "HDD") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(a$df %>% dplyr::filter(na_item == "CDD") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(a$df %>% dplyr::filter(na_item == "HICP_Energy") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))

  # additional filters (e.g. if user uses data that requires a filter we have not encountered before)
  # for illustration, simply filter on "time" variable
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict
  dictionary$time <- lubridate::NA_Date_
  dictionary[, "time"] <- as.Date("2022-10-01")
  additional_filters <- c("time")
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)
  a <- aggregate.model:::download_eurostat(to_obtain = to_obtain,
                                           additional_filters = additional_filters,
                                           quiet = FALSE)
  # expected as before:
  # download 4 datasets from eurostat: namq_10_a10, namq_10_gdp, prc_hicp_midx, nrg_chdd_m
  # list with two entries: a df and updated "to_obtain" where the eurostat vars are found == TRUE
  # NOTE: HDD, CDD, HICP_Energy are not in dataset b/c I delete incomplete quarters (and 1 month is incomplete)
  # NOTE: I don't think this is a real issue, this is just an illustration of an additional filter
  # expected new:
  # only data for "2022-10-01"
  expect_length(a, 2)
  expect_type(a, "list")
  expect_named(a, c("df", "to_obtain"))
  expect_true(all(a$to_obtain[which(to_obtain$database == "eurostat"), "found"]))
  expect_false(all(a$to_obtain[which(to_obtain$database != "eurostat"), "found"]))
  expect_identical(unique(a$df$time), as.Date("2022-10-01"))


})


test_that("download_edgar works correctly", {

  skip_on_cran()
  skip_on_ci()

  # setup
  # make sure all 3 different files need to be downloaded
  # TOTAL and (Combustion/Livestock) need to load in different sheets of Excel file
  specification <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "EmiCO2Combustion",
      "EmiN2OTotal",
      "EmiCH4Livestock"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "HDD + CDD + HICP_Energy + GValueAdd",
      "HICP_Energy",
      "GValueAdd"
    )
  )
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)

  # basic functionality
  a <- aggregate.model:::download_edgar(to_obtain = to_obtain, quiet = FALSE)
  # expected:
  # download 3 datasets from edgar: CO2, CH4, N2O
  # list with two entries: a df and updated "to_obtain" where the edgar vars are found == TRUE
  # emissions data is monthly, so should have been converted to quarterly
  expect_length(a, 2)
  expect_type(a, "list")
  expect_named(a, c("df", "to_obtain"))
  expect_true(all(a$to_obtain[which(to_obtain$database == "edgar"), "found"]))
  expect_false(all(a$to_obtain[which(to_obtain$database != "edgar"), "found"]))
  expect_identical(a$df %>% dplyr::filter(na_item == "EmiCH4Livestock") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(a$df %>% dplyr::filter(na_item == "EmiCO2Combustion") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(a$df %>% dplyr::filter(na_item == "EmiN2OTotal") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))

  # IPCC code allows to sum across sub-codes
  # e.g. code "1.A" sums up all codes "1.A.xxx"
  # check manually whether this worked correctly for EmiCO2Combustion
  #url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/v70_FT2021_CO2_m_2000_2021.zip"
  url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022.zip"
  tmp_download <- tempfile(fileext = "zip")
  download.file(url = url, destfile = tmp_download, mode = "wb")
  tmp_extract <- tempdir()
  unzip(zipfile = tmp_download, files = "IEA_EDGAR_CO2_m_1970_2022.xlsx", exdir = tmp_extract)
  tmp <- readxl::read_excel(path = file.path(tmp_extract, "IEA_EDGAR_CO2_m_1970_2022.xlsx"),
                            sheet = "IPCC 2006",
                            skip = 9)
  manual <- tmp %>%
    dplyr::select("Country_code_A3", "Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "ipcc_code_2006_for_standard_report") %>%
    dplyr::filter(Country_code_A3 == "AUT") %>%
    dplyr::rename(geo = Country_code_A3) %>%
    dplyr::mutate(geo = "AT") %>%
    tidyr::pivot_longer(cols = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                        names_to = "Month",
                        values_to = "values") %>%
    dplyr::mutate(Month = dplyr::case_when(Month == "Jan" ~ "01",
                                           Month == "Feb" ~ "02",
                                           Month == "Mar" ~ "03",
                                           Month == "Apr" ~ "04",
                                           Month == "May" ~ "05",
                                           Month == "Jun" ~ "06",
                                           Month == "Jul" ~ "07",
                                           Month == "Aug" ~ "08",
                                           Month == "Sep" ~ "09",
                                           Month == "Oct" ~ "10",
                                           Month == "Nov" ~ "11",
                                           Month == "Dec" ~ "12",
                                           TRUE ~ "error")) %>%
    dplyr::mutate(time = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    dplyr::select(-Year, -Month) %>%
    dplyr::filter(ipcc_code_2006_for_standard_report %in% c("1.A.1.a", "1.A.1.bc", "1.A.2", "1.A.3.a", "1.A.3.b_noRES", "1.A.3.c", "1.A.3.d", "1.A.3.e", "1.A.4")) %>%
    dplyr::group_by(geo, time) %>%
    dplyr::summarise(values = sum(values)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = lubridate::year(time),
                  quarter = lubridate::quarter(time)) %>%
    dplyr::group_by(geo, year, quarter) %>%
    dplyr::summarise(values = sum(values),
                     nobs = dplyr::n(),
                     time = min(time)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(nobs == 3L) %>%
    dplyr::select(-year, -quarter, -nobs) %>%
    dplyr::mutate(na_item = "EmiCO2Combustion") %>%
    as.data.frame()
  expect_identical(a$df %>% dplyr::filter(na_item == "EmiCO2Combustion"), manual)

})

test_that("load_locally() works correctly", {

  skip_on_cran()
  skip_on_ci()

  # local data for testing purposes stored in "./tests/testthat/testdata/"
  # complete data in "./complete/" :
  # emin2ototal.rds = emissions n2o total (country-level), 88x4, .rds
  # emico2combustion.csv = emissions co2 combustion, 88x4, .csv
  # emich4livestock.xlsx = emissions ch4 livestock, 88x4, .xlsx
  # incomplete/modified data to test precedence in "./incomplete/" :
  # emin2ototal_trunc.rds = time < as.Date("2015-01-01"), 60x4, .rds
  # emich4livestock_trunc.rds = time < as.Date("2018-01-01"), 72x4, .rds

  # test that can load different data types
  # setup
  specification <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "EmiCO2Combustion",
      "EmiN2OTotal",
      "EmiCH4Livestock"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "HDD + CDD + HICP_Energy + GValueAdd",
      "HICP_Energy",
      "GValueAdd"
    )
  )
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict
  # make the edgar emissions data to be loaded locally
  indices <- which(dictionary$model_varname %in% c("EmiCO2Combustion", "EmiN2OTotal", "EmiCH4Livestock"))
  dictionary[indices, "database"] <- "local"
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)

  # basic functionality
  a <- aggregate.model:::load_locally(to_obtain = to_obtain, inputdata_directory = test_path("testdata", "complete"), quiet = FALSE)
  expect_length(a, 2)
  expect_type(a, "list")
  expect_named(a, c("df", "to_obtain"))
  expect_true(all(a$to_obtain[which(to_obtain$database == "local"), "found"]))
  expect_false(all(a$to_obtain[which(to_obtain$database != "local"), "found"]))
  expect_identical(NROW(a$df), 3L*88L) # each df should be 88 rows

  # internally, Moritz has allowed for inputdata_directory to be a data.frame
  # check that this also works as intended
  # reset
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)
  # create data.frame
  x <- readRDS(test_path("testdata", "complete", "emin2ototal.rds"))
  a <- aggregate.model:::load_locally(to_obtain = to_obtain, inputdata_directory = x, quiet = FALSE)
  expect_length(a, 2)
  expect_type(a, "list")
  expect_named(a, c("df", "to_obtain"))
  expect_true(a$to_obtain[which(to_obtain$model_varname == "EmiN2OTotal"), "found"])
  expect_false(all(a$to_obtain[which(to_obtain$model_varname != "EmiN2OTotal"), "found"]))
  expect_identical(NROW(a$df), 88L) # each df should be 88 rows, only loaded 1

})

test_that("load_or_download_variables() works correctly", {

  skip_on_cran()
  skip_on_ci()

  # specification 1: requires only eurostat download
  specification <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm"
    )
  )
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict

  a <- aggregate.model:::load_or_download_variables(
    specification = module_order,
    dictionary = dictionary,
    primary_source = "download",
    inputdata_directory = NULL,
    save_to_disk = NULL,
    quiet = FALSE,
    constrain.to.minimum.sample = TRUE
  )

  # check that obtained all variables
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)
  expect_setequal(unique(to_obtain$model_varname), unique(a$na_item))

  # specification 2: requires only edgar download
  specification <- dplyr::tibble(
    type = c(
      "n",
      "n"
    ),
    dependent = c(
      "EmiN2OTotal",
      "EmiCH4Livestock"
    ),
    independent = c(
      "EmiCO2Combustion",
      ""
    )
  )
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict

  a <- aggregate.model:::load_or_download_variables(
    specification = module_order,
    dictionary = dictionary,
    primary_source = "download",
    inputdata_directory = NULL,
    save_to_disk = NULL,
    quiet = FALSE,
    constrain.to.minimum.sample = TRUE
  )

  # check that obtained all variables
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)
  expect_setequal(unique(to_obtain$model_varname), unique(a$na_item))

  # specification 3: requires only local loading
  specification <- dplyr::tibble(
    type = c(
      "n",
      "n"
    ),
    dependent = c(
      "EmiN2OTotal",
      "EmiCH4Livestock"
    ),
    independent = c(
      "EmiCO2Combustion",
      ""
    )
  )
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict
  # make the edgar emissions data to be loaded locally
  indices <- which(dictionary$model_varname %in% c("EmiCO2Combustion", "EmiN2OTotal", "EmiCH4Livestock", "EmiCO2Industry"))
  dictionary[indices, "database"] <- "local"

  a <- aggregate.model:::load_or_download_variables(
    specification = module_order,
    dictionary = dictionary,
    primary_source = "download", # even though specify download here, will use local directory b/c dictionary specifies "local"
    inputdata_directory = test_path("testdata", "complete"),
    save_to_disk = NULL,
    quiet = FALSE,
    constrain.to.minimum.sample = TRUE
  )
  b <- aggregate.model:::load_or_download_variables(
    specification = module_order,
    dictionary = dictionary,
    primary_source = "local", # do local loading first, should not matter b/c dictionary specifies "local"
    inputdata_directory = test_path("testdata", "complete"),
    save_to_disk = NULL,
    quiet = FALSE,
    constrain.to.minimum.sample = TRUE
  )

  # check that obtained all variables
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)
  expect_setequal(unique(to_obtain$model_varname), unique(a$na_item))
  expect_identical(a, b)

  # specification 4: mixed inputs local, eurostat, edgar with primary_source = "local"
  # this means local files take precedence, even if database is "eurostat" or "edgar"

  # local data for testing purposes stored in "./tests/testthat/testdata/"
  # complete data in "./complete/" :
  # emin2ototal.rds = emissions n2o total (country-level), 88x4, .rds
  # emico2combustion.csv = emissions co2 combustion, 88x4, .csv
  # emich4livestock.xlsx = emissions ch4 livestock, 88x4, .xlsx
  # incomplete/modified data to test precedence in "./incomplete/" :
  # emin2ototal_trunc.rds = time < as.Date("2015-01-01"), 60x4, .rds
  # emich4livestock_trunc.rds = time < as.Date("2018-01-01"), 72x4, .rds

  specification <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "EmiCO2Combustion",
      "EmiN2OTotal" # this will be database == "edgar" (so either downloaded or local depending on precedence)
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "HDD + CDD + HICP_Energy + GValueAdd",
      "EmiCH4Livestock" # this will be database == "local", so should always be local
    )
  )
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict
  # make the "EmiCH4Livestock" to be loaded locally
  indices <- which(dictionary$model_varname == "EmiCH4Livestock")
  dictionary[indices, "database"] <- "local"
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)

  # should see difference in EmiN2OTotal; EmiCH4Livestock should always be from local
  # use incomplete directory so that we know they come from the local data
  a <- aggregate.model:::load_or_download_variables(
    specification = module_order,
    dictionary = dictionary,
    primary_source = "download",
    inputdata_directory = test_path("testdata", "incomplete"),
    save_to_disk = NULL,
    quiet = FALSE,
    constrain.to.minimum.sample = FALSE # don't constrain, so can see clearly
  )
  b <- aggregate.model:::load_or_download_variables(
    specification = module_order,
    dictionary = dictionary,
    primary_source = "local",
    inputdata_directory = test_path("testdata", "incomplete"),
    save_to_disk = NULL,
    quiet = FALSE,
    constrain.to.minimum.sample = FALSE # don't constrain, so can see clearly
  )

  expect_setequal(unique(to_obtain$model_varname), unique(a$na_item))
  expect_setequal(unique(to_obtain$model_varname), unique(b$na_item))
  # check that in both cases, obtained EmiCH4Livestock locally, i.e. time < as.Date("2018-01-01")
  # this means most recent quarter should be Y2017-Q4 = "2017-10-01"
  expect_identical(a %>% dplyr::filter(na_item == "EmiCH4Livestock") %>% dplyr::pull(time) %>% max(), as.Date("2017-10-01"))
  expect_identical(b %>% dplyr::filter(na_item == "EmiCH4Livestock") %>% dplyr::pull(time) %>% max(), as.Date("2017-10-01"))
  # check that in case a, have full data for EmiN2OTotal but only until time < as.Date("2015-01-01") in case b
  expect_identical(a %>% dplyr::filter(na_item == "EmiN2OTotal") %>% dplyr::pull(time) %>% max(), as.Date("2022-10-01"))
  expect_identical(b %>% dplyr::filter(na_item == "EmiN2OTotal") %>% dplyr::pull(time) %>% max(), as.Date("2014-10-01"))

  # test that saving works
  specification <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm"
    )
  )
  module_order <- aggregate.model:::check_config_table(specification)
  dictionary <- aggregate.model::dict

  a <- aggregate.model:::load_or_download_variables(
    specification = module_order,
    dictionary = dictionary,
    primary_source = "download",
    inputdata_directory = NULL,
    save_to_disk = test_path("testdata", "saved", "test_save_to_disk_functionality.rds"),
    quiet = FALSE,
    constrain.to.minimum.sample = TRUE
  )
  does_exist <- file.exists(test_path("testdata", "saved", "test_save_to_disk_functionality.rds"))
  expect_true(does_exist)

  if (does_exist) {
    info <- file.info(test_path("testdata", "saved", "test_save_to_disk_functionality.rds"))
    # check that has been updated recently (in last minute)
    recently <- (Sys.time() < info$mtime + 60) # adds 60 seconds
    expect_true(recently)
  }

})
