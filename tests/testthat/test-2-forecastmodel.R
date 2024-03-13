options(timeout=1000)

# # You'd then also provide a helper that skips tests where you can't
# # be sure of producing exactly the same output
# expect_snapshot_plot <- function(name, code) {
#   # Other packages might affect results
#   skip_if_not_installed("ggplot2", "2.0.0")
#   # Or maybe the output is different on some operation systems
#   skip_on_ci()
#   # You'll need to carefully think about and experiment with these skips
#
#   name <- paste0(name, ".png")
#
#   # Announce the file before touching `code`. This way, if `code`
#   # unexpectedly fails or skips, testthat will not auto-delete the
#   # corresponding snapshot file.
#   announce_snapshot_file(name = name)
#
#   # To use expect_snapshot_file() you'll typically need to start by writing
#   # a helper function that creates a file from your code, returning a path
#   save_png <- function(code, width = 400, height = 400) {
#     path <- tempfile(fileext = ".png")
#     png(path, width = width, height = height)
#     on.exit(dev.off())
#     code
#
#     path
#   }
#
#   path <- save_png(code)
#   expect_snapshot_file(path, name)
# }


# To use expect_snapshot_file() you'll typically need to start by writing
# a helper function that creates a file from your code, returning a path
save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")

  if(ggplot2::is.ggplot(code)){
    ggplot2::ggsave(filename = path, plot = code, width = 7, height = 5)
  } else {
    png(path, width = width, height = height)
    on.exit(dev.off())
    code
  }
  path
}

# You'd then also provide a helper that skips tests where you can't
# be sure of producing exactly the same output
expect_snapshot_plot <- function(name, code) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")
  # Or maybe the output is different on some operation systems
  #skip_on_os("windows")
  skip_on_ci()
  # You'll need to carefully think about and experiment with these skips

  name <- paste0(name, ".png")
  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path <- save_png(code)
  expect_snapshot_file(path, name)
}

test_that("Test that forecasting works - with fixed data",{

  ## Test AR1 and fully exogenous ----

  spec <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "FinConsExpHH",
      "GCapitalForm",
      "Emissions",
      "GDP"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "FinConsExpGov",
      "Emissions",
      "GDP",
      ""
    )
  )
  expect_silent(
    b <- run_model(
      specification = spec,
      dictionary = NULL,
      inputdata_directory = sample_input,
      max.ar = 4,
      max.dl = 4,
      primary_source = "local",
      quiet = TRUE
    ))

  set.seed(123)
  expect_message(bb <- forecast_model(b, plot.forecast = FALSE), regexp = "No exogenous values")

  skip_on_ci()
  expect_snapshot_plot("Forecast_plot",code = plot(bb))
})

#
test_that("Testing nowcasting and dealing with ragged edges works with fixed data (EMCC Data)",{

  cur_dict <- dict %>%
    dplyr::bind_rows(dplyr::tibble(model_varname = "EmiGHGTotal",
                                   full_name = "Total GHG Emissions from Edgar (not quite but combination from Industry, Combustion and a bit of non-CO2"))

  # you can see, the first two
  specification <- dplyr::tibble(
    type = c(
      "d",
      "n",
      "n",
      "n",
      "d",
      "n"
    ),
    dependent = c(
      "TOTS",
      "Import",
      "EmiCO2Combustion",
      "EmiCO2Industry",
      "EmiGHGTotal",
      "GValueAddIndus"
    ),
    independent = c(
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "HDD + HICP_Gas + HICP_Electricity + GValueAdd",
      "HICP_Gas + HICP_Electricity + GValueAddIndus",
      "EmiCO2Combustion + EmiCO2Industry + EmiCH4Livestock + EmiN2OTotal",
      "HICP_Gas + HICP_Electricity + Export + TOTS"
    )
  )


  # Model Run for AT --------------------------------------------------------
  expect_silent(
    model <- run_model(specification = specification,
                       dictionary = cur_dict,
                       inputdata_directory = test_path("testdata", "ragged_edge"),
                       primary_source = "local",
                       present = FALSE,
                       quiet = TRUE,
                       constrain.to.minimum.sample = FALSE)

  )

  set.seed(123)
  expect_message(bb <- forecast_model(model, plot.forecast = FALSE), regexp = "No exogenous values")

  # check that a dataframe can be returned
  bb_df <- plot(bb, return.data = TRUE)
  expect_s3_class(bb_df, class = "tbl_df")
  expect_true(all(names(bb_df) == c("time", "na_item", "values", "type", "max", "min", "p975",
                                    "p025", "p75", "p25")))
  expect_true(all(unique(bb_df$type) == c("Endogenous Forecast", "Insample Fit", "Observation")))

  bb_df_exog <- plot(bb, return.data = TRUE, exclude.exogenous = FALSE)
  expect_s3_class(bb_df_exog, class = "tbl_df")
  expect_true(all(names(bb_df_exog) == c("time", "na_item", "values", "type", "max", "min", "p975",
                                    "p025", "p75", "p25")))
  expect_true(all(unique(bb_df_exog$type) == c("Exogenous Forecast", "Endogenous Forecast", "Observation",
                                               "Insample Fit")))


  expect_s3_class(plot(bb, return.data = TRUE), class = "tbl_df")


  skip_on_ci()
  expect_snapshot_plot("Forecast_plot_ragged",code = plot(bb))
})
