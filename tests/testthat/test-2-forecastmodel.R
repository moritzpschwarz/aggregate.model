
# You'd then also provide a helper that skips tests where you can't
# be sure of producing exactly the same output
expect_snapshot_plot <- function(name, code) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")
  # Or maybe the output is different on some operation systems
  skip_on_ci()
  # You'll need to carefully think about and experiment with these skips

  name <- paste0(name, ".png")

  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  # To use expect_snapshot_file() you'll typically need to start by writing
  # a helper function that creates a file from your code, returning a path
  save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    code

    path
  }

  path <- save_png(code)
  expect_snapshot_file(path, name)
}

test_that("Test that forecasting works",{

  ## Test AR1 and fully exogenous ----

  spec <- tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "JL",
      "TOTS",
      "B",
      "CP",
      "J"
    ),
    independent = c(
      "TOTS - CP - CO - J - A",
      "YF + B",
      "CP + J",
      "",
      "CO"
    )
  )

  fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
  fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
  filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

  b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  )


  expect_message(forecast_model(b), regexp = "No exogenous values")

  expect_snapshot_plot("Forecast_plot",plot(forecast_model(b)))


})