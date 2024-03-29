# overall test -------

spec <- tibble(
  type = c(
    "d",
    "d",
    "n"
  ),
  dependent = c(
    "JL",
    "TOTS",
    "B"
  ),
  independent = c(
    "TOTS - CP - CO - J - A",
    "YF + B",
    "CP + J"
  )
)

fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

a <- run_model(
  specification = spec,
  dictionary = NULL,
  inputdata_directory = NULL,
  filter_list = filter_list,
  download = TRUE,
  save_to_disk = here::here("input_data/test.xlsx"),
  present = FALSE
)



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

# Execute the first time to get the data
# b <- run_model(
#   specification = spec,
#   dictionary = NULL,
#   inputdata_directory = NULL,
#   filter_list = filter_list,
#   download = TRUE,
#   save_to_disk = here::here("input_data/"),
#   present = FALSE
# )

b <- run_model(
  specification = spec,
  dictionary = NULL,
  inputdata_directory = here::here("input_data/"),
  filter_list = filter_list,
  download = FALSE,
  save_to_disk = NULL,
  present = FALSE
)


c <- run_model(
  specification = spec,
  dictionary = NULL,
  inputdata_directory = here::here("input_data/"),
  filter_list = filter_list,
  download = FALSE,
  save_to_disk = NULL,
  present = FALSE,
  ardl_or_ecm = "ecm"
)


b$module_order_eurostatvars

