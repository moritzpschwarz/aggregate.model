devtools::load_all()

spec <- dplyr::tibble(
  type = c(
    #"d",
    #"d",
    "n",
    "n",
    "n",
    "n",
    "d",
    "n",
    "n",
    #"d",
    "n",
    "n"
  ),
  dependent = c(
    #"StatDiscrep",
    #"TOTS",
    "Import",
    "FinConsExpHH",
    "GCapitalForm",
    "Emissions",
    "GDP",
    "GValueAddGov", # as in NAM, technical relationship
    "GValueAddManuf", # more complicated in NAM, see 2.3.3 and 6.3.1
    #"DomDemand", # as in NAM
    "GValueAddConstr" ,
    "GValueAddWholesaletrade"
  ),
  independent = c(
    #"TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    #"GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "",
    "FinConsExpGov + FinConsExpHH",
    "GDP + Export + GValueAddIndus",
    "GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr + GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance + GValueAddRealest + GValueAddResearch + GValueAddArts",
    "FinConsExpGov", # as in NAM, technical relationship
    "Export + LabCostManuf", # NAM uses 'export market indicator' not exports - unclear what this is, NAM uses unit labour cost in NOR manufacturing relative to the foreign price level - here is just total labour cost
    #"FinConsExpHH + FinConsExpGov + GCapitalForm",
    "LabCostConstr + BuildingPermits", # in NAM some form of YFP2J = 0.3JBOL + 0.2JF P N + 0.3JO + 0.3JOIL. Unclear what this is. Using Building Permits instead
    "Export + LabCostService"
  )
)
fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
fc <- list(geo = "AT", unit = "THS_T")
fd <- list(geo = "AT", s_adj = "SCA")
fe <- list(geo = "AT", s_adj = "SCA", unit = "I15")
ff <- list(geo = "AT", s_adj = "SCA", unit = "I16")
filter_list <- list(
  "P7" = fa,
  "YA0" = fb,
  "P31_S14_S15" = fa,
  "P5G" = fa,
  "B1G" = fa,
  "P3_S13" = fa,
  "P6" = fa,
  "GHG" = fc,
  "B1GQ" = fa,
  "PSQM" = fe,
  "LM-LCI-TOT" = ff
)

model_result_4_new <- run_model(
  specification = spec,
  filter_list = filter_list,
  #download = TRUE,
  inputdata_directory = "data-raw/csv/",
  #save_to_disk = "data-raw/csv/input.csv",
  trend = TRUE,
  #max.lag = 4,
  saturation.tpval = 0.001
)


model_result_0_new <- run_model(
  specification = spec,
  filter_list = filter_list,
  download = FALSE,
  #inputdata_directory = "data-raw/csv/",
  inputdata_directory = sample_input,
  trend = TRUE,
  max.ar = 0
)


forecast_model(model_result_0_new)
