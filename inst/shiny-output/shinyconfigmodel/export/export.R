
  library(aggregate.model)
  setwd("C:/Users/thepa/My Drive/7 - Code/GitHub/aggregate.model/inst/shiny-output/shinyconfigmodel")

  spec <- readr::read_csv("export/export_spec.csv")
  spec <- spec[,-1]
  dict <- readr::read_csv("export/export_dict.csv")
  dict <- dict[,-1]

  model <- aggregate.model::run_model(specification = spec,
                                      dictionary = dict,
                                      inputdata_directory = "", # this is not working correctly yet with local data, but I was not able to find the issue. Download works as intended.
                                      primary_source = "download",
                                      save_to_disk = "inputdata/processed_inputdata.csv",
                                      present = FALSE,
                                      quiet = FALSE,
                                      use_logs = "both",
                                      trend = TRUE,
                                      ardl_or_ecm = "ardl",
                                      max.ar = 4,
                                      max.dl = 2,
                                      saturation = TRUE,
                                      saturation.tpval = 0.05,
                                      max.block.size = 20,
                                      gets_selection = TRUE,
                                      selection.tpval = 0.05,
                                      constrain.to.minimum.sample = TRUE)

      print(model)
      aggregate.model::forecast_model(
        model = model,
        #exog_predictions = NULL,
        n.ahead =10,
        #ci.levels = c(0.5, 0.66, 0.95),
        exog_fill_method = "AR",
        ar.fill.max = "4",
        plot.forecast = TRUE,
        uncertainty_sample = 100,
        #quiet = FALSE
  )
  
