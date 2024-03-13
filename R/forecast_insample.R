#' Forecast model insample to evaluate forecasting performance
#' @param sample_share Share of the sample that should be used for the insample forecasting. Must be a numeric and must be either 1 or smaller but larger than 0.
#' @inheritParams forecast_model
#'
#' @return List element.
#' @export
#'
#'
forecast_insample <- function(model, sample_share = 0.5, uncertainty_sample = 100, exog_fill_method = "AR", plot.forecast = TRUE) {

  # we first must identify the minimum sample across modules
  time_samples <- dplyr::tibble()
  for(i in 1:nrow(model$module_collection)){
    # i = 1
    model$module_collection$dataset[i][[1]] %>%
      dplyr::select("time") %>%
      dplyr::mutate(dep_var = model$module_collection$dependent[i]) %>%
      dplyr::bind_rows(time_samples,.) -> time_samples
  }

  time_samples %>%
    dplyr::group_by(.data$dep_var) %>%
    dplyr::summarise(min = min(.data$time),
                     max = max(.data$time)) %>%
    dplyr::distinct(dplyr::across(c("max","min"))) -> time_minmax


  # With those times, we can now find the share to forecast
  all_times <- seq(time_minmax$min,time_minmax$max, by = "quarter")
  time_to_use <- all_times[ceiling(length(all_times)*sample_share):length(all_times)]

  all_models <- list()
  for(j in 1:length(time_to_use)){
    print(time_to_use[j])
    # now let's prepare the model object
    suppressWarnings(
      try(insample_model <- run_model(

        specification = model$args$specification,
        dictionary = model$args$dictionary,
        trend = model$args$trend,
        primary_source = model$args$primary_source,

        max.ar = model$args$max.ar,
        max.dl = model$args$max.dl,
        max.block.size = model$args$max.block.size,

        ardl_or_ecm = model$args$ardl_or_ecm,
        use_logs = model$args$use_logs,
        saturation = model$args$saturation,
        saturation.tpval = model$args$saturation.tpval,
        gets_selection = model$args$gets_selection,
        selection.tpval = model$args$selection.tpval,

        present = FALSE,
        quiet = TRUE,

        inputdata_directory = model$processed_input_data %>% dplyr::filter(.data$time <= as.Date(time_to_use[j]))
      ), silent = TRUE))

    if(exists("insample_model")){
      all_models[[j]] <- insample_model
      rm(insample_model)
    }
  }

  #save(all_models, file = "all_models_3.RData")
  #load("all_models_2.RData")

  model$processed_input_data %>%
    dplyr::distinct(dplyr::across("na_item")) %>%
    dplyr::pull("na_item") -> all_data_vars

  exog_vars <- all_data_vars[!all_data_vars %in% model$module_collection$dependent]

  model$processed_input_data %>%
    dplyr::filter(.data$na_item %in% exog_vars) -> exog_data


  forecasted_unknownexogvalues <- list()
  forecasted_knownexogvalues <- list()

  for(i in 1:length(all_models)){
    #i = 20
    if(is.null(all_models[[i]])){next}

    start <- time_to_use[i]
    end <- time_to_use[length(time_to_use)]
    if(start == end){next}
    nsteps <- length(seq.Date(from = as.Date(start), to = as.Date(end), by = "quarter"))


    print(paste0("Forecast ", i, " from ", start, " to ", end))
    forecasted_unknownexogvalues[[i]] <- forecast_model(model = all_models[[i]],
                                                        n.ahead = nsteps,
                                                        uncertainty_sample = uncertainty_sample,
                                                        plot.forecast = FALSE,
                                                        exog_fill_method = exog_fill_method,
                                                        quiet = TRUE)

    # forecasted_knownexogvalues[[i]] <- forecast_model(
    #   all_models[[i]], n.ahead = nsteps, uncertainty_sample = uncertainty_sample, plot.forecast = FALSE,
    #   exog_predictions = exog_data %>% dplyr::filter(.data$time >= as.Date(start),
    #                                                  .data$time <= as.Date(end)))
  }

  # save(forecasted_unknownexogvalues, file = "forecasted_models.RData")
  # load("forecasted_models.RData")

  overall_to_plot_central <- dplyr::tibble()
  overall_to_plot_alls <- dplyr::tibble()

  for(i in 1:length(forecasted_unknownexogvalues)){
    # i = 3

    if(is.null(forecasted_unknownexogvalues[[i]])){next}
    #print(i)

    #plot(forecasted_unknownexogvalues[[i]])

    forecasted_unknownexogvalues[[i]]$forecast %>%
      dplyr::select("dep_var","central.estimate") %>%
      dplyr::mutate(start = time_to_use[i]) %>%
      tidyr::unnest("central.estimate") %>%
      tidyr::pivot_longer(-c("dep_var", "start", "time")) %>%
      tidyr::drop_na("value") -> centrals

    forecasted_unknownexogvalues[[i]]$forecast %>%
      dplyr::select("dep_var","all.estimates") %>%
      dplyr::mutate(start = time_to_use[i]) %>%
      dplyr::mutate(quantiles = purrr::map(.data$all.estimates, function(x){
        tidyr::pivot_longer(x, -"time") %>%
          dplyr::summarise(max = max(.data$value),
                           min = min(.data$value),
                           p975 = stats::quantile(.data$value, probs = 0.975),
                           p025 = stats::quantile(.data$value, probs = 0.025),
                           p75 = stats::quantile(.data$value, probs = 0.75),
                           p25 = stats::quantile(.data$value, probs = 0.25), .by = .data$time) %>%
          tidyr::pivot_longer(-"time", names_to = "quantile")
      })) %>%
      dplyr::select(-"all.estimates") %>%
      dplyr::full_join(centrals %>%
                         dplyr::distinct(.data$dep_var, .data$name), by = "dep_var") %>%
      tidyr::unnest("quantiles") -> alls

    dplyr::bind_rows(overall_to_plot_central, centrals) -> overall_to_plot_central
    dplyr::bind_rows(overall_to_plot_alls, alls) -> overall_to_plot_alls

  }

  overall_to_plot_central %>%
    dplyr::mutate(value = ifelse(grepl("^ln.", .data$name), exp(.data$value), .data$value)) -> overall_to_plot_central_exp

  overall_to_plot_alls %>%
    dplyr::mutate(value = ifelse(grepl("^ln.", .data$name), exp(.data$value), .data$value)) %>%
    tidyr::pivot_wider(id_cols = c("dep_var","name","start","time"), names_from = "quantile", values_from = "value") -> overall_to_plot_alls_exp

  forecasted_unknownexogvalues[[length(forecasted_unknownexogvalues)]]$orig_model$full_data %>%
    dplyr::rename(dep_var = "na_item") %>%
    dplyr::filter(.data$dep_var %in% overall_to_plot_central_exp$dep_var,
                  .data$time > min(overall_to_plot_central$start)) -> full_data


  #add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
  # bb_insample$central %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(forecast_len = add.months(start, 24)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(!(time > forecast_len)) -> central
  #
  #
  # bb_insample$uncertainty %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(forecast_len = add.months(start, 24)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(!(time > forecast_len)) -> uncertainty
  #

  # With those times, we can now find the share to forecast
  share_to_show <- 1-(ifelse((1-sample_share)*2<=1,(1-sample_share)*2, 1))
  time_to_show <- all_times[ceiling(length(all_times)*share_to_show):length(all_times)]

  ggplot2::ggplot() +
    ggplot2::geom_line(data = model$full_data %>%
                         dplyr::rename(dep_var = "na_item") %>%
                         dplyr::filter(.data$dep_var %in% extract_dep_vars,
                                       .data$time %in% time_to_show),
                       #.data$time > as.Date("2010-01-01")),
                       ggplot2::aes(x = .data$time, y = .data$values), linewidth = 1) +

    ggplot2::facet_wrap(~dep_var, scales = "free") +
    ggplot2::geom_ribbon(data = overall_to_plot_alls_exp, ggplot2::aes(ymin = .data$min, x = .data$time, ymax = .data$max, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = overall_to_plot_alls_exp, ggplot2::aes(ymin = .data$p025, x = .data$time, ymax = .data$p975, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = overall_to_plot_alls_exp, ggplot2::aes(ymin = .data$p25, x = .data$time, ymax = .data$p75, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +

    ggplot2::geom_line(data = overall_to_plot_central_exp, ggplot2::aes(y = .data$value, x = .data$time, color = as.factor(.data$start)), inherit.aes = FALSE) +
    ggplot2::facet_wrap(~.data$dep_var, scales = "free") +
    #ggplot2::scale_color_brewer(palette = "PRGn") +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::coord_cartesian(expand = TRUE) +

    ggplot2::labs(x = NULL, y = NULL, title = "Automatic Forecasting Hindcasts") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> plt


  #plotly::ggplotly(plt)


  if(plot.forecast){
    print(plt)
  }




  out <- list()
  out$plot <- plt
  out$central <- overall_to_plot_central_exp
  out$uncertainty <- overall_to_plot_alls_exp
  out$hist_data <- full_data
  return(out)
}
