#' Plot a Forecast Object of the Aggregate Model
#'
#' @param x An object of class aggmod.forecast, which is the output from the forecast_model function.
#' @param exclude.exogenous Logical. Should exogenous values be plotted? Default is FALSE.
#' @param order.as.run Logical. Should the plots be arranged in the way that the model was run? Default FALSE.
#' @param interactive Logical. Should the resulting plot be launched in an interactive way (the plotly package is required for this).
#' @param first_date Character. First date value to be shown. Must be a character value that can be turned into a date using as.Date() or NULL.
#' @param grepl_variables Regular Expression Character. Can be used to select variables to be plotted. Experimental feature so use with care.
#' @param return.data Logical. Do not return a plot but rather just the final dataset that has been created for the plot.
#' @param ... Further arguments (currently not in use).
#'
#' @export
#'
#' @examples
#' spec <- dplyr::tibble(
#' type = c(
#'   "d",
#'   "d",
#'   "n"
#' ),
#' dependent = c(
#'   "StatDiscrep",
#'   "TOTS",
#'   "Import"
#' ),
#' independent = c(
#'   "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
#'   "GValueAdd + Import",
#'   "FinConsExpHH + GCapitalForm"
#' )
#' )
#'\donttest{
#' a <- run_model(specification = spec, dictionary = NULL,
#' inputdata_directory = NULL, primary_source = "download",
#' save_to_disk = NULL, present = FALSE)
#' plot(forecast_model(a))
#'}
plot.aggmod.forecast <- function(x, exclude.exogenous = TRUE, order.as.run = FALSE, interactive = FALSE, first_date = NULL, grepl_variables = NULL, return.data = FALSE, ...){

  if(!isa(x, "aggmod.forecast")){
    stop("Input object not of type aggmod.forecast. Run 'forecast_model' again and use the output of that function.")
  }

  if(!is.null(first_date)){if(!is.character(first_date) | !lubridate::is.Date(as.Date(first_date))){stop("When supplying 'first_date', the it must be a character and must be (able to be converted to) a Date.")}}

  {if(!is.null(x$nowcast_data)){
    x$nowcast_data
  } else {
    x$orig_model$full_data
  }} %>%
    dplyr::mutate(var = .data$na_item,
                  na_item = gsub("\\.hat","",.data$na_item),
                  fit = as.character(stringr::str_detect(.data$var, ".hat"))) -> plot_df


  if(order.as.run & !exclude.exogenous){
    cat("As 'order.as.run' is TRUE, exogenous values will not be shown. So 'exclude.exogenous' will be ignored.\n")
    exclude.exogenous <- TRUE
  }
  if(exclude.exogenous){
    # determine which variables to exclude, when we are not including exogenous variables
    plot_df %>%
      dplyr::distinct(dplyr::across(c("na_item", "fit"))) %>%
      dplyr::mutate(fit = as.logical(.data$fit)) %>%
      dplyr::filter(.data$fit) %>%
      dplyr::pull("na_item") -> when_excluding_exog

    plot_df %>%
      dplyr::filter(.data$na_item %in% when_excluding_exog) -> plot_df
  }




  # dplyr::left_join(x$dictionary %>%
  #             dplyr::rename(na_item = model_varnma) %>%
  #             dplyr::select(na_item, model_varname), by = "na_item")

  # plot_df %>%
  #   ggplot2::ggplot(ggplot2::aes(x = time, y = values, color = fit, group = var, fit = var)) +
  #   ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  #
  #   ggplot2::facet_wrap(~na_item, scales = "free") +
  #
  #   ggplot2::labs(x = NULL, y = NULL) +
  #
  #   ggplot2::scale_y_continuous(labels = scales::comma) +
  #   ggplot2::scale_color_viridis_d() +
  #
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(legend.position = "none",
  #         panel.grid.major.x = ggplot2::element_blank(),
  #         panel.grid.minor.x = ggplot2::element_blank(),
  #         panel.grid.minor.y = ggplot2::element_blank()) -> initial_plot

  # get the central forecasts
  x$forecast %>%
    dplyr::select("central.estimate") %>%
    tidyr::unnest("central.estimate") %>%
    tidyr::pivot_longer(-"time") %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = "time", names_from = "name", values_from = "value") -> central_forecasts

  # find out which of the central forecasts should be exponentiated (because they were run in ln)
  central_forecasts %>%
    names %>%
    stringr::str_detect(., "^ln.") -> to_exponentiate

  # ---

  # Dealing with uncertainty (all forecasts) --------
  # unnest the set of all estimates from the original object
  x$forecast %>%
    dplyr::select("dep_var", "all.estimates") %>%
    tidyr::unnest("all.estimates") %>%
    tidyr::pivot_longer(-c("time", "dep_var")) -> all_forecasts

  dplyr::tibble(dep_var = names(central_forecasts),
                expo = to_exponentiate,
                #all = c("time",unique(all_forecasts$dep_var))) -> to_exponentiate_tibble
                all = c("time",unique(x$forecast$dep_var))) -> to_exponentiate_tibble

  central_forecasts %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(names(central_forecasts)[to_exponentiate]), exp)) %>%
    dplyr::rename_with(.fn = ~gsub("ln.","",.)) %>%

    tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
    dplyr::full_join(x$orig_model$module_order %>%
                       dplyr::select("dependent") %>%
                       dplyr::rename(na_item = "dependent"), by = "na_item") %>%

    dplyr::mutate(fit = "forecast") -> forecasts_processed


  all_forecasts %>%
    dplyr::mutate(value = dplyr::case_when(.data$dep_var %in% to_exponentiate_tibble$all[to_exponentiate_tibble$expo == TRUE] ~ exp(.data$value),
                                           TRUE ~ .data$value)) %>%
    dplyr::rename(na_item = "dep_var", values = "value") %>%
    dplyr::full_join(x$orig_model$module_order %>%
                       dplyr::select("dependent") %>%
                       dplyr::rename(na_item = "dependent"), by = "na_item") %>%
    dplyr::mutate(fit = "Forecast Uncertainty") -> all_forecasts_processed

  all_forecasts_processed %>%
    tidyr::drop_na("time") %>%
    dplyr::group_by(.data$na_item, .data$time, .data$fit) %>%
    dplyr::summarise(max = max(.data$values),
                     min = min(.data$values),
                     p975 = stats::quantile(.data$values, probs = 0.975),
                     p025 = stats::quantile(.data$values, probs = 0.025),
                     p75 = stats::quantile(.data$values, probs = 0.75),
                     p25 = stats::quantile(.data$values, probs = 0.25)) -> all_forecasts_processed_q


  # here we get the last fitted value
  plot_df %>%
    dplyr::filter(.data$fit == "TRUE") %>%
    dplyr::group_by(.data$na_item) %>%
    dplyr::filter(.data$time == max(.data$time, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"var") %>%
    dplyr::mutate(fit = "forecast") -> last_fitted_value

  # and the last historical value
  plot_df %>%
    dplyr::filter(.data$fit == "FALSE") %>%
    dplyr::group_by(.data$na_item) %>%
    dplyr::filter(.data$time == max(.data$time, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"var") %>%
    dplyr::mutate(fit = "forecast") -> last_hist_value

  last_fitted_value %>%
    dplyr::group_by(.data$time, .data$na_item) %>%
    dplyr::summarise(max = .data$values,
                     min = .data$values,
                     p975 = .data$values,
                     p025 = .data$values,
                     p75 =  .data$values,
                     p25 =  .data$values, .groups = "drop") %>%
    dplyr::mutate(fit = "Forecast Uncertainty") %>%
    dplyr::bind_rows(.,all_forecasts_processed_q) %>%
    {if(!is.null(grepl_variables)){dplyr::filter(., grepl(grepl_variables,.data$na_item))} else {.}} %>%
    tidyr::drop_na() -> all_forecasts_processed_q

  # Exogenous forecasts --------

  x$exog_data %>%
    dplyr::select(-dplyr::any_of(c("q_1","q_2","q_3","q_4"))) %>%
    tidyr::pivot_longer(-"time", values_to = "values", names_to = "na_item") %>%
    dplyr::bind_rows(last_hist_value %>% dplyr::mutate(fit = "Forecast/Assumption of\nExogenous Variables")) %>%
    dplyr::arrange(.data$time) %>%
    dplyr::mutate(fit = "Forecast/Assumption of\nExogenous Variables") %>%
    {if(!is.null(grepl_variables)){dplyr::filter(., grepl(grepl_variables,.data$na_item))} else {.}} -> exog_forecasts

  ggplot_options <- list(
    if (!exclude.exogenous) {ggplot2::geom_line(data = exog_forecasts, ggplot2::aes(x = .data$time, y = .data$values, color = .data$fit))} else {NULL}
  )


  plotting_df_ready <- dplyr::bind_rows(forecasts_processed, last_fitted_value) %>%
    dplyr::bind_rows(plot_df) %>%

    {if(order.as.run){
      dplyr::mutate(.,na_item = factor(.data$na_item, levels = x$orig_model$module_order$dependent)) %>%
        tidyr::drop_na("na_item") %>%
        dplyr::arrange(.data$time, .data$na_item)} else {.}} %>%

    {if(!is.null(first_date)){dplyr::filter(., .data$time >= as.Date(first_date))} else {.}} %>%

    {if(!is.null(grepl_variables)){dplyr::filter(., grepl(grepl_variables,.data$na_item))} else {.}}


  plotting_df_ready %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$values, color = .data$fit)) +

    ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = .data$min, x = .data$time, ymax = .data$max, fill = .data$fit), linewidth = 0.1, alpha = 0.3, inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = .data$p025, x = .data$time, ymax = .data$p975, fill = .data$fit), linewidth = 0.1, alpha = 0.3, inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = .data$p25, x = .data$time, ymax = .data$p75, fill = .data$fit), linewidth = 0.1, alpha = 0.5, inherit.aes = FALSE, na.rm = TRUE) +

    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +

    ggplot_options +

    ggplot2::facet_wrap(~.data$na_item, scales = "free") +

    ggplot2::labs(x = NULL, y = NULL) +

    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::scale_color_viridis_d() +

    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> p

  if(return.data){
    # prepare a nice dataset to be spit out
    plotting_df_ready %>%
      {if(!exclude.exogenous){dplyr::bind_rows(.,exog_forecasts %>%
                                                 dplyr::mutate(fit = "Exogenous Forecast"))} else {.}} %>%
    dplyr::select(-"var") %>%
      dplyr::full_join(all_forecasts_processed_q %>%
                         dplyr::select(-"fit"), by = dplyr::join_by("time", "na_item")) %>%
      dplyr::mutate(fit = dplyr::case_when(fit == "forecast" ~ "Endogenous Forecast",
                                           fit == "TRUE" ~ "Insample Fit",
                                           fit == "FALSE" ~ "Observation",
                                           TRUE ~ fit)) %>%
      dplyr::rename(type = "fit") %>%
      dplyr::arrange(dplyr::desc(.data$time), .data$na_item, .data$type) %>%
      return()
  } else {
    if(interactive){
      plotly::ggplotly(p)
    } else {
      suppressWarnings(suppressMessages(print(p)))
    }
  }






}
