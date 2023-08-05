#' Internal function to set-up the forecasting of estimated relationships
#'

#' @param i Current module that is being cycled through
#' @param exog_df_ready Outcome of forecast_exogenous_values() which is the set of forecasted exogenous values
#' @param current_spec The current specification for the module being forecasted
#' @param prediction_list The full list of all predictions. The results of the function will be saved in this list.
#' @inheritParams forecast_model
#'
#' @return A list containing, among other elements, the data required to carry out the forecast for this estimated module.
#'
forecast_setup_estimated_relationships <- function(model, i, exog_df_ready, n.ahead, current_spec, prediction_list, uncertainty_sample) {

  # set up
  # get isat obj
  model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model) %>% .[[1]] -> isat_obj

  # get data obj
  model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$dataset) %>% .[[1]] -> data_obj

  # determine ARDL or ECM
  is_ardl <- is.null(model$args$ardl_or_ecm) | identical(model$args$ardl_or_ecm,"ARDL")

  # determine log y
  ylog <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$use_logs %in% c("both","y")

  # determine log x
  xlog <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$use_logs %in% c("both","x")

  # determine x vars
  x_vars_basename <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$x_vars_basename

  y_vars_basename <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$dep_var_basename

  # check quarterly dummies to drop
  q_pred_todrop <- c("q_1","q_2","q_3","q_4")[!c("q_1","q_2","q_3","q_4") %in% colnames(isat_obj$aux$mX)]

  # check if mconst is used
  if ("mconst" %in% colnames(isat_obj$aux$mX)) {
    mconst <- TRUE
  } else {
    mconst <- FALSE
  }

  # identify any ar terms in the estimated data
  pred_ar_needed <- colnames(isat_obj$aux$mX)[grepl("ar[0-9]+",colnames(isat_obj$aux$mX))]

  # this condition checks whether there are any ar terms that need to be created
  if (!is.null(pred_ar_needed) & !identical(character(0),pred_ar_needed)) {

    # if we need AR terms, the following loop creates the names of those variables (incl. considering whether they are logged)
    ar_vec <- 0:max(as.numeric(gsub("ar","",pred_ar_needed)))
    y_names_vec <- c()
    for (ar in ar_vec) {
      # ar = 0
      y_names_vec <- c(y_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),y_vars_basename))
    }
  } else {
    # if we do not need any AR terms then we simply use the standard name (and add ln. if necessary)
    y_names_vec <- paste0(ifelse(ylog,"ln.",""),y_vars_basename)
    ar_vec <- 0
  }

  if (!identical(character(0),x_vars_basename)) {
    x_names_vec <- c()
    for (ar in ar_vec) {
      # ar = 0
      x_names_vec <- c(x_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),x_vars_basename))
    }

    x_names_vec_nolag <- paste0(ifelse(ylog,"ln.",""),x_vars_basename)
  } else {
    x_names_vec <- NULL
    x_names_vec_nolag <- NULL
  }

  # get iis dummies
  if (!is.null(gets::isatdates(isat_obj)$iis)) {
    iis_pred <- matrix(0,
                       nrow = nrow(exog_df_ready),
                       ncol = nrow(gets::isatdates(isat_obj)$iis),
                       dimnames  = list(NULL,
                                        gets::isatdates(isat_obj)$iis$breaks)) %>%
      dplyr::as_tibble()
  }

  # get sis dummies
  if (!is.null(gets::isatdates(isat_obj)$sis)) {
    sis_pred <- matrix(1,
                       nrow = nrow(exog_df_ready),
                       ncol = nrow(gets::isatdates(isat_obj)$sis),
                       dimnames  = list(NULL,
                                        gets::isatdates(isat_obj)$sis$breaks)) %>%
      dplyr::as_tibble()
  }

  if ("trend" %in% names(coef(isat_obj))) {
    trend_pred <- dplyr::tibble(trend = (max(isat_obj$aux$mX[,"trend"]) + 1):(max(isat_obj$aux$mX[,"trend"]) + n.ahead))
  }


  exog_df_ready %>%

    # select the relevant variables
    dplyr::select("time", dplyr::any_of(c("q_1","q_2","q_3","q_4")), dplyr::any_of(names(data_obj))) %>%

    # drop not used quarterly dummies
    dplyr::select(-dplyr::any_of(q_pred_todrop)) %>%

    {if ("trend" %in% names(coef(isat_obj))) {
      dplyr::bind_cols(.,trend_pred)
    } else { . }} %>%

    {if (!is.null(gets::isatdates(isat_obj)$iis)) {
      dplyr::bind_cols(.,iis_pred)
    } else { . }} %>%

    {if (!is.null(gets::isatdates(isat_obj)$sis)) {
      dplyr::bind_cols(.,sis_pred)
    } else { . }} %>%

    {if (xlog) {
      dplyr::mutate(.,
                    dplyr::across(.cols = dplyr::any_of(x_vars_basename), .fns = list(ln = log), .names = "{.fn}.{.col}"),
                    #dplyr::across(dplyr::starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}"
      )
    } else {.}} -> current_pred_raw

  current_pred_raw_all <- current_pred_raw

  # Deal with current_spec not being fully exogenous
  if (!all(current_spec$independent %in% names(exog_df_ready)) && !all(is.na(current_spec$independent))) {

    missing_vars <- current_spec$independent[!current_spec$independent %in% names(exog_df_ready)]

    for (mvar in missing_vars) {
      # mvar = "p5g"
      model$module_order_eurostatvars %>%
        dplyr::filter(.data$dependent == mvar) %>%
        dplyr::pull(.data$index) -> mvar_model_index

      prediction_list %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        dplyr::pull(.data$predict.isat_object) %>%
        .[[1]] -> mvar_model_obj

      mvar_logs <- model$module_collection %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        .$model.args %>%
        .[[1]] %>%
        .$use_logs

      prediction_list %>%
        dplyr::filter(index == mvar_model_index) %>%
        dplyr::pull(all.estimates) %>%
        .[[1]] %>%
        dplyr::select(-"time") -> mvar_all.estimates


      mvar_euname <- model$module_collection %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        dplyr::pull("dependent")

      mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), mvar_euname)

      # TODO: Here we can implement the forecast plume
      # currently we are using 'yhat' below - but the mvar_model_obj has all values of the ci.levels

      # name all the individual estimates
      colnames(mvar_all.estimates) <- paste0(mvar_name,".all.",seq(uncertainty_sample))

      # get all the individual estimates into a column of a tibble
      mvar_all.estimates.tibble <- dplyr::as_tibble(mvar_all.estimates) %>%
        dplyr::mutate(index = 1:dplyr::n()) %>%
        tidyr::nest(data = -index) %>%
        dplyr::select(-index) %>%
        setNames(paste0(mvar_name,".all"))

      # add the mean yhat estimates and the all estimates together
      mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
        setNames(mvar_name) #%>%
      # dplyr::bind_cols(mvar_all.estimates.tibble)

      # Old version not including the all.estimates
      # mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
      #   setNames(mvar_name)

      # What I think I need to TODO for the forecast plume (uncertainty):
      # look at how pred_df is created, find a way to check if it contains a list column with ".all"
      # if so, do the following prediction across all columns
      # the key is in the row that starts with dplyr::bind_rows(current_pred_raw %>%
      # there, currently the .all columns are dropped

      if (!mvar_name %in% x_names_vec_nolag) {
        if (paste0("ln.",mvar_name) %in% x_names_vec_nolag) {
          mvar_tibble %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(mvar_euname), log, .names = "ln.{.col}")) %>%
            dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname))) -> mvar_tibble

          mvar_all.estimates.tibble %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(paste0(mvar_euname, ".all")), ~purrr::map(.,log), .names = "ln.{.col}")) %>%
            dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname,".all"))) -> mvar_all.estimates.tibble
        } else {
          stop("Error occurred in adding missing/lower estimated variables (likely identities) to a subsequent/higher model. This is likely being caused by either log specification or lag specifiction. Check code.")
        }
      }

      current_pred_raw <- dplyr::bind_cols(current_pred_raw,mvar_tibble)
      current_pred_raw_all <- dplyr::bind_cols(current_pred_raw_all,mvar_all.estimates.tibble)
    }
  }

  data_obj %>%
    dplyr::select("time", dplyr::all_of(x_names_vec_nolag)) %>%

    ########### TODO CHHHHEEEEEEECK. Don't think this makes sense. This happens if e.g. a value for one variable is released later
    # The drop_na below was used because for GCapitalForm the value for July 2022 was missing - while it was there for FinConsExpHH
    # Now the question is whether the drop_na messes up the timing
    tidyr::drop_na() %>% # UNCOMMENT THIS WHEN NOT HAVING A FULL DATASET

    dplyr::bind_rows(current_pred_raw %>%
                       #dplyr::select(time, dplyr::all_of(x_names_vec_nolag), dplyr::any_of("trend"))) -> intermed
                       dplyr::select("time", dplyr::all_of(x_names_vec_nolag))) -> intermed

  # add the lagged x-variables
  if(ncol(intermed) > 1){
    to_be_added <- dplyr::tibble(.rows = nrow(intermed))
    for (j in ar_vec) {
      if(j == 0){next}
      intermed %>%
        dplyr::mutate(dplyr::across(-time, ~dplyr::lag(., n = j))) %>%
        dplyr::select(-time) -> inter_intermed

      inter_intermed %>%
        setNames(paste0("L", j, ".", names(inter_intermed))) %>%
        dplyr::bind_cols(to_be_added, .) -> to_be_added
    }
    intermed <- dplyr::bind_cols(intermed, to_be_added)
  }

  intermed %>%
    dplyr::left_join(current_pred_raw %>%
                       dplyr::select("time", dplyr::any_of("trend"), dplyr::starts_with("q_"),
                                     dplyr::starts_with("iis"), dplyr::starts_with("sis")),
                     by = "time") %>%
    tidyr::drop_na() %>%
    dplyr::select(-"time") %>%
    dplyr::select(dplyr::any_of(row.names(isat_obj$mean.results))) %>%
    return() -> pred_df

  #print(pred_df)

  ########
  # if necessary, repeat creating the pred_df with all estimates
  chk_any_listcols <- current_pred_raw_all %>%
    dplyr::summarise_all(class) %>%
    tidyr::gather(variable, class) %>%
    dplyr::mutate(chk = class == "list") %>%
    dplyr::summarise(chk = any(chk)) %>%
    dplyr::pull(chk)

  if(chk_any_listcols){
    ## repeat the above with all
    data_obj %>%
      dplyr::select(time, dplyr::all_of(x_names_vec_nolag)) %>%
      dplyr::mutate(across(dplyr::all_of(x_names_vec_nolag), ~as.list(.))) %>%

      ########### TODO CHHHHEEEEEEECK. Don't think this makes sense. This happens if e.g. a value for one variable is released later
      # The drop_na below was used because for GCapitalForm the value for July 2022 was missing - while it was there for FinConsExpHH
      # Now the question is whether the drop_na messes up the timing
      tidyr::drop_na() %>% # UNCOMMENT THIS WHEN NOT HAVING A FULL DATASET

      dplyr::bind_rows(current_pred_raw_all %>%
                         #dplyr::select(time, dplyr::all_of(x_names_vec_nolag), dplyr::any_of("trend"))) -> intermed
                         dplyr::rename_with(dplyr::everything(), .fn = ~gsub(".all","",.)) %>%
                         dplyr::mutate(dplyr::across(-"time", .fn = ~as.list(.))) %>%
                         dplyr::select(time, dplyr::all_of(paste0(x_names_vec_nolag)))) -> intermed.all


    # same for .all: add the lagged x-variables
    to_be_added.all <- dplyr::tibble(.rows = nrow(intermed.all))
    for (j in 1:max(ar_vec)) {
      intermed.all %>%
        dplyr::mutate(dplyr::across(-time, ~dplyr::lag(., n = j))) %>%
        dplyr::select(-time) -> inter_intermed.all

      inter_intermed.all %>%
        setNames(paste0("L", j, ".", names(inter_intermed.all))) %>%
        dplyr::bind_cols(to_be_added.all, .) -> to_be_added.all
    }

    dplyr::bind_cols(intermed.all, to_be_added.all) %>%
      dplyr::left_join(current_pred_raw_all %>%
                         dplyr::select(time, dplyr::any_of("trend"), dplyr::starts_with("q_"),
                                       dplyr::starts_with("iis"), dplyr::starts_with("sis")),
                       by = "time") %>%
      tidyr::drop_na() %>%
      dplyr::select(-time) %>%
      dplyr::select(dplyr::any_of(row.names(isat_obj$mean.results))) %>%
      return() -> pred_df.all
  }


  final_i_data <- dplyr::tibble(
    data = list(intermed %>%
                  dplyr::left_join(current_pred_raw %>% dplyr::select("time", dplyr::starts_with("q_"),
                                                                      dplyr::starts_with("iis"),
                                                                      dplyr::starts_with("sis")),
                                   by = "time") %>%
                  tidyr::drop_na()))

  out <- list()
  out$pred_df <- pred_df
  out$isat_obj <- isat_obj
  out$final_i_data <- final_i_data
  out$chk_any_listcols <- chk_any_listcols
  out$current_pred_raw <- current_pred_raw
  out$current_pred_raw_all <- if(exists("current_pred_raw_all")){current_pred_raw_all}
  out$pred_df.all <- if(exists("pred_df.all")){pred_df.all}
  return(out)

}