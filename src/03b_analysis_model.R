library(here)
library(beepr)
library(ROLLing)

#The following code is to be run while ROLLing is still under development. At the end, this is to be replaced by library(ROLLing).
library(devtools)
setwd("~/GitHub/ROLLing")
load_all()

#' Fit a series of ROLLing models with OSCAR regularisation.
#'
#' @param weighted Whether weights are desired.
#' @param topic Whether topic feature is added.
#'
#' @return A list of ROLLing models. 
fitROLLingModel = function(weighted = FALSE, topic = FALSE){
  cli_alert_info("Collecting data ...")
  df = readRDS(here("output", "03a_coded_data", "wodata.rds"))

  # 1. Prepare the initial ingredients: get coxDF, enter all input
  featsChosen = c("argTypeNew",
            "noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
            "haveBridges", "identifiable", "local", "justFirst", "justLast", 
              "interrog", "animate",
            "self", "addressee", "length", "pronom", "noPrevMentionsFar", "noNextMentionsFar")
  if(topic) featsChosen = c(featsChosen, "topic")
  featsQuant = c("noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
  "justFirst", "justLast", "length", "noPrevMentionsFar", "noNextMentionsFar")
  origDataInfo = list(featsChosen = featsChosen, formulaType = "reg", order = "argOrder", id = "verbID")
  id = "verbID"
  order = "argOrder"
  formulaType = "reg"

  #Create the coxDF
  coxDF = ROLLing::getCoxData(df, featsChosen, id, order, formulaType,
    scale = F, scaleCols = featsQuant)

  #Create the splits for CV
  splitVec = splitData(coxDF, k = 5, seed = 2024L)

  cli_alert_info("Fitting model ...")
  #2. Do the actual fitting
  if(!weighted){
    model_mle = suppressWarnings(ROLLing_mle(coxDF))
    models = ROLLing_genlasso_path(coxDF,
      model_mle,
      lambdas_lasso = 10 ^ seq(-10, 10, 2),
      lambdas_ridge = 10 ^ seq(-10, 10, 2), 
      lambdas_genlasso = 10 ^ seq(-10, 10, 2),
      hyper_genlasso = list(corThreshold = .3),
      splitVec = splitVec, cv = TRUE,
      elnetMode = "all",
      origDataInfo = origDataInfo)
  } else {
    weights = getWeights(df)
    model_mle = suppressWarnings(ROLLing_mle(coxDF, weights = weights))
    models = ROLLing_genlasso_path(coxDF,
      model_mle,
      lambdas_lasso = 10 ^ seq(-10, 10, 2),
      lambdas_ridge = 10 ^ seq(-10, 10, 2), 
      lambdas_genlasso = 10 ^ seq(-10, 10, 2),
      hyper_genlasso = list(corThreshold = .3),
      weights = weights,
      splitVec = splitVec, cv = TRUE,
      elnetMode = "all",
      origDataInfo = origDataInfo)
  }
  cli_alert_info("Fitting finished!")
  models
}

#' Fit a series of ROLLing models with OSCAR regularisation.
#'
#' @param df A coxDF.
#'
#' @return A list of ROLLing models. 
getWeights = function(df){
  df = df %>%
    group_by(verbID) %>%
    mutate(argTypeSeq = paste0(argTypeNew, collapse = "-"),
              n_arg = n()) %>%
    ungroup %>%
    group_by(argTypeSeq) %>%
    mutate(argTypeSeqFreq = sum(1/sqrt(n_arg))) %>%
    mutate(weight = 1 / argTypeSeqFreq) %>%
    ungroup %>%
    mutate(weight = weight / sum(weight) * nrow(.))
  df$weight
}

#' Fit a series of ROLLing models with OSCAR regularisation.
#'
#' @param genlasso_models A list of ROLLing models with equal top performance.
#'
#' @return The ROLLing model with the most conservative regularisation.
pickBestModel = function(genlasso_models){
  genlasso_models$coefs %>%
    mutate(id = 1:nrow(.)) %>%
    filter(.$lambda_lasso == max(.$lambda_lasso)) %>%
    filter(.$lambda_ridge == max(.$lambda_ridge)) %>%
    filter(.$lambda_genlasso == max(.$lambda_genlasso)) %>%
    slice_head() %>% pull(id)
}

#' Quickly evaluate the optimal model. Model predictions are written to
#' output/03b_models, and accuracy is shown on the console.
#'
#' @param model The optimal ROLLing model.
quickEvalModel = function(model){
  predicts = predict(model)
  mean_perc = predicts$by_clause$correct %>% mean
  cli_alert_info(glue("Model performance: {round(mean_perc * 100, 2)}%"))
  write_csv(predicts$by_clause, here("output", "03b_models", "predicts_by_clause.csv"))
  write_csv(predicts$by_arg, here("output", "03b_models", "predicts_by_arg.csv"))
}

#' Save the list of models and best model to output/03b_models as RDS files.
#'
#' @param all_models A list of models
#' @param best_model_index The index of the best model.
saveModels = function(all_models, best_model_index){
  cli_alert_info("Saving model data ...")
  saveRDS(all_models, here("output", "03b_models", "all_models.rds"))
  saveRDS(all_models[[best_model_index]], here("output", "03b_models", "best_model.rds"))
}

main = function(...){
  #The first two lines should be replaced with the commented-out line later:
  #models = fitROLLingModel(...)
  #best_model_index = pickBestModel(models)
  models = readRDS(here("output", "03b_models", "20241111_allmodels.rds"))[[1]]
  best_model_index = 1
  quickEvalModel(models[[best_model_index]])
  saveModels(models, best_model_index)
  cli_alert_success("Done!")
}

main()