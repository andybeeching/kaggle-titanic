# /* !/usr/bin/Rscript */

#' ## Model selection
#' Compare and assess best-of-breed models to determine candidate with
#' optimal fit for predicting survival of the Titanic disaster.
#' Best-of-breed is decided by initial output metrics, e.g. lowest
#' deviance score between GLM models, or Accuracy for other types.
#'
#' ### Receiver Operating Characteristic Curves
#' ROC curves plot the *Sensitivity* (true positive rate) of a model
#' against the *Specificity* (false positive rate) at various threshold
#' settings. In conjunction with the summary statistic AUC (Area Under
#' Curve) it can be used to assess the performance of a model.
#'
#' The following snippet plots ROC curves for a number of models
#' trained earlier in order to directly compare their performance
#' against one another. This is accomplished using the `plotROC`
#' function from the *discern* package which wraps the process of
#' creating confusion matrices to generate the plots and arranging
#' them on one canvas. The package can be installed from:
#' `install_github('andybeeching/discern')`
plotROC(
  models    = list(glmFit2, glmFit9, rfFit, cfFit, adaFit),
  res       = "Fate",
  cv        = cv,
  colors    = c("orange", "green", "blue", "yellow", "red"),
  labels    = c("glmFit2", "glmFit9", "rfFit", "cfFit", "adaFit")
)

#' ### Re-sampling Result Accuracy Distribution
#' Another way to assess models is to compare the distribution of
#' results from their respective k-fold validations. Repeated
#' validation means each model has 30 results - one for every
#' re-sample.
#'
#' To compare, gather the results for each model through caret's
#' `resample` method, then plot.
cvResults <- resamples(list(
  glm1 = glmFit1,
  glm2 = glmFit2,
  glm8 = glmFit8,
  glm9 = glmFit9,
  ada  = adaFit,
  rf   = rfFit,
  rf2  = rfFit,
  cf   = cfFit,
  svm  = svmFit
))
summary(cvResults)
dotplot(cvResults, metric="ROC")
bwplot(cvResults) # from lattice

#' ### Learning curves
#' An important performance facet to examine for any model is its overall
#' generalisability, that is, determine whether it may be inherently biased
#' (overfit), or conversely contain too much variance (underfit).
#'
#' One way to assert bias vs variance in a model is to plot how well
#' it performs when trained on increasingly larger amounts of data.
#'
#' Essentially the training dataset is broken into sub-training sets
#' based on ratio intervals (e.g. 10%, 20%, 30% etc...), and each
#' time the model is trained (with the original formula) it is used
#' to predict the response variable of the known cross-validation
#' dataset, and the Accuracy recorded. The resultant line is known as
#' a "learning curve".
#'
#' Ideally the curve would oscillate a little at the start as the
#' amount of data used to train the model doubles and triples, but
#' then flatten out as observations normalise and fill-out the
#' observation space.
#'
#' Once more a method from the "discern" package is used to perform
#' the above analysis. `plotLearningCurves` iteratively trains a list
#' of models using the cIn the below example on GLM models are
plotLearningCurves(
  models = list(glmFit2, glmFit9),
  labels = c("Glm2", "Glm9"),
  metric = "ROC",
  ctrlFn = fitControl,
  df     = tr,
  cv     = cv,
  colors = c("orange", "blue"),
  seed   = SEED
)

# /*
writeLines("\n-------------")
writeLines("Analysis: DONE")
# */

