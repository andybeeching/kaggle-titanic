# /* !/usr/bin/Rscript */

#' # Titanic Data Analysis
#' This file is the main entry point into the analysis, which is broken
#' down by concern to aid maintenance and extensibility.
#'
#' - *clean.r*    -- Data pre-processing
#' - *explore.r*  -- Data inspection
#' - *create.r*   -- Feature engineering
#' - *model.r*    -- Model fitting
#' - *validate.r* -- Model validation
#'
#' ## Setup
#' Prep the R environment:
#' - Suppress package messages and warnings when generating Knitr docs
#' - Teardown previous user vars in workspace
#' - Set working directory to script directory
#' - Reset plot setup
#' - Set global seed value
#' - load dependencies
# /*
writeLines('Beginning Titanic Data Analysis')
writeLines('-------------')
writeLines('\nStart: Reset workspace, load dependencies & source data')
# */

SEED <- 83;

#+ suppress-warnings, include = FALSE
if (isTRUE(getOption("knitr.in.progress"))) {
  knitr::opts_chunk$set(message = FALSE, warning = FALSE)
} else {
  setwd(dirname(sys.frame(1)$ofile))
}

rm(list=ls())
par(mfrow=c(1,1))

library(Amelia)        # for missmap()
library(tree)          # for exploring
library(caret)         # for modelling
library(caretEnsemble) # for modelling
library(discern)       # for analysis - install_github('andybeeching/discern')

#' ### Obtain data
#' Kaggle doesn't permit direct downloads, so assume manually placed in
#' the "data" directory. Additionally, save the data as a fresh
#' workspace.
# TRAIN_DATA_URL <- "https://www.kaggle.com/c/titanic-gettingStarted/download/train.csv"
# TEST_DATA_URL  <- "https://www.kaggle.com/c/titanic-gettingStarted/download/test.csv"
# download.file(TRAIN_DATA_URL, destfile="../data/train.csv", method="curl")
# download.file(TEST_DATA_URL, destfile="../data/test.csv", method="curl")
ts <- date()
raw <- read.csv('../data/train.csv')
test <- read.csv('../data/test.csv')
save(raw, ts, file="../data/raw.rda")
save(test, ts, file="../data/test.rda")

# /*
# reference modules to prepare data for modelling
#  - NOTE: model.r and analyse.r generally used independently due to
#    runtime cost of training models and running validation tests.
# */
if (isTRUE(getOption("knitr.in.progress"))) {
  knitr::spin_child('clean')
  knitr::spin_child('explore.r')
  knitr::spin_child('create.r')
  knitr::spin_child('model.r')
  knitr::spin_child('analyse.r')
} else {
  source('clean')
  source('explore.r')
  source('create.r')
  source('model.r')
  source('validate.r')
}

#' ### Prediction
#' Predict the response variable *Survived* for the original test
#' dataset. Through submission the best result was in fact random
#' forest, so this is the model referenced in the script.
predictions <- predict(ensembleFit, newdata = test.munged)

#' Re-level vector to binary response, associate with passenger ID,
#' and write CSV file.
predictions <- round(predictions)
levels(predictions) <- c(0, 1)
output <- as.data.frame(predictions)
output$PassengerId <- test$PassengerId
write.csv(output[,c("PassengerId", "Survived")],
          file="../Titanic_predictions.csv", row.names=FALSE, quote=FALSE)

