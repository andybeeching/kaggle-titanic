# /* !/usr/bin/Rscript */

#' ## Model fitting
#' Given problem is of a classification nature, various ML algos apply.
#' Start with a basic CART model to view the major splits of a decision
#' tree which will hint at important variables, then use Caret to train
#' and iterate upon a number of different models. Caret's unified
#' interface is especially a time-saver in this regard.
#'
#' One important note: The SEED is set before every model so that it is
#' consistent when models are run out of order during development of the
#' analysis.

# /*
writeLines("\n-------------");
writeLines("Modelling: Glm logistic, CART, Boosting and Ensembles")
# */

#' ### Define `caret` training parameters
#' Mitigate overfitting (high variance) by resampling the training
#' samples to cross-validate fitted model's performance on future data.
#' This is applied as a K-fold CV where K=10 repeated 3 times.
#'
#' The comparison performance metric of choice is ROC (AUC). To retain
#' the necessary output in the models the `twoClassSummary()` fn (from
#' `caret`) is specified as the summary function in order to to compute
#' the sensitivity, specificity and AUC.
#'
#' The training control function is also configured to compute the
#' class probabilities for held-out samples during the resample for
#' the performance comparison.
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

#' ### Basic CART model
#' Fit a classification tree to view important splits.
set.seed(SEED)

treeFit = tree(Fate ~ .,
  data=tr
)
plot(treeFit)
text(treeFit)
summary(treeFit)

#' ### Logistic regression models
#' To start the modelling proper, iterate through a number of logistic
#' regression model trained with caret (as Generalised Linear Models -
#' GLM).
#'
#' Initially all features are added, then removed or combined as
#' interactive covariates, with interesting interactions captured in
#' binary features via class compression.
#'
#' The impact in changes to the input formula were measured by each
#' model's deviance (a measure of lack of fit between model and data).
#' For this metric larger is worse. This said, lower deviance is also
#' indicative to an overfitted model (high bias).
#'
#' It was noted that this method raised "rank-deficient" errors, likely
#' due to the low number of observations in the dataset. A GLM with all
#' features in the training set scored 0.74163 on Kaggle.
#'
#' #### GLM all features
set.seed(SEED)

glmFit1 <- train(Fate ~ .,
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)

#' #### Basic GLM
#' Try a model with the selected variables in inferred order of
#' importance.
set.seed(SEED)

glmFit2 <- train(Fate ~ Sex + Class + Age + FamilyCount + Embarked +
  FareLog + BoatPriority,
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)

# /*
# Model: Logistic Regression with interaction variables
set.seed(SEED)

glmFit3 <- train(Fate ~ Sex + Class + Title + Sex*Class +
  Sex*Class*Title,
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)
# */

#' #### GLM with interaction variables
#' Used to discern key interactions for potential class compression.
#' In the following model the interaction between
#' *ClassLower:PcSurvived* was deemed significant.
#' `tr[tr$PcSurvived == 1, c("Fate", "Class")]` reveals that most
#' people travelling in lower class with a Parent/child who
#' survived, also survived (in the training set).
set.seed(SEED)

glmFit4 <- train(Fate ~ Class + Sex + Title + Sex*Embarked +
  Class*PcSurvived*SbSurvived,
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)

# /*
# Model: Logistic Regression with class compression
set.seed(SEED)

glmFit5 <- train(Fate ~ Sex + Class + Title +
  I(Title=="Mr"&Class=="Lower"),
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)

# Model: Logistic Regression with class compression
set.seed(SEED)

glmFit6 <- train(Fate ~ Sex + Class + Title +
  I(Title=="Mr"&Class=="Lower") +
  I(Class=="Middle"&Title=="Master") +
  I(Sex=="female"&Embarked=="S") +
  I(Class=="Lower"&PcSurvived==1),
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)

# Model: Logistic Regression adding PcSurvived and SbSurvived
set.seed(SEED)

glmFit7 <- train(Fate ~ Sex + Class + Title + PcSurvived + SbSurvived +
  I(Title=="Mr"&Class=="Lower") +
  I(Class=="Middle"&Title=="Master") +
  I(Sex=="female"&Embarked=="S") +
  I(Class=="Lower"&PcSurvived==1),
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)
# */

#' #### GLM with class compression
#' In this model a number of important features have been added as
#' covariates alongside some engineered features capturing various
#' important interactions identified previously. This submission
#' scored 0.79904
set.seed(SEED)

glmFit8 <- train(Fate ~ Sex + Class + Title + PcSurvived + SbSurvived +
  FareLog + FamilyCount +
  I(Title=="Mr"&Class=="Lower") +
  I(Class=="Middle"&Title=="Master") +
  I(Sex=="female"&Embarked=="S") +
  I(Class=="Lower"&PcSurvived==1),
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)

# /*
# Model: Logistic Regression with selected variables
#   - Submission scored 0.79904
set.seed(SEED)

glmFit9 <- train(Fate ~ Class + Sex + Title + Age + Embarked + FamilyCount + PcSurvived + SbSurvived + Side,
  data = tr,
  method = "glm",
  metric = "ROC",
  trControl = fitControl
)
# */

#' ### Tree Models
#' After training increasingly complex GLM models next comes a very
#' popular type of model for classification problems - tree-based
#' models. Although the first CART model didn't yield any great
#' insight, other tree-based methods are far more sophisticated in
#' nature, able to run thousands of permutations and combine them to
#' create a highly optimised and complex models.
#'
#' To keep it fair between different model types (not too mention
#' simple!) the same formula was used to fit each model:
#' `Fate ~ Class + Sex + Title + Age + Embarked + FamilyCount + PcSurvived + SbSurvived + FareLog'`
#'
#' The features listed as co-variates were derived from the best of
#' the GLM models. Those excluded were deemed superfluous due to
#' their lack of effect on the deviance change in the models they
#' were included.
#'
#' Finally, in all cases the same training control function used to
#' train the GLM logistic regression models was re-purposed.
#'
#' ---
#' NOTE: These models can take a while to train, hang in there...
#'
#' #### Ada boosted tree model
#' Boosting constitutes an ensemble model as an iterative algo that
#' combines simple classification rules with 'mediocre' performance
#' in terms of misclassification error rate to produce highly
#' accurate classification rule.
#'
#' Ada is statistical boosting based on additive logistic regression.
#' Here classification trees are generated via rpart and the caret
#' package constitute the base classifiers.
#'
#' For this model a tuning grid configured for varying iterations
#' (25 & 50), maxdepth (4 & 6), and nu shrinkage parameter (0.1 & 1)
#' was created. All permutations were exercised.
#'
#' One can also visualise feature variance with `varplot()` from the
#' ada package. The model reported an accuracy of 0.84, and scored
#' 0.80383 on Kaggle's leaderboard.
set.seed(SEED)

adaGrid <- expand.grid(
  .iter = c(25, 50),
  .maxdepth = c(4, 6),
  .nu = c(0.1, 1)
)

adaFit <- train(Fate ~ Class + Sex + Title + Age + Embarked +
  FamilyCount + PcSurvived + SbSurvived + FareLog,
  data = tr,
  method = "ada",
  metric = "ROC",
  tuneGrid = adaGrid,
  trControl = fitControl
)

varplot(adaFit$finalModel)
print(adaFit)

#' #### Random forest
#' The (in)famous rf model. This is an ensemble method similar to
#' bagged trees (performed with boostrap aggregation), whereby
#' observations are resampled and predictions are recalculated, with
#' majority vote dictating the best model.
#'
#' Random forests also bootstrap the variables at each split in the
#' tree and use the Out-of-Bag Error rate to evaluate strength of
#' model, correlation between models, and variable importance.
#'
#' For this model a tuning grid with mtry set to 3 (as per Strobl et al
#' advice that this value should equate to the sq.root of number of
#' covariates - 9) was created. Variable importance was determined
#' using the `importance()` fn from the random forest package. They
#' are ranked by the Gini purity metric. This model scored 0.81340
set.seed(SEED)

rfGrid <- expand.grid(.mtry = c(3))

rfFit <- train(Fate ~ Class + Sex + Title + Age + Embarked + FamilyCount + PcSurvived + SbSurvived + FareLog,
  data = tr,
  method = "rf",
  metric = "ROC",
  tuneGrid = rfGrid,
  trControl = fitControl
)

importance(rfFit$finalModel)
varImpPlot(rfFit$finalModel)
print(rfFit)

#' #### Conditional forest
#' An alternative to random forests which handle factor co-variates
#' with many levels better than their better known brethren. A tuning
#' grid with the same tuning grid as the random forest was created,
#' and the same variables used to discern important variables.
#'
#' Ultimately it scored 0.80861, lower than the random forest model.
set.seed(SEED)

cfGrid <- expand.grid(.mtry = c(3))

cfFit <- train(Fate ~ Class + Sex + Title + Age + Embarked + FamilyCount + PcSurvived + SbSurvived + FareLog,
  data = tr,
  method = "cforest",
  metric = "ROC",
  tuneGrid = cfGrid,
  trControl = fitControl
)

print(cfFit)

#' ### Support vector machine
#'
#' SVM build a model that assigns observations into one category or the other
#' based on a kernel method (comparator function). It is an example of a
#' non-probabilistic binary linear classifier.
#'
#' For this model it was specified that the data should be pre-processed by
#' centering and scaling it, as SVM is sensitive, especially when using a
#' Gaussian kernel.
#'
#' Furthermore, the `tuneLength` which determines the number of levels for
#' each tuning parameters generated by train() was set to 9. This produces a
#' tuning grid of c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64), which corresponds to
#' the value passed in for the `C` parameter of the kernel method. In turn,
#' this controls the "penalty" for misclassified training examples.
#'
#' In this case the Sigma value determined by Caret. The submission scored a
#' lowly 0.78469.
set.seed(SEED)

svmFit <- train(Fate ~ Class + Sex + Title + Age + Embarked + FamilyCount + PcSurvived + SbSurvived + Side + FareLog + BoatPriority,
  data = tr,
  method = "svmRadial",
  metric = "ROC",
  tuneLength = 9,
  preProcess = c("center", "scale"),
  trControl = fitControl
)

print(svmFit)

#' Meta-ensemble model
#' This type of model combines models of different types (or the same even)
#' in the same vein as ensemble models such as random forest, with majority
#' vote scoring (hence the number of models should be odd).
#'
#' Although this could be done crudely by simply running a prediction for
#' the desired records and comparing the results to generate the mode for
#' each, using `caretEnsemble` means a uses a glm is used instead to create
#' a simple linear blend of models with the same re-sampling parameters.
#' Ideally the methods used produce models with low correlation in an effort
#' to cover more of the variance within a given dataset.
#'
#' To use `caretEnsemble` another training control function needs to be
#' created, which closely mirrors that used to construct the independent
#' models above, but which also specifies the resample index callback. This
#' ensures they are the same across all models.
#'
#' Equally, the same tuning grids are used to re-train ada, rf, and cf
#' models, as well as the same formula. This ensemble scored 0.80383, which
#' just goes to show more complex models don't always beat simple ones.
fitEnsembleControl <- trainControl(
  method          = "repeatedcv",
  number          = 10,
  repeats         = 3,
  savePredictions = TRUE,
  classProbs      = TRUE,
  index           = createResample(tr$Fate, 10),
  summaryFunction = twoClassSummary
)

set.seed(SEED)
ensembleList <- caretList(Fate ~ Class + Sex + Title + Age + Embarked + FamilyCount + PcSurvived + SbSurvived + FareLog,
  data       = tr,
  metric     = "ROC",
  trControl  = fitEnsembleControl,
  methodList=c("glm", "svmRadial"),
  tuneList   = list(
    ada = caretModelSpec(method="ada", tuneGrid=adaGrid),
    rf  = caretModelSpec(method="rf", tuneGrid=rfGrid),
    cf  = caretModelSpec(method="cforest", tuneGrid=cfGrid)
  )
)

ensembleFit  <- caretEnsemble(ensembleList)

# /*
writeLines("\n-------------")
writeLines("Modelling: DONE")
# */

