# /* !/usr/bin/Rscript */

#' ## Data munging
#' Pre-process and explore datasets in preparation for feature creation.
#'
#' <dl>
#'  <dt>survival</dt>
#'  <dd>Survival (0 = No; 1 = Yes)</dd>
#'  <dt>pclass</dt>
#'  <dd>Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd)</dd>
#'  <dt>name</dt>
#'  <dd>Name (Title-Firstnames-Surname)</dd>
#'  <dt>sex</dt>
#'  <dd>Sex (Male/Female)
#'  <dt>age</dt>
#'  <dd>Age (in years, float)
#'  <dt>sibsp</dt>
#'  <dd>Number of Siblings/Spouses aboard</dd>
#'  <dt>parch</dt>
#'  <dd>Number of Parents/Children aboard</dd>
#'  <dt>ticket</dt>
#'  <dd>Ticket Number (alphanumeric)</dd>
#'  <dt>fare</dt>
#'  <dd>Passenger Fare (in US$)</dd>
#'  <dt>cabin</dt>
#'  <dd>Cabin (alphanumeric)</dd>
#'  <dt>embarked</dt>
#'  <dd>Port of Embarkation (C=Cherbourg; Q=Queenstown; S=Southampton)</dd>
#' </dl>
#'
#' #### Family Relation Details
#' With respect to the family relation variables (i.e. *Sibsp* and *Parch*)
#' some relations were ignored. The following are the definitions used
#' for sibsp and parch (aboard Titanic):
#'
#' - Sibling: Brother, Sister, Stepbrother, or Stepsister of Passenger
#' - Spouse: Husband or Wife of Passenger (Mistresses and Fiances Ignored)
#' - Parent: Mother or Father of Passenger
#' - Child: Son, Daughter, Stepson, or Stepdaughter of Passenger
#'
#' Other family relatives excluded from this study include cousins,
#' nephews/nieces, aunts/uncles, and in-laws. Some children travelled only
#' with a nanny, therefore `parch=0` for them. As well, some travelled with
#' very close friends or neighbors in a village, however, the definitions do
#' not support such relations.

# /*
writeLines("\n-------------")
writeLines("Combining datasets, cleaning & imputing for exploration")
# */

#' ### Combine raw datasets
#' The data provided by Kaggle is pre-split into "Training" and "Test" sets,
#' the former possessing the "ground truth" (or real outcome), the latter
#' requiring prediction.
#'
#' In order to obtain the clearest possible picture of the date, as well as
#' clean, impute missing data, and create new features, it is necessary to
#' combine the two sets into one. The two sets will be re-created based on
#' record ID before the modelling phase of the analysis.
#'
#' First a `Survived` column is added to the "Test" set (since it was missing
#' and is required to match the columns in the "Training" set), and then the
#' two combined:
test$Survived <- NA
combi <- rbind(raw, test)

#' ### Inspection
#' First assert the viability of creating a predictor for the response
#' variable by examining the proportions of each level. Prevailing wisdom
#' holds that if any proportion < 15% (0.15) then it might be classifed
#' "rare", in which case it is much more difficult to model.
prop.table(table(combi$Survived))

#' Next use str to obtain a quick overview of the combined dataset dimensions
#' and features (auto-created onRead).
str(combi)

#' Looks like *Name* and *Cabin* should be of type `character`, not `factor`:
combi$Name <- as.character(combi$Name)
combi$Cabin <- as.character(combi$Cabin)

#' Determine missing values:
#' - is.na tests for any missing vals
#' - missmap in Amelia package visualizes this
sum(is.na(combi))

missmap(combi,
  main="Titanic Training Data - Missings Map",
  col=c("yellow", "black"),
  legend=FALSE
)

#' *Age*, *Cabin*, *Fare* and *Embarkation* have missing data points. Impute
#' *Embarkation* data using most common value. The others require the
#' creation of new synthetic features for optimal imputation.
summary(combi$Embarked)
combi$Embarked[which(combi$Embarked == "")] <- "S"

#' Before engineering new features, convert and re-level `numeric` classes to
#' factors, and rename *Survived* => *Fate* (as "Survived" is one of the
#' levels). This aids understanding of confusion matrices, visualizations,
#' and summary output.
combi$Class <- as.factor(combi$Pclass)
levels(combi$Class) <- c("Upper", "Middle", "Lower")
combi$Fate <- as.factor(combi$Survived)
levels(combi$Fate) <- c("Perished", "Survived")

# /*
writeLines("\n-------------")
writeLines("Munging DONE")
# */

