# /* !/usr/bin/Rscript */

# /*
writeLines("\n-------------")
writeLines("Exploring combined dataset to view topography & select features")
# */

#' ## Exploration
#' Quickly summarise some key features:
summary(combi[c("Age", "Sex", "SibSp", "Parch", "Fare", "Class", "Fate")])

#' From the above we can list the following facts about the passengers:
#' - Majority boarded at Southampton.
#' - Majority in 3rd class.
#' - Majority perished.
#' - There were ~twice as many men than women.
#' - Mean age ~30.
#'
#' ### Examining Fare
#' It seems some passengers had a fare of zero, which should only apply to
#' infants (<1 year). Let's assert that:
combi[which(combi$Fare == 0), c("Name", "Age")]

#' Although *Age* is missing from some of the records, it seems some adults
#' have managed to pay 0 Fare, or more likely they are part of a group and
#' the fare has been registered to another person. However, before the data
#' can be processed the missing Age data must be imputed.
#'
#' ### Distributions
#' Examine the distribution of *Age*, *Class* and *Sex*:
barplot(table(combi$Pclass),
  names.arg = c("First", "Second", "Third"),
  main="Transport class", col="black"
)

barplot(table(combi$Sex),
  names.arg = c("Female", "Male"),
  main="Gender", col="black"
)

hist(combi$Age,
  main = "Passenger Age",
  col  = "black",
  xLab = NULL
)

#' Nothing above looks too out of the ordinary except for a slight skewing of
#' Age data towards young people (0-30).
#'
#' ### Categorical Features
#' Next examine the relationship between various feature pairs. For this we
#' employ mosaic plots to most effectively show the distributions of
#' categorical variables against one another.
#'
#' Since we're most interested in identifying trends with respect to the
#' outcome variable "Fate", only the training data is used to generate the
#' plots (as it contains the ground truth for the records within).
mosaicplot(Survived ~ Pclass,
  data  = raw,
  main  = "Passenger Fate by Traveling Class",
  shade = FALSE,
  color = TRUE,
  dir   = c("v", "h"),
  xlab  = "Survived",
  ylab  = "Travel Class"
)

mosaicplot(Survived ~ SibSp,
  data  = raw,
  main  = "Passenger Fate by Siblings + Spouse",
  shade = FALSE,
  color = TRUE,
  dir   = c("v", "h"),
  xlab  = "Survived",
  ylab  = "Siblings + Spouse"
)

mosaicplot(Survived ~ Parch,
  data  = raw,
  main  = "Passenger Fate by Parents and Children",
  shade = FALSE,
  color = TRUE,
  dir   = c("v", "h"),
  xlab  = "Survived",
  ylab  = "Parents + Kids"
)

mosaicplot(Survived ~ Embarked,
  data  = raw,
  main  = "Passenger Fate by Embarkation",
  shade = FALSE,
  color = TRUE,
  dir   = c("v", "h"),
  xlab  = "Survived",
  ylab  = "Embarked"
)

mosaicplot(Survived ~ Sex,
  data  = raw,
  main  = "Passenger Fate by Gender",
  shade = FALSE,
  color = TRUE,
  dir   = c("v", "h"),
  xlab  = "Survived",
  ylab  = "Gender"
)

mosaicplot(Survived ~ Sex + Pclass,
  data  = raw,
  main  = "Passenger Fate by Gender and Class",
  shade = FALSE,
  color = TRUE,
  off   = c(1,5,5),
  dir   = c("h", "v", "v"),
  xlab  = "Travelling Class",
  ylab  = "Survived"
)

#' Here we can identify a number of relationships, and therefore possible
#' correlations and/or con-founders:
#' - *Survived* vs *Pclass*; definite relationship
#' - *Survived* vs *Gender*; definite relationship
#' - *Survived* vs *Pclass* vs *Gender*; definite relationship
#'
#' ### Continuous Features
#' Also explore continuous variables such as *Age* through boxplots.
boxplot(Age ~ Survived,
  data = raw,
  xlab = "Survived",
  ylab = "Age"
)

boxplot(Age ~ Class,
  data = combi,
  xlab = "Pclass",
  ylab = "Age"
)

boxplot(Age ~ Class + Survived,
  data = combi,
  xlab = "Pclass",
  ylab = "Age"
)

#' The plots demonstrate a possible relationship between *Age* and *Fate*,
#' worthy of further exploration or feature engineering. Additionally it
#' confirms a suspicion that higher class equates to older age. This maybe
#' potentially important when constructing the model later.

# /*
writeLines("\n-------------")
writeLines("Exploring DONE")
# */

