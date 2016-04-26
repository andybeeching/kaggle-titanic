# /* !/usr/bin/Rscript */

#' ## Feature engineering
#' Create new features from the munged data to aid modelling.

# /*
writeLines("\n-------------")
writeLines("Creating new features in combined dataset for modelling")
# */

#' ### Create the "Title" Feature
#' Although *Age*, *Gender* and travelling class are already provided, they
#' might be augmented by a variable capturing the socio-economic status of the
#' passenger. This would help differentiate between passengers travelling in
#' the same class who may erstwhile be very similar.
#'
#' Such a variable can also act as a proxy for lifeboat access, with the
#' assumption that those from the upper classes had an advantage held more
#' ready access than those travelling in 3rd class.
#'
#' Peeking at the first few records, it seems the title comes after surname,
#' conveniently delimited by a comma:
head(combi$Name)

#' The format is such: `<surname>,<title> <forenames> (<maiden name>)`
#' It is also useful to note that married women are referred to by their
#' husband's name. This could hold implications for determining the size of a
#' given party travelling together.
#'
#' To create the feature, extract the title for each record, group similar
#' titles to reduce the number of levels, and convert to a factor.
stripTitle <- function(data) {
  names    <- data$Name
  startIdx <- regexpr("\\,", names, perl=TRUE)+2
  endIdx   <- regexpr("\\.", names, perl=TRUE)-1
  titles   <- substr(names, startIdx, endIdx)
  return (titles)
}

groupTitles <- function(data, titles, newTitle) {
  for (item in titles) {
    data$Title[which(data$Title == item)] <- newTitle
  }
  return (data$Title)
}

combi$Title <- stripTitle(combi)

combi$Title <- groupTitles(combi,
  c("Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer"),
  "Sir"
)

combi$Title <- groupTitles(combi,
  c("Lady", "the Countess", "Dona"),
  "Lady"
)

combi$Title <- groupTitles(combi,
  c("Ms", "Mme"),
  "Mrs"
)

combi$Title <- groupTitles(combi, c("Mlle"), "Miss")
combi$Title <- as.factor(combi$Title)
unique(combi$Title)

#' ### Impute Age Data
#' *Age* was one of the features identified as missing data during the initial
#' exploration of the combined dataset. A number of strategies might be
#' followed to impute the missing values:
#' - Use mean Age of passengers. Overly simplistic, probable skew of data.
#' - Use mean Age of passengers grouped by travelling class. Still too
#' simplistic, doesn't account for adults/children/gender
#' - Use mean Age grouped by "Title", a derived feature from *Name*. More
#' nuanced than alternatives, but potentially more complicated.
#' - Chose prediction via general linear models
ageFit <- train(Age ~ Title + Pclass + Fare + Sex,
  data = combi[which(!is.na(combi$Age)),],
  method = "glm"
)

agePred <- round(predict(ageFit,
  newdata = combi[which(is.na(combi$Age)),]
), 1)

combi[which(is.na(combi$Age)),]$Age <- agePred

summary(combi$Age)

#' ### Create "FareLog" feature
#' The exploration phase revealed that the *Fare* data was missing some values,
#' and that it was highly skewed. First we need to impute the Fare data for
#' any adult with a value of "0", then transform the data to counter the skew.

# Implementation note: which() returns idxs, so beware nesting them as they
# are relative to their immediate context, not overall dataset context!
imputeMedian <- function(data, feature, filter) {
  for (item in filter) {
    subIdx <- which(feature == item)
    result <- median(data[subIdx], na.rm = TRUE)

    # maps NA records in lexical subset to source data set and replaces
    data[subIdx[which(is.na(data[subIdx]))]] <- result
  }
  return (data)
}

zeroed <- which(combi$Fare == 0 & combi$Age > 1)
combi$Fare[zeroed] <- NA
combi$Fare <- imputeMedian(combi$Fare, combi$Class, unique(combi$Class))
combi$FareLog <- log10(combi$Fare)
quantile(combi$FareLog, na.rm=TRUE)

#' ### Create "BoatPriority" feature
#' Capture the "women and children first" policy for lifeboats in a binary
#' feature. TRUE applies to all females and/or children under the age of 13.
#' 13 chosen as a cut-off as this was the conventional age of adult men in the
#' early 20th century.
combi$BoatPriority <- 0
combi$BoatPriority[which(combi$Sex == "female" | combi$Age < 13)] <- 1
combi$BoatPriority <- as.factor(combi$BoatPriority)

#' ### Create the "FamilyID" feature
#' It would be useful to group passengers together by family, or travelling
#' group. It's plausible that during a time of emergency they would have stuck,
#' and perhaps lived or died together. This can be useful for modelling.
#'
#' Although ostensibly it would be easiest to simply group passengers by
#' surname, this approach doesn't account for common surnames, such as "Smith",
#' or "Jones". It also doesn't account for people travelling together with
#' different names (e.g. maiden name).
#'
#' Also, trying to use the *Sibsp* and *Parch* relative count variables to
#' differentiate identically named families suffers from a lack of efficacy
#' for small groups, and mis-matches in the counts between passengers
#' travelling together on the same TicketID.
#'
#' For example, these three travellers are on the same ticket, but have
#' mismatched family indicators:
combi[which(combi$Ticket == "PC 17582"), c("Name", "SibSp", "Parch")]
#'
#' So, first group by surname and ticketid - the combined entity being the
#' FamilyID. Then normalize any differences between TicketID and Surname to the
#' same FamilyID.
stripSurname <- function(data) {
  names    <- data$Name
  startIdx <- 0
  endIdx   <- regexpr("\\,", names, perl=TRUE)-1
  surnames <- substr(names, startIdx, endIdx)
  return (surnames)
}

combi$Surname <- stripSurname(combi)
combi$FamilyID <- paste0(combi$Surname, as.character(combi$Ticket))

for (tix in unique(combi$Ticket)) {
  members <- combi[combi$Ticket == tix,]

  if ((dim(members)[1] > 1) & (length(unique(members$Surname))[1] > 1)) {
    combi$FamilyID[combi$Ticket == tix] <- tix
  }
}

#' Create derivative features from "FamilyID"
#' - *FamilyCount* total size of family based on *FamilyID*
#' - *SpouseSibling* indicates if any siblings or spouses survived
#' - *ParentChild* indicates if any parents or children survived
#' - *KinSurvived* indicates if any kin survived
for (id in unique(combi$FamilyID)) {
    m <- combi[combi$FamilyID == id,]
    p <- m[m$Parch > 0,]
    s <- m[m$SibSp > 0,]

    K <- if (length(which(m$Fate == "Survived")) > 0) 1 else 0
    P <- if (length(which(p$Fate == "Survived")) > 0) 1 else 0
    S <- if (length(which(s$Fate == "Survived")) > 0) 1 else 0

    combi$FamilyCount[combi$FamilyID == id] <- dim(m)[1]
    combi$KinSurvived[combi$FamilyID == id] <- K
    combi$PcSurvived[combi$FamilyID == id] <- P
    combi$SbSurvived[combi$FamilyID == id] <- S
}
combi$FamilyCount <- factor(combi$FamilyCount)
combi$KinSurvived <- factor(combi$KinSurvived)
combi$PcSurvived <- factor(combi$PcSurvived)
combi$SbSurvived <- factor(combi$SbSurvived)

#' *FamilyID* has too many levels to be practical, so re-assign any families
#' with less than 5 members an ID of "small" to facilitate analysis (so the
#' feature doesn't dominate models, especially those which are tree-based).
combi$FamilyID[combi$FamilyCount <= 4] <- "Small"
combi$FamilyID <- factor(combi$FamilyID)

#' ### Create "CabinKnown" feature
#' Simple feature capturing whether a passenger's cabin was known. Apparently
#' the majority of these came from a list recovered from the wreck, rather than
#' the office of the company who built the Titanic. Still, it contribute to the
#' final model.
combi$Cabin[combi$Cabin == ""] <- NA
combi$CabinKnown <- 0
combi$CabinKnown[which(!is.na(combi$Cabin))] <- 1
combi$CabinKnown <- as.factor(combi$CabinKnown)

#' ### Create "Side" feature
#' It was claimed that the "Boat Priority" policy was enforced differently
#' between the Starboard and Port sides. The former allowed men to board if
#' there were no women and children waiting, the latter didn't.
#'
#' We extract this info from the *Cabin* feature. Odd numbers were Starboard,
#' even for Port.
sides <- sub("\\D*(\\d+).*", "\\1", combi$Cabin)
unique(sides)
sides[sides %in% c("F", "D", "T")] <- NA
sides[which(is.na(sides))] <- "U"
sides[as.numeric(sides) %% 2 == 0] <- "P"
sides[as.numeric(sides) %% 2 != 0] <- "S"
combi$Side <- as.factor(sides)

#' Finally prepare the dataset for modelling:
#' - Pare down the dataframe to relevant features
#' - Split combi back into the original "train" and "test" sets
#' - Create further "training" (__tr__) and "validation" (__CV__) datasets
#' at a split of 85/15.
#' - Ratio chosen due to small sample size (more data for the model)
#' - The names were chosen for brevity, and not to clash with the train()
#' function in `caret`
combi <- combi[c("Fate",
  "Sex",
  "BoatPriority",
  "Age",
  "Title",
  "Class",
  "Embarked",
  "FareLog",
  "FamilyCount",
  "FamilyID",
  "KinSurvived",
  "PcSurvived",
  "SbSurvived",
  "CabinKnown",
  "Side"
)]

raw.munged <- combi[1:dim(raw)[1],]
test.munged <- combi[(dim(raw)[1] + 1):dim(combi)[1],]

inTrain <- createDataPartition(y=raw.munged$Fate, p=0.85, list=FALSE);
tr <- raw.munged[inTrain,]
cv <- raw.munged[-inTrain,]

# /*
writeLines("\n-------------")
writeLines("Feature engineering DONE")
# */
