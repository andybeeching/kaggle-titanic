# Kaggle Titanic Data Analysis

- *Task*: Create a model to predict which passengers survived the
  tradgedy.
- *From*: https://www.kaggle.com/c/titanic-gettingStarted
- *Type*: Binary classification

The Titanic sunk on maiden voyage, killing 1502 out of 2224
passengers and crew. There were not enough lifeboats, though some
groups more likely to survive than others (women, children and
upper-class).

### A learning exercise
This is my first attempt at a Kaggle project, and also with the R
language, so for that reason I decided to liberally document the
source code and methodology as I progressed.

Additionally I tried to shun R packages, aside from the mighty
`caret` and its meta cousin `caretEnsemble`, in order to focus on
what R can do out-of-the-box. Although fantastic packages exist to
aid data munging, visualisation, and model training, and which likely
would have made this analysis easier/prettier/more sophisticated, using
them would have shielded me from encountering many of R's native
features and oddities.

Since I believe a good-level of competency in vanilla R will be
advantageous if I would like to author my own packages, or help
contribute to others. I intend to try out different approaches in
future kaggles and Data Science projects.

### Kudos
Many of the ideas within this analysis are inspired/borrowed from
various other sources. All credit, copyright and props to the
respective authors for sharing their code. As abovementioned, this is a
practice data analysis to get a feel for R and its capabilities.

- https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
- http://www.ultravioletanalytics.com/2014/10/30/kaggle-titanic-competition-part-i-intro/
- http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r
- http://rforwork.info/2012/12/23/binary-classification-a-comparison-of-titanic-proportions-between-logistic-regression-random-forests-and-conditional-trees/
- https://www.kaggle.com/c/titanic/forums/t/4693/is-cabin-an-important-predictor/25690
- https://inclass.kaggle.com/c/deloitte-tackles-titanic/forums/t/9841/getting-high-scores-without-looking-at-actual-data-set/

### Generating the report
Execute the following command from the root of the repo in your shell
of choice. It assumes *Knitr* has been installed.
```
# from CLI
> Rscript -e "library(knitr); knitr::spin('./src/main.r')"

# from R prompt
> library(knitr)
> knitr::spin("./src/main.r")
```
