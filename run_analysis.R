require(dplyr)
require(plyr)
  
# a scheme to turn feature name to useful variable name suitable as data.frame column
reformat <- function(x) {
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("[^[:alpha:]]", " ", x)
  x <- gsub("^\\s+|\\s+$", "", x)
  x <- gsub("\\s{2,3}", " ", x)
  x <- gsub("\\s", "_", x)
  x
}

# extract indices/names matching “mean” or “std"
extractInfo <- function(fn) {
  lines <- readLines(fn)
  
  indices <- numeric()
  names <- character()
  
  for (i in seq_along(lines)) {
    text = lines[i]
    if ( grepl("std", text) || grepl("mean", text))
    {
      indices <- c(indices, i)
      names <- c(names, reformat(text))
    }
  }
  
  list(indices, names)
}

# 1. merge test/train sets
# 2. aggregate on subjects
createTidyData <- function() {
  r <- extractInfo("features.txt")
  indices <- r[[1]]
  names <- r[[2]]
  
  test <- read.table("test/X_test.txt")
  train <- read.table("train/X_train.txt")
  subjects_test <- read.table("test/subject_test.txt")
  subjects_train <- read.table("train/subject_train.txt")

  test <- mutate(select(test, indices), subject=subjects_test[,1])
  train <- mutate(select(train, indices), subject=subjects_train[,1])

  combined <- join(x=test, y=train, type="full")
  
  colnames(combined) <- c(names, "subject")

  data <- aggregate(. ~ subject, combined, mean)
  write.table(data, file="output.txt", row.names=F)
}

