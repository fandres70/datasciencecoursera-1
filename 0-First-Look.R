# First look at data
# efg, 2014-02-02

lookAtSubjects <- function(BASEDIR, GROUP)
{
  cat("Group", GROUP, "\n")

  ##### subject_XXXX.txt file (XXXX is "test" or "train")
  filename <- paste0(BASEDIR, GROUP, "/subject_", GROUP, ".txt")
  s <- readLines(filename)
  print(length(s))
  # table(s)   # OK, data can be converted to numeric without problems

  subject.id <- as.numeric(s)
  print(table(subject.id))

  cat(length(s) / length(table(subject.id)), "lines per subject\n")

  ##### y_XXXX.txt file
  filename <- paste0(BASEDIR, GROUP, "/y_", GROUP, ".txt")
  s <- readLines(filename)
  print(length(s))
  # table(s)   # OK, data can be converted to numeric without problems

  activity <- as.numeric(s)
  print(table(activity))

  print(table(subject.id, activity))

  ##### X_XXXX.txt file
  filename <- paste0(BASEDIR, GROUP, "/X_", GROUP, ".txt")
  s <- readLines(filename)
  print(length(s))

  print(table(nchar(s)))

  # All records are 8976 bytes.  Observation of first few columns suggests 16 bytes/column.
  # Therefore, must be 8976 / 16 = 561 columns = expected number of features in features.txt.
  x <- scan(filename)           # fast way to parse very regular file
  dim(x) <- c(561, length(s))   # Want a matrix
  y <- t(x)                     # Really want transpose in a data.frame
  feature.data <- data.frame(source=GROUP, subject.id=subject.id, activity=activity, y,
                             stringsAsFactors=FALSE)
  invisible( feature.data )
}

BASEDIR <- "C:/2014/Getting-Cleaning-Data/UCI HAR Dataset/"     ##### fix if moved

setwd(BASEDIR)
sink("0-First-Look.txt", split=TRUE)

test  <- lookAtSubjects(BASEDIR, "test")
dim(test)

train <- lookAtSubjects(BASEDIR, "train")
dim(train)

combined <- rbind(test,train)
dim(combined)

print( table(combined$subject.id, combined$activity, combined$source) )

sink()

