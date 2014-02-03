# Peer Assessment Assignment, Coursera Getting and Cleaning Data class
# efg, Feb. 2014

# The assignment included five specific tasks that were to be performed.
# Comments below that start with 1., 2., 3., 4., 5., show the approximate
# locations in the code that implement these tasks.

################################################################################
### Setup

# Use BASEDIR = "." for current working directory.
BASEDIR <- "C:/2014/Getting-Cleaning-Data/UCI HAR Dataset"    ##### fix if moved
setwd(BASEDIR)

Activity.Names <- c("Walk", "WalkUp", "WalkDown", "Sit", "Stand", "Lying")

################################################################################
### Helper function
###
### readGroup ("test" or "train") from subdirectory and return data.frame with
### tidy data.

readGroup <- function(BASEDIR, GROUP)
{
  # Read subject_GROUP.txt file (GROUP is "test" or "train")
  filename <- paste0(BASEDIR, "/", GROUP, "/subject_", GROUP, ".txt")
  subject.id <- as.numeric(readLines(filename))

  # y_GROUP.txt file with labels
  filename <- paste0(BASEDIR, "/", GROUP, "/y_", GROUP, ".txt")
  activity.index <- as.numeric(readLines(filename))

  stopifnot(length(subject.id) == length(activity.index))

  # X_GROUP.txt file with feature data
  filename <- paste0(BASEDIR, "/", GROUP, "/X_", GROUP, ".txt")
  # All records are 8976 bytes wide.  Each field takes 16 bytes.
  # Therefore, must be 8976 / 16 = 561 columns = expected number of features.
  x <- scan(filename)                   # fast way to parse very regular file
  dim(x) <- c(561, length(subject.id))  # Want a matrix
  y <- t(x)                             # Really want transpose of matrix

  # Note conversion of activity.index to descriptive name below to:
  # "3. Use descriptive activity names to name the activities in the dataset"

  GROUP.data <- data.frame(source=GROUP,   # test or train
                           subject.id=subject.id,
                           activity=Activity.Names[activity.index],
                           y,   # deal with column names later
                           stringsAsFactors=FALSE)
  invisible(GROUP.data)
}

################################################################################
### Load test and train data

test.data  <- readGroup(BASEDIR, "test")
dim(test.data)

train.data <- readGroup(BASEDIR, "train")
dim(train.data)

################################################################################
### "1. Merge the training and test sets to create one dataset."

combined.data <- rbind(test.data, train.data)
dim(combined.data)

################################################################################
### Let's cleanup feature names to be more R "friendly"

filename <- paste0(BASEDIR, "/features.txt")
feature.names <- read.table(filename, sep=" ", header=FALSE, as.is=TRUE)
feature.names <- feature.names$V2   # only want 2nd column

# Change "()" to "E" for "estimate" as in "estimated from these signals"
feature.names <- gsub("\\()", "E", feature.names)

# Remove dashes and commas
feature.names <- gsub("-|,", ".", feature.names)

# Fix angle data with paretheses. "(" -> ".". ")" -> ""
feature.names <- gsub("\\(", ".", feature.names)
feature.names <- gsub(")",   "",  feature.names)

# Prefix all names with vNNN. to maintain link to original documentation.
# (These can always be removed later if not wanted.)
feature.names <- sprintf("v%3.3d.%s", 1:length(feature.names),
                           feature.names)

head(feature.names)
tail(feature.names)

### Add "friendly" feature names to combined.data
### "4. Appropriately label the dataset with desciptive" names
names(combined.data)[4:ncol(combined.data)] <- feature.names

################################################################################
### I could write the combined data to disk if desired at this point.  The
### instructions are ambiguous on whether this is wanted.  Since the file is
### huge (~64 MB), I'm electing NOT to create the file at this time.

#write.csv(combined.data, "Combined.csv", row.names=FALSE)

################################################################################
### "2. Extract only the measurements on the mean and standard deviation ..."

# Feature estimates for mean and standard deviation are represented in the
# column names as meanE and stdE [since the () was replaced by "E" above].
#
# In addition to the meanE and stdE columns, we also want columns 1:3, namely
# source, subject.id and activity.

MeanStdExtractColumns <- c(1:3, grep("meanE|stdE", names(combined.data)))

MeanStdExtract <- combined.data[, MeanStdExtractColumns]
dim(MeanStdExtract)
names(MeanStdExtract)

################################################################################
### "5. Create a second, independent tidy dataset with the average of each
### variable for each activity for each subject"

splits <- split(MeanStdExtract, list(MeanStdExtract$subject.id,
                                     MeanStdExtract$activity))
length(splits)

x <- splits[[1]]

Tidy.Summary <- do.call(rbind, lapply(splits,
                  function(x)
                  {
                    data.frame(subject.id = x$subject.id[1],
                               activity   = x$activity[1],
                               t(colMeans(x[,-1:-3])),  # note transpose here
                               stringsAsFactors=FALSE)
                  }
                ))

# Order by subject.id and activity to see all activities for subject.id together.
Tidy.Summary <- Tidy.Summary[order(Tidy.Summary$subject.id, Tidy.Summary$activity),]

# This file is the one to be submitted as part of the assignment
write.csv(Tidy.Summary, "Samsung-Tidy-Summary.csv", row.names=FALSE)

################################################################################
### Create descriptive stats to show various counts. Useful for double-checking.

table(combined.data$source)
table(combined.data$subject.id)
table(combined.data$activity)

# Number of records by activity by subject.id by source
table(combined.data$subject.id, combined.data$activity, combined.data$source)

