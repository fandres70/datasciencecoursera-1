Peer Assessment Assignment
==========================

Coursera _Getting and Cleaning Data_ class           

Setup


```r
BASEDIR <- "C:/2014/Getting-Cleaning-Data/UCI HAR Dataset"
setwd(BASEDIR)

Activity.Names <- c("Walk", "WalkUp", "WalkDown", "Sit", "Stand", "Lying")
```


The helper function `readGroup` reads the _train_ and _test_ files.

Note this comment below:  __3. Use descriptive activity names to name the activities in the dataset__


```r
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
```


Load test and train data:


```r
test.data <- readGroup(BASEDIR, "test")
dim(test.data)
```

```
## [1] 2947  564
```

```r

train.data <- readGroup(BASEDIR, "train")
dim(train.data)
```

```
## [1] 7352  564
```


Per instructions (__1. Merge the training and test sets to create one dataset__), merge the _train_ and _test_ data to create one dataset:


```r
combined.data <- rbind(test.data, train.data)
dim(combined.data)
```

```
## [1] 10299   564
```

Let's cleanup feature names to be more R "friendly" (__4. Appropriately label the dataset with desciptive__ names):


```r
filename <- paste0(BASEDIR, "/features.txt")
feature.names <- read.table(filename, sep = " ", header = FALSE, as.is = TRUE)
feature.names <- feature.names$V2  # only want 2nd column

# Change '()' to 'E' for 'estimate' as in 'estimated from these signals'
feature.names <- gsub("\\()", "E", feature.names)

# Remove dashes and commas
feature.names <- gsub("-|,", ".", feature.names)

# Fix angle data with paretheses. '(' -> '.'. ')' -> ''
feature.names <- gsub("\\(", ".", feature.names)
feature.names <- gsub(")", "", feature.names)

# Prefix all names with vNNN. to maintain link to original documentation.
# (These can always be removed later if not wanted.)
feature.names <- sprintf("v%3.3d.%s", 1:length(feature.names), feature.names)

names(combined.data)[4:ncol(combined.data)] <- feature.names

head(feature.names)
```

```
## [1] "v001.tBodyAcc.meanE.X" "v002.tBodyAcc.meanE.Y" "v003.tBodyAcc.meanE.Z"
## [4] "v004.tBodyAcc.stdE.X"  "v005.tBodyAcc.stdE.Y"  "v006.tBodyAcc.stdE.Z"
```

```r
tail(feature.names)
```

```
## [1] "v556.angle.tBodyAccJerkMean.gravityMean" 
## [2] "v557.angle.tBodyGyroMean.gravityMean"    
## [3] "v558.angle.tBodyGyroJerkMean.gravityMean"
## [4] "v559.angle.X.gravityMean"                
## [5] "v560.angle.Y.gravityMean"                
## [6] "v561.angle.Z.gravityMean"
```


I could write the `combined.data` to disk if desired at this point.  The
instructions are ambiguous on whether this is wanted.  Since the file is
huge (~64 MB), I'm electing NOT to create the file at this time.   

__2. Extract only the measurements on the mean and standard deviation ...__

Feature estimates for mean and standard deviation are represented in the
column names as meanE and stdE [since the () was replaced by "E" above].
In addition to the meanE and stdE columns, we also want columns 1:3, namely
source, subject.id and activity.


```r
MeanStdExtractColumns <- c(1:3, grep("meanE|stdE", names(combined.data)))

MeanStdExtract <- combined.data[, MeanStdExtractColumns]
dim(MeanStdExtract)
```

```
## [1] 10299    69
```

```r
names(MeanStdExtract)
```

```
##  [1] "source"                          "subject.id"                     
##  [3] "activity"                        "v001.tBodyAcc.meanE.X"          
##  [5] "v002.tBodyAcc.meanE.Y"           "v003.tBodyAcc.meanE.Z"          
##  [7] "v004.tBodyAcc.stdE.X"            "v005.tBodyAcc.stdE.Y"           
##  [9] "v006.tBodyAcc.stdE.Z"            "v041.tGravityAcc.meanE.X"       
## [11] "v042.tGravityAcc.meanE.Y"        "v043.tGravityAcc.meanE.Z"       
## [13] "v044.tGravityAcc.stdE.X"         "v045.tGravityAcc.stdE.Y"        
## [15] "v046.tGravityAcc.stdE.Z"         "v081.tBodyAccJerk.meanE.X"      
## [17] "v082.tBodyAccJerk.meanE.Y"       "v083.tBodyAccJerk.meanE.Z"      
## [19] "v084.tBodyAccJerk.stdE.X"        "v085.tBodyAccJerk.stdE.Y"       
## [21] "v086.tBodyAccJerk.stdE.Z"        "v121.tBodyGyro.meanE.X"         
## [23] "v122.tBodyGyro.meanE.Y"          "v123.tBodyGyro.meanE.Z"         
## [25] "v124.tBodyGyro.stdE.X"           "v125.tBodyGyro.stdE.Y"          
## [27] "v126.tBodyGyro.stdE.Z"           "v161.tBodyGyroJerk.meanE.X"     
## [29] "v162.tBodyGyroJerk.meanE.Y"      "v163.tBodyGyroJerk.meanE.Z"     
## [31] "v164.tBodyGyroJerk.stdE.X"       "v165.tBodyGyroJerk.stdE.Y"      
## [33] "v166.tBodyGyroJerk.stdE.Z"       "v201.tBodyAccMag.meanE"         
## [35] "v202.tBodyAccMag.stdE"           "v214.tGravityAccMag.meanE"      
## [37] "v215.tGravityAccMag.stdE"        "v227.tBodyAccJerkMag.meanE"     
## [39] "v228.tBodyAccJerkMag.stdE"       "v240.tBodyGyroMag.meanE"        
## [41] "v241.tBodyGyroMag.stdE"          "v253.tBodyGyroJerkMag.meanE"    
## [43] "v254.tBodyGyroJerkMag.stdE"      "v266.fBodyAcc.meanE.X"          
## [45] "v267.fBodyAcc.meanE.Y"           "v268.fBodyAcc.meanE.Z"          
## [47] "v269.fBodyAcc.stdE.X"            "v270.fBodyAcc.stdE.Y"           
## [49] "v271.fBodyAcc.stdE.Z"            "v345.fBodyAccJerk.meanE.X"      
## [51] "v346.fBodyAccJerk.meanE.Y"       "v347.fBodyAccJerk.meanE.Z"      
## [53] "v348.fBodyAccJerk.stdE.X"        "v349.fBodyAccJerk.stdE.Y"       
## [55] "v350.fBodyAccJerk.stdE.Z"        "v424.fBodyGyro.meanE.X"         
## [57] "v425.fBodyGyro.meanE.Y"          "v426.fBodyGyro.meanE.Z"         
## [59] "v427.fBodyGyro.stdE.X"           "v428.fBodyGyro.stdE.Y"          
## [61] "v429.fBodyGyro.stdE.Z"           "v503.fBodyAccMag.meanE"         
## [63] "v504.fBodyAccMag.stdE"           "v516.fBodyBodyAccJerkMag.meanE" 
## [65] "v517.fBodyBodyAccJerkMag.stdE"   "v529.fBodyBodyGyroMag.meanE"    
## [67] "v530.fBodyBodyGyroMag.stdE"      "v542.fBodyBodyGyroJerkMag.meanE"
## [69] "v543.fBodyBodyGyroJerkMag.stdE"
```


__5. Create a second, independent tidy dataset with the average of each
variable for each activity for each subject__ 


```r
splits <- split(MeanStdExtract, list(MeanStdExtract$subject.id,
                                     MeanStdExtract$activity))
length(splits)
```

```
## [1] 180
```

```r

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
```


Create descriptive stats to show various counts. Useful for double-checking.


```r
table(combined.data$source)
```

```
## 
##  test train 
##  2947  7352
```

```r
table(combined.data$subject.id)
```

```
## 
##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18 
## 347 302 341 317 302 325 308 281 288 294 316 320 327 323 328 366 368 364 
##  19  20  21  22  23  24  25  26  27  28  29  30 
## 360 354 408 321 372 381 409 392 376 382 344 383
```

```r
table(combined.data$activity)
```

```
## 
##    Lying      Sit    Stand     Walk WalkDown   WalkUp 
##     1944     1777     1906     1722     1406     1544
```


Number of records by activity by subject.id by source:

```r
table(combined.data$subject.id, combined.data$activity, combined.data$source)
```

```
## , ,  = test
## 
##     
##      Lying Sit Stand Walk WalkDown WalkUp
##   1      0   0     0    0        0      0
##   2     48  46    54   59       47     48
##   3      0   0     0    0        0      0
##   4     54  50    56   60       45     52
##   5      0   0     0    0        0      0
##   6      0   0     0    0        0      0
##   7      0   0     0    0        0      0
##   8      0   0     0    0        0      0
##   9     50  50    45   52       42     49
##   10    58  54    44   53       38     47
##   11     0   0     0    0        0      0
##   12    60  51    61   50       46     52
##   13    62  49    57   57       47     55
##   14     0   0     0    0        0      0
##   15     0   0     0    0        0      0
##   16     0   0     0    0        0      0
##   17     0   0     0    0        0      0
##   18    65  57    73   56       55     58
##   19     0   0     0    0        0      0
##   20    68  66    73   51       45     51
##   21     0   0     0    0        0      0
##   22     0   0     0    0        0      0
##   23     0   0     0    0        0      0
##   24    72  68    69   58       55     59
##   25     0   0     0    0        0      0
##   26     0   0     0    0        0      0
##   27     0   0     0    0        0      0
##   28     0   0     0    0        0      0
##   29     0   0     0    0        0      0
##   30     0   0     0    0        0      0
## 
## , ,  = train
## 
##     
##      Lying Sit Stand Walk WalkDown WalkUp
##   1     50  47    53   95       49     53
##   2      0   0     0    0        0      0
##   3     62  52    61   58       49     59
##   4      0   0     0    0        0      0
##   5     52  44    56   56       47     47
##   6     57  55    57   57       48     51
##   7     52  48    53   57       47     51
##   8     54  46    54   48       38     41
##   9      0   0     0    0        0      0
##   10     0   0     0    0        0      0
##   11    57  53    47   59       46     54
##   12     0   0     0    0        0      0
##   13     0   0     0    0        0      0
##   14    51  54    60   59       45     54
##   15    72  59    53   54       42     48
##   16    70  69    78   51       47     51
##   17    71  64    78   61       46     48
##   18     0   0     0    0        0      0
##   19    83  73    73   52       39     40
##   20     0   0     0    0        0      0
##   21    90  85    89   52       45     47
##   22    72  62    63   46       36     42
##   23    72  68    68   59       54     51
##   24     0   0     0    0        0      0
##   25    73  65    74   74       58     65
##   26    76  78    74   59       50     55
##   27    74  70    80   57       44     51
##   28    80  72    79   54       46     51
##   29    69  60    65   53       48     49
##   30    70  62    59   65       62     65
```


efg, 2014-02-02
