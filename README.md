Peer Assessment Assignment
==========================

Coursera _Getting and Cleaning Data_ class           

Preliminaries
-------------
The purpose of the project was to collect, work with, and clean the dataset related to the project [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

The file **getdata\_projectfiles\_UCI HAR Dataset.zip** was downloaded from the class site and a directory structure with files were unzipped into the **UCI HAR Dataset** directory.

Files in the unzipped directory included a **README.txt** with general information, and two files describing the features in the data, **features.txt** and **features\_info.txt**.  An **activity\_labels.txt** file gave text descriptions for the six activities that were studied.

**test** and **train** subdirectories each had three files and an **Inertial Signals** directory.  As described in the README.txt file, The **subject_xxxx.txt** files defined the subject ids and the **y_xxxx.txt** files defined the activities for which measurements were made and recorded in the **X_xxx.txt** files.  The **xxxx** in these filenames was either **test** or **train**.

The raw data in the **Inertial** directories were not needed for the programming assignment.

First Look
----------

The script **0-First-Look.R** was used to experiment with and understand the data.  The script was re-written and became the script **run\_analysis.R**, which was submitted as part of the assignment.

The run\_analysis.R script
--------------------------

The following output from *knitr* shows many of the R statements in this script along with output to explain how it processed the data in BASEDIR from the unzipped file:



```r
BASEDIR <- "C:/2014/Getting-Cleaning-Data/UCI HAR Dataset"
setwd(BASEDIR)

Activity.Names <- c("Walk", "WalkUp", "WalkDown", "Sit", "Stand", "Lying")
```


The helper function `readGroup` reads the _train_ and _test_ files.

Note this comment below marks one of the assigned tasks:
__3. Use descriptive activity names to name the activities in the dataset__ 


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


`readGroup` combines data from three files for the `test` and `train` groups and returns a data.frame.  `readGroup` converts the activity indices to descriptive words using a simple subscript lookup as the data.frame is defined.


Loading the test and train data is as easy as two calls to `readGroup`:


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


Per instructions (__1. Merge the training and test sets to create one dataset__), the _train_ and _test_ data were merged into a single dataset:


```r
combined.data <- rbind(test.data, train.data)
dim(combined.data)
```

```
## [1] 10299   564
```

Let's cleanup feature names to be more R "friendly" (__4. Appropriately label the dataset with descriptive__ names):


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


The statements above replaced certain characters in the original feature names to make them more R friendly.  [This processing probably should have been performed first, and used in the `readGroup` function to name the columns in the "X" file.]


I could write the `combined.data` to disk if desired at this point.  The
instructions are ambiguous as to whether this is wanted.  Since the file is
huge (~64 MB), I'm electing NOT to create the file at this time.   

__2. Extract only the measurements on the mean and standard deviation ...__

Feature estimates for mean and standard deviation are represented in the
column names as meanE and stdE [since the () was replaced by "E" above].
In addition to the meanE and stdE columns, we also want columns 1:3, namely
source, subject.id and activity.

Adding additional columns to the subset would be easy through a modification of the `MeanStdExtrctColumns` object below.


```r
MeanStdExtractColumns <- c(1:3, grep("meanE|stdE", names(combined.data)))

MeanStdExtract <- combined.data[, MeanStdExtractColumns]
dim(MeanStdExtract)
```

```
## [1] 10299    69
```


__5. Create a second, independent tidy dataset with the average of each
variable for each activity for each subject__ 

The `split` function breaks the original data into a list by subject and activity.


```r
splits <- split(MeanStdExtract, list(MeanStdExtract$subject.id,
                                     MeanStdExtract$activity))
length(splits)
```

```
## [1] 180
```

```r

#  x <- splits[[1]]

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

dim(Tidy.Summary)
```

```
## [1] 180  68
```

```r

# This file is the one to be submitted as part of the assignment
write.csv(Tidy.Summary, "Samsung-Tidy-Summary.csv", row.names=FALSE)       
```


The `do.call` function above looks a bit complicated, but it's fairly easy to develop.  After the `splits` are defined, one of the splits can be assigned to object `x` (which is commented out above), and used to develop a function that returns a data.frame.  I find this approach more flexible than options offered by the `plyr` package.

The `Tidy.Summary` object is sorted by subject and activity to group all activities for the same subject together.

The following shows descriptive stats with various frequency counts, which are useful for double-checking and analysis.


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


Code Book
---------

The "X" files contained 561 features from analysis of the Samsung data for 30 subjects and 6 activities. The 30 subjects were divided into **test* and **train** subsets in the original data. 

The **combined.data** object contained three labels and these 561 features for all 30 subjects and 6 activities.

The three labels for the combined.data include:
* **source** = "test" or "train" to easily extract a subset from `combined.data`.
* **subject.id** = subject number from 1 to 30
* **activity** = "Walk", "WalkUp", "WalkDown", "Sit", "Stand", "Lying"

The 561 feature variables were given R friendly names.  These names all started with vNNN, where NNN = 001 to 561.  The NNN is an index to find the data in the "X" file, or to find the unmodified name in the original **features.txt** file.

The following is a list of of data columns in the **Tidy.Summary** object, which were written to the **Samsung-Tidy-Summary.txt** file.



```r
names(MeanStdExtract)[-1]
```

```
##  [1] "subject.id"                      "activity"                       
##  [3] "v001.tBodyAcc.meanE.X"           "v002.tBodyAcc.meanE.Y"          
##  [5] "v003.tBodyAcc.meanE.Z"           "v004.tBodyAcc.stdE.X"           
##  [7] "v005.tBodyAcc.stdE.Y"            "v006.tBodyAcc.stdE.Z"           
##  [9] "v041.tGravityAcc.meanE.X"        "v042.tGravityAcc.meanE.Y"       
## [11] "v043.tGravityAcc.meanE.Z"        "v044.tGravityAcc.stdE.X"        
## [13] "v045.tGravityAcc.stdE.Y"         "v046.tGravityAcc.stdE.Z"        
## [15] "v081.tBodyAccJerk.meanE.X"       "v082.tBodyAccJerk.meanE.Y"      
## [17] "v083.tBodyAccJerk.meanE.Z"       "v084.tBodyAccJerk.stdE.X"       
## [19] "v085.tBodyAccJerk.stdE.Y"        "v086.tBodyAccJerk.stdE.Z"       
## [21] "v121.tBodyGyro.meanE.X"          "v122.tBodyGyro.meanE.Y"         
## [23] "v123.tBodyGyro.meanE.Z"          "v124.tBodyGyro.stdE.X"          
## [25] "v125.tBodyGyro.stdE.Y"           "v126.tBodyGyro.stdE.Z"          
## [27] "v161.tBodyGyroJerk.meanE.X"      "v162.tBodyGyroJerk.meanE.Y"     
## [29] "v163.tBodyGyroJerk.meanE.Z"      "v164.tBodyGyroJerk.stdE.X"      
## [31] "v165.tBodyGyroJerk.stdE.Y"       "v166.tBodyGyroJerk.stdE.Z"      
## [33] "v201.tBodyAccMag.meanE"          "v202.tBodyAccMag.stdE"          
## [35] "v214.tGravityAccMag.meanE"       "v215.tGravityAccMag.stdE"       
## [37] "v227.tBodyAccJerkMag.meanE"      "v228.tBodyAccJerkMag.stdE"      
## [39] "v240.tBodyGyroMag.meanE"         "v241.tBodyGyroMag.stdE"         
## [41] "v253.tBodyGyroJerkMag.meanE"     "v254.tBodyGyroJerkMag.stdE"     
## [43] "v266.fBodyAcc.meanE.X"           "v267.fBodyAcc.meanE.Y"          
## [45] "v268.fBodyAcc.meanE.Z"           "v269.fBodyAcc.stdE.X"           
## [47] "v270.fBodyAcc.stdE.Y"            "v271.fBodyAcc.stdE.Z"           
## [49] "v345.fBodyAccJerk.meanE.X"       "v346.fBodyAccJerk.meanE.Y"      
## [51] "v347.fBodyAccJerk.meanE.Z"       "v348.fBodyAccJerk.stdE.X"       
## [53] "v349.fBodyAccJerk.stdE.Y"        "v350.fBodyAccJerk.stdE.Z"       
## [55] "v424.fBodyGyro.meanE.X"          "v425.fBodyGyro.meanE.Y"         
## [57] "v426.fBodyGyro.meanE.Z"          "v427.fBodyGyro.stdE.X"          
## [59] "v428.fBodyGyro.stdE.Y"           "v429.fBodyGyro.stdE.Z"          
## [61] "v503.fBodyAccMag.meanE"          "v504.fBodyAccMag.stdE"          
## [63] "v516.fBodyBodyAccJerkMag.meanE"  "v517.fBodyBodyAccJerkMag.stdE"  
## [65] "v529.fBodyBodyGyroMag.meanE"     "v530.fBodyBodyGyroMag.stdE"     
## [67] "v542.fBodyBodyGyroJerkMag.meanE" "v543.fBodyBodyGyroJerkMag.stdE"
```


Here's how to interpret the "v" feature variables in this subset:

**vnnn.Signal.Stat[.Dimension]**

**Vnnn** = the nnn index keeps a linkage to the original data and documenation.  The "v" prefix for "variable" was used to make the names R "friendly".

**Signal** = 

  tBodyAcc-XYZ
  tGravityAcc-XYZ
  tBodyAccJerk-XYZ
  tBodyGyro-XYZ
  tBodyGyroJerk-XYZ
  tBodyAccMag
  tGravityAccMag
  tBodyAccJerkMag
  tBodyGyroMag
  tBodyGyroJerkMag
  fBodyAcc-XYZ
  fBodyAccJerk-XYZ
  fBodyGyro-XYZ
  fBodyAccMag
  fBodyAccJerkMag
  fBodyGyroMag
  fBodyGyroJerkMag   
  

The "t" or "f" prefix indicates whether the variable is part of a time series, or frequency data derived from Fourier analysis.

The "-XYZ" part of the name indicates which dimensions, X, Y and Z have separate variables.  Some quantities do not have dimensions.

See the file **features\_info.txt** for more information about the computed features.  [Sadly, the documentation does not provide sufficient information to reproduce the computed values from the raw data.]

**Stat** = meanE | stdE

Per the homework assignment, only the mean (mean) and standard deviation (std) estimated (E) values were included in the **Tidy.Summary** object.



efg, 2014-02-03
