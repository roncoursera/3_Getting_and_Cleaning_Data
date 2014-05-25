# require(RCurl) # only required for download
require(reshape2) # melt, dcast

# http://stackoverflow.com/questions/8986818/automate-zip-file-reading-in-r
read.zip <- function(zipfile, row.names=NULL, dec=".") {
    # Create a name for the dir where we'll unzip
    zipdir <- tempfile()
    # Create the dir using that name
    dir.create(zipdir)
    # Unzip the file into the dir
    unzip(zipfile, exdir=zipdir)
    # Get the files into the dir
    files <- list.files(zipdir)
    # Throw an error if there's more than one
    if(length(files)>1) stop("More than one data file inside zip")
    # Get the full name of the file
    file <- paste(zipdir, files[1], sep="/")
    # Read the file
    read.csv(file, row.names, dec)
}

# =============================================================================
# Retrieve Data
# =============================================================================
# get data if data does not exist
# url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# datweb <- getBinaryURL(url,ssl.verifypeer=FALSE)
# temp <- file("data.zip",open="wb")
# writeBin(datweb, temp)
# close(temp)

# permission denied problem
# is that the f*n Win 7 attrib problem again?!?!?
# dat = read.zip("data.zip")

# assume data is in your working directory
# dat = read.table("data.csv")

# =============================================================================
# Read Data
# =============================================================================
datadir="UCI HAR Dataset"
subjectfiles = c("train/subject_train.txt","test/subject_test.txt")
classfiles = c("train/y_train.txt","test/y_test.txt")
classlabelfile = "activity_labels.txt"
datafiles = c("train/X_train.txt","test/X_test.txt")
datalabelfile = "features.txt"

# read subject label files (
subj1 = read.table(paste(datadir,subjectfiles[1],sep="/"))
subj2 = read.table(paste(datadir,subjectfiles[2],sep="/"))

# read activity label files (iow, the activity classification)
act1 = read.table(paste(datadir,classfiles[1],sep="/"))
act2 = read.table(paste(datadir,classfiles[2],sep="/"))

# read data files
dat1 = read.table(paste(datadir,datafiles[1],sep="/"))
dat2 = read.table(paste(datadir,datafiles[2],sep="/"))

# should have same number of columns
dim(dat1) # 7352  561
dim(dat2) # 2947  561

# classification labels
classlab = read.table(paste(datadir,classlabelfile,sep="/"))

# data labels
datlab = read.table(paste(datadir,datalabelfile,sep="/"))

# =============================================================================
# Process Data
# =============================================================================

# -----------------------------------------------------------------------------
# clean up column names to better match Google R naming conventions
# http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#identifiers
# -----------------------------------------------------------------------------
# extract column names (datlab) into a char vector
cnames = datlab$V2

# remove parenthesis ()
cnames=gsub("\\(\\)","",cnames)

# remove parenthesis with info (somthing)
cnames=gsub("\\(",".",cnames)
cnames=gsub("\\)",".",cnames)

# remove commas, replace with '.'
cnames=gsub("\\,",".",cnames)

# remove hyphens, replace with '.'
cnames=gsub("\\-",".",cnames)

# remove underscores, replace with '.'
cnames=gsub("\\-",".",cnames)

# remove double dots, replace with '.'
cnames=gsub("\\.\\.",".",cnames)

# -----------------------------------------------------------------------------
# 1. Merges the training and the test sets to create one data set.
# -----------------------------------------------------------------------------
dat = rbind(dat1,dat2)
dim(dat) #[1] 10299   561
rm(dat1)
rm(dat2)

subj = rbind(subj1,subj2)
dim(subj) #[1] 10299   1
rm(subj1)
rm(subj2)

act = rbind(act1,act2)
dim(act) #[1] 10299   1
rm(act1)
rm(act2)

# -----------------------------------------------------------------------------
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# -----------------------------------------------------------------------------
# do we need to remove meanFreq? I assume so below
colidx = grepl("\\.mean\\.", cnames) | grepl("\\.mean$", cnames) | grepl("\\.std",cnames)
paramdat = dat[,colidx] # extract just mean and std parametric data
dim(paramdat) # [1] [1] 10299    66
rm(dat)

paramlab = cnames[colidx] # colnames for this data set
length(paramlab) # [1] 66

# -----------------------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set
# -----------------------------------------------------------------------------
actnames=as.vector(classlab$V2)
getActNames <- function(x) {
    actnames[x]
}
actlabels = lapply(act, getActNames) # returns a list of activities from list of action codes

# -----------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive activity names.
# (added action numerics and subject labeling at this point to support step 5)
# -----------------------------------------------------------------------------
namedData = cbind(actlabels, subj, paramdat)
colnames(namedData) = c("activity", "subject", paramlab)

# -----------------------------------------------------------------------------
# 5. Creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
# -----------------------------------------------------------------------------
groupData <-melt(namedData, id=c("subject", "activity"))
meanData <- dcast(groupData, subject + activity ~ variable, fun.aggregate=mean)
dim(meanData) # 180  68

# meanData[1:10,1:4]
#    subject           activity tBodyAcc.mean.X tBodyAcc.mean.Y
# 1        1             LAYING       0.2215982    -0.040513953
# 2        1            SITTING       0.2612376    -0.001308288
# 3        1           STANDING       0.2789176    -0.016137590
# 4        1            WALKING       0.2773308    -0.017383819
# 5        1 WALKING_DOWNSTAIRS       0.2891883    -0.009918505
# 6        1   WALKING_UPSTAIRS       0.2554617    -0.023953149
# 7        2             LAYING       0.2813734    -0.018158740
# 8        2            SITTING       0.2770874    -0.015687994
# 9        2           STANDING       0.2779115    -0.018420827
# 10       2            WALKING       0.2764266    -0.018594920

# =============================================================================
# Save the Data, tab-delimited, with column headers
# =============================================================================
write.table(meanData, file="tidydata.txt", quote=FALSE, sep="\t",
    row.names=FALSE, col.names=TRUE)

