library(dplyr)

# Declare variables for every text files containing data
s.test.url <- "test/subject_test.txt"
x.test.url <- "test/x_test.txt"
y.test.url <- "test/y_test.txt"

s.train.url <- "train/subject_train.txt"
x.train.url <- "train/x_train.txt"
y.train.url <- "train/y_train.txt"

features.url <- "features.txt"
activity.url <- "activity_labels.txt"

# Function to read String as String by default
read.set <- function(dest){
  fread(dest,header = FALSE, stringsAsFactors = FALSE)
}

# Function that simplifies setnames
set.names <- function(cur.frame,name){
  setnames(cur.frame, old = names(cur.frame), new = name)
}

###########################################################
# Q.1 Merges the training and the test sets to create one data set.
subject.set <- rbind(read.set(s.train.url),read.set(s.test.url))
set.names(subject.set,"Subject_Test")

activity.set <- rbind(read.set(y.train.url),read.set(y.test.url))
set.names(activity.set,"Activity_Num")

activity.label <- read.set(activity.url)
set.names(activity.label,c("Activity_Num","Activity_Name"))
data.set <- rbind(data.table(read.table(x.train.url)),data.table(read.table(x.test.url)))
features <-   fread(features.url,header = FALSE, stringsAsFactors = FALSE)[, V2]
set.names(data.set,features)

full.set <-cbind(cbind(subject.set,activity.set),data.set)

##########################################################
# Q.2 Extracts only the measurements on the mean and standard deviation for each measurement. 

features.mean.std <- grepl("Subject_Test|Activity_Num|mean\\(\\)|std\\(\\)",names(full.set))
features.mean.std.full.set <- full.set[, features.mean.std,with = FALSE]

##########################################################
# Q.3 Uses descriptive activity names to name the activities in the data set

features.mean.std.full.labels.set<- merge(activity.label, features.mean.std.full.set , by="Activity_Num", all.x = TRUE)
features.mean.std.full.labels.set <- arrange(features.mean.std.full.labels.set,Subject_Test,Activity_Num)

##########################################################
# Q.4 Appropriately labels the data set with descriptive variable names. 

names(features.mean.std.full.labels.set) <- gsub("-X","-Axe_X",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("-Y","-Axe_Y",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("-Z","-AXE_Z",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("Acc",".Acceleration",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("Gyro",".Gyroscope",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("Mag",".Magnitude",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("^t","Time_Domain.",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("^f","Frequency_Domain.",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("-mean","-Mean",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("-std","-Standard_Deviation",names(features.mean.std.full.labels.set))
names(features.mean.std.full.labels.set) <- gsub("BodyBody","Body",names(features.mean.std.full.labels.set))

##########################################################
# Q.5 

mean.by.subject_text-Activity_Num = ddply(features.mean.std.full.labels.set, c("Subject_Test","Activity_Num"), numcolwise(mean))
write.table(mean.by.subject_text-Activity_Num, file = "sensor_avg_by_act_sub.txt")