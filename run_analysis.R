run_analysis <- function(){
        
        ## reads the various data files as long as the folder is in the working directory
        testY <- read.table("./UCI Har Dataset/test/y_test.txt")
        testX <- read.table("./UCI Har Dataset/test/X_test.txt")
        testSubject <- read.table("./UCI Har Dataset/test/subject_test.txt")
        trainY <- read.table("./UCI Har Dataset/train/y_train.txt")
        trainX <- read.table("./UCI Har Dataset/train/X_train.txt")
        trainSubject <- read.table("./UCI Har Dataset/train/subject_train.txt")
        features <- read.table("./UCI Har Dataset/features.txt")
        aLabel <- read.table("./UCI Har Dataset/activity_labels.txt")
        
        
        ##merges the test and training sets to create one data set
        test <- cbind(testY, testSubject, testX)
        train <- cbind(trainY, trainSubject, trainX)
        total <- rbind(test, train)
        
        ##gives names to each column based on the names in the features text file
        colnames(total) <- c("activity", "subject", as.character(features[,2]))
        
        ##determine which columns contain the means and std and sort them in order of lowest to highest
        meanCol <- grep(".[mM]ean.", colnames(total))
        stdCol <- grep(".std.", colnames(total))
        relevant <- c(meanCol, stdCol)
        relevant <- sort(relevant)
        
        ##Seperates out the columns containing only means and std
        final <- total[,c(1,2,relevant)]
        
        ##Replacing the activity names with descriptive names
        final$activity <- sub("^1$", "WALKING", final$activity)
        final$activity <- sub("^2$", "WALKINGupstairs", final$activity)
        final$activity <- sub("^3$", "WALKINGdownstairs", final$activity)
        final$activity <- sub("^4$", "SITTING", final$activity)
        final$activity <- sub("^5$", "STANDING", final$activity)
        final$activity <- sub("^6$", "LAYING", final$activity)
        
        ##Sorting the data frame so all the same subjects are grouped together, then all the same activites are grouped together
        final <- final[order(final$subject,final$activity),]
        
        ##Splitting the data frame based on the subject number
        s <- split(final, final$subject)
        ##Split each subject's data based on the activity name
        ss <- sapply(s, function(x) split(x,x$activity))
        
        ##Finding the mean of each variable for each activity and each subject
        average <- sapply(ss, function(x) colMeans(x[,3:ncol(x)]))
        average <- t(average)
        
        ##Making some labels for the activities and subject number
        label <- sort(as.character(aLabel$V2))
        labels <- rep(label,30)
        subject <- sort(rep(1:30,6))
        
        ##Creating the final table where first column is each test subject's number,
        ##second column is the name of the activity and the following columns are the means
        submit <- cbind(subject, labels, average)
        
        ##Writes the final table as a text file into the working directory
        write.table(submit, file = "submit.txt", row.names = FALSE)
        
}
