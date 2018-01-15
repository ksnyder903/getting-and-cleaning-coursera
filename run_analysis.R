
##This script completes the final assignment in the Coursera course, Getting and Cleaning Data

##The script is intended to do each of these steps:

##1 Merges the training and the test sets to create one data set.
##2 Extracts only the measurements on the mean and standard deviation for each measurement.
##3. Uses descriptive activity names to name the activities in the data set

##4. Appropriately labels the data set with descriptive variable names.
##5. From the data set in step 4, creates a second, independent tidy data set 
    #with the average of each variable for each activity and each subject.

#Note that the script addresses each step in order, 
  #HOWEVER steps 3 and 4 are actually completed within the code for Step 1.
  #The Step 3 and Step 4 code proves these steps have already been completed. 
  #Additionally, there is notation within Step 1 to call out where these steps have been completed.



#bring in the datasets
        
        x_test <- read.table("./Data/X_test.txt")
        y_test <- read.table("./Data/y_test.txt")
        
        x_train <- read.table("./Data/X_train.txt")
        y_train <- read.table("./Data/y_train.txt")
        
        act_lab <- read.table("./Data/activity_labels.txt")
        
        feat <- read.table("./Data/features.txt")
        
        sub_test  <- read.table("./Data/subject_test.txt")
        
        sub_train  <- read.table("./Data/subject_train.txt")
        

 ##Step 1 Merges the training and the test sets to create one data set.
        
        library(dplyr)
        
        ##because we want to use the ID function when bind_row'ing, we need to fix the colnames of the x tables prior to that operation
        ##based on observations of the sizes of the data frames and from the README, feat is the colnames for x_test and x_train
        
        
        #note this step here also covers Step 4 in the assignment (Appropriately labels the data set with descriptive variable names.)
        names(x_test) <- feat$V2
        names(x_train) <- feat$V2
        
        
        #now let's go ahead and join the test and training sets together for x, y, and sub
        x_all <- bind_rows("test"=x_test, "train"=x_train, .id="ds" )
        y_all <- bind_rows("test"=y_test, "train"=y_train, .id="ds" )
        sub_all <- bind_rows("test"=sub_test, "train"=sub_train, .id="ds" )
        
        rm(x_test, x_train, y_test, y_train, sub_test, sub_train) #clean up our enviornment to only the unioned data sets
        
        
        
        #now we clip together the x and y tables
        
        all <- bind_cols(sub_all, y_all, x_all)
        
        head(all)
        
        #let's do some columning renaming for the columns that came from subject and y
        
        all_rn <- rename(all, subject=V1, activity=V11)
        
        rm(all)
        
        #eliminate duplicate columns
        
        all_pd <- select(all_rn, -(c(ds1, ds2)))
        
        head(all_pd)
        
        #last step here is to add the activity names
        #act_lab joins to our all_pd dataset on the V1 variable.
        #but let's rename act_lab to make things a little easier on us
        
        act_lab_rn <- rename(act_lab, activity=V1, activity_name=V2)
        
        #now we join act_lab_rn to all_pd
        
        complete <- left_join(act_lab_rn, all_pd, by="activity") #note this step here also covers #3 in the assignment (Uses descriptive                                                                      activity names to name the activities in the data set)
        
        
        
        ##and now we have one complete data set!
        
        #quick clean up of unneeded data sets
        rm(act_lab, all_rn)
  
        
##2 Extracts only the measurements on the mean and standard deviation for each measurement.
        
        ##first let's create a list of all the feature names that are mean or std
        
        ##i've used regex here to isolate those features that are mean() or std() and ignoring those like meanFreq since those aren't clearly means
        
        headers <- grep("mean\\(\\)|std\\(\\)", names(complete), value=TRUE)
        
        #now we'll subset the data set. we still want to include our descriptive variables, so we select those in addition to the mean/std variables
        complete_m_sd <- complete[, c("activity", "activity_name", "ds", "subject", headers)]
        
        
##3. Uses descriptive activity names to name the activities in the data set
        
        ##I've already covered number 3 when I was combining the data sets. 
        
        #here you can see the first five rows of the data set and the first two columns, activity (numeric) and activity name (the descrptive activity name)
        complete_m_sd[1:5, c("activity", "activity_name")]


##4. Appropriately labels the data set with descriptive variable names.
        
        ##I've already covered number 4 when I was combining the data sets. 
        
        #here you can see the first five rows of the data set, and can see the original feature names (formerly V1, V2, etc) have been renamed with the according measures
        
        head(complete_m_sd)   
        
        
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        
        library(dplyr)
        
        #clean up the unecessary fields for this step
        complete_final <- select(complete_m_sd, -c(activity, ds))
        
        
        #create the summary table that is grouped by activity name and subject, with averages (means) for all columns
        tidy2 <- complete_final %>% group_by(activity_name,subject) %>% summarize_all(funs(mean))
        
        
#write the table to a file
        
        write.table(tidy2, "coursera_gettingandcleaning_finalassignment_tidydata.txt", row.name=FALSE)