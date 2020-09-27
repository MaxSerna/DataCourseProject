library(dplyr)
library(mgsub)
rm(list = ls())

# We will download the data directly from the url. All the data will be
# available in the repo nonetheless
temp <- tempfile()
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
              temp)
features <- read.table(unz(temp,
                           'UCI HAR Dataset/features.txt'),
                       col.names = c('n', 'functions'))
activities <- read.table(unz(temp,
                             'UCI HAR Dataset/activity_labels.txt'),
                         col.names = c('code', 'activity'))
subject_test <- read.table(unz(temp,
                               "UCI HAR Dataset/test/subject_test.txt"),
                           col.names = "subject")
x_test <- read.table(unz(temp,
                         "UCI HAR Dataset/test/X_test.txt"),
                     col.names = features$functions)
y_test <- read.table(unz(temp,
                         "UCI HAR Dataset/test/y_test.txt"),
                     col.names = "code")
subject_train <- read.table(unz(temp,
                                "UCI HAR Dataset/train/subject_train.txt"),
                            col.names = "subject")
x_train <- read.table(unz(temp,
                          "UCI HAR Dataset/train/X_train.txt"),
                      col.names = features$functions)
y_train <- read.table(unz(temp,
                          "UCI HAR Dataset/train/y_train.txt"),
                      col.names = "code")
unlink(temp)
test <- cbind(subject_test,
              y_test,
              x_test)
training <- cbind(subject_train,
                  y_train,
                  x_train)
raw <- rbind(training,
              test)
step2 <- raw %>% 
  select(subject,
         code,
         contains(c('mean',
                    'std')
         )
  )
step3 <- step2
step3$code <- activities[step3$code, 2]
tidy <- step3; rm(step3)
colnames(tidy)[1:2] <- c('Subject', 'Activities')
colnames(tidy) <- colnames(tidy) %>% 
  mgsub(pattern = c('Acc', 'Gyro', 'BodyBody',
                    'Mag', '^t', '^f',
                    'tBody', 'mean', 'std',
                    'freq', 'angle', 'gravity'),
        replacement = c('Accelerometer', 'Gyroscope', 'Body',
                        'Magnitude', 'Time', 'Frequency',
                        'TimeBody', 'Mean', 'STD',
                        'Frequency', 'Angle', 'Gravity'),
        ignore.case = TRUE
  )
tidy_means <- tidy %>% 
  group_by(Subject,
           Activities) %>% 
  summarise_all('mean')
write.table(tidy_means,
            file = 'C:/Users/max_s/Desktop/EconomíaUDG/Varios/Coursera/G.and.C.Data/Week4/tidy_means.txt',
            row.names = FALSE)
rm(list = setdiff(x = ls(),
                  y = c("raw",
                        "tidy_means",
                        "tidy"))
)

