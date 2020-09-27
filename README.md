# Getting and Cleaning Data Course Project: Peer-graded assignment
#### Max Serna
#### 27/09/2020

<p align="justify">

In this repository I will submit my work on the Coursera's Getting and Cleaning Data Course Project.
The instructions for it say as following:

## Course instructions.

### Review criteria
1. The submitted data set is tidy.
2. The Github repo contains the required scripts.
3. GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
4. The README that explains the analysis files is clear and understandable.
5. The work submitted for this project is the work of the student who submitted it.

### Getting and Cleaning Data Course Project
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

{http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones}

Here are the data for the project:

{https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip}

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

**Good luck!**

## Files in this repository

There are a few files which may not be of interest for the reader, such as `docs` directory or `DataCourseProject.Rproj`. These consist of files that were autogenerated when opening a new repo or project in RStudio, and hence they can ge ignored.  
The following are core files important for the completion of the Coursera´s assignment.

* *run_analysis.R*

This is the script to run the instructions. A description for it´s content can be found in the Codebook file. Mainly, the script
conducts all the necessary code to complete the assignment, and returns the text file that is afterwards submitted to Coursera.

* *Codebook.Rmd*

This is the R Markdown file that contains the CodeBook for this project. I recommend the reader to visit (https://maxserna.github.io/DataCourseProject/) to have the complete view. I describe all the data and the code
used to complete the assignment. Refer to it for any doubt about the script. The `Rmd` script in which the last `html` page relies is 
named `CodeBook.Rmd`, which is located in here too.
Alternatively, a PDF file can be found in this repository, as well as the R Markdown script that was used to construct it (see `CodeBookPDF.Rmd`). There are only a few differences between the two `Rmd` scripts that do not alter the code but just the display format.

</p>