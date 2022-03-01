#Gregory Bruich, Ph.D.
#Economics 50
#Harvard University
#Send suggestions and corrections to gbruich@fas.harvard.edu

#Lab 4: The Tennessee STAR Experiment

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

## load packages
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(stargazer)) install.packages("stargazer"); library("stargazer")

#Methods/concepts: treatment effect estimation in stratified experiments, bar graphs,
#multivariable regression, statistical inference, statistical vs. practical significance

#LAB DESCRIPTION 

#The Tennessee Student/Teacher Achievement Ratio (STAR) Experiment was implemented in 1985-
#  1986 in 79 schools, involving more than 11,600 students.  Both students and teachers were 
#randomly assigned to small and regular size classes starting in kindergarten.  In this lab, you will 
#measure the causal effect of class size on student achievement in kindergarten, as measured by 
#year-end test scores for N=5,710 kindergarten children.  For more details on the variables included in 
#these data, see Table 1.  A list and description of each of the Stata and R commands needed for this 
#lab are contained in Table 2 and Table 3, respectively.  For more background on the experiment, see 
#Krueger (1999) or Chetty et al. (2011).  

#QUESTIONS

#1.	In the Tennessee STAR Experiment, both students and teachers were randomly assigned to 
#small and large classes.  Explain briefly why it is important to randomly assign not just 
#students but also teachers in order to determine the causal effect of class size.

#2.	Using the star.dta file, how does average class size (class_size) compare in small kindergarten 
#classes vs. regular kindergarten classes (small == 1 vs. small == 0)?

df <- read_dta("star.dta")
class_size_small <- with(df, mean(class_size[small == 1]))
class_size_regular <- with(df, mean(class_size[small == 0]))

#3.	At the end of kindergarten school year, students took four Stanford Achievement Tests: Math-
#SAT math, Reading-SAT read, Word-SAT wordskill, and Listening-SAT listen.  It is common in 
#education research to convert test scores into more meaningful units.  One way is to generate 
#a new variable sat_index that combines the exam scores into one overall metric measured in 
#"standard deviation units" (or \sigma's in the lingo of education researchers) as follows:   

#a.	For each of the four exam scores, subtract the control group mean and divide by the 
#control group standard deviation to define four "standardized" exam scores. Some 
#pseudo code is: standardized math score = (math score - control_mean(math score)) 
#divided by control_sd(math score), where control_mean(math score) and control_sd(math 
#score) are calculated for observations with small == 0.

df$std_math <- (df$math - (with(df, mean(math[small == 0])))) /(with(df, sd(math[small == 0])))
df$std_read <- (df$read - (with(df, mean(read[small == 0])))) /(with(df, sd(read[small == 0])))
df$std_wordskill <- (df$wordskill - (with(df, mean(wordskill[small == 0])))) /(with(df, sd(wordskill[small == 0])))
df$std_listen <- (df$listen - (with(df, mean(listen[small == 0])))) /(with(df, sd(listen[small == 0])))

#b.	Then generate sat_index as the average of these four standardized exam scores.  Some 
#pseudo code is:  sat_index = mean(standardized math score, standardized reading 
#score, standardized word score, standardized listening score)

df$sat_index <- (df$std_math + df$std_read + df$std_wordskill + df$std_listen) / 4

#c.	Plot a histogram of sat_index for small kindergarten classes (small == 1) and for 
#regular kindergarten classes (small == 0).  What do you notice in the histograms?

g1 <- ggplot(df, aes(x=sat_index, 
                 fill=factor(small, labels=c("Regular class", "Small class")), 
                 y=..density..)) +
  geom_histogram(alpha=.5, position="identity", bins = 40) +
  labs(x = "Average of four standardized exam scores", fill = "Class Size")

#4.	Returning to question 1, we will assess whether the data are consistent with teachers having 
#been randomly assigned to classrooms by testing for balance of teacher characteristics.  The 
#STAR experiment consisted of 325 teachers, but there are 5,710 students in these data.  We 
#will conduct this and all of our subsequent analyses in this lab at the teacher-level, rather 
#than at the student-level.  

#a.	Aggregate the data by teacher_id, so that you end up with a 325 observation data set 
#with information on small, school_id, teacher_id, teacher_masters, teacher_white, 
#teacher_black, teacher_experience as well as the mean of sat_index across all the 
#students in the teacher's class (which we'll use in question 5).

by_teacher <- df %>% group_by(teacher_id, small, school_id, teacher_masters, teacher_white, teacher_black, teacher_experience)  %>% select(small, school_id, teacher_id, teacher_masters, teacher_white, teacher_black, teacher_experience, sat_index) 
                     
classes <- by_teacher %>% summarise(sat_index = mean(sat_index)) 
                            
#b.	Estimate a linear regression (lm in R or regress in stata) of teacher_experience on an 
#intercept and small.  Use the estimated coefficient on small to report the difference in 
#average teacher experience in small vs. large classes.  Calculate a 95% confidence 
#interval for this difference: Estimated Difference \pm 1.96 \times standard error.

teacher_experience_small <- lm(teacher_experience ~ small, data = df)

#c.	Repeat question b for teacher_masters, teacher_white, and teacher_black.

teacher_experience_masters <- lm(teacher_experience ~ teacher_masters, data = df)
teacher_experience_white <- lm(teacher_experience ~ teacher_white, data = df)
teacher_experience_black <- lm(teacher_experience ~ teacher_black, data = df)

#d.	Are the differences in teacher characteristics in small vs. large classes statistically 
#significantly different from zero?  Are they practically significant?  What do you 
#conclude about whether the random assignment was successful in balancing teacher 
#characteristics?

#5.	The STAR experiment was a stratified randomized experiment, also known as a randomized 
#block experiment, because students were randomly assigned to classes at their own school.  
#The strata were therefore the school.  Intuitively, students could only be randomly assigned 
#to a class at their school and not for example a school across town.  The practical implication 
#is that it was as-if each of the 79 schools conducted their own separate experiment.  

#The most standard approach to obtain one overall estimate is to modify the regressions we 
#ran in Lab 3 by adding indicator variables for each school as additional control variables.  This 
#is now a multivariable regression.  Recall that we only care about the regression coefficient 
#on the variable small, and can safely ignore the 79 other estimated coefficients. 

#a.	Using the teacher-level data with 325 observations, run a multivariable regression of 
#sat_index on the small class indicator small, controlling for school fixed effects (e.g., 
#regress with i.school_id in Stata; or lm with factor(school_id) in R).  

teacher_experience_small_factor <- lm(sat_index ~ small + factor(school_id), data = df)

#b.	Use the estimated coefficient on the small class indicator small to report your estimate 
#of the causal effect of class size from this regression.   Calculate a 95% confidence 
#interval for this difference: Estimated Difference \pm 1.96 \times standard error.

#c.	Visualize the estimated treatment effect using a bar graph, with one bar representing 
#the control group and a second bar representing the treatment group.  The height of 
#the bar for the control group should equal the control group mean of sat_index.  The 
#height of the bar for the treatment group should equal the sum of the control group 
#mean and regression coefficient on small from the regression in part a.  Add square 
#brackets to the treatment group bar to visualize the 95% confidence interval from part b.

mean_sat_index <- with(df, mean(sat_index[small == 0]))
treatment_group_average <- mean_sat_index + teacher_experience_small_factor$coefficients[2]

data_for_graph <- data.frame(c(mean_sat_index, treatment_group_average), 
                             c(NA, 0.023), 
                             c("Control group", "Treatment group"))  
data_for_graph

# Change name of 1st column of data_for_graph to "Moved"
names(data_for_graph)[1] <- "Experience"

# Change name of 2nd column of data_for_graph to "se"
names(data_for_graph)[2] <- "se"

# Change name of 2nd column of data_for_graph to "Group"
names(data_for_graph)[3] <- "Group"

#Add upper bound on 95% CI
data_for_graph$ub <- data_for_graph$Experience + 1.96*data_for_graph$se

#Add lower bound on 95% CI
data_for_graph$lb <- data_for_graph$Experience - 1.96*data_for_graph$se

#Look at the data frame we have created
data_for_graph

# Bar graph displaying results
g2 <- ggplot(data=data_for_graph, aes(x=Group, y=Experience, fill=Group)) +
  geom_bar(stat="identity", show.legend = FALSE, width=.6) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.1, size = 0.7, color="black") +
  scale_fill_manual(values=c("red", "blue")) +
  labs(y = "Average of four standardized exam scores\n")

#6.	For this Lab, please submit the following:
#a.	Your do-file or .R script file to Gradescope
#b.	A single PDF document with the answers and graphs submitted to Gradescope.  
#c.	There will also be a Google form that is projected to the screen in Lab
