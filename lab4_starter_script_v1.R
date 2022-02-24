#Gregory Bruich, Ph.D.
#Economics 50
#Harvard University
#Send suggestions and corrections to gbruich@fas.harvard.edu

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

## load packages
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

#Opportunity Insights style bar graphs
#Example Bar graph with two bars: height 0.01 and 0.4
#Add standard error for second bar: 0.05
data_for_graph <- data.frame(c(0.01, .4), 
                             c(NA, 0.05), 
                             c("Control group", "Treatment group"))  
data_for_graph

# Change name of 1st column of data_for_graph to "Moved"
names(data_for_graph)[1] <- "Moved"

# Change name of 2nd column of data_for_graph to "se"
names(data_for_graph)[2] <- "se"

# Change name of 2nd column of data_for_graph to "Group"
names(data_for_graph)[3] <- "Group"

#Add upper bound on 95% CI
data_for_graph$ub <- data_for_graph$Moved + 1.96*data_for_graph$se

#Add lower bound on 95% CI
data_for_graph$lb <- data_for_graph$Moved - 1.96*data_for_graph$se

#Look at the data frame we have created
data_for_graph

# Bar graph displaying results
ggplot(data=data_for_graph, aes(x=Group, y=Moved, fill=Group)) +
  geom_bar(stat="identity", show.legend = FALSE, width=.6) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.1, size = 0.7, color="black") +
  scale_fill_manual(values=c("red", "blue")) +
  labs(y = "Moved Using Experimental Voucher")

ggsave("fig1_test.png")

