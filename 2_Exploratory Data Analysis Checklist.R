### Exploratory Data Analysis with R / Roger D. Peng // Book
### 2. Exploratory Data Analysis Checklist
### page: 17/24

# 1. Formulate your question
# 2. Read in your data
# 3. Check the packaging
# 4. Run str()
# 5. Look at the top and the bottom of your data
# 6. Check your “n”s
# 7. Validate with at least one external data source
# 8. Try the easy solution first
# 9. Challenge your solution
# 10. Follow up

# set working directory 
setwd("D:/PROJECT/R/data_exploratory_dar")

list.files()

#####
# install.packages("dplyr")
# install.packages("readr")
#####
library("dplyr")
library("ggplot2")
library(readr)


#####
# 1. Formulate your question
#####
# - Are air pollution levels higher on the east coast than on the west coast?
# But a more specific question might be
# - Are hourly ozone levels on average higher in New York City than they are in
# Los Angeles?
# - Which counties in the United States have the highest levels of ambient ozone
# pollution?
#####
# 2. Read in your data
#####
# The data are available from the EPA’s Air Quality System web page 
#  I’ve simply downloaded the zip file from the web site, unzipped the
# archive, and put the resulting file in a directory called “data”
# http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html#Raw

# The character string provided to the col_types argument 
# specifies the class of each column in the dataset. 
# Each letter represents the class of a column: 
#“c” for character, 
#“n” for numeric”,
#“i” for integer

ozone <- read_csv("data/hourly_44201_2015.csv", col_types = "ccccinnccccccncnnccccccc")
head(ozone)

names(ozone) <- make.names(names(ozone))

#####
# 3. Check the packaging
#####
nrow(ozone)
ncol(ozone)

#####
# 4. Run str()
#####
# you can examine the classes of each of the columns 
# to make sure they are correctly specified 
#(i.e. numbers are numeric and strings are character, etc.).
str(ozone)

#####
# 5. Look at the top and the bottom of your data
#####
head(ozone[, c(6:7, 10)], 10)

tail(ozone[, c(6:7, 10)])

ozone[10000:10005, c(6:7, 10)]

#####
# 6. Check your “n”s
#####

table(ozone$Time.Local)

# We can take a look at which observations were measured at time “00:01”

2<3


filter(ozone, Time.Local == "07:00") %>%
        select(State.Name, County.Name, Date.Local,
               Time.Local, Sample.Measurement)

filter(ozone, State.Name == "Alabama" 
       & County.Name == "Baldwin" 
       & Date.Local == "2015-03-01") %>%
        select(Date.Local, Time.Local, Sample.Measurement) %>%
        as.data.frame

select(ozone, State.Name) %>% unique %>% nrow

unique(ozone$State.Name)

#####
# 7. Validate with at least one external data source
#####

summary(ozone$Sample.Measurement)

# deciles of the data
quantile(ozone$Sample.Measurement, seq(0, 1, 0.1))

# Knowing that the national standard for ozone is something like 0.075, 
# we can see from the data that
# The data are at least of the right order of magnitude (i.e. the units are correct)
# • The range of the distribution is roughly what we’d expect, given the regulation
# around ambient pollution levels
# • Somehourlylevels(lessthan10%)areabove0.075butthismaybereasonablegiven
# the wording of the standard and the averaging involved.

#####
# 8. Try the easy solution first
#####
# Which counties in the United States have the highest levels 
# of ambient ozone pollution?
ranking <- group_by(ozone, State.Name, County.Name) %>%
        summarize(ozone = mean(Sample.Measurement)) %>%
        as.data.frame %>%
        arrange(desc(ozone))

head(ranking, 10)

tail(ranking, 10)

filter(ozone, State.Name == "California" & County.Name == "Mariposa") %>% nrow

ozone <- mutate(ozone, Date.Local = as.Date(Date.Local))


# TROUBLES with month
filter(ozone, State.Name == "Arizona" & County.Name == "Pinal") %>%
        mutate(month = factor(months(Date.Local), levels = month.name)) %>%
        select(State.Name, County.Name, month, Date.Local)
        group_by(month) %>%
        summarize(ozone = mean(Sample.Measurement))


filter(ozone, State.Name == "Oklahoma" & County.Name == "Caddo") %>% nrow
filter(ozone, State.Name == "Oklahoma" & County.Name == "Caddo") %>%
        mutate(month = factor(months(Date.Local), levels = month.name)) %>%
        group_by(month) %>%
        summarize(ozone = mean(Sample.Measurement))

#####
# 9. Challenge your solution
#####

# First we set our random number generator and resample the indices of the rows of
# the data frame with replacement. The statistical jargon for this approach is a bootstrap
# sample. We use the resampled indices to create a new dataset, ozone2 , that shares many
# of the same qualities as the original but is randomly perturbed.

set.seed(10234)
N <- nrow(ozone)
idx <- sample(N, N, replace = TRUE)
ozone2 <- ozone[idx, ]

ranking2 <- group_by(ozone2, State.Name, County.Name) %>%
        summarize(ozone = mean(Sample.Measurement)) %>%
        as.data.frame %>%
        arrange(desc(ozone))

cbind(head(ranking, 10),
      head(ranking2, 10))

# We can see that the rankings based on the resampled data (columns 4–6 on the right) are
# very close to the original, with the first 7 being identical. Numbers 8 and 9 get flipped in
# the resampled rankings but that’s about it. This might suggest that the original rankings
# are somewhat stable.
# We can also look at the bottom of the list to see if there were any major changes.

cbind(tail(ranking, 10),
      tail(ranking2, 10))

#####
# 10. Follow up
#####

# 1. Do you have the right data? Sometimes at the conclusion of an exploratory data
# analysis,theconclusionisthatthedatasetisnotreallyappropriateforthisquestion.
# In this case, the dataset seemed perfectly fine for answering the question of which
# counties had the highest levels of ozone.
# 2. Do you need other data? One sub-question we tried to address was whether the
# countyrankings werestableacrossyears.Weaddressedthisbyresamplingthedata
# oncetoseeiftherankingschanged,butthebetterwaytodothiswouldbetosimply
# get the data for previous years and re-do the rankings.
# 3. Doyouhavetherightquestion?Inthiscase,it’snotclearthatthequestionwetried
# to answer has immediate relevance, and the data didn’t really indicate anything to
# increasethe question’s relevance.Forexample, itmighthavebeen moreinteresting
# to assess which counties were in violation of the national ambient air quality
# standard, because determining this could have regulatory implications. However,
# this is a much more complicated calculation to do, requiring data from at least 3
# previous years.

#####

