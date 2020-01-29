library("tidyverse")
library("gapminder")
library(dplyr)

q1 <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

q2 <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

q3 <- c(1 , 2 , 1 , 3 , 4 , 1 , 1 , 4 , 2 , 1 , 3 , 4 , 3 , 2 , 1 , 3 , 4 , 1 , 2 , 3 , 1 , 1 , 2 , 1 , 1 , 3 , 4)

expenditure <- read.table("expenditure.txt", header = T)
head(expenditure)

avgq1 <- mean(q1)

sdq1 <- sd(q1)

z90 <- qnorm((1-.90)/2, lower.tail = FALSE)

nq1 <- length(q1)

low_q1 <- avgq1 - (z90 * (sdq1/sqrt(nq1)))
up_q1 <- avgq1 + (z90 * (sdq1/sqrt(nq1)))

confintq1 <- c(low_q1, up_q1)
confintq1

avgq2 <- mean(q2)

sdq2 <- sd(q2)

z95 <- qnorm((1-.95)/2, lower.tail = FALSE)

nq2 <- length(q2)

low_q2 <- avgq2 - (z95 * (sdq2/sqrt(nq2)))
up_q2 <- avgq2 + (z95 * (sdq2/sqrt(nq2)))

confintq2 <- c(low_q2, up_q2)
confintq2

q3mod <- factor(q3, levels = c(1,2,3,4), labels = c("freshman", "sophomore", "junior", "senior"))
q3mod

regionq3 <- expenditure %>% 
  select(Region)
regionq3

exmod <- expenditure %>%
  mutate(regionmod = case_when(Region == 1 ~ "Northeast", Region == 2 ~ "North Central", Region == 3 ~ "South", Region == 4 ~ "West"))
head(exmod)

whiskmod <- exmod %>% 
  select(regionmod)
whiskmod

ggplot(exmod, aes(x = X1, y = Y)) + geom_line(color = "red") +
  geom_point(color = "black")

ggplot(exmod, aes(x = X2, y = Y)) + geom_line(color = "blue") +
  geom_point(color = "black")

ggplot(exmod, aes(x = X3, y = Y)) + geom_line(color = "green") +
  geom_point(color = "black")

whisk <- exmod %>% 
  select(regionmod, Y)

NEwhisk <- whisk %>%
  filter(regionmod == 'Northeast')

NCwhisk <- whisk %>%
  filter(regionmod == 'North Central')

Swhisk <- whisk %>%
  filter(regionmod == 'South')

Wwhisk <- whisk %>%
  filter(regionmod == 'West')

ggplot(whisk, aes(x = regionmod, y = Y)) +
  geom_boxplot(aes(fill = 'Region'))

ggplot(exmod, aes(x = X1, y = Y)) + geom_line(color = "purple") +
  geom_point(color = "black")

whiskt <- exmod %>% 
  select(regionmod, Y, X1)

NEfin <- whiskt %>%
  filter(regionmod == 'Northeast')

NCfin <- whiskt %>%
  filter(regionmod == 'North Central')

Sfin <- whiskt %>%
  filter(regionmod == 'South')

Wfin <- whiskt %>%
  filter(regionmod == 'West')

ggplot(whiskt, aes(x = X1, y = Y)) + 
  geom_line(aes(color = regionmod)) +
  geom_point(aes(shape = regionmod))