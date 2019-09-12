#install.packages("fs")
#install.packages("pacman")
library(fs)
library(tidyverse)

print(paste("We're in this directory:", getwd()))

# READING IN DATA #
filename <- dir_ls(glob = "*.csv")
df <- read_csv(filename)
df$X1 <- NULL
View(df)


# MAKING EVERYTHING FACTORS #
# Creating and applying filter to find all char-cols 
char_filter <- sapply(df, function(x) {is.character(x)}) 
char_cols <- names(df)[char_filter]
# Removing name and mutating
fact_cols <- char_cols[-1] 
df <- df %>% mutate_at(fact_cols, function(x) {as.factor(x)})


#############
# EXERCISES #
#############

#EXERCISE 1# 
#1: 
largeish_feet <- filter(df, shoesize >= 39)

#2: 
can_touch_floor <- filter(df, str_detect(touch_floor, "Yes"))

#3: 
hold_breath_for_long <- filter(df, breath_hold > mean(breath_hold))

#4: 
medium_balloon_balance <- filter(df, 60 > balloon_balance & balloon_balance > 13)

#5: 
super_specific <- filter(df, (shoesize >= 39) & (str_detect(touch_floor, "Yes")) &
                           (breath_hold > mean(breath_hold)) & (60 > balloon_balance & balloon_balance > 13)) %>%
                             select(name, shoesize, touch_floor, breath_hold, balloon_balance)
#Exercise 2: Shoe_size 
################
# PRETTY PLOTS #
################
shoe_plot <- ggplot(data = df, aes(x = shoesize)) + geom_histogram(binwidth = 1, fill = "darkblue", color = "blue")
shoe_plot
taste_plot <- ggplot(data = df, aes(x = taste_blind)) + geom_bar(fill = "darkgreen")
taste_plot
