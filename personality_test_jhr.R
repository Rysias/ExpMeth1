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

#EXERCISE 1: Filters
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

#EXERCISE 2: Arranging
#1 
slow_tongues <- arrange(df, desc(tongue_twist)) %>% select(name, tongue_twist)
slow_tongues

#2 
best_romberg <- df %>% filter(romberg_closed == max(romberg_closed)) %>% select(name, romberg_closed, romberg_open) %>%
                    arrange(desc(romberg_closed), desc(romberg_open))
shame_romberg <-  filter(df, romberg_open == max(romberg_open)) %>% select(name)
best_romberg

#EXERCISE 3: SELECTING
#1 select same column twice: It doesn't throw error (useful with everything())
select(df, name, name)

#2
cols <- c("name", "shoesize", "touch_floor")
select(df, cols)

#3 
rearranged_df <- select(df, gender, shoesize, everything())
head(rearranged_df)

#EXERCISE 4: MUTATE
#1 
twist_word_count <- 99
df <- df %>% mutate(words_per_sec = tongue_twist / twist_word_count)
df$words_per_sec

#2
df <- df %>% mutate(breath_min = breath_hold %/% 60) %>% mutate(breath_sec = breath_hold - (60*breath_min))
select(df, name, breath_hold, breath_min, breath_sec)

#bonus
df <- df %>% mutate(wps_avg_dist = words_per_sec - mean(words_per_sec))
select(df, name, wps_avg_dist)


#EXERCISE 5: SUMMARISE 
#1; gender diff balloon balance
gender_ballons <- df %>% group_by(gender) %>% summarise(mean(balloon_balance), sd(balloon_balance), n())
gender_ballons
print("there seems to be a much higher mean, but also a higher standard deviation in the male sample")
# doing t-test
male_balloons <- (df %>% filter(gender == "male"))[["balloon_balance"]]
female_balloons <- (df %>% filter(gender == "female"))[["balloon_balance"]]
t_test <- t.test(male_balloons, female_balloons)

print(paste("the p-value of the test is", t_test$p.value))
if (t_test$p.value > 0.05) {
  print("we can NOT reject the null at a 5% significance level")
} else {
  print("we can reject the null at a 5% significance level")
}

#2: sound_level and coca_preference
sound_cola <- df %>% group_by(taste_cola) %>% summarise(mean(sound_level_pref))
sound_cola
print("There seems to be a small difference! let's do a t-test")

l_sound <- (df %>% filter(taste_cola == "Option L"))[["sound_level_pref"]]
s_sound <- (df %>% filter(taste_cola == "Option S"))[["sound_level_pref"]]
t_test <- t.test(s_sound, l_sound)

print(paste("the p-value of the test is", t_test$p.value))
if (t_test$p.value > 0.05) {
  print("we can NOT reject the null at a 5% significance level")
} else {
  print("we can reject the null at a 5% significance level")
}



#3: tongue_twister and handedness
tongue_hand <- df %>% group_by(handedness) %>% summarise(mean(tongue_twist), sd(tongue_twist), n())
tongue_hand 

# There is a big imbalance between left-handed and right-handed which makes the left-handed values much more shaky



################
# PRETTY PLOTS #
################
shoe_plot <- ggplot(data = df, aes(x = shoesize)) + geom_histogram(binwidth = 1, fill = "darkblue", color = "blue")
shoe_plot
taste_plot <- ggplot(data = df, aes(x = taste_blind)) + geom_bar(fill = "darkgreen")
taste_plot
