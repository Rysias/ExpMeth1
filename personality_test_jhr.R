#install.packages("fs")
library(fs)
library(tidyverse)
print(getwd())

# Reading in data
filename <- dir_ls(glob = "*.csv")
df <- read_csv(filename)
df$X1 <- NULL

ggplot(data = df, aes(x = shoesize)) + geom_histogram(binwidth = 1)

?geom_histogram()

min(df$shoesize)

