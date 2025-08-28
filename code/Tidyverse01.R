if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)


set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)
# 5.1 Row Manipulation-----------------------------------------------

# 5.1.1 Subset Rows
filter(iris_sub, Species == "virginica")

filter(iris_sub, Species %in% c("virginica", "versicolor"))

filter(iris_sub, Species != "virginica")

filter(iris_sub, !(Species %in% c("virginica", "versicolor")))

filter(iris_sub, Sepal.Length > 5)

filter(iris_sub, Sepal.Length >= 5)

filter(iris_sub, Sepal.Length < 5)

filter(iris_sub, Sepal.Length <= 5)

# Sepal.Length is less than 5 AND Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 & Species == "setosa")
# same; "," works like "&"
filter(iris_sub,
       Sepal.Length < 5, Species == "setosa")
# Either Sepal.Length is less than 5 OR Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 | Species == "setosa")

# 5.1.2 Arrange Rows
arrange(iris_sub, Sepal.Length)

arrange(iris_sub, desc(Sepal.Length))

# 5.1.3 Exercise
iris_3 <- filter(iris_sub, Sepal.Width > 3.0)

iris_setosa <- filter(iris_sub, Species == "setosa")

iris_3_setosa <- filter(iris_sub,Sepal.Length > 3.0 & Species == "setosa")

# 5.2 Column Manipulation---------------------------------------------

# 5.2.1 select columns
select(iris_sub, Sepal.Length)

select(iris_sub, c(Sepal.Length, Sepal.Width))

select(iris_sub, -Sepal.Length)

select(iris_sub, -c(Sepal.Length, Sepal.Width))

select(iris_sub, starts_with("Sepal"))

select(iris_sub, -starts_with("Sepal"))

select(iris_sub, ends_with("Width"))

select(iris_sub, -ends_with("Width"))

#5.2.2 Add Columns

#nrow() returns the number of rows of the dataframe
x_max <- nrow(iris_sub)

x <- 1:x_max

#add a new column #name row ID as x
mutate(iris_sub,row_id = x)

###new column of Sepal.Length NOT SURE ON THIS
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)
### DID NOT WORK, unsure why

#5.2.3 Exercise
iris_pw <- select(iris_sub, c(Petal.Width, Species))

iris_petal <- select(iris_sub, starts_with("Petal"))

iris_pw_two <- mutate(iris_sub, Petal.Width*2)
###UNSURE for doubling

#5.3 Piping--------------------------------------------------------------

#5.3.1 Pipe - Hotkey Ctrl+Shift+M
df_vir_sl <- iris_sub %>% filter(Species == "virginica") %>% select(Sepal.Length)

#5.3.2 Exercise
iris_pipe <- iris_sub %>% filter(Species == "setosa") %>% mutate( Petal_Width_two = 2 * Petal.Width)
###Error
### FIXED error was had iris_sub in the mutate()

###MUTATE ATTEMPT AGAIN

# twice `Sepal.Length` and add as a new column
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)
###Worked think my view of the console was just too small

#5.4 Group Manipulation ----------------------------------------------

#5.4.1 Grouping

iris_sub %>% group_by(Species)

#5.4.2 Summarize

iris_sub %>% 
  group_by(Species) %>% summarize(mu_sl = mean(Sepal.Length))

iris_sub %>% 
  group_by(Species) %>% summarize(mu_sl = mean(Sepal.Length), sum_sl = sum(Sepal.Length))

iris_sub$Sepal.Length
mean(iris_sub$Sepal.Length)

#5.4.3 Summarize with Mutate

# grouping by "Species", then take means "Speal.Length" for each species
iris_sub %>% 
  group_by(Species) %>% mutate(mu_sl = mean(Sepal.Length)) %>% ungroup()

#make sure to ungroup()

#calculate the mean of Petal.Length and the sum of Petal.Length

iris_sub %>% 
  group_by(Species) %>% summarize(m_sl = mean(Petal.Length), sum_pl = sum(Petal.Length))

#5.6 Join -------------------------------------------------------------------------
#left_join(): merge data frames based on column(s)

df1 <- tibble(Species = c("A", "B", "C"), x = c(1,2,3))

df2 <- tibble(Species = c("A","B","C"),y=c(10,12,13))

left_join(x=df1, y=df2, by = "Species")

df3 <- tibble(Species = c("A", "A", "B", "C"), y = c(4, 5, 6, 7))

left_join(x = df1, y = df3, by = "Species")

df4 <- tibble(Species = c("A", "A", "C"), y = c(4, 5, 7))

left_join(x = df1, y = df4, by = "Species")
#No data in the slot for joining results in NA or Not Available
#-------------------------------------------------------------------------------
## DR.TERUI EXERCISE 1
# filter iris_sub to only contain "virginica"

filter(iris_sub, Species == "virginica")

iris_sub %>% filter(Species =="virginica")

## EXERCISE 2
#select column "sepal.width" in iris_sub

select(iris_sub, Sepal.Width)

iris_sub %>% select(Sepal.Width)

## EXERCISE 3
#filter iris_sub to contain only "virginica" then pipe to select "Sepal.Width" then pipe to create a new column called sw_3 = 3*Sepal.Width and assign to "df0"

df0 <- iris_sub %>% filter(Species =="virginica") %>% select(Sepal.Width) %>% mutate(sw_3 = 3*Sepal.Width)
