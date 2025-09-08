### Section 6 ggplot------------------------------------------------------------------------

pacman::p_load(tidyverse)

ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width))

#With Pipe
iris %>% ggplot(mapping = aes(x = Sepal.Length, y = Sepal.Width))

#Please note that aes() refers to columns in the data frame. 
#Variables names that do not exist in the data frame cannot be used

##6.01 Point

g_point<-iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width))+geom_point(
)

g_point_c<-iris %>% ggplot(aes(x=Sepal.Length,y=Sepal.Width,colour = Species))+geom_point()

##6.02 Line

#geom_line() to add line layer

df0 <- tibble(x = rep(1:50, 3), y = x * 2)

df0 %>% ggplot(aes(x=x,y=y))+geom_line()

df0 %>% ggplot(aes(x=x,y=y))+geom_line(color="green") + geom_point(color="salmon")

##6.03 Histograms

#geom_histogram() to add histogram layer

iris %>% ggplot(aes(x=Sepal.Length))+geom_histogram()

#bin default is 30, can set width

iris %>% ggplot(aes(x=Sepal.Length))+geom_histogram(binwidth = 0.5)

#change bin number

iris %>% ggplot(aes(x = Sepal.Length)) + geom_histogram(bins = 50)

iris %>% ggplot(aes(x=Sepal.Length, fill=Species)) + geom_histogram(color="black")

##6.04 Box Plot

iris %>% ggplot(aes(x=Species, y=Sepal.Length))+geom_boxplot()

iris %>% ggplot(aes(x=Species, y=Sepal.Length,fill=Species))+geom_boxplot()

iris %>% ggplot(aes(x=Species, y=Sepal.Length,fill=Species))+geom_boxplot(color="tomato")

iris %>% ggplot(aes(x=Species, y= Sepal.Length, fill=Species))+geom_boxplot(color="black")

?geom_histogram

?geom_boxplot

##TERUI EXERCISE------------------------------------------------------------------

# draw a scatter plot of Petal.Length(y) and Petal.Width(x)
# assign to g_petal

g_petal<-iris %>% ggplot(aes(x=Petal.Width,y=Petal.Length))+geom_point()

#draw a box plot between Species(x) and Petal.Length(y)
#fill the box with Species
#assign to g_petal_box
g_petal_box <- iris %>% ggplot(aes(x=Species,y=Petal.Length,fill=Species))+geom_boxplot()

# add a new layer of point, x= Species and y= Petal.Length
g_petal_box + geom_point()

#Change axis titles

g_petal_box + labs(x="Plant Species", y= "Petal Length")

##Exam Details and Exercises-----------------------------------------------------

#dataframe mtcars

df_mtcars<- as_tibble(mtcars)
df_mtcars

#select rows where cylinders is 4

df_mtcars %>%filter(cyl==4)

#select columns mpg, cyl, disp, wt, vs, carb

df_mtcars %>% select(c(mpg,cyl,disp,wt,vs,carb))

#select rows with cyl is greater than 4, then select columns from before
#assign to df_sub

df_mtcars %>% filter(cyl > 4) %>% select(c(mpg,cyl,disp,wt,vs,carb))

df_sub<- df_mtcars %>% filter(cyl > 4) %>% select(c(mpg,cyl,disp,wt,vs,carb))

#type the following code and run it

v_car<-rownames(mtcars)

#add a new column called "car: to df_mtcars
#then reassign it to df_mtcars

df_mtcars %>% mutate(car=v_car)

df_mtcars<-df_mtcars %>% mutate(car=v_car)

#identify the lightest car with cyl = 8
df_mtcars %>% filter(cyl==8) %>% select(cyl,wt,car) %>% arrange(wt)

df_mtcars %>% filter(cyl==8)%>% arrange(wt)

#calculate the average wt of cars within each group of gear numbers 
#Consider using the group_by() and summarize()
#assign to df_mean

df_mean<- df_mtcars %>% group_by(gear) %>% summarize(avg.wt = mean(wt))
df_mean

#combination of dplyr operations with ggplot

df_mtcars %>% ggplot(aes(x=wt,y=mpg))+geom_point()

#draw a figure between wt and qsec but only those with cyl = 6

df_mtcars %>% filter(cyl==6) %>% ggplot(aes(x=wt,y=qsec))+geom_point()

# draw a figure between mean weight and mean qsec for each group of gear

#MY ATTEMPT
#df_mtcars %>% group_by(gear) %>% summarize(c(avg.wt = mean(wt), mqsec = qsec)) %>% ggplot(aes(x=avg.wt,y=mqsec))+geom_point()

#ACTUAL - **dont need c() within the summarize()**
df_mtcars %>% group_by(gear) %>% summarise(avg.wt=mean(wt),avg.qsec=mean(qsec)) %>% ggplot(aes(x=avg.wt,y=avg.qsec))+geom_point()

q()
