
# libraries
library(rlang)
library(lubridate)
library(ggplot2)
library(dplyr)


# import data
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# my functions

# MENTOR HELP REQUESTED
# make a fill column for a df w/ given column name, given column value
my_fill_col <- function(df, col, x) {
#    ny$City <- 'NY'
#    ny
}

# make a pretty plot function
# https://thomasadventure.blog/posts/turning-your-ggplot2-code-into-a-function/
my_plot_maker_2 <- function(data, x, y, z) {
    ggplot(data = data, aes(x = {{x}}, fill = {{y}})) + geom_histogram(position = "identity", alpha = .4, binwidth=z)
}

# get mode function
# https://stackoverflow.com/questions/12187187/how-to-retrieve-the-most-repeated-value-in-a-column-present-in-a-data-frame
get_mode <- function(x){
  return(names(sort(table(x), decreasing = T, na.last = T)[1]))
}

# add a city name column
#ny <- my_fill_col(ny$City, 'NY')
ny$City <- 'NY'
#wash1 <- my_fill_col(wash, City, 'DC')
wash$City <- 'DC'
#chi1 <- my_fill_col(chi, City, 'Chicago')
chi$City <- 'Chicago'

# add some null values so you can combine data later
wash$Gender <- ''
wash$Birth.Year <- ''


# combine tables
bikedata = rbind(ny, wash, chi)


# split dates
# get month
bikedata$Month.Name = format(as.Date(bikedata$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%b')
# get day of week
bikedata$Day.Name = format(as.Date(bikedata$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%A')
# get hour of day in 24 hr format
bikedata$Hour <- format(as.POSIXct(bikedata$Start.Time, format="%Y-%m-%d %H:%M:%S"),"%H")
bikedata$Hour <- as.numeric(bikedata$Hour)


# occurs most often in the start time? 
# look at what hours appear most in data
qplot(x=Hour, data=bikedata, binwidth = 1)


# most frequent day of week value
get_mode(bikedata$Day.Name)

# most frequent month value
get_mode(bikedata$Month)

# get table of weekend days and hour of start.time

# https://www.r-bloggers.com/2020/08/how-to-subset-a-data-frame-column-data-in-r/
# Subsetting multiple columns from a data frame
days_bikedata <- bikedata[,c("Day.Name", "Hour")]

# make table of weekend data
weekends_bikedata <- days_bikedata %>% filter(grepl('Saturday|Sunday', Day.Name))

# get histogram of hours of start.time and weekends vs weekdays
my_plot_maker_2(weekends_bikedata, weekends_bikedata$Hour, weekends_bikedata$Day.Name, 1)


# https://www.r-bloggers.com/2020/08/how-to-subset-a-data-frame-column-data-in-r/
# Subsetting multiple columns from a data frame
weekdays_bikedata <- days_bikedata %>% filter(grepl('Monday|Tuesday|Wednesday|Thursday|Friday', Day.Name))
weekdays_bikedata$Day.Name <- factor(weekdays_bikedata$Day.Name, levels = c("Monday",  "Tuesday", "Wednesday","Thursday", "Friday"))

# can be sorted?
weekdays_bikedata[order(weekdays_bikedata$Hour, weekdays_bikedata$Day.Name), ]

# plot
my_plot_maker_2(weekdays_bikedata, weekdays_bikedata$Hour, weekdays_bikedata$Day.Name, 1)


# popular travel times by city
my_plot_maker_2(bikedata, bikedata$Hour, bikedata$City, 1)


# popular travel times by month
#ggplot(data=bikedata, aes(x = as.numeric(Hour), fill = bikedata$Month)) + geom_histogram(position = "dodge", alpha = 0.4, binwidth=2)

# change fill and outline color manually 
# https://www.datanovia.com/en/lessons/ggplot-histogram/
ggplot(data=bikedata, aes(x = Hour, fill = bikedata$Month)) + geom_histogram(position = "dodge", alpha = 1, binwidth = 1) +
scale_fill_manual(values = c( "#F8D90F", "#0191B4","#000000", "#FE7A15","#35BBCA","#D3DD18")) +
scale_color_manual(values = c( "#F8D90F", "#0191B4","#000000", "#FE7A15","#35BBCA","#D3DD18"))



# popular travel times by weekday vs. weekend.

# make a weekend column
bikedata$Weekend <- 'No'

# get rid of week end days
bikedata <- within(bikedata, {Weekend[startsWith(Day.Name, "S")] = "Yes"})

# plot
my_plot_maker_2(bikedata, bikedata$Hour, bikedata$Weekend, 1)


# overall look: for each city
# for weekend/weekday
# hours/count w/ colour = month

#3 pairs of city graphs

# weekend bike use by month in NY
graph_ny <- subset(bikedata, City=="NY" & Weekend == "Yes")
graph_ny.Month <- as.numeric('Month')
my_plot_maker_2(graph_ny, graph_ny$Hour, graph_ny$Month, 1)

graph_ny <- subset(bikedata, City=="NY" & Weekend == "No")
graph_ny.Month <- as.numeric('Month')
my_plot_maker_2(graph_ny, graph_ny$Hour, graph_ny$Month, 1)


graph_wash <- subset(bikedata, City=="DC" & Weekend == "Yes")
graph_wash.Month <- as.numeric('Month')
my_plot_maker_2(graph_wash, graph_wash$Hour, graph_wash$Month, 1)

graph_wash <- subset(bikedata, City=="DC" & Weekend == "No")
graph_wash.Month <- as.numeric('Month')
my_plot_maker_2(graph_wash, graph_wash$Hour, graph_wash$Month, 1)
   
                
graph_chi <- subset(bikedata, City=="Chicago" & Weekend == "Yes")
graph_chi.Month <- as.numeric('Month')
my_plot_maker_2(graph_chi, graph_chi$Hour, graph_chi$Month, 1)


graph_chi <- subset(bikedata, City=="Chicago" & Weekend == "No")
graph_chi.Month <- as.numeric('Month')
my_plot_maker_2(graph_chi, graph_chi$Hour, graph_chi$Month, 1)

# concatenate stations
my_ny = ny
my_wash = wash
my_chi = chi

my_ny$Trip.Name = paste(my_ny$Start.Station, my_ny$End.Station)
my_wash$Trip.Name = paste(my_wash$Start.Station, my_wash$End.Station)
my_chi$Trip.Name = paste(my_chi$Start.Station, my_chi$End.Station)

# drop roundtrips
clean_ny <- subset(my_ny, as.character(Start.Station) != as.character(End.Station)) 
clean_wash <- subset(my_wash, as.character(Start.Station) != as.character(End.Station)) 
clean_chi <- subset(my_chi, as.character(Start.Station) != as.character(End.Station)) 

# get mode
# https://www.programmingr.com/statistics/how-to-calculate-mode-in-r/

getMode <- function(x) {
keys <- unique(x)
keys[which.max(tabulate(match(x, keys)))]
}
getMode(clean_ny$Trip.Name)
getMode(clean_wash$Trip.Name)
getMode(clean_chi$Trip.Name)


# get count of trips
x = length(which(clean_ny$Trip.Name == 'E 7 St & Avenue A Cooper Square & E 7 St'))
y = length(which(clean_wash$Trip.Name == 'Lincoln Memorial Jefferson Memorial'))
z = length(which(clean_chi$Trip.Name == 'Lake Shore Dr & Monroe St Streeter Dr & Grand Ave'))

x
y
z

nrow(clean_ny)
nrow(clean_wash)
nrow(clean_chi)


# get most popular as percentage of all trips
33/53705
143/85405
32/8357

# Your solution code goes here

# What are trip durations like in general?
all_durations_by_city_and_hour <- ggplot(bikedata, aes(Hour, Trip.Duration, colour = City, size = Trip.Duration)) + geom_point()
print(all_durations_by_city_and_hour)

# get mean Trip.Duration between 
# the hours of 8-9AM and 5-6PM for each city

# split dates
# get month
clean_ny$Month.Name = format(as.Date(clean_ny$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%b')
# get day of week
clean_ny$Day.Name = format(as.Date(clean_ny$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%A')
# get hour of day in 24 hr format
clean_ny$Hour <- format(as.POSIXct(clean_ny$Start.Time, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone()),"%H")

# split dates
# get month
clean_wash$Month.Name = format(as.Date(clean_wash$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%b')
# get day of week
clean_wash$Day.Name = format(as.Date(clean_wash$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%A')
# get hour of day in 24 hr format
clean_wash$Hour <- format(as.POSIXct(clean_wash$Start.Time, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone()),"%H")


# split dates
# get month
clean_chi$Month.Name = format(as.Date(clean_chi$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%b')
# get day of week
clean_chi$Day.Name = format(as.Date(clean_chi$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%A')
# get hour of day in 24 hr format
clean_chi$Hour <- format(as.POSIXct(clean_chi$Start.Time, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone()),"%H")



# drop weekends
clean_ny[!clean_ny$Day.Name == "Sunday", ]
clean_ny[!clean_ny$Day.Name == "Saturday", ]
clean_DC[!clean_DC$Day.Name == "Sunday", ]
clean_DC[!clean_DC$Day.Name == "Saturday", ]
clean_Chi[!clean_Chi$Day.Name == "Sunday", ]
clean_Chi[!clean_Chi$Day.Name == "Saturday", ]


# get means by hour
# https://www.geeksforgeeks.org/how-to-calculate-the-mean-by-group-in-r-dataframe/
# Specify data column
Chicago.Means <- aggregate(x = clean_chi$Trip.Duration,
                                 
                                 # Specify group indicator
                                 by = list(clean_chi$Hour),     
                                 
                                 # Specify function (i.e. mean)
                                 FUN = mean)

Chicago.Means$City <- 'Chicago'
Chicago.Means





# get means by hour
# https://www.geeksforgeeks.org/how-to-calculate-the-mean-by-group-in-r-dataframe/
# Specify data column
DC.Means <- aggregate(x = clean_wash$Trip.Duration,
                                 
                                 # Specify group indicator
                                 by = list(clean_wash$Hour),     
                                 
                                 # Specify function (i.e. mean)
                                 FUN = mean)

DC.Means$City <- 'DC'
DC.Means






# get means by hour
# https://www.geeksforgeeks.org/how-to-calculate-the-mean-by-group-in-r-dataframe/
# Specify data column
NY.Means <- aggregate(x = clean_ny$Trip.Duration,
                                 
                                 # Specify group indicator
                                 by = list(clean_ny$Hour),     
                                 
                                 # Specify function (i.e. mean)
                                 FUN = mean)

NY.Means$City <- 'NY'
NY.Means


# convert means data
meansdata %>%
    mutate(Hours = hour(seconds_to_period(x)),
           Minutes = minute(seconds_to_period(x))) %>%
    select(Group.1, x, City, Hours, Minutes)

# get max values
# XXXXxXXx
summary(meansdata)


group <- as.factor(meansdata$City)

plot(meansdata$Group.1, meansdata$Minutes, pch = as.numeric(group), col = group)

# Legend
legend("topright", legend = levels(group),
 col = c("green", "red", "black"),
 pch = c(3, 2, 1) )



system('python -m nbconvert Explore_bikeshare_data.ipynb')
