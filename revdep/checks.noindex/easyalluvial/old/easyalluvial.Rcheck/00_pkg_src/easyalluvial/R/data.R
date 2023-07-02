

#'Quarterly mean relative sunspots number from 1749-1983
#'
#'
#'@format A data frame with 940 rows and 4 variables
#'\describe{
#'  \item{year}{}
#'  \item{qu}{quarter} \item{spots}{total number of sunspots}
#'  \item{mean_spots_per_year}{}
#'
#'  }
#'@source Andrews, D. F. and Herzberg, A. M. (1985) Data: A Collection of Problems from Many Fields for the Student and Research Worker. New York: Springer-Verlag.
#'
"quarterly_sunspots"


#'Quarterly mean arrival delay times for a set of 402 flights
#'
#'Created from nycflights13::flights
#'
#'@format A data frame with 1608 rows and 6 variables
#'\describe{
#'  \item{tailnum}{
#'  a unique identifier created from tailnum, origin, destination and carrier}
#'  \item{carrier}{carrier code} \item{origin}{origin code}
#'  \item{dest}{destination code} \item{qu}{quarter}
#'  \item{mean_arr_delay}{average delay on arrival as either on_time or late}
#'
#'  }
#'@source nycflights13::flights
#'
"quarterly_flights"



#'mtcars dataset with cyl, vs, am ,gear, carb as factor variables and car model
#'names as id
#'@format A data frame with 32 rows and 12 variables
#'\describe{
#'
#'\item{mpg}{Miles/(US) gallon}
#'\item{cyl}{ Number of cylinders}
#'\item{disp}{Displacement (cu.in.)}
#'\item{hp}{Gross horsepower}
#'\item{drat}{Rear axle ratio}
#'\item{wt}{Weight (1000 lbs)}
#'\item{qsec}{ 1/4 mile time}
#'\item{vs}{Engine}
#'\item{am}{Transmission}
#'\item{gear}{Number of forward gears}
#'\item{carb}{Number of carburetors}
#'\item{ids}{car model name}
#'}
#'@source datasets
"mtcars2"


#'titanic data set'
#'@format A data frame with 891 rows and 10 variables
#'\describe{
#'
#'\item{Survived}{Survived}
#'\item{Pclass}{Pclass}
#'\item{Sex}{Sex}
#'\item{Age}{Age}
#'\item{SibSp}{SibSp}
#'\item{Parch}{Parch}
#'\item{Fare}{Fare}
#'\item{Cabin}{Cabin}
#'\item{Embarked}{Embarked}
#'\item{title}{title}
#'}
#'@source datasets
"titanic"
