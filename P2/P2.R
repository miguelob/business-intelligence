library(arules)
library(data.table)
data("AdultUCI")

#creo tablas a partir de dataframes divididos del ADULTUCI
dt1 <- data.table(AdultUCI[,c( "fnlwgt", "age", "sex" )], key ="sex")
dt2 <- data.table(AdultUCI[,c( "sex", "race" )], key= "sex")
dt3 <- data.table(AdultUCI[,c( "sex", "income" )], key = "sex")

#Merge de las tablas
mr1 <- merge(dt1,dt2, all.x=T)
mr2 <- merge(mr1,dt3, all.x=T)

#Merge manual/falso que funciona
merged <- AdultUCI[,c( "fnlwgt", "age", "sex", "income", "race" )]

#creo un cubo de 4 dimensiones
cube1 <- tapply(merged$fnlwgt, merged[,c( "age", "sex","race", "income")], 
                FUN=function(x){return(sum(x))})

cube1
dimnames(cube1)

#SLICE
cube1[ ,"Female","White","large"]
cube1["22" ,"Female","White","large"]

# #####  Dice
cube1[c("20","21","22"), 
             c("Male"), 
             c("White", "Asian-Pac-Islander"),]

########### Roll- accross
apply(cube1, c("age", "income"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

########### Pivot + rollup 
apply(cube1, c("income", "sex"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(cube1, c("sex", "income"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

## REPETICIÓN MANUAL

#LOCATION
state_table <- 
  data.frame(key=c("MAD", "BAR", "PAR", "ROM", "BER"),
             name=c("Madrid", "Barcelona", "Paris", "Roma", 
                    "Berlín"),
             country=c("España", "España", "Francia", "Italia", "Alemania"))
#TIME
month_table <- 
  data.frame(key=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
#Product
prod_table <- 
  data.frame(key=c("Mercedes GLC", "BMW X3", "Audi Q5"),
             price=c(60000, 55000, 67000))

# Function to generate the Sales table
gen_sales <- function(no_of_recs) {
  
  # Generate transaction data randomly
  loc <- sample(state_table$key, no_of_recs, 
                replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2012, 2013), no_of_recs, replace=T)
  prod <- sample(prod_table$key, no_of_recs, replace=T, prob=c(1, 3, 2))
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3))
  
  prod <-as.character(prod)
  cost<-rep(1:no_of_recs, 0)
  for(i in 1:no_of_recs) 
  {
    if (prod[i] == "Mercedes GLC" ) cost[i]<-60000
    if (prod[i] == "BMW X3" ) cost[i]<-55000
    if (prod[i] == "Audi Q5" ) cost[i]<-67000
  }
  amount <-unit*cost
  ### amount <- unit*prod_table[prod,]$price
  
  sales <- data.frame(month=time_month,
                      year=time_year,
                      loc=loc,
                      prod=prod,
                      unit=unit,
                      amount=amount)
  
  # Sort the records by time order
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

# Now create the sales fact table
sales_fact <- gen_sales(1000)

# Look at a few records
head(sales_fact)

# Build up a cube (Multi-dimensional cube)
#Creation of a cube for sales for the dimensions
# product, month, year loc

revenue_cube <- 
  tapply(sales_fact$amount, 
         sales_fact[,c("prod", "month", "year", "loc")], 
         FUN=function(x){return(sum(x))})
# A different alternative
revenue_cube2 <- 
  tapply(sales_fact$amount, 
         sales_fact[,c( "year", "loc","prod", "month")], 
         FUN=function(x){return(sum(x))})


# Showing the cells of the cude
revenue_cube

# Showing the cells of the cude
revenue_cube2

#Showing names of the multi-dimensional cube
dimnames(revenue_cube)

### OLAP OPERTORS - EXAMPLES

# #####   Slice
# Fixing some dimensions to analyze the rest

# cube data in Jan, 2012
revenue_cube[, "1", "2012",]


# cube data in Jan, 2012
revenue_cube["Mercedes GLC", "1", "2012",]


# #####  Dice
# Limitation of dimensions to certain values  keeping the number of 
# dimensions
revenue_cube[c("BMW X3","Audi Q5"), 
             c("1","2","3"), 
             ,
             c("ROM","BER")]

########### Roll- accross - Application of an aggregation function to collapse
#a number of dimensions
# Example: annual revenue for each product collapsing the location
apply(revenue_cube, c("year", "prod"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

########### Drill - accross (Reverse of roll - accross)
# Application of an aggregation function to a finer level of 
#granularity
# Example: annual and monthly revenue for each product collapsing the location
apply(revenue_cube, c("year", "month", "prod"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

########### Pivot + rollup 
# Analysis of the combination of a pair of selected dimensions

# Example: annual and monthly revenue collapsing product and location

apply(revenue_cube, c("year", "month"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(revenue_cube, c( "month", "year"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


# Example: product and location collapsing year and month

apply(revenue_cube, c("prod", "loc"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(revenue_cube, c("loc", "prod"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})  

