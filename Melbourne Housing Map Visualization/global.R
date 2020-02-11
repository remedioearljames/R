library(dplyr)
MyData <- read.csv(file="data/melb_data.csv", header=TRUE, sep=",")

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )

cleandata <- MyData %>%
  select(
    suburb = Suburb,
    Address = Address,
    Rooms = Rooms,
    Type = Type,
    Price = Price,
    Method = Method,
    Seller = SellerG,
    Date = Date,
    Distance = Distance,
    Postcode = Postcode,
    Bedroom = Bedroom2,
    Bathroom = Bathroom,
    Car = Car,
    Landsize = Landsize,
    BuildingArea = BuildingArea,
    YearBuilt = YearBuilt,
    CouncilArea = CouncilArea,
    Lat= Lattitude,
    Lng = Longtitude,
    Region = Regionname
  )
 
