# Course Project

# Data Quality report

# Load the library
library(dplyr)
library(forcats)
library(tidyverse)
library(car)
library(plotly)
library(viridis)

ev.data <- read.csv('C:\\Users\\jasmi\\OneDrive - University of Oklahoma\\DSA-5103-IDA\\Course Project\\Electric_Vehicle_Population_Data.csv',na.strings = c("", "NA"))
# Check the count of NA's in train dataset
na_counts <- sapply(ev.data, function(x) sum(is.na(x)))
na_counts

#Mostly Clean data

ev.numeric <- ev.data %>% select(where(is.numeric))
ev.char <- ev.data %>% select(where(is.character))
ev.logical <- ev.data %>% select(where(is.logical))



# Define quantile functions for data quality report
Q1 <- function(x, na.rm = TRUE) {
  quantile(x, na.rm = na.rm)[2]
}

Q3 <- function(x, na.rm = TRUE) {
  quantile(x, na.rm = na.rm)[4]
}



# Define numeric summary function to create numeric data quality report
myNumericSummary <- function(x) {
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm = TRUE),
    min(x, na.rm = TRUE), Q1(x, na.rm = TRUE), median(x, na.rm = TRUE), Q3(x, na.rm = TRUE),
    max(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}


numericSummary <- ev.numeric %>% summarise(across(everything(), myNumericSummary))
numericSummary <-cbind(stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"), numericSummary)


glimpse(numericSummary)


numericSummaryFinal <- numericSummary %>%
  pivot_longer("Postal.Code":"X2020.Census.Tract", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())


numericSummaryFinal %>% kable()

filtered_data <- ev.data %>%
  filter(State != "WA")

head(filtered_data)

# Plot 

#plot(ev.numeric)

# Data Quality on Factor

getmodes1 <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1))
  }
}
getmodes2 <- function(v,type=2) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==2) {
    return (names(which.max(tbl[-m1])))
  }
}
getmodes3 <- function(v,type=-1){
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==-1) {
    return (names(which.min(tbl)))
  }
}
getmodesCnt1 <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl))
  }
}
getmodesCnt2 <- function(v,type=2){
  tbl <- table(v)
  5
  m1<-which.max(tbl)
  if (type==2) {
    return (max(tbl[-m1]))
  }
}
getmodesCnt3 <- function(v,type=-1){
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==-1) {
    return (min(tbl))
  }
}
freqRatio <- function(x){
  round(getmodesCnt1(x)/getmodesCnt2(x), digits=2)
}
missing_pct <- function(x){
  100*sum(is.na(x))/length(x)
}
unique_pct <- function(x){
  100*n_distinct(x)/length(x)
}
myFactorSummary<-function(x){
  c(length(x), n_distinct(x), unique_pct(x), sum(is.na(x)), missing_pct(x), freqRatio(x), getmodes1(x), getmodesCnt1(x),
    getmodes2(x), getmodesCnt2(x), getmodes3(x), getmodesCnt3(x))
}
factorSummary<- ev.char %>%
  summarise(across(c(VIN..1.10.:Electric.Utility), myFactorSummary))

factorSummary<-cbind(
  stat=c("n","unique","unique_pct","missing","missing_pct","freqRatio","1st mode","1st mode freq","2nd mode",
         "2nd mode freq","least common","least common freq"), factorSummary)
factorSummaryFinal <- factorSummary %>%
  pivot_longer("VIN..1.10.":"Electric.Utility", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

factorSummaryFinal %>% kable()


# Create the scatter plot
ggplot(ev.data, aes(x = Make, y = Model, color = `Base.MSRP`)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    x = "Make",
    y = "Model",
    color = "Base.MSRP"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(ev.data, aes(x = Model.Year, y = Base.MSRP, color = `Base.MSRP`)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    x = "Model Year",
    y = "Base.MSRP",
    color = "Base.MSRP"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot for data distribution

par(mfrow=c(1, 6))
boxplot(ev.numeric$Postal.Code, main="Postal.Code")
boxplot(ev.numeric$Model.Year, main="Model.Year")
boxplot(ev.numeric$Electric.Range, main="Electric.Range")
boxplot(ev.numeric$Base.MSRP, main="Base.MSRP")
boxplot(ev.numeric$X2020.Census.Tract, main="X2020.Census.Tract")
boxplot(ev.numeric$Legislative.District, main="Legislative.District")

# Group by vehicle type

ggplot(ev.data, aes(x = Electric.Vehicle.Type, y = Electric.Range, fill = Electric.Vehicle.Type)) +
  geom_boxplot() +
  labs(title = "Electric Range by Electric Vehicle Type", x = "Electric Vehicle Type", y = "Electric Range") +
  scale_fill_manual(values = c("EV" = "blue", "PHEV" = "green")) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(ev.data, aes(x = Electric.Vehicle.Type, y = Base.MSRP, fill = Electric.Vehicle.Type)) +
  geom_boxplot() +
  labs(title = "Base MSRP by Electric Vehicle Type", x = "Electric Vehicle Type", y = "Base MSRP") +
  scale_fill_manual(values = c("EV" = "blue", "PHEV" = "green")) +
  theme_minimal() +
  theme(legend.position = "none")


# Check if each make,model year and model have a non-zero base MSRP
filtered_data <- ev.data  %>%
  group_by(Make, Model,Model.Year) %>%
  summarize(HasNonZeroBaseMSRP = any(`Base.MSRP` > 0))


# state-level electric vehicle adoption data
state_data <- ev.data %>%
  group_by(State, Electric.Vehicle.Type) %>%
  summarize(Adoption = n()) %>%
  filter(Electric.Vehicle.Type %in% c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)"))

# city-level in WA electric vehicle adoption data
city_data <- ev.data %>%
  filter(State == 'WA' &
           Electric.Vehicle.Type %in% c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)")) %>%
  group_by(City, Electric.Vehicle.Type) %>%
  summarize(Adoption = n())

# Electric vehicle adoption data by model year
adoption_by_year <- ev.data %>%
  group_by(Model.Year, Electric.Vehicle.Type) %>%
  summarize(Adoption = n()) %>%
  filter(Electric.Vehicle.Type %in% c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)"))

# Adoption over the year
ggplot(adoption_by_year, aes(x = Model.Year, y = Adoption, color = Electric.Vehicle.Type)) +
  geom_line() +
  labs(title = "Electric Vehicle Adoption Over Model Years", x = "Model Year", y = "Adoption Count") +
  theme_minimal()


ggplot(city_data, aes(x = City, y = Adoption, fill = Electric.Vehicle.Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
  labs(title = "Electric Vehicle Adoption by City", x = "City", y = "Adoption Count") +
  theme_minimal()

# Before removing the state 

library(plotly)
plot_geo(state_data,
                  locationmode='USA-states',
                  frame = ~Electric.Vehicle.Type) %>%
  add_trace(locations = ~State,
            z= ~Adoption,
            color= ~Adoption) %>%
  layout(geo=list(scope='usa'))



# Adoption of electric vehicles in cities (interactive)
plot_ly(
  data = city_data,
  x = ~City,
  y = ~Adoption,
  text = ~paste("City: ", City, "Adoption: ", Adoption),
  mode = "markers",
  type = "scatter",
  marker = list(size = 10, color = ~Adoption, colorscale = "Viridis")
)
