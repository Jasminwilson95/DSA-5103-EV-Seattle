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

filtered_data <- ev.data %>%
  filter(State != "WA")

head(filtered_data)

# Remove all Non-WA state data
ev.data <- ev.data %>%
  filter(State == "WA")

# Even after removing other states, there are NA's in vehicle location 
# Cannot remove these as they contain important base MSRP values that can be used for evaulation
# Since there are Long Beach rows available, impute those values

ev.data <- ev.data %>%
  mutate(Vehicle.Location = ifelse
         (City == "Long Beach" & is.na(Vehicle.Location), 
           Vehicle.Location[complete.cases(Vehicle.Location)][1], Vehicle.Location))
#Mostly Clean data

ev.numeric <- ev.data %>% select(where(is.numeric))
ev.char <- ev.data %>% select(where(is.character))
ev.logical <- ev.data %>% select(where(is.logical))

# Separate latitude and longitude into two columns
ev.data <- ev.data %>%
  separate("Vehicle.Location", into = c("Type", "Longitude", "Latitude"), sep = " ")
ev.data$Longitude <- gsub("\\(|\\)", "", ev.data$Longitude)
ev.data$Latitude <- gsub("\\(|\\)", "", ev.data$Latitude)

ev.data <- ev.data %>%
  select(-Type)

# Convert the separated columns to numeric (if necessary)
ev.data$Latitude <- as.numeric(ev.data$Latitude)
ev.data$Longitude <- as.numeric(ev.data$Longitude)


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
  summarise(Adoption = n(), ev.rate = Adoption / nrow(ev.data) * 100)

# city-level in WA electric vehicle adoption data
county_data <- ev.data %>%
  filter(State == 'WA' &
           Electric.Vehicle.Type %in% c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)")) %>%
  group_by(County, Electric.Vehicle.Type) %>%
  summarise(Adoption = n(), ev.rate = Adoption / nrow(ev.data) * 100)

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


ggplot(city_data, aes(x = City, y = ev.rate, fill = Electric.Vehicle.Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
  labs(title = "Electric Vehicle Adoption by City", x = "City", y = "EV Adoption Rate (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(county_data, aes(x = County, y = ev.rate, fill = Electric.Vehicle.Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8) +
  labs(title = "Electric Vehicle Adoption by County", x = "County", y = "EV Adoption Rate (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

# Geographical Trend Analysis

# EV Adoption Rates by County
county_ev.rates <- ev.data %>%
  group_by(County) %>%
  summarise(ev.count = n(), ev.rate = ev.count / nrow(ev.data) * 100) %>%
  arrange(desc(ev.rate))

# Plotting EV Adoption Rates by County
ggplot(county_ev.rates, aes(x = reorder(County, ev.rate), y = ev.rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "EV Adoption Rates by County",
       x = "County",
       y = "EV Adoption Rate (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EV Adoption Rates by City
city_ev.rates <- ev.data %>%
  group_by(City) %>%
  summarise(ev.count = n(), ev.rate = ev.count / nrow(ev.data) * 100) %>%
  arrange(desc(ev.rate))

# Plotting EV Adoption Rates by City
ggplot(city_ev.rates, aes(x = reorder(City, ev.rate), y = ev.rate)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "EV Adoption Rates by City",
       x = "City",
       y = "EV Adoption Rate (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Factors Contributing to Regional Differences

# EV Type Distribution
ev_type_distribution <- ev.data %>%
  group_by(`Electric.Vehicle.Type`) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plotting EV Type Distribution
ggplot(ev_type_distribution, aes(x = reorder(`Electric.Vehicle.Type`, count), y = count)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "EV Type Distribution",
       x = "EV Type",
       y = "Count")

# EV Make Distribution
ev_make_distribution <- ev.data %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plotting EV Make Distribution
ggplot(ev_make_distribution, aes(x = reorder(Make, count), y = count)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "EV Make Distribution",
       x = "EV Make",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EV Model Distribution
ev_model_distribution <- ev.data %>%
  group_by(Model) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plotting EV Model Distribution
ggplot(ev_model_distribution, aes(x = reorder(Model, count), y = count)) +
  geom_bar(stat = "identity", fill = "orchid") +
  labs(title = "EV Model Distribution",
       x = "EV Model",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Legislative District Policies (if available in the dataset)

# ... (add code to analyze legislative district policies if available)

# Electric Utility Distribution
electric_utility_distribution <- ev.data %>%
  group_by(`Electric.Utility`) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plotting Electric Utility Distribution
ggplot(electric_utility_distribution, aes(x = reorder(`Electric.Utility`, count), y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Electric Utility Distribution",
       x = "Electric Utility",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(arules)
# Select columns for analysis (adjust as needed)
ev_subset <- ev.data[, c("County", "City", "Electric.Vehicle.Type", "Make", "Model", "Electric.Utility")]

# Convert the data to transactions
transactions <- as(ev_subset, "transactions")

# Mine association rules
rules <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.7))

# Print the rules
inspect(rules)

# Assuming 'ev_data' is your dataset
# Replace 'ev_data' with your actual dataset name

# Load necessary libraries
library(tidyverse)

# Top county by frequency
top_county <- ev.data %>%
  group_by(County) %>%
  summarise(MakeCount = n()) %>%
  arrange(desc(MakeCount)) %>%
  slice_head(n = 1)

# Top 10 makes by frequency in the top county
top_makes_top_county <- ev.data %>%
  filter(County == top_county$County) %>%
  group_by(Make) %>%
  summarise(MakeCount = n()) %>%
  arrange(desc(MakeCount)) %>%
  slice_head(n = 10)

# Plot top 10 makes by frequency in the top county
ggplot(top_makes_top_county, aes(x = fct_reorder(Make, MakeCount), y = MakeCount, fill = Make)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Top 10 Electric Vehicle Makes by Frequency in", top_county$County, "County"),
       x = "Electric Vehicle Make",
       y = "Frequency",
       fill = "Make") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top city by frequency
top_city <- ev.data %>%
  group_by(City) %>%
  summarise(MakeCount = n()) %>%
  arrange(desc(MakeCount)) %>%
  slice_head(n = 1)

# Top 10 makes by frequency in the top city
top_makes_top_city <- ev.data %>%
  filter(City == top_city$City) %>%
  group_by(Make) %>%
  summarise(MakeCount = n()) %>%
  arrange(desc(MakeCount)) %>%
  slice_head(n = 10)

# Plot top 10 makes by frequency in the top city
ggplot(top_makes_top_city, aes(x = fct_reorder(Make, MakeCount), y = MakeCount, fill = Make)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Top 10 Electric Vehicle Makes by Frequency in", top_city$City, "City"),
       x = "Electric Vehicle Make",
       y = "Frequency",
       fill = "Make") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Top county by frequency
top_county <- ev_data %>%
  group_by(County) %>%
  summarise(ModelCount = n()) %>%
  arrange(desc(ModelCount)) %>%
  slice_head(n = 1)

# Top 10 models by frequency in the top county
top_models_top_county <- ev.data %>%
  filter(County == top_county$County) %>%
  group_by(Model) %>%
  summarise(ModelCount = n()) %>%
  arrange(desc(ModelCount)) %>%
  slice_head(n = 10)

# Plot top 10 models by frequency in the top county
ggplot(top_models_top_county, aes(x = fct_reorder(Model, ModelCount), y = ModelCount, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Top 10 Electric Vehicle Models by Frequency in", top_county$County, "County"),
       x = "Electric Vehicle Model",
       y = "Frequency",
       fill = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top city by frequency
top_city <- ev_data %>%
  group_by(City) %>%
  summarise(ModelCount = n()) %>%
  arrange(desc(ModelCount)) %>%
  slice_head(n = 1)

# Top 10 models by frequency in the top city
top_models_top_city <- ev.data %>%
  filter(City == top_city$City) %>%
  group_by(Model) %>%
  summarise(ModelCount = n()) %>%
  arrange(desc(ModelCount)) %>%
  slice_head(n = 10)

# Plot top 10 models by frequency in the top city
ggplot(top_models_top_city, aes(x = fct_reorder(Model, ModelCount), y = ModelCount, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Top 10 Electric Vehicle Models by Frequency in", top_city$City, "City"),
       x = "Electric Vehicle Model",
       y = "Frequency",
       fill = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Assuming 'ev_data' is your dataset
# Replace 'ev_data' with your actual dataset name

# Load necessary libraries
library(tidyverse)

# Top 10 models by frequency in WA state
top_models_wa <- ev.data %>%
  group_by(Model) %>%
  summarise(ModelCount = n()) %>%
  arrange(desc(ModelCount)) %>%
  slice_head(n = 10)

# Plot top 10 models by frequency in WA state
ggplot(top_models_wa, aes(x = fct_reorder(Model, ModelCount), y = ModelCount, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Electric Vehicle Models by Frequency in WA State",
       x = "Electric Vehicle Model",
       y = "Frequency",
       fill = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top 10 makes by frequency in WA state
top_makes_wa <- ev.data %>%
  group_by(Make) %>%
  summarise(MakeCount = n()) %>%
  arrange(desc(MakeCount)) %>%
  slice_head(n = 10)

# Plot top 10 makes by frequency in WA state
ggplot(top_makes_wa, aes(x = fct_reorder(Make, MakeCount), y = MakeCount, fill = Make)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Electric Vehicle Makes by Frequency in WA State",
       x = "Electric Vehicle Make",
       y = "Frequency",
       fill = "Make") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Identify the top 10 makes by frequency
top_makes <- ev.data %>%
  group_by(Make) %>%
  summarise(MakeCount = n()) %>%
  arrange(desc(MakeCount)) %>%
  slice_head(n = 10)

# Filter the dataset to include only the top 10 makes
ev_data_top_makes <- ev.data %>%
  filter(Make %in% top_makes$Make)

# Calculate the average electric range by make
avg_range_by_make <- ev_data_top_makes %>%
  group_by(Make) %>%
  summarise(AvgElectricRange = mean(Electric.Range))

# Plot the average electric range for the top 10 makes
ggplot(avg_range_by_make, aes(x = Make, y = AvgElectricRange, fill = Make)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Electric Range by Top 10 Makes",
       x = "Make",
       y = "Average Electric Range") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Identify the top 10 makes by frequency
top_models <- ev.data %>%
  group_by(Model) %>%
  summarise(ModelCount = n()) %>%
  arrange(desc(ModelCount)) %>%
  slice_head(n = 10)

# Filter the dataset to include only the top 10 makes
ev_data_top_models <- ev.data %>%
  filter(Model %in% top_models$Model)

# Calculate the average electric range by make
avg_range_by_models <- ev_data_top_models %>%
  group_by(Model) %>%
  summarise(AvgElectricRange = mean(Electric.Range))

# Plot the average electric range for the top 10 makes
ggplot(avg_range_by_models, aes(x = Model, y = AvgElectricRange, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Electric Range by Top 10 Models",
       x = "Model",
       y = "Average Electric Range") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Group by Make and Model, calculate average electric range
avg_range_by_make_model <- ev.data %>%
  group_by(Make, Model) %>%
  summarise(AvgElectricRange = mean(Electric.Range)) %>%
  arrange(desc(AvgElectricRange)) %>%
  slice_head(n = 10)

# Plot the top 10 average electric range by makes and models
ggplot(avg_range_by_make_model, aes(x = reorder(paste(Make, Model), AvgElectricRange), y = AvgElectricRange)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Average Electric Range by Makes and Models",
       x = "Make and Model",
       y = "Average Electric Range") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))