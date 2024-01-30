install.packages("tidyverse")
library ("tidyverse")
data("diamonds")
view(diamonds)
head(diamonds)
colnames(diamonds)
## Creating own data frame.
names <- c("Kossie", "vide","cintie")
page <- c(24, 20, 22)
people<-data.frame(names,age)
data()
booking <- read.csv ("hotel_book.csv")
library (readxl)
demodata <- read_excel("demo.xls")
head(demodata)
rename (penguins, species_new = species)
rename_with(penguins, tolower)
penguins2 <- penguins %>% 
  arrange (-bill_length_mm)
penguins %>% 
  group_by(species,island)%>%
  drop_na()%>%
  summarise(max_length = max(bill_length_mm), mean_length = mean(bill_length_mm))
head(booking)
booking %>%
  select (hotel, is_canceled, lead_time)
booking2 <- booking %>% select(hotel, is_canceled, lead_time)
view(booking2)
rename (booking2, hotel_type = hotel)
booking3 %>%
  group_by (hotel)%>%
  drop_na()%>%
  summarise(mean_lead_time= mean(lead_time), max_lead_time = max(lead_time))
booking3 <- arrange(booking2,-lead_time)

