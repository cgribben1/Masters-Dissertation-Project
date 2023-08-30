library(leaflet)
library(sf)
library(viridis)
library(scales)
library(gridExtra)

district_boundaries <- st_read("C:/Users/curtu/OneDrive - Durham University/Documents/Chicago_map")


### Black Population Percentage
black_pop <- data.frame(
dist_num = c(1, 2, 3, 4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25),
pop_per = 100*dem$black..)



map_black_pop <- merge(district_boundaries, black_pop, by = "dist_num")



### Overcompensation-only
crime_data_initial <- data.frame(
dist_num = c(1, 2, 3, 4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25),
crime_rate = original_average_crime_count_by_district$per_capita)

crime_data_gen_IV <- data.frame(
dist_num = c(1, 2, 3, 4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25),
crime_rate = new_crime_count_two_year_gen_IV_by_district$per_capita)

crime_data_gen_VIII <- data.frame(
dist_num = c(1, 2, 3, 4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25),
crime_rate = new_crime_count_two_year_gen_VIII_by_district$per_capita)



### Overcompensation Plus Confirmation Bias
crime_data_initial <- data.frame(
dist_num = c(1, 2, 3, 4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25),
crime_rate = original_average_crime_count_by_district$per_capita)

crime_data_gen_IV <- data.frame(
dist_num = c(1, 2, 3, 4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25),
crime_rate = new_crime_count_two_year_gen_IV_by_district_conf$per_capita)

crime_data_gen_VIII <- data.frame(
dist_num = c(1, 2, 3, 4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25),
crime_rate = new_crime_count_two_year_gen_VIII_by_district_conf$per_capita)



map_initial <- merge(district_boundaries, crime_data_initial, by = "dist_num")

map_gen_IV <- merge(district_boundaries, crime_data_gen_IV, by = "dist_num")

map_gen_VIII <- merge(district_boundaries, crime_data_gen_VIII, by = "dist_num")



ggplot() +
  geom_sf(data = map_initial, aes(fill = crime_rate)) +
  scale_fill_gradient(low = "steelblue1", high = "red3", name = "Reported Crime Rate", limits = c(0, 0.268)) +
  labs(title = "District per Capita Crime Rates Across Chicago, Starting Condition", subtitle = "Five years of original dataset (2012 - 2016)") +
  theme_minimal()



ggplot() +
  geom_sf(data = map_gen_IV, aes(fill = crime_rate)) +
  scale_fill_gradient(low = "steelblue1", high = "red3", name = "Reported Crime Rate", limits = c(0, 0.268)) +
  labs(title = "District per Capita Crime Rates Across Chicago, Fourth Simulated Year", subtitle = "Five years of original dataset (2012 - 2016), effective two-year rolling window, 'α' = 0.1, 'β' = 0.5") +
  theme_minimal()



ggplot() +
  geom_sf(data = map_gen_VIII, aes(fill = crime_rate)) +
  scale_fill_gradient(low = "steelblue1", high = "red3", name = "Reported Crime Rate", limits = c(0, 0.268)) +
  labs(title = "District per Capita Crime Rates Across Chicago, Eighth Simulated Year", subtitle = "Five years of original dataset (2012 - 2016), effective two-year rolling window, 'α' = 0.1, 'β' = 0.5") +
  theme_minimal()






ggplot() +
  geom_sf(data = map_black_pop, aes(fill = pop_per)) +
  scale_fill_gradient(low = "bisque", high = "darkslategrey", name = "Black Population %", limits = c(0, 100)) +
  labs(title = "District Black Population Percentages Across Chicago") +
  theme_minimal()
