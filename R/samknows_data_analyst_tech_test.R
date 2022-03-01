# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(scales)
library(reactable)
library(reactablefmtr)
library(htmltools)

# Functions ---------------------------------------------------------------

#' Prepare Speeds Data via aggregation
#'
#' @param data The dataframe of speed data
#' @param req_success Do you require successful tests only?
#' @param min_date Minimum date to include in the data
#' @param max_date_excl Maximum date to filter the data for (exclusive)
#' @param type The type of data (name used in column outputs e.g. upload/download)
#' @param percentile What percentile of "X% days you got a speed of Y or greater) as a decimal
prep_speeds <- function(data, req_success, min_date, max_date_excl, type = '', percentile = 0.6){
  
  filtered <- data %>%
    filter(
      did_test_complete_successfully | !req_success, # OR(successful_test, don't require success) - if require then test must be success, if not then always true
      time_of_measurement >= min_date,
      time_of_measurement < max_date_excl,
      )  
  
  # Return the average across all records
  average <- filtered %>%
    group_by(person_id) %>%
    summarise(across(ends_with('Mbps'), # support regardless of type
                     ~mean(.x, na.rm = TRUE), 
                     .names = 'average_{type}_speed')
    )
  
  # Calculate the daily average then get the 1-percentile to return X% of days you get at least Y
  percentiled <- filtered %>%
    group_by(person_id, floor_date(time_of_measurement, 'day')) %>%
    summarise(across(ends_with('Mbps'), 
                     ~mean(.x, na.rm = TRUE), .names = 'daily_average'),
              .groups = 'drop_last') %>% # just drop the day component of the data
    # need to do 1- as we want to capture e.g. 60% of days >= value so want the 40th percentile 
    summarise('{type}_speed_{percentile*100}_percentile' := quantile(daily_average, probs = c(1-percentile)))
  
  average %>%
    left_join(percentiled, by = 'person_id') %>%
    return()
}


# Attempt to use SamKnows colours for bonus points
colours <- c('#0049e6', '#6772e5', '#1868fb', '#37383c', '#5b6272')

# Set Params --------------------------------------------------------------

cities <- c('Samsville', 'Databury')
min_date <- ymd(20210101)
max_date <- ymd(20210201)
req_success <- TRUE
percentile <- 0.6

# Load Data ---------------------------------------------------------------

persons <- read_csv('data/details_for_each_person.csv', 
                    col_types = 'icfc') #using shorthand for column types

#used up/downstream names out of habit
ds_speed <- read_csv('data/download_speed_measurements.csv',
                     col_types = 'iTdl')
us_speed <- read_csv('data/upload_speed_measurements.csv',
                     col_types = 'iTdl')

# Combine Data ------------------------------------------------------------

ds_agg <- prep_speeds(ds_speed, req_success, min_date, max_date, 'download', percentile)
us_agg <- prep_speeds(us_speed, req_success, min_date, max_date, 'upload', percentile)

# Combine data together for future use
comb_data <- persons %>% 
  filter(city %in% cities) %>%
  left_join(ds_agg, by = c('person_id')) %>%
  left_join(us_agg, by = c('person_id'))

write.csv(comb_data, 'output/combined_data_output.csv')

# Data Quality Checks -----------------------------------------------------

# As this is more of an exploratory part I opted not to convert repeated code into functions for more
#   flexibility of changing the code even though there are repeated components.
speed_limits <- tibble::tribble(
                  ~type_of_broadband_connection, ~max_download, ~max_upload,
                                         "ADSL",         25L,           1,
                                         "VDSL",        100L,          20,
                                        "Fibre",       1000L,         Inf
                  )

## Individual Record Checks ------------------------------------------------

# Check if any specific measurements are 10% above the expected max up/download rates for each product:

ds_quality1 <- ds_speed %>%
  filter(did_test_complete_successfully) %>%
  left_join(persons, by = 'person_id') %>%
  left_join(speed_limits, by = 'type_of_broadband_connection') %>%
  mutate(too_speedy_ds_multiplier = measured_download_speed_in_Mbps/max_download) %>%
  filter(too_speedy_ds_multiplier > 1.1) # can change the tolerance of what you would accept
# Many records have download speeds far beyond expected for their technology

us_quality1 <- us_speed %>%
  filter(did_test_complete_successfully) %>%
  left_join(persons, by = 'person_id') %>%
  left_join(speed_limits, by = 'type_of_broadband_connection') %>%
  mutate(too_speedy_us_multiplier = measured_upload_speed_in_Mbps/max_upload) %>%
  filter(too_speedy_us_multiplier > 1.1)  # can change the tolerance of what you would accept
# Many records have uploads speeds far beyond expected for their technology


## Cross report issues -----------------------------------------------------

# Check if there are any customers who have a vast majority of their records above/below the expected

ds_quality2 <- ds_speed %>%
  filter(did_test_complete_successfully) %>%
  left_join(persons, by = 'person_id') %>%
  left_join(speed_limits, by = 'type_of_broadband_connection') %>%
  # create variable to measure x above expected max speed
  mutate(too_speedy_ds_multiplier = measured_download_speed_in_Mbps/max_download) %>%
  group_by(person_id) %>%
  summarise(average_too_speedy_mult = mean(too_speedy_ds_multiplier),
            perc_too_speedy = sum(too_speedy_ds_multiplier > 1.1)/n()) %>%
  filter(perc_too_speedy > 0.9) # 90% of tests or more are over 1.1x faster than the expected max
# 6 Customers probably are recorded as wrong product 


## Cross report issues
us_quality2 <- us_speed %>%
  filter(did_test_complete_successfully) %>%
  left_join(persons, by = 'person_id') %>%
  left_join(speed_limits, by = 'type_of_broadband_connection') %>%
  mutate(too_speedy_us_multiplier = measured_upload_speed_in_Mbps/max_upload) %>%
  group_by(person_id) %>%
  summarise(average_too_speedy_mult = mean(too_speedy_us_multiplier),
            perc_too_speedy = sum(too_speedy_us_multiplier > 1.1)/n()) %>%
  filter(perc_too_speedy > 0.9)
# 6 Customers probably are recorded as wrong product 

intersect(us_quality2$person_id, ds_quality2$person_id) # same 6 customers both times, these should have their product changed in the data

nrow(ds_quality1 %>% filter(!(person_id %in% ds_quality2$person_id))) # 0
nrow(us_quality1 %>% filter(!(person_id %in% us_quality2$person_id))) # 0
# It seems at the threshold set of 1.1, there are not any other unusual results (but could be some below 1.1 threshold)

# What about the other way?
ds_quality3 <- ds_speed %>%
  filter(did_test_complete_successfully) %>%
  left_join(persons, by = 'person_id') %>%
  left_join(speed_limits, by = 'type_of_broadband_connection') %>%
  mutate(not_speedy_ds_multiplier = measured_download_speed_in_Mbps/max_download) %>%
  group_by(person_id) %>%
  summarise(average_not_speedy_mult = mean(not_speedy_ds_multiplier),
            perc_not_speedy = sum(not_speedy_ds_multiplier < 0.1)/n()) %>%
  filter(perc_not_speedy > 0.9)
# Another 6 people

# Upload speeds can vary/don't have data for fibre, so won't look at them.

# store for later
people_to_exclude <- union(ds_quality3$person_id, ds_quality2$person_id)


## Other Checks ------------------------------------------------------------

persons %>% 
  group_by(person_id) %>%
  filter(n() > 1)
# No duplicate people across multiple cities 

## Check test success rates visually
ds_speed %>%
  group_by(person_id) %>%
  summarise(test_success_rate = mean(did_test_complete_successfully)) %>%
  left_join(persons, by = 'person_id') %>%
  filter(!(person_id %in% people_to_exclude)) %>%
  ggplot(aes(x = test_success_rate, fill = name_of_isp)) +
  geom_histogram() +
  facet_grid(name_of_isp~type_of_broadband_connection) +
  theme_bw() +
  theme(legend.position = 'top') +
  scale_fill_manual(values = colours) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = 'Test Success Rate',
       y = 'Count of people',
       title = 'Distribution of test success rate by ISP and technology', 
       fill = 'ISP',
       caption = 'Useus have a better test success rate than Fibrelicious, \nhowever fibre has the best success rate overall')

ggsave('output/test_success_rate_by_isp_product.png')

ds_speed %>%
  group_by(person_id) %>%
  summarise(test_success_rate = mean(did_test_complete_successfully)) %>%
  left_join(persons, by = 'person_id') %>%
  filter(!(person_id %in% people_to_exclude)) %>%
  ggplot(aes(x = test_success_rate, fill = city)) +
  geom_histogram() +
  facet_grid(city~type_of_broadband_connection) +
  theme_bw() +
  theme(legend.position = 'top') +
  scale_fill_manual(values = colours) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = 'Test Success Rate',
       y = 'Count of people',
       title = 'Distribution of test success rate by city and technology', 
       fill = 'City',
       caption = 'City seems to have little impact on test success rate')

ggsave('output/test_success_rate_by_city_product.png')
# Overall nothing to imply a data issue as such, just that the success differs by ISP and technology.


## Findings ----------------------------------------------------------------

# Test success rate differs noticeably by ISP and technology - not necessary a data quality issue but something to be aware of
# There are 6 customers who almost certainly have the wrong product recorded within the data (which aligns with what is in `simulate_test_data.R` I found after)
# There are no other tests outside of expected (+10%) speed parameters
# There are not duplicate customers
# Test success rate does not vary noticeably by city


# Summarisation and Plots -------------------------------------------------

# Tables
#
pivoted_data <- comb_data %>% 
  filter(!(person_id %in% people_to_exclude)) %>%
  group_by(type_of_broadband_connection, city, name_of_isp) %>%
  summarise(across(starts_with('average'), .fns = list(min = min, mean = mean, max = max), .names = '{.fn}_{.col}'),
            vol = n(),
            .groups = 'drop') %>%
  pivot_wider(names_from = name_of_isp, values_from = where(is.numeric))
  
# There is a smarter way to do this by making a function, but it would involve a lot of work 
# dealing with the different column names (or probably easier just reformatting the data itself)
# so I have just left the code duplicated with column name changes (as such only one is commented)
ds_table <- reactable(pivoted_data %>% select(-contains('upload')), # filter to just download columns
          columns = list( # define list of columns for the table and their formats
            city = colDef(name = 'City'), 
            type_of_broadband_connection = colDef(name = 'Technology'), 
            min_average_download_speed_Fibrelicious = colDef(name = 'Min',
                                                             format = colFormat(digits = 2), #2 decimal places
                                                             aggregate = "min", # how to aggregate when grouped up
                                                             header = function(value) { # put the unit in the header
                                                               units <- div(style = "color: #999", "Mbps")
                                                               div(title = value, value, units)}
                                                             ),
            mean_average_download_speed_Fibrelicious = colDef(name = 'Mean',
                                                              format = colFormat(digits = 2),
                                                              aggregate = "mean",
                                                              header = function(value) {
                                                                units <- div(style = "color: #999", "Mbps")
                                                                div(title = value, value, units)}
                                                              ), 
            max_average_download_speed_Fibrelicious = colDef(name = 'Max',
                                                             format = colFormat(digits = 2),
                                                             aggregate = "max",
                                                             header = function(value) {
                                                               units <- div(style = "color: #999", "Mbps")
                                                               div(title = value, value, units)}
                                                             ),
            min_average_download_speed_Useus = colDef(name = 'Min',
                                                      format = colFormat(digits = 2),
                                                      aggregate = "min",
                                                      header = function(value) {
                                                        units <- div(style = "color: #999", "Mbps")
                                                        div(title = value, value, units)}
                                                      ), 
            mean_average_download_speed_Useus = colDef(name = 'Mean',
                                                       format = colFormat(digits = 2),
                                                       aggregate = "mean",
                                                       header = function(value) {
                                                         units <- div(style = "color: #999", "Mbps")
                                                         div(title = value, value, units)}
                                                        ), 
            max_average_download_speed_Useus = colDef(name = 'Max',
                                                      format = colFormat(digits = 2),
                                                      aggregate = "max",
                                                      header = function(value) {
                                                        units <- div(style = "color: #999", "Mbps")
                                                        div(title = value, value, units)}
                                                      ),
            vol_Fibrelicious = colDef(name = 'Volume',
                                      aggregate = "sum"), 
            vol_Useus = colDef(name = 'Volume',
                               aggregate = "sum")
          ),
          columnGroups = list( # Create grouped column headers for the ISPs for nicer titles
            colGroup(name = 'Fibrelicious (Download)', columns = c('min_average_download_speed_Fibrelicious', 'mean_average_download_speed_Fibrelicious', 'max_average_download_speed_Fibrelicious', 'vol_Fibrelicious')),
            colGroup(name = 'Useus(Download)', columns = c('min_average_download_speed_Useus', 'mean_average_download_speed_Useus', 'max_average_download_speed_Useus', 'vol_Useus'))
          ), 
          groupBy = 'type_of_broadband_connection', # Group rows by type of product as this is a bigger impact over city
          bordered = TRUE, striped = TRUE, highlight = TRUE, filterable = TRUE, # themeing stuff
          theme = reactableTheme(
            color = 'white',
            backgroundColor = colours[4],
            borderColor = colours[5],
            stripedColor = colours[2],
            highlightColor = colours[3],
            style = list(fontFamily = "Segoe UI, Helvetica, Arial, sans-serif"),
          )        
          )
# save table
save_reactable(ds_table, 'output/download_speed_summary.html')

us_table <- reactable(pivoted_data %>% select(-contains('download')),
          columns = list(
            city = colDef(name = 'City'), 
            type_of_broadband_connection = colDef(name = 'Technology'), 
            min_average_upload_speed_Fibrelicious = colDef(name = 'Min',
                                                             format = colFormat(digits = 2),
                                                             aggregate = "min",
                                                             header = function(value) {
                                                               units <- div(style = "color: #999", "Mbps")
                                                               div(title = value, value, units)}
            ),
            mean_average_upload_speed_Fibrelicious = colDef(name = 'Mean',
                                                              format = colFormat(digits = 2),
                                                              aggregate = "mean",
                                                              header = function(value) {
                                                                units <- div(style = "color: #999", "Mbps")
                                                                div(title = value, value, units)}
            ), 
            max_average_upload_speed_Fibrelicious = colDef(name = 'Max',
                                                             format = colFormat(digits = 2),
                                                             aggregate = "max",
                                                             header = function(value) {
                                                               units <- div(style = "color: #999", "Mbps")
                                                               div(title = value, value, units)}
            ),
            min_average_upload_speed_Useus = colDef(name = 'Min',
                                                      format = colFormat(digits = 2),
                                                      aggregate = "min",
                                                      header = function(value) {
                                                        units <- div(style = "color: #999", "Mbps")
                                                        div(title = value, value, units)}
            ), 
            mean_average_upload_speed_Useus = colDef(name = 'Mean',
                                                       format = colFormat(digits = 2),
                                                       aggregate = "mean",
                                                       header = function(value) {
                                                         units <- div(style = "color: #999", "Mbps")
                                                         div(title = value, value, units)}
            ), 
            max_average_upload_speed_Useus = colDef(name = 'Max',
                                                      format = colFormat(digits = 2),
                                                      aggregate = "max",
                                                      header = function(value) {
                                                        units <- div(style = "color: #999", "Mbps")
                                                        div(title = value, value, units)}
            ),
            vol_Fibrelicious = colDef(name = 'Volume',
                                      aggregate = "sum"), 
            vol_Useus = colDef(name = 'Volume',
                               aggregate = "sum")
          ),
          columnGroups = list(
            colGroup(name = 'Fibrelicious (Upload)', columns = c('min_average_upload_speed_Fibrelicious', 'mean_average_upload_speed_Fibrelicious', 'max_average_upload_speed_Fibrelicious', 'vol_Fibrelicious')),
            colGroup(name = 'Useus(Upload)', columns = c('min_average_upload_speed_Useus', 'mean_average_upload_speed_Useus', 'max_average_upload_speed_Useus', 'vol_Useus'))
          ), 
          groupBy = 'type_of_broadband_connection',
          bordered = TRUE, striped = TRUE, highlight = TRUE, filterable = TRUE,
          theme = reactableTheme(
            color = 'white',
            backgroundColor = colours[4],
            borderColor = colours[5],
            stripedColor = colours[2],
            highlightColor = colours[3],
            style = list(fontFamily = "Segoe UI, Helvetica, Arial, sans-serif"),
          )        
)

save_reactable(us_table, 'output/upload_speed_summary.html')

# FINDINGS 
# In general (as seen in the next plot) ADSL has a superior speed with Useus, regardless of city
# However for VDSL and Fibre, the users of Fibrelicious have a high speed - with in both cases
# customers in Samsville having higher speeds compared to those in Databury by c.15-20Mbps in VDSL and c.10-20Mbps on Fibre.

# Plots
comb_data %>%
  filter(!(person_id %in% people_to_exclude)) %>% # remove people with wrong product
  ggplot(aes(x = average_download_speed, y = name_of_isp, fill = name_of_isp)) +
  geom_boxplot() + # shows spread of data and in this case how little overlap there is
  facet_grid(city~type_of_broadband_connection, scales = 'free') + # free scales due to vastly different ranges per product
  theme_bw() +
  theme(legend.position = 'top',
        text = element_text(colour = colours[4]),
        plot.subtitle = element_text(colour = colours[5])) +
  scale_fill_manual(values = colours) +
  labs(x = 'Average Download Speed (Mbps)',
       y = 'Internet Service Provider',
       title = 'Average Download Speeds by ISP, City, and Technology', 
       subtitle = 'Useus provide better speeds for ADSL, \nbut Fibrelicious are faster on VDSL+Fibre.',
       fill = 'ISP',
       caption = 'NOTE: x-axis scales are different per-technology') 
  
ggsave('output/download_speed_comparison.png')

# Question: If I am a consumer living in Databury and I have a Fibre connection, am I going to get a better/worse speed from Fibrelicious or from Useus? If so, how much better/worse?
# Answer: Based on the graph, better speeds with Fibrelicious (as their name suggests), by about ~40Mbps download.

# Day time impact - a little slow to producce.
ds_speed %>%
  left_join(persons, by = 'person_id') %>%
  filter(did_test_complete_successfully,
         !(person_id %in% people_to_exclude)) %>%
  mutate(time_of_day = `date<-`(time_of_measurement, '2000-01-01'), # this is a hack to get the axis format to just display hours as scale_time has less formatting options...
         dow = wday(time_of_measurement, label = TRUE)) %>% # lubridate provides an ordered factor by default
  ggplot(aes(x = time_of_day, y = measured_download_speed_in_Mbps, group = name_of_isp, fill = name_of_isp)) +
  geom_smooth() +
  #geom_point(alpha = 0.2) + # absolutely useless do not do this.
  facet_grid(type_of_broadband_connection~dow, scales = 'free_y') +
  theme_bw() +
  theme(legend.position = 'top',
        text = element_text(colour = colours[4]),
        plot.subtitle = element_text(colour = colours[5])) +
  scale_fill_manual(values = unname(colours[c(1, 4)])) +
  scale_x_datetime(labels = scales::date_format('%H')) +
  # add lines at 6pm and 10pm for peak times
  geom_vline(xintercept = ymd_hms(20000101180000), linetype = 'dashed', col = colours[2])+
  geom_vline(xintercept = ymd_hms(20000101220000), linetype = 'dashed', col = colours[2])+
  labs(x = 'Hour of the Day',
       y = 'Download Speed (smoothed)',
       title = 'Download Speeds by time of day', 
       subtitle = 'Performance dips in the evening (between 6 and 10pm) when traffic increases across most products\n
       Lower speed ISP for ADSL and VDSL seem to be more stable throughout the peak time',
       fill = 'ISP',
       caption = 'NOTE: y-axis scales are different per-technology')

ggsave('output/time_of_day_comparison.png')
