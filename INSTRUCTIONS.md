# Samknows Data Analyst Tech Test

Please add your solution code for the following questions to `R/samknows_data_analyst_tech_test.R`. 

Throughout, the main things being assessed are:

* Design        e.g. variable names, code readability, having a logical flow and process, labeling of graphs
* Data quality  e.g. paying attention to outliers, missing data etc.
* Explanations  e.g. justifying your approach, comments in the code file will be sufficient
  
We are looking for a well-commented code file, and requested outputs saved in `output/`. 

Of course if you find it easier to use R Markdown to provide explanations/justifications in a report format that will be more than acceptable. 

## 1 - Packages
Load any relevant packages for reshaping data, joining, visualization, and handling datetimes.

## 2 - Data Import and Cleaning
The files you will need are:

* `data/download_speed_measurements.csv`
* `data/upload_speed_measurements.csv`
* `data/details_for_each_person.csv`
  
Field descriptions are in `DATA_DICTIONARY.md`.

Please import these files and combine them into a single table which:

  * has the structure `person_id, city, type_of_broadband_connection, name_of_isp, average_download_speed, average_upload_speed`
  * has 1 line per person (i.e. calculate a single average download and upload speed for each person)
  * only contains people in the cities 'Samsville' and 'Databury'
  * only contains download and upload measurements which have run successfully (i.e. put a filter on did_test_complete_successfully)
  * only contains tests from the month of January 2021 (i.e. put a filter on time_of_measurement).

### (optional)

There are alternative summary statistics used to measure download speed which are more nuanced than the simple average. An example is:

**Calculate the average download speed each day, and take the 60th percentile.**

The value of this metric tells you that 'on at least 60% of days, average download speed will be at least x.'

Calculate this metric for each person in the dataset and add it into your table.

## 3 - Data Quality

Like any dataset, this one has problems; for example:
  * There are people in different cities
  * People who have differing rates of missing data
  * There are people with average speeds which are either too high or too low for their `type_of_broadband_connection` (i.e. mislabeled data)
  * There are people who have OK data overall, but some individual tests fall outside of parameters.

Highlight any data quality issues in this dataset which you think may be relevant.

## 4 - Data summarization and presentation
a) Now that you have a table which gives the average download and upload speed for each person, make a couple of summary tables which illustrate the differences in download and upload speed between ISPs for each connection type.
  * Remember that there are two ISPs (Fibrelicious and Useus) with customers in two cities (Samsville and Databury) with three different types of connection (ADSL, VDSL, and Fibre).
  * If possible, make a couple of brief comments to summarize the findings.

b) Make a visualization of the distribution of average download speeds by ISP by city by connection type, which is designed so that a technically aware stakeholder can read the graph and see the differences between ISP for each package in each city.

c) If I am a consumer living in Databury and I have a Fibre connection, am I going to get a better/worse speed from Fibrelicious or from Useus? If so, how much better/worse?

d) **(optional)** Look at the differences in download speed by ISP at different times of the day or days of the week (for example, at what times of day are download speeds highest and lowest?).
