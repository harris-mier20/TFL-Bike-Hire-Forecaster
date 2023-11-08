# TFL-Bike-Hire-Forecaster

## Product Overview

### The Problem
Transport for London and other stakeholders, such as the primary sponsor Santander, in the TFL bike hire service need to know what areas of central London have the most demand for cycle hire infrastructure. They need to know this so they can prioritise areas that are in most need for new investment. Santander, the primary sponsor, and stakeholder, contributes £6.25 m per year to maintain and improve the service, correct allocation of resources is very important.

### The Hypothesis
If we define the ‘activity’ of a single TFL bike hire station as the sum of the number of ‘journey starts’ and ‘journey ends’ in a day, we believe there will be measurable differences in the rate of change of bike hire activity between the different central London postcodes from year to year. We also believe we will be able forecast future changes in activity. The results will help assess the feasibility of using historic data to forecast changes in demand and we can predict where future infrastructure would have the most impact.

### The Solution
We will produce a map overlay that segments the central London postcodes. User interaction with an individual postcode segment will display information including descriptive analytics of activity over different timescales, perhaps with a written possible explanation for any seasonality trends using our intuition. It will also include a forecast for future activity at that given postcode.

### The Data
TFL have made all trip data from the bike hire scheme available and easily accessible [here](https://cycling.data.tfl.gov.uk). From that link, there is a separate csv file for every 6 days of data from 2016 to 2022 where each entry records a single journey and contains the start and end station, time and date information and the journey duration. We will combine all data sets from 2018 to 2022 and restructure it so that each row instead represents an day from 2018 to mid 2023 and each column is specific to each individual station’s ‘activity’. This will enable us plot, describe and forecast the change in activity for each individual bike station from 2018 and onwards while also being able to pick up on any seasonality from day to day, week to week, and so on.

### The KPIs
The performance of the forecasting model can be assessed on its accuracy or error in future prediction using a training, validation, and test set. The performance of the product itself would be assessed on its ability to influence decision making. If the app successfully conveys the intended message and presents trends and forecasts in different central London postcodes, this should encourage the client, TFL, to correctly allocate new infrastructure in the relevant locations.

### The Risks
There is a risk that there is no observable seasonality trends or measurable differences in rate of change of activity between the central London post codes. In this case the data product can be used to forecast the change in demand for bike hire in London in general, and the results can be used to provide estimates for the resource required to maintain the service quality as a whole.

## Code Structure

### Data Preprocessing
This code is located in the data-pre-processing folder. It contains 3 R files. One file, get-postcodes.R, takes a csv file that can be found [here](https://cycling.data.tfl.gov.uk/usage-stats/371JourneyDataExtract22May2023-28May2023.csv), it identifies all the unique station names before using the google API to identify all the postcodes for the stations before filtering out postcodes that are in central london, EC and WC, it exports a reference file, which can be found in the root directory under the name 'station-postcodes-central.csv'.

Another file, data-extraction.R, scrapes and merges all data, found [here](https://cycling.data.tfl.gov.uk/), for every Santander bike journey made since 2018, some processing is done to ensure consistent formatting.

Another file, data-restructure.R, takes the output csv of this data and completes various functions to pre process the data, counting every journey started and ended for each central london station and arranges and exports a new csv that presents this data day by day. Missing data has been filled in with mean imputation. The data can be found in the root directory of this repo with the name 'tfl-bike-daily-activity-central-london.csv'

Lastly, we run another script, get-postcode-usage.R, to count the activity across multiple stations within each central London postcode, WC1, WC2, EC1, EC2, EC3, EC4. We export this data as daily-activity-by-postcode.csv and We use this data for processing with the app.

### Server Side Data Processing.
We have a script data-processing which processes the data from daily-activity-by-postcode.csv and generates multiple data frames to be referenced by the app server, before the app-master.R renders the outputs on the UI.

### App UI

