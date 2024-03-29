# Data entry workflow for occupancy data within the National Park Service

## Introduction
This repository contains `R` files for the `RShiny` web app that is used to enter Person Per Vehicle (PPV) data for the National Park Service.
> [!IMPORTANT]
> This app was designed by myself and is the current workflow that is being used by the National Park Service to enter PPV data.

## Overview
In order to estimate visitation to parks within the National Park Service, some parks will use vehicle counters in conjunction with PPV multipliers to get an estimate of the number of people that enter the park. This requires that the park staff survey vehicles entering the park during random sampling periods over the course of the year.
These sampling periods are 1 hour long and include representation of all days of the week, all months of the year, and all times of the day.
These data points need to converted into a consistent format that can be used for analysis.

## Motivation
Conducting PPV analysis of parks within the National Park Service can be a very time consuming task. 
Various parks may have hundreds of cars that enter every hour, entering these data points can become very tedious. 
This `RShiny` web app is used as a tool to help automate the data entry process and allow more time for analysis.
a
## Data Format
Below are some examples for the format of the data that is used as input and output for the app.

### Tallysheet example (input data)

| | **1**          | **2**          | **3**          | **4**          | **5**          | **6**          | **7+**          | |
|----------------|------------|------------|------------|------------|------------|------------|------------|------------------|
| **Tallymarks**| Tallymarks | Tallymarks | Tallymarks | Tallymarks | Tallymarks | Tallymarks | Tallymarks |                |
| **Total Vehicles**| 23         | 34         | 10         | 14         | 4          | 4          | 1          | **TOTAL**          |
| **Person Multiplie**| x1      | x2         | x3         | x4         | x5         | x6         | **sum of entries**  |  |
| **Total People**| 23         | 68         | 30         | 56         | 20         | 24         | 8          | **TOTAL**          |

### 2-page XLSX example (output data)
| **Observation Number** | **Entrance** | **Month** | **Date** | **Day of week** | **Time of day** | **Occupants** |
|------------------------|--------------|-----------|----------|-----------------|-----------------|--------------|
| 1                      | Main         | October   | 10/1/23  | Saturday        | PM              | 1            |
| 2                      | Main         | October   | 10/1/23  | Saturday        | PM              | 1            |
| 3                      | Main         | October   | 10/1/23  | Saturday        | PM              | 2            |
| 4                      | East         | October   | 10/1/23  | Saturday        | PM              | 1            |
| 5                      | East         | October   | 10/1/23  | Saturday        | PM              | 4            |
| 6                      | East         | October   | 10/1/23  | Saturday        | PM              | 8            |

| **Sheet Number** | **Number of Occupants** | **Entrance** | **Date** | **Month** | **Day of week** | **Time of day** |
|------------------|-------------------------|--------------|----------|-----------|-----------------|-----------------|
| 1                | 136                     | Main         | 10/1/23  | October   | Saturday        | PM              |
| 2                | 61                      | East         | 10/1/23  | October   | Saturday        | AM              |
| 3                | 23                      | East         | 10/2/23  | October   | Sunday          | PM              |
| 4                | 114                     | Main         | 10/2/23  | October   | Sunday          | AM              |
| 5                | 69                      | Main         | 10/5/23  | October   | Wednesday       | PM              |

## App Structure
The app is broken up into 2 main sections: `Data Entry` and `Insights`.

### Data Entry Page
Below is a screenshot of the `Data Entry` page of the app. The app was designed with the user in mind, so the app is very intuitive and easy to use. Adding and removing sheets are as simple as clicking a button.
If the user has to step away from the app, they can save their progress to an .xlsx file and append to it later. The app also automatically formats the data to the correct format. Doing this manually in Excel can allow for human error, so this is a very useful feature. Some parks have multiple lanes of traffic, so the app allows data entry for multiple lanes and automatically combines the data into a single number, (again, this is to reduce human error).
![alt text](docs/data-entry.png)

### Insights Page
The Insights Page was designed to give the end-user a quick yet powerful way to visualize and analyze their data with `Poisson
regression`. By uploading the data they previously entered, the app will automatically run a `Poisson regression` and output the results in a conditional formatted table. This gives the end-user a detailed summary of the shape of their data as well as any anomalies that may be present. If the user wants to dig deeper they can apply custom date ranges for `Poisson regression` and visualize it with a Gannt chart. To test wether these date ranges are statistically significant, the app will run a `Poisson regression` and output a graph with the mean and 95% confidence intervals. This allows for the end-user to quickly apply different combinations of date ranges to see what the best fit is for their data.

#### Data Tab
![alt text](docs/insights-data.png)
The data tab allows the user to see the data in a table format. They can sort columns, or search for specific values. This might be useful if the user wants to see the data directly before running any further analysis.

#### Tables Tab
![alt text](docs/insights-tables.png)
The tables tab is where the end-user will spend most of their time. This tab contains statistic cards at the top that give a quick view of their data shape as well as Min/Max/Mean/Median values. The tab also contains conditionally formatted tables split by month, day of the week, as well as the time of day. At a quick glance the end-user can see which months dominate in terms of visitation, and what days of the week and times of day are the busiest. As seen in the screenshot below, although there are more vehicles that enter the park in May, the average number of occupants per vehicle is higher in July. This can be a valuable insight for the end-user to consider when changing how they estimate visitation.

#### Charts Tab
<p float="left">
    <img src="docs/insights-charts-1.png" height="400" />
    <img src="docs/insights-charts-2.png" height="400" /> 
</p>
The charts tab is where the end-user can visualize the data with histograms and scatter plots. The histograms can be split by month or aggregated by all months. The scatter plot shows the average across all months which can be useful in choosing how to group the data. These charts are rendered in conjunction with `ggplot2` and `Plotly.js` which allows for the end-user to interact with the visuals and download them as images for sharing.

#### Custom Tab
![alt text](docs/insights-custom-1.png)
The custom tab is where the end-user can apply custom date ranges to a `Poission regression` and visualize not only the 95% confidence intervals, but also a Gannt chart that shows the date ranges over the year. This can be useful in seeing where one season ends and another begins, especially in cases where one season may begin towards the end of the year and ends at the beginning of the next year (i.e winter seasons).