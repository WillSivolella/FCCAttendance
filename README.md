# FC Cincinnati Attendance Analysis

## Overview

This repository contains data and analysis tools for understanding factors influencing match attendance in FC Cincinatti league matches and forecasting attendance for the 2023 season. The project aims to identify key determinants of match attendance and provide insights for improving turnout.

## Repository Structure

- **.RData** and **.Rhistory**: R session information and command history.
- **2021.2022Data.twb**: Tableau workbook containing visualizations for the 2021 and 2022 seasons.
- **AttendanceData.csv**: Combined collected and forecasted data; used for forecasting.
- **AttendanceDataPresentation.pptx**: Presentation of findings and insights.
- **AttendanceForecastData.csv**: Forecasted 2023 attendance.
- **AttendanceTrainData.csv**: Collected data relevant to 2021 and 2022 seasons.
- **DataVisualizationAndAnalysis.R**: R script for data cleaning, visualization, analysis and forecasting.
- **Forecast.twb**: Tableau workbook for attendance forecasts for 2023 season.
- **ForecastData.csv**: Dataset containing forecasted attendance.

## Getting Started

To use this repository:

1. Clone or download the repository to your local machine.
2. Open `DataVisualizationAndAnalysis.R` in RStudio or your preferred R environment to explore the dataset and run initial analyses.
3. Use Tableau Desktop to open `.twb` files for interactive visualizations and forecasts.
4. Review the `AttendanceDataPresentation.pptx` for a summary of findings and insights.

## Data Overview

The attendance datasets include the following key columns:

- Weekend: If match is on weekend or weekday
- Promotion.Night: If there is a promotion
- FCC.Rank: FC cincinnati table position
- Opp.Rank: Opposition table position
- Attendance: The number of attendees.

## Analysis and Forecasting

The analysis involves:

- Exploring trends and patterns in past attendance data.
- Identifying factors that significantly impact attendance (e.g., team rankings, promotion nights, weekend/weekday).
- Developing predictive models to forecast future match attendance.
