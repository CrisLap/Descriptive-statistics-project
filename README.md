Descriptive-statistics-project
Exploratory analysis of the Texas real estate market

This is a project concerning the exploratory data analysis of real estate market in Texas 🏡.

I used the dataset present in the realestate_texas.csv file. This consists of 240 observables of 8 variables, which give us information about the sales per month of the real estates of Beaumont, Bryan-College Station, Tyler and Wichita Falls, from 2010 to 2014. Going into detail, the variables are:

city: reference city; year: reference year; month: reference month; sales: total number of sales; listings: total number of active listings; volume: total value of sales in millions of dollars; median_price: median sale price in dollars; months_inventory: amount of time required to sell all the active listings at the current sales rate, in months.

I described the features of the data set, by generating, according to the variable under consideration, frequency distribution tables or summaries containing the measures of position, the measures of variability and the measures of shape. Furthermore, to better visualize trends and patterns in data, I graphically represented them, mainly using the ggplot2 package.

The project consists in the development of the following points:

Desciption of the different variable types within the dataset;

Analysis of indices of position (mean, median, quantiles, etc.), variability (dispersion index, coefficient of dispersion, relative variance, standard deviation, Gini index, etc.) and distribution form (Fisher asymmetry index, Kurtosis index);

Definition of an "Effectiveness" variable to look at which city/which year has been the most effective in terms of sales;

Use of moments, dplyr, ggplot, gghalves packages to show significative diagrams, boxplots, line charts and plots.
