My activities during the statistics undergrad research internship in 2025 spring. The aim was to fit time-series models to daily South Korean electricity demand from 2015 to 2024 and make one-step rolling predictions. I have used the results for my bachelor's thesis. I have conducted all the procedures in a Korean environment. Language settings may negatively interfere to the results. 

The R script api.R downloads the calendar data (public holidays of South Korea) via the Korean government's public API service. Insert your own API key. 
The R script code_v3.R is a raw exposition of the data pre-processing, fitting, and diagnostics procedures. The order of the script is generally but not strictly linear. 

Time-series models used are Holt-Winters, ARIMA (both with and without seasonal components), and AR-GARCH. External variables are quarterly GDP, daily temperatures, weekends and holidays of South Korea. 
