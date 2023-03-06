---
title: "Analysis 1: UNC Salaries"
author: "FIRSTNAME LASTNAME"
date:  "°ËÔÂ 28, 2020"
output: html_document
---

# Instructions

**Overview:** For each question, show your R code that you used to answer each question in the provided chunks. When a written response is required, be sure to answer the entire question in complete sentences outside the code chunks. When figures are required, be sure to follow all requirements to receive full credit. Point values are assigned for every part of this analysis.

**Helpful:** Make sure you knit the document as you go through the assignment. Check all your results in the created HTML file.

**Submission:** Submit via an electronic document on Sakai. Must be submitted as an HTML file generated in RStudio. 

# Introduction

Universities are typically opaque, bureaucratic institutions. To be transparent to tax payers, many public schools, such as the University of North Carolina, openly report **[salary information](http://www.newsobserver.com/news/databases/public-salaries/)**. In this assignment, we will analyze this information to answer pivotal questions that have endured over the course of time. The most recent salary data for UNC-Chapel Hill faculty and staff has already been downloaded in CSV format and titled *"UNC_System_Salaries Search and Report.csv"*. If you scan the spreadsheet, you will notice that Dr. Mario is not listed. People get depressed when they see that many digits after the decimal.

To answer all the questions, you will need the R package `tidyverse` to make figures and utilize `dplyr` functions.




# Data Information

Make sure the CSV data file is contained in the folder of your RMarkdown file. First, we start by using the `read_csv` function from the `readr` package found within the tidyverse. The code below executes this process by creating a tibble in your R environment named "salary".


































