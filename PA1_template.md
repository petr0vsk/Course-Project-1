title |	author | date |	output
------|--------|------|-------
Reproducible Research: Peer Assessment 1 | Aleksander Petrovskii | March 21, 2017 | html_document |


# Reproducible Research: Peer Assessment 1
*** 
## 1. Loading and preprocessing the data

> knitr::opts_chunk$set(echo = TRUE)  
> rm(list=ls())  
> library(tidyr)  
> library(dplyr)  
> library(stringr)  
> library(ggplot2)  
> project.dir <- "/home/petr0vsk/Project3"  
> stopifnot( dir.exists(file.path(project.dir))  )  
> setwd(file.path(project.dir))  
> steps.raw <- read.csv("activity.csv",  header = TRUE)   
> str(steps.raw)  

