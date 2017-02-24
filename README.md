# MRSA_NHAMCS
NHAMCS Data Read-in &amp; Descriptive Analysis

# Context

R code for the final project for BUSPH Biostatistics 845 (Applied Statistical Modeling and Programming in R). Built to improve 2016 summer research on the analysis of National Hospital Ambulatory Medical Care Survey (NHAMCS) for explorary analysis of skin and soft-tissue infections (SSTIs) for high-risk populations (homeless) based on the current literature on MRSA skin infection/colonization in communities.

The commit includes...
 - interactive function for .eta (Stata) file search in directory and read-in for R
 - application of svy package (http://r-survey.r-forge.r-project.org/survey/) to obtain appropriate estimates from complex sample survey
 - development of generalized SSTI diagnosis based on ICD-9 using conditional pattern recognition with grepl()
 - simple descriptives, Chi-square, univariate and multivariate logistic regressions

# Motivation

Currently there are no available packages or code to download and read-in NHAMCS data directly into R (*only exists for SAS and Stata). Alongside investigating MRSA and SSTIs in the homeless at emergency departments, the code was a preliminary attempt to develop a optimal method to read in NHAMCS datasets into R.
