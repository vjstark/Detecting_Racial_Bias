# Loan Sharks: Detecting bias against minorities in home loan applications #

The goal of this project is to analyze if there are discriminatory policies followed in home loan lending by banks and other financial institutions.
We implement various steps in this end-to-end data science project.

* ### Data Collection
Find relevant datasets which can help solve the problem
* ### Data Cleaning
Fix missing values and any ambiguity in dataset
* ### Exploratory Data Analysis
Find underlying trends in data
* ### Feature Engineering and Selection: 
Pick relevant features from data and create some new features from existing ones
* ### Data Modelling
Fit the data to a model to do predictive analysis
* ### Model Validation
Evaluate model performance
* ### Model Deployment
Integrate model to production environment.

## Motivation

There have been a number of reports like the following:

- [NPR Gap between white and black homeownership in Baltimore](https://www.npr.org/2018/08/07/632497683/in-baltimore-the-gap-between-white-and-black-homeownership-persists)

- [UrbanWire. What explains the gap between black and white young adults](https://www.urban.org/urban-wire/what-explains-homeownership-gap-between-black-and-white-young-adults)

- [CNN. Racial bias cost](https://www.cnn.com/2018/12/06/perspectives/black-home-ownership-undervalued-brookings/index.html)

- [Why does a homeownership gap exist between whites and minorities](https://www.urban.org/urban-wire/what-explains-homeownership-gap-between-black-and-white-young-adults)


The overarching theme remains the same. Minorities such as African Americans, Hispanics, Latinos, Asians, etc. continue to struggle to obtain financing for purchasing/refinancing homes at a higher rate when compared with whites.

## Data Sources
Our primary data source is the [HMDA](https://www.consumerfinance.gov/data-research/hmda/historic-data/) data provided by the Consumer Protection Financial Bureau for the years 2007-08 and 2014-17. The period of 2007-2008 was one when the housing recession in America was at its peak and the trends during this duration give us insights into the house buying habits of Americans when the economy was bad thus helping make our analysis more accurate.

Some other data sources that we incorporated

* [Zillow Home Value Dataset](https://www.zillow.com/research/data/)

* [Census Dataset](https://www.census.gov/data/datasets/2010/dec/summary-file-1.html)

We have put together the datasets that we used throughout the project. You can find it [here](https://drive.google.com/open?id=1Uu54SK0kIauKGvFao5rE0_NqrlMBEHJU)


Let�s look at some technicalities, beginning with the structure of this repository. 

* ### Documents
It has all the documentation for this project, including project proposal, project plan, some information about loan
approval process in the United States and final report.


* ### src
This folder has all the source code regarding the project. We have used R programming and code is in the form of R notebook.
In this folder, we have implemented each step mention above.

> #### EDA
Exploratory Data analysis on primary data to find the underlying trends in the data. EDA is done for each state(IL and PA) and for various years

> #### Secondary_Data
EDA on secondary datasets from Zillow (Home Value Data per county) and Homeownership Share Data.

> #### Feature Selection
Implementation of feature engineering and selection. 

> #### Train
Build relevant models according per year to predict whether the applicant will get loan or not.

> #### Test
Scripts to verify model implementation.

> #### Utils
R scripted helper functions which are used in various scripts above to avoid redundancy.

> #### Deploy
Plumber scripts to demonstrate model deployment.

> #### Experiments
Experiments that was done to get started with data exploration and method implementation. 


## Get the scripts up and running.

Consider few things before you run the code on your machine. 

* You need to create environment by installing essential libraries in R Studio. The script for the same can be found at the start of each code.

* We load utility functions from utils.r in most of the scripts. Make sure they are properly loaded.

* Path for dataset needs to be updated whenever needed.

If you face any other issues, please shoot us an email.

> Ananta: aiyengar@hawk.iit.edu

> Omkar: opawar@hawk.iit.edu

> Virat: vjoshi1@hawk.iit.edu

> Bhuvnesh: btejwani@hawk.iit.edu

> Xiaoman: xshen24@hawk.iit.edu

To learn more about the project, please refer to the Report in Documents Folder. 

### Any suggestions will be appreciated. 
