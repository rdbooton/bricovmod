# bricovmod
Bristol COVID-19 Modelling

Public data needed from https://coronavirus.data.gov.uk/#regions
Download both coronavirus-deaths_latest.csv and coronavirus-cases_latest.csv (last 5 days are not complete for reporting)

Update data from ONS for regional deaths 
https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsinvolvingcovid19englandandwalesmarch2020
But included in this repos as "coronavirus-deaths-regional.csv" up until 17th April 2020 - this is the regional death count from weekly occurrences xlsx which can be downloaded on ONS - this will need updating

bricovmodV3.R file:

- Can change "SPECREGION" according to specific region in UK 

- Specify model ODEs via desolve "X.Y.model"

- Epidemic simulator "epid" 

- Sampling LHS "Sampling" 

- Run model and return .csv files "outFUN"

- Fitting & plotting method available at end of code




plotfitsV2.R file:

- Can specify which plots interested in - comment out / in to get tables, plots etc
