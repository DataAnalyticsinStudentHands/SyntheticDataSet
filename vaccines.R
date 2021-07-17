#vaccine merges
#population by zip - https://worldpopulationreview.com/zips/texas (until we get Sam working)
pop_zip <- read.csv(file = "~/Downloads/csvData.csv")
#vaccination by zip - until we get from Harris County  - https://dshs.texas.gov/covidvaccine/
vax_zip <- read.csv(header = FALSE, file = "~/Downloads/vaxzip.xlsx")
