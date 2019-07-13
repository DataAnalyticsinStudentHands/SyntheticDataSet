# Workflow for creation of SAM


### main.R

Calls  “getcensusdata.R” to create a data.frame with the Census data for Harris County.

Calls “getHCADParcels.R” to create a data.frame with housing information from HCAD for Harris County.

Calls "householdGenerator.R""

### Step 1: householdGenerator.R

The tracts are done in parallel to make the right number of houses and groupquarters in each tract based on the U.S. Census.

Generates a random seed for each household to sample with, and calls functions from the other scripts

Calls "householdFunctions.R"

	getNumberOfVehiclesForHouseholds
		- Depends on family size
		
	getHouseholdIncome
		- No dependencies
		
	getHouseholdHealthInsurance
		- Depends on age and disability
		
groupquartersFunctions.R

	getNumberOfVehiclesForGroupquarters
		- Depends on sex and employment
		
	getIncomeForGroupquarters
		- No dependencies
		
	getHealthInsuranceForGroupquarters
		- Depends on age and disability
		
individualFunctions.R

	getSexRaceAndAge
		- Depends on member status (ex: Child, Adult)
		
	getSchoolEnrollment
		- Depends on sex and age
		
	getEducationAttainment
		- Depends on sex and age
		
	getEmployment
		- Depends on sex and age
		
	getDisability
		- Depends on age
		
	getEnglishSpeakingSkillsAndNativity
		- Depends on age and race
		
	getCitizenAndLangAtHome
		- Depends on nativity and English fluency
		
	getVets
		 - Depends on sex, age, and employment
		 
	getTransport
		- Depends on sex and employment
		
	getTravelTime
		- Depends on transportation to work
		
### Step 2:

Merges with HCAD parcels - Required information:

*  Tract number
*  Account/ID number
*  Building Types/Use Codes
*  Living Units per Building
*  Nr. of buildings per Account

