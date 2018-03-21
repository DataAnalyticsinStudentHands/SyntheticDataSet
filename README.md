# SyntheticDataSet
Citymodels is an R package made to build models of city populations to use in investigating public health questions. The current package uses data mined from the U.S. Census to model populations by tract. The finished code for this is in the citymodels folder. The source package is included as citymodel_0.1.0.tar.gz and can be downloaded and installed into R using the install.packages function with the type option set to "source". 

In the Scripts folder is an example case use of this package in modeling Houston, as well as some folders for possible expansions of the project with modules to add health data from the Center for Disease Control and better geospacial resolution using spacial files from the Harris County Appraisal District. Some of these will eventually be incorporated in the Citymodels package.

Possible questions this kind of model could be used to investigate are in the Shiny Apps folder. Asthma App and Diabetes App are demonstrations of using the model to extrapolate some of the effects of public health interventions. Tiffany App, Nick App, and Adelle App are apps from our Modeling Metabolism students who were interested inpublic health health questions that may be modeled further at a later time.

Inputs is a folder of csv files created from the U.S. Census API that were previously used to generate a Houston model. How the data is mined and organized has since changed in building the models, but as some of the apps still rely on the old structure to compare the model to raw data, they will remain here untill the apps have been updated.
