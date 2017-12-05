## First-time installation instructions ##
# There are some non-R dependencies
# devtools dependencies:
# - lib-openssl
# - libcurl4-openssl
# Achilles dependencies:
# - java jre and jdk
# If Achilles DatabaseConnector fails, install it separately:
# - install_github("ohdsi/DatabaseConnector")
# If rJava does not load, then execute Rstudio as superuser:
# - sudo rstudio
install.packages("devtools")
library(devtools)
install_github("thehyve/Achilles", ref="vocab_mapping")
library(Achilles)

## Settings for achilles output ##
# Path to folder where Achilles json files will be stored
ACHILLES_DATA_PATH <- "/path/to/achilles_data"
DATA_NAME  <- "sample1" # The name will appear in AchillesWeb. Has to be unique

## Database connection details ##
connectionDetails <- createConnectionDetails(dbms="postgresql",
                                             server="localhost/caliber",
                                             user="postgres",
                                             password="",
                                             port=5432,
                                             schema="cdm5")

#Create schema webapi
DatabaseConnector::executeSql(connect(connectionDetails), "CREATE SCHEMA webapi;")

## Achilles ##
achillesResults <- achilles(connectionDetails,
                            cdmDatabaseSchema="cdm5",
                            resultsDatabaseSchema = "webapi",
                            cdmVersion = "5",
                            smallcellcount = 5,
                            runHeel = TRUE,
                            validateSchema = FALSE)

# Give the WebApi all access rights to the new tables created by the previous function.
# Needed for Record Counts in Atlas.
DatabaseConnector::executeSql(connect(connectionDetails), "GRANT SELECT ON ALL TABLES IN SCHEMA webapi TO webapi;")

# export
outputPath <- paste(ACHILLES_DATA_PATH, DATA_NAME, sep="/")
exportToJson(connectionDetails,
             cdmDatabaseSchema="cdm5",
             resultsDatabaseSchema = "webapi",
             outputPath = outputPath,
             cdmVersion = "5")

# Update the datasources file #
Achilles::addDatasource(outputPath, DATA_NAME)


## Vocabulary mapping statistics ##
listAllMappings(connectionDetails
               ,resultsDatabaseSchema = "webapi"
               ,measurement_source_vocabularies = c("JNJ_CPRD_T_ET_LOINC", "JNJ_CPRD_ET_LOINC", "JNJ_CPRD_SCORE_LOINC") 
               ,measurement_unit_source_vocabularies = c("CPRD_UNIT") 
               ,measurement_value_source_vocabularies = c("CPRD_QUALIFIER") 
               ,observation_source_vocabularies = c("JNJ_CPRD_T_ET_LOINC", "JNJ_CPRD_ET_LOINC", "JNJ_CPRD_SCORE_LOINC")
               ,observation_qualifier_source_vocabularies = c("CPRD_QUALIFIER") 
               ,drug_source_vocabularies = c("CPRD_PRODUCT") 
               ,specialty_source_vocabularies = c("JNJ_CPRD_PROV_CMS2","JNJ_CPRD_PROV_SPEC") 
)

# export
outputPath <- paste(outputPath, "vocabulary", sep="/")
exportVocabStats(connectionDetails, resultsDatabaseSchema = "webapi", outputPath = outputPath)
