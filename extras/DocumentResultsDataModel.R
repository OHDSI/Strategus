# ------------------------------------------------------------------------------
# This script does the following:
# - Uses a local PG instance to create the results data model. The data
#   model is then exported to produce a SQL script that is then used by GHA
#   to build the SchemaSpy results data model documentation.
# - Creates the SchemaSpy SchemaMeta.xml used to build the documentation.
#   This XML contains the table descriptions, column descriptions and
#   FK relationships between the entities.
#   (https://schemaspy.readthedocs.io/en/latest/configuration/schemaMeta.html)
#
# Notes about resources found in the /extras/rdms directory:
# - table_descriptions.csv: Hand currated table descriptions. Column descriptions
#   come from the module rdms files.
# - SqlExtractor.java: Program to parse out the OhdsiShinyModules R files to
#   extract the SQL used by the results viewer. The output of this program is
#   contained in ohdsi_shiny_modules_sql_queries.csv
# - SqlJoinRelationshipExtractor.java: Program to parse
#   ohdsi_shiny_modules_sql_queries.csv and identify the JOIN clauses to
#   document the relationships in the data model. Results of this program are
#   in ohdsi_shiny_modules_table_relationships.csv.
# - results_table_realtionships.xlsx: Manually reviewed the ohdsi_shiny_modules_table_relationships.csv
#   and extracted the list of tables and their foreign key relationships.
# - results_table_relationships.csv: Exported the unique entries from
#   results_table_realtionships.xlsx. This resource is then used when constructing
#   the SchemaSpy SchemaMeta.xml
# ------------------------------------------------------------------------------
library(Strategus)
library(dplyr)

# fullResultsDataModel will hold the full results model to create the
# SchemaSpyMeta.xml
fullResultsDataModel <- tibble::tibble()
rdms <- CohortGenerator::readCsv(
  file = system.file(
    file.path("csv", "databaseMetaDataRdms.csv"),
    package = "Strategus"
  ),
  warnOnCaseMismatch = F
)
rdms$tableDefinedBy <- "Strategus"

fullResultsDataModel <- fullResultsDataModel %>%
  bind_rows(rdms %>% select(tableDefinedBy, tableName, columnName, description))

sql <- "-- Strategus Tables\n"
sql <- paste0(sql, ResultModelManager::generateSqlSchema(schemaDefinition = rdms))

# Iterate over all of the modules in the project
# and produce the full results data model specification
# for each module
moduleFileList <- list.files("./R", pattern = "^Module-.*\\.R$")
fileNameCleaned <- sub("^Module-", "", moduleFileList)  # Remove "Module-"
fileNameCleaned <- sub("\\.R$", "", fileNameCleaned)  # Remove ".R"
moduleList <- paste0(fileNameCleaned, "Module")

for(module in moduleList) {
  m <- get(module)$new()
  rdms <- m$getResultsDataModelSpecification()

  sql <- paste0(sql, "-- ", module, " Tables\n")
  sql <- paste0(sql, ResultModelManager::generateSqlSchema(schemaDefinition = rdms))

  if (!"description" %in% colnames(rdms)) {
    rdms$description <- ""
  }
  rdms$tableDefinedBy <- module

  fullResultsDataModel <- fullResultsDataModel %>%
    bind_rows(rdms %>% select(tableDefinedBy, tableName, columnName, description))
}

# NOTE: This code was to initially save the table information to a csv file
# that will be manually edited to include the table descriptions
# tableDescriptions <- fullResultsDataModel %>%
#   select(tableDefinedBy, tableName) %>%
#   distinct() %>%
#   mutate(description = "") %>%
#   arrange(tableName)
# CohortGenerator::writeCsv(
#   x = tableDescriptions,
#   file = "./extras/rdms/table_descriptions.csv"
# )

# Save the OHDSI-SQL
SqlRender::writeSql(
  sql = sql,
  targetFile = "./extras/rdms/full_data_model_ohdsi.sql"
)

# Render for PostgreSQL
pgSql <- SqlRender::render(
  sql = sql,
  database_schema = "results"
)
pgSql <- SqlRender::translate(
  sql = pgSql,
  targetDialect = "postgresql"
)
SqlRender::writeSql(
  sql = pgSql,
  targetFile = "./extras/rdms/full_data_model_pg.sql"
)

# Write out the SchemaSpy SchemaMeta.xml ---------------------------
library(xml2)

# Create the root element with attributes
schemaMeta <- xml_new_root("schemaMeta",
                           "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                           "xsi:noNamespaceSchemaLocation" = "http://schemaspy.org/xsd/6/schemameta.xsd")

# Add comments node
xml_add_child(
  schemaMeta,
  "comments",
  "The tables in the results data model are grouped using a unique prefix for
  each Strategus module. For example, the CharacterizationModule results tables
  all start with \"c_\".")

# Create tables node
tables <- xml_add_child(schemaMeta, "tables")

# Iterate over the fullResultsDataModel to create the descriptions
# of the tables & columns.
uniqueTableNames <- unique(fullResultsDataModel$tableName)
tableDescriptions <- CohortGenerator::readCsv(
  file = "./extras/rdms/table_descriptions.csv"
)
resultsTableRelationships <-CohortGenerator::readCsv(
  file = "./extras/rdms/results_table_relationships.csv"
)
for (i in seq_along(uniqueTableNames)) {
  # Add table node with attributes
  currentTableName <- uniqueTableNames[i]
  #print(currentTableName)
  # Get the table description, if it exists
  currentTableDescriptionInfo <- tableDescriptions %>%
    filter(.data$tableName == currentTableName)
  currentTableDescription <- ""
  if (nrow(currentTableDescriptionInfo) == 1) {
    currentTableDescription <- paste0(currentTableDescriptionInfo$tableDefinedBy[1], ": ", currentTableDescriptionInfo$description[1])
  }
  table <- xml_add_child(tables, "table", name = currentTableName, comments = currentTableDescription)

  # Get the columns
  columnsForCurrentTable <- fullResultsDataModel %>%
    filter(.data$tableName == currentTableName)

  for (j in 1:nrow(columnsForCurrentTable)) {
    curColumnName <- columnsForCurrentTable$columnName[j]
    description <- columnsForCurrentTable$description[j]
    #print(paste0("  -- ", curColumnName))
    # Add column node with attributes
    columnNode <- xml_add_child(table, "column", name = curColumnName, comments = description)
    # Determine if this table + column has a FK relationship to any other tables
    curColumnFk <- resultsTableRelationships %>%
      filter(.data$tableName == currentTableName & .data$columnName == curColumnName)
    if (nrow(curColumnFk) > 0) {
      #print(paste0("-- FK FOUND FOR: ", currentTableName, ".", curColumnName))
      for (k in 1:nrow(curColumnFk)) {
        fkTable <- curColumnFk$fkTableName[k]
        fkColumn <- curColumnFk$fkColumnName[k]
        xml_add_child(columnNode, "foreignKey", table = fkTable, column = fkColumn)
      }
    }
  }
}

# Write the XML string to a file, omitting the XML declaration
write_xml(schemaMeta, "./extras/rdms/schema_meta.xml")


# Used to create the DB locally to assist in documenting the
# results model.
# connectionDetails <- DatabaseConnector::createConnectionDetails(
#   dbms = "postgresql",
#   server = "127.0.0.1/strategus",
#   user = "user",
#   password = "password"
# )
# connection <- DatabaseConnector::connect(connectionDetails)
# DatabaseConnector::executeSql(
#   connection = connection,
#   sql = "drop schema results cascade;create schema results;"
# )
# sql <- SqlRender::readSql("./extras/rdms/full_data_model_pg.sql")
# DatabaseConnector::executeSql(
#   connection = connection,
#   sql = sql
# )
# DatabaseConnector::disconnect(connection)
