# This script will produce a SQL script that is then used by GHA
# to document the full results data model for all modules in Strategus
rdms <- CohortGenerator::readCsv(
  file = system.file(
    file.path("csv", "databaseMetaDataRdms.csv"),
    package = "Strategus"
  ),
  warnOnCaseMismatch = F
)
sql <- "-- Strategus Tables\n"
sql <- paste0(sql, ResultModelManager::generateSqlSchema(schemaDefinition = rdms))

# Iterate over all of the modules in the project
# and produce the SQL for the results data model
moduleFileList <- list.files("./R", pattern = "^Module-.*\\.R$")
fileNameCleaned <- sub("^Module-", "", moduleFileList)  # Remove "Module-"
fileNameCleaned <- sub("\\.R$", "", fileNameCleaned)  # Remove ".R"
moduleList <- paste0(fileNameCleaned, "Module")

for(module in moduleList) {
  m <- get(module)$new()
  rdms <- m$getResultsDataModelSpecification()
  sql <- paste0(sql, "-- ", module, " Tables\n")
  sql <- paste0(sql, ResultModelManager::generateSqlSchema(schemaDefinition = rdms))
}

# Save the OHDSI-SQL
SqlRender::writeSql(
  sql = sql,
  targetFile = "./extras/rdms/full_data_model_ohdsi.sql"
)

# Render for PostgreSQL
pgSql <- SqlRender::render(
  sql = sql,
  database_schema = "public"
)
pgSql <- SqlRender::translate(
  sql = pgSql,
  targetDialect = "postgresql"
)
SqlRender::writeSql(
  sql = pgSql,
  targetFile = "./extras/rdms/full_data_model_pg.sql"
)



