# EvidenceSynthesisModule -------------
#' @title Meta-analysis with the \href{https://ohdsi.github.io/EvidenceSynthesis/}{HADES EvidenceSynthesis Package}
#' @export
#' @description
#' Module for for combining causal effect estimates and study diagnostics
#' across multiple data sites in a distributed study. This includes functions
#' for performing meta-analysis and forest plots
EvidenceSynthesisModule <- R6::R6Class(
  classname = "EvidenceSynthesisModule",
  inherit = StrategusModule,
  public = list(
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the EvidenceSynthesis package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateResultsExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      jobContext <- private$jobContext

      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      if (!dir.exists(resultsFolder)) {
        dir.create(
          path = resultsFolder,
          recursive = T
        )
      }
      private$.writeAnalysisSpecs(
        analysisSpecs = jobContext$settings$evidenceSynthesisAnalysisList,
        resultsFolder = resultsFolder
      )

      private$.executeEvidenceSynthesis(
        connectionDetails = connectionDetails,
        databaseSchema = jobContext$moduleExecutionSettings$resultsDatabaseSchema,
        settings = jobContext$settings$evidenceSynthesisAnalysisList,
        esDiagnosticThresholds = jobContext$settings$esDiagnosticThresholds,
        resultsFolder = resultsFolder,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        maxCores = jobContext$moduleExecutionSettings$maxCores
      )

      file.copy(
        from = private$.getResultsDataModelSpecificationFileLocation(),
        to = file.path(jobContext$moduleExecutionSettings$resultsSubFolder, "resultsDataModelSpecification.csv")
      )
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = "") {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      if (resultsConnectionDetails$dbms == "sqlite" & resultsDatabaseSchema != "main") {
        stop("Invalid schema for sqlite, use databaseSchema = 'main'")
      }

      connection <- DatabaseConnector::connect(resultsConnectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))

      # Create the results model
      sql <- ResultModelManager::generateSqlSchema(
        csvFilepath = private$.getResultsDataModelSpecificationFileLocation()
      )
      sql <- SqlRender::render(sql = sql, warnOnMissingParameters = TRUE, database_schema = resultsDatabaseSchema)
      sql <- SqlRender::translate(sql = sql, targetDialect = resultsConnectionDetails$dbms)
      DatabaseConnector::executeSql(connection, sql)
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = private$.getResultsDataModelSpecificationFileLocation(),
        warnOnCaseMismatch = FALSE
      )

      # add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)
      jobContext <- private$jobContext
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      ResultModelManager::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        resultsFolder = resultsFolder,
        purgeSiteDataBeforeUploading = FALSE, # ES is not site specific
        specifications = private$.getResultsDataModelSpecification()
      )
    },
    #' @description Validate the module specifications
    #' @param moduleSpecifications The EvidenceSynthesis module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    },
    #' Create an evidence synthesis source
    #'
    #' @param sourceMethod            The source method generating the estimates to synthesize. Can be "CohortMethod" or
    #'                                "SelfControlledCaseSeries"
    #' @param databaseIds             The database  IDs to include. Use `databaseIds = NULL` to include all database IDs.
    #' @param analysisIds             The source method analysis IDs to include. Use `analysisIds = NULL` to include all
    #'                                analysis IDs.
    #' @param likelihoodApproximation The type of likelihood approximation. Can be "adaptive grid" or "normal".
    #'
    #' @return
    #' An object of type `EvidenceSynthesisSource`.
    createEvidenceSynthesisSource = function(sourceMethod = "CohortMethod",
                                             databaseIds = NULL,
                                             analysisIds = NULL,
                                             likelihoodApproximation = "adaptive grid") {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertChoice(sourceMethod, c("CohortMethod", "SelfControlledCaseSeries"), add = errorMessages)
      if (is.character(databaseIds)) {
        checkmate::assertCharacter(databaseIds, null.ok = TRUE, add = errorMessages)
      } else {
        checkmate::assertIntegerish(databaseIds, null.ok = TRUE, add = errorMessages)
      }
      checkmate::assertIntegerish(analysisIds, null.ok = TRUE, add = errorMessages)
      checkmate::assertChoice(likelihoodApproximation, c("adaptive grid", "normal"), add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      analysis <- list()
      for (name in names(formals(self$createEvidenceSynthesisSource))) {
        analysis[[name]] <- get(name)
      }
      class(analysis) <- "EvidenceSynthesisSource"
      return(analysis)
    },
    #' Create parameters for a random-effects meta-analysis
    #'
    #' @details
    #' Use DerSimonian-Laird meta-analysis
    #'
    #' @param alpha  The alpha (expected type I error) used for the confidence intervals.
    #' @param evidenceSynthesisAnalysisId description
    #' @param evidenceSynthesisDescription description
    #' @param evidenceSynthesisSource description
    #' @param controlType description
    createRandomEffectsMetaAnalysis = function(alpha = 0.05,
                                               evidenceSynthesisAnalysisId = 1,
                                               evidenceSynthesisDescription = "Random-effects",
                                               evidenceSynthesisSource = NULL,
                                               controlType = "outcome") {
      if (evidenceSynthesisSource$likelihoodApproximation != "normal") {
        stop("Random-effects meta-analysis only supports normal approximation of the likelihood.")
      }
      analysis <- list()
      for (name in names(formals(self$createRandomEffectsMetaAnalysis))) {
        analysis[[name]] <- get(name)
      }
      class(analysis) <- c("RandomEffectsMetaAnalysis", "EvidenceSynthesisAnalysis")
      return(analysis)
    },
    #' Create a parameter object for the function computeFixedEffectMetaAnalysis
    #'
    #' @details
    #' Create an object defining the parameter values.
    #'
    #' @param alpha  The alpha (expected type I error) used for the confidence intervals.
    #' @param evidenceSynthesisAnalysisId description
    #' @param evidenceSynthesisDescription description
    #' @param evidenceSynthesisSource description
    #' @param controlType description
    createFixedEffectsMetaAnalysis = function(alpha = 0.05,
                                              evidenceSynthesisAnalysisId = 1,
                                              evidenceSynthesisDescription = "Fixed-effects",
                                              evidenceSynthesisSource = NULL,
                                              controlType = "outcome") {
      analysis <- list()
      for (name in names(formals(self$createFixedEffectsMetaAnalysis))) {
        analysis[[name]] <- get(name)
      }
      class(analysis) <- c("FixedEffectsMetaAnalysis", "EvidenceSynthesisAnalysis")
      if (evidenceSynthesisSource$likelihoodApproximation != "normal") {
        stop("Fixed-effects meta-analysis only supports normal approximation of the likelihood.")
      }
      return(analysis)
    },
    #' Create a parameter object for the function computeBayesianMetaAnalysis
    #'
    #' @details
    #' Create an object defining the parameter values.
    #'
    #' @param chainLength  Number of MCMC iterations.
    #' @param burnIn  Number of MCMC iterations to consider as burn in.
    #' @param subSampleFrequency  Subsample frequency for the MCMC.
    #' @param priorSd  A two-dimensional vector with the standard deviation of the prior for mu and tau, respectively.
    #' @param alpha  The alpha (expected type I error) used for the credible intervals.
    #' @param robust  Whether or not to use a t-distribution model; default: FALSE.
    #' @param df  Degrees of freedom for the t-model, only used if robust is TRUE.
    #' @param seed  The seed for the random number generator.
    #' @param evidenceSynthesisAnalysisId description
    #' @param evidenceSynthesisDescription description
    #' @param evidenceSynthesisSource description
    #' @param controlType description
    createBayesianMetaAnalysis = function(chainLength = 1100000,
                                          burnIn = 1e+05,
                                          subSampleFrequency = 100,
                                          priorSd = c(2, 0.5),
                                          alpha = 0.05,
                                          robust = FALSE,
                                          df = 4,
                                          seed = 1,
                                          evidenceSynthesisAnalysisId = 1,
                                          evidenceSynthesisDescription = "Bayesian random-effects",
                                          evidenceSynthesisSource = NULL,
                                          controlType = "outcome") {
      analysis <- list()
      for (name in names(formals(self$createBayesianMetaAnalysis))) {
        analysis[[name]] <- get(name)
      }
      class(analysis) <- c("BayesianMetaAnalysis", "EvidenceSynthesisAnalysis")
      return(analysis)
    },
    #' Create EvidenceSynthesis diagnostics thresholds
    #'
    #' @description
    #' Threshold used to determine if we pass or fail diagnostics.
    #'
    #' @param mdrrThreshold         What is the maximum allowed minimum detectable relative risk
    #'                              (MDRR)?
    #' @param easeThreshold         What is the maximum allowed expected absolute systematic error
    #'                              (EASE).
    #' @param i2Threshold           What is the maximum allowed I^2 (measure of between-database
    #'                              heterogeneity in random-effects models)?
    #' @param tauThreshold          What is the maximum allowed tau (measure of between-database
    #'                              heterogeneity in Bayesian random-effects models)?
    #'
    #' @return
    #' An object of type `EsDiagnosticThresholds`.
    createEsDiagnosticThresholds = function(mdrrThreshold = 10,
                                            easeThreshold = 0.25,
                                            i2Threshold = 0.4,
                                            tauThreshold = log(2)) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertNumeric(mdrrThreshold, len = 1, lower = 0, add = errorMessages)
      checkmate::assertNumeric(easeThreshold, len = 1, lower = 0, add = errorMessages)
      checkmate::assertNumeric(i2Threshold, len = 1, lower = 0, add = errorMessages)
      checkmate::assertNumeric(tauThreshold, len = 1, lower = 0, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      thresholds <- list(
        mdrrThreshold = mdrrThreshold,
        easeThreshold = easeThreshold,
        i2Threshold = i2Threshold,
        tauThreshold = tauThreshold
      )
      class(thresholds) <- "EsDiagnosticThresholds"
      return(thresholds)
    },
    #' @description Creates the module Specifications
    #' @param evidenceSynthesisAnalysisList A list of objects of type `EvidenceSynthesisAnalysis` as generated
    #'                                      by either the \href{../../Strategus/html/EvidenceSynthesisModule.html#method-createFixedEffectsMetaAnalysis}{\code{EvidenceSynthesisModule$createFixedEffectsMetaAnalysis()}}
    #'                                      or \href{../../Strategus/html/EvidenceSynthesisModule.html#method-createBayesianMetaAnalysis}{\code{EvidenceSynthesisModule$createBayesianMetaAnalysis()}} function.
    #' @param esDiagnosticThresholds       An object of type`EsDiagnosticThresholds` as generated by the
    #'                                      \href{../../Strategus/html/EvidenceSynthesisModule.html#method-createEsDiagnosticThresholds}{\code{EvidenceSynthesisModule$createEsDiagnosticThresholds()}} function.
    createModuleSpecifications = function(evidenceSynthesisAnalysisList,
                                          esDiagnosticThresholds = self$createEsDiagnosticThresholds()) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertList(evidenceSynthesisAnalysisList, min.len = 1, add = errorMessages)
      for (i in 1:length(evidenceSynthesisAnalysisList)) {
        checkmate::assertClass(evidenceSynthesisAnalysisList[[i]], "EvidenceSynthesisAnalysis", add = errorMessages)
      }
      checkmate::assertClass(esDiagnosticThresholds, "EsDiagnosticThresholds", add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      analysis <- list()
      for (name in names(formals(self$createModuleSpecifications))) {
        analysis[[name]] <- get(name)
      }

      specifications <- super$createModuleSpecifications(
        moduleSpecifications = analysis
      )
      return(specifications)
    }
  ),
  private = list(
    .writeAnalysisSpecs = function(analysisSpecs, resultsFolder) {
      message("Writing evidence synthesis analysis specifications")
      tempFileName <- tempfile()
      evidenceSynthesisAnalysis <- tibble()
      for (analysisSettings in analysisSpecs) {
        ParallelLogger::saveSettingsToJson(analysisSettings, tempFileName)
        analysis <- tibble(
          evidenceSynthesisAnalysisId = analysisSettings$evidenceSynthesisAnalysisId,
          evidenceSynthesisDescription = analysisSettings$evidenceSynthesisDescription,
          sourceMethod = analysisSettings$evidenceSynthesisSource$sourceMethod,
        ) |>
          mutate(definition = readChar(tempFileName, file.info(tempFileName)$size))
        evidenceSynthesisAnalysis <- bind_rows(evidenceSynthesisAnalysis, analysis)
      }
      unlink(tempFileName)
      fileName <- file.path(resultsFolder, "es_analysis.csv")
      CohortGenerator::writeCsv(evidenceSynthesisAnalysis, fileName)
    },
    .ensureEmptyAndExists = function(outputTable, resultsFolder) {
      diagnostics <- private$.createEmptyResult(outputTable)
      fileName <- file.path(resultsFolder, paste0(outputTable, ".csv"))
      private$.writeToCsv(data = diagnostics, fileName = fileName, append = FALSE)
    },
    .executeEvidenceSynthesis = function(connectionDetails, databaseSchema, settings, esDiagnosticThresholds, resultsFolder, minCellCount, maxCores) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))

      outputTables <- c(
        "es_cm_result",
        "es_cm_diagnostics_summary",
        "es_sccs_result",
        "es_sccs_diagnostics_summary"
      )
      invisible(lapply(outputTables, function(x) {
        private$.ensureEmptyAndExists(x, resultsFolder)
      }))

      message("Performing evidence synthesis across databases")
      invisible(lapply(
        X = settings,
        FUN = private$.doAnalysis,
        connection = connection,
        databaseSchema = databaseSchema,
        resultsFolder = resultsFolder,
        minCellCount = minCellCount,
        esDiagnosticThresholds = esDiagnosticThresholds,
        maxCores = maxCores
      ))
    },
    # analysisSettings = settings[[4]]
    .doAnalysis = function(analysisSettings, connection, databaseSchema, resultsFolder, minCellCount, esDiagnosticThresholds, maxCores) {
      perDbEstimates <- private$.getPerDatabaseEstimates(
        connection = connection,
        databaseSchema = databaseSchema,
        evidenceSynthesisSource = analysisSettings$evidenceSynthesisSource
      )
      if (nrow(perDbEstimates$estimates) == 0) {
        message <- sprintf(
          "No unblinded estimates found for source method '%s'",
          analysisSettings$evidenceSynthesisSource$sourceMethod
        )
        if (!is.null(analysisSettings$evidenceSynthesisSource$databaseIds)) {
          message <- sprintf(
            "%s restricting to database IDs %s",
            message,
            paste(analysisSettings$evidenceSynthesisSource$databaseIds, collapse = ", ")
          )
        }
        if (!is.null(analysisSettings$evidenceSynthesisSource$analysisIds)) {
          message <- sprintf(
            "%s restricting to analysis IDs %s",
            message,
            paste(analysisSettings$evidenceSynthesisSource$analysisIds, collapse = ", ")
          )
        }
        warning(message)
        return()
      }

      fullKeys <- perDbEstimates$estimates[, c(perDbEstimates$key, "analysisId")] |>
        distinct()

      cluster <- ParallelLogger::makeCluster(min(10, maxCores))
      ParallelLogger::clusterRequire(cluster, "dplyr")
      on.exit(ParallelLogger::stopCluster(cluster))

      message(sprintf("Performing analysis %s (%s)", analysisSettings$evidenceSynthesisAnalysisId, analysisSettings$evidenceSynthesisDescription))
      estimates <- ParallelLogger::clusterApply(
        cluster = cluster,
        x = split(fullKeys, seq_len(nrow(fullKeys))),
        fun = private$.doSingleEvidenceSynthesis,
        perDbEstimates = perDbEstimates,
        analysisSettings = analysisSettings,
        minCellCount = minCellCount
      )
      estimates <- bind_rows(estimates)

      message("- Calibrating estimates")
      estimates <- estimates |>
        inner_join(perDbEstimates$trueEffectSizes, by = intersect(names(estimates), names(perDbEstimates$trueEffectSizes)))
      if (analysisSettings$controlType == "outcome") {
        if (analysisSettings$evidenceSynthesisSource$sourceMethod == "CohortMethod") {
          controlKey <- c("targetId", "comparatorId", "analysisId")
        } else if (analysisSettings$evidenceSynthesisSource$sourceMethod == "SelfControlledCaseSeries") {
          controlKey <- c("exposureId", "nestingCohortId", "covariateId", "analysisId")
        }
      } else if (analysisSettings$controlType == "exposure") {
        if (analysisSettings$evidenceSynthesisSource$sourceMethod == "CohortMethod") {
          controlKey <- c("outcomeId", "analysisId")
        } else if (analysisSettings$evidenceSynthesisSource$sourceMethod == "SelfControlledCaseSeries") {
          controlKey <- c("outcomeId", "analysisId")
        }
      } else {
        stop(sprintf("Unknown control type '%s'", analysisSettings$controlType))
      }
      groupKeys <- estimates[, controlKey]
      groupKeys <- apply(groupKeys, 1, paste, collapse = "_")

      estimates <- ParallelLogger::clusterApply(
        cluster = cluster,
        x = split(estimates, groupKeys),
        fun = private$.calibrateEstimates
      )
      estimates <- bind_rows(estimates) |>
        mutate(evidenceSynthesisAnalysisId = analysisSettings$evidenceSynthesisAnalysisId)

      # Save diagnostics
      diagnostics <- estimates[, c(perDbEstimates$key, "analysisId", "evidenceSynthesisAnalysisId", "mdrr", "ease", "i2", "tau")] |>
        mutate(mdrrDiagnostic = case_when(
          is.na(.data$mdrr) ~ "NOT EVALUATED",
          .data$mdrr < esDiagnosticThresholds$mdrrThreshold ~ "PASS",
          TRUE ~ "FAIL"
        )) |>
        mutate(easeDiagnostic = case_when(
          is.na(.data$ease) ~ "NOT EVALUATED",
          abs(.data$ease) < esDiagnosticThresholds$easeThreshold ~ "PASS",
          TRUE ~ "FAIL"
        )) |>
        mutate(i2Diagnostic = case_when(
          is.na(.data$i2) ~ "NOT EVALUATED",
          abs(.data$i2) < esDiagnosticThresholds$i2Threshold ~ "PASS",
          TRUE ~ "FAIL"
        )) |>
        mutate(tauDiagnostic = case_when(
          is.na(.data$tau) ~ "NOT EVALUATED",
          abs(.data$tau) < esDiagnosticThresholds$tauThreshold ~ "PASS",
          TRUE ~ "FAIL"
        )) |>
        mutate(unblind = ifelse(.data$mdrrDiagnostic != "FAIL" &
          .data$easeDiagnostic != "FAIL" &
          .data$i2Diagnostic != "FAIL" &
          .data$tauDiagnostic != "FAIL", 1, 0))
      if (analysisSettings$evidenceSynthesisSource$sourceMethod == "CohortMethod") {
        fileName <- file.path(resultsFolder, "es_cm_diagnostics_summary.csv")
      } else if (analysisSettings$evidenceSynthesisSource$sourceMethod == "SelfControlledCaseSeries") {
        fileName <- file.path(resultsFolder, "es_sccs_diagnostics_summary.csv")
      } else {
        stop(sprintf("Saving diagnostics summary not implemented for source method '%s'", analysisSettings$evidenceSynthesisSource$sourceMethod))
      }
      private$.writeToCsv(data = diagnostics, fileName = fileName, append = TRUE)

      # Save estimates
      estimates <- estimates |>
        select(-"trueEffectSize", -"ease", -"i2", -"tau", -"mdrr")
      if (analysisSettings$evidenceSynthesisSource$sourceMethod == "CohortMethod") {
        estimates <- estimates |>
          select(-"outcomeOfInterest")
        fileName <- file.path(resultsFolder, "es_cm_result.csv")
      } else if (analysisSettings$evidenceSynthesisSource$sourceMethod == "SelfControlledCaseSeries") {
        fileName <- file.path(resultsFolder, "es_sccs_result.csv")
      } else {
        stop(sprintf("Saving results not implemented for source method '%s'", analysisSettings$evidenceSynthesisSource$sourceMethod))
      }
      private$.writeToCsv(data = estimates, fileName = fileName, append = TRUE)
    },
    # group = split(estimates, groupKeys)[[1]]
    .calibrateEstimates = function(group) {
      ncs <- group[!is.na(group$trueEffectSize) & group$trueEffectSize == 1 & !is.na(group$seLogRr), ]
      pcs <- group[!is.na(group$trueEffectSize) & group$trueEffectSize != 1 & !is.na(group$seLogRr), ]
      if (nrow(ncs) >= 5) {
        null <- EmpiricalCalibration::fitMcmcNull(logRr = ncs$logRr, seLogRr = ncs$seLogRr)
        ease <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(null)
        calibratedP <- EmpiricalCalibration::calibrateP(
          null = null,
          logRr = group$logRr,
          seLogRr = group$seLogRr,
          twoSided = TRUE
        )
        calibratedOneSidedP <- EmpiricalCalibration::calibrateP(
          null = null,
          logRr = group$logRr,
          seLogRr = group$seLogRr,
          twoSided = FALSE,
          upper = TRUE
        )
        if (nrow(pcs) >= 5) {
          model <- EmpiricalCalibration::fitSystematicErrorModel(
            logRr = c(ncs$logRr, pcs$logRr),
            seLogRr = c(ncs$seLogRr, pcs$seLogRr),
            trueLogRr = log(c(ncs$trueEffectSize, pcs$trueEffectSize)),
            estimateCovarianceMatrix = FALSE
          )
        } else {
          model <- EmpiricalCalibration::convertNullToErrorModel(null)
        }
        calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(model = model, logRr = group$logRr, seLogRr = group$seLogRr)
        group$calibratedRr <- exp(calibratedCi$logRr)
        group$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
        group$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
        group$calibratedP <- calibratedP$p
        group$calibratedOneSidedP <- calibratedOneSidedP$p
        group$calibratedLogRr <- calibratedCi$logRr
        group$calibratedSeLogRr <- calibratedCi$seLogRr
        group$ease <- ease$ease
      } else {
        group$calibratedRr <- NA
        group$calibratedCi95Lb <- NA
        group$calibratedCi95Ub <- NA
        group$calibratedP <- NA
        group$calibratedOneSidedP <- NA
        group$calibratedLogRr <- NA
        group$calibratedSeLogRr <- NA
        group$ease <- NA
      }
      return(group)
    },
    # row <- split(fullKeys, seq_len(nrow(fullKeys)))[[2]]
    # row <- tibble(targetId = 8413, comparatorId = 8436, outcomeId = 1078, analysisId = 2)
    .doSingleEvidenceSynthesis = function(row, perDbEstimates, analysisSettings, minCellCount) {
      sumMinCellCount <- function(counts, minCellCount) {
        if (length(counts) == 0) {
          return(NA)
        }
        hasNegative <- any(counts < 0)
        sumCount <- sum(abs(counts))
        if (sumCount == 0) {
          return(sumCount)
        }
        if (hasNegative) {
          if (sumCount < minCellCount) {
            sumCount <- -minCellCount
          } else {
            sumCount <- -sumCount
          }
        } else {
          if (sumCount < minCellCount) {
            sumCount <- -minCellCount
          }
        }
        return(sumCount)
      }
      computeMdrrFromSe <- function(seLogRr, alpha = 0.05, power = 0.8) {
        # Based on the computation of a two-sided p-value, power can be computed as
        # power = 1-pnorm(qnorm(1 - alpha/2) - (log(mdrr) / seLogRr))/2
        # That can be translated in into:
        mdrr <- exp((qnorm(1 - alpha / 2) - qnorm(2 * (1 - power))) * seLogRr)
        return(mdrr)
      }

      subset <- perDbEstimates$estimates |>
        inner_join(row, by = c(perDbEstimates$key, "analysisId"))
      llApproximations <- perDbEstimates$llApproximations |>
        inner_join(row, by = c(perDbEstimates$key, "analysisId"))
      if (analysisSettings$evidenceSynthesisSource$likelihoodApproximation == "normal") {
        llApproximations <- llApproximations |>
          filter(!is.na(.data$logRr) & !is.na(.data$seLogRr))
        includedDbs <- llApproximations$databaseId
      } else if (analysisSettings$evidenceSynthesisSource$likelihoodApproximation == "adaptive grid") {
        includedDbs <- unique(llApproximations$databaseId)
        llApproximations <- llApproximations |>
          select(
            point = .data$logRr,
            value = .data$logLikelihood,
            .data$databaseId
          ) |>
          group_by(.data$databaseId) |>
          group_split()
      }
      nDatabases <- length(includedDbs)
      subset <- subset |>
        filter(.data$databaseId %in% includedDbs)
      if (analysisSettings$evidenceSynthesisSource$sourceMethod == "CohortMethod") {
        counts <- tibble(
          targetSubjects = sumMinCellCount(subset$targetSubjects, minCellCount),
          comparatorSubjects = sumMinCellCount(subset$comparatorSubjects, minCellCount),
          targetDays = sumMinCellCount(subset$targetDays, 0),
          comparatorDays = sumMinCellCount(subset$comparatorDays, 0),
          targetOutcomes = sumMinCellCount(subset$targetOutcomes, minCellCount),
          comparatorOutcomes = sumMinCellCount(subset$comparatorOutcomes, minCellCount),
        )
      } else if (analysisSettings$evidenceSynthesisSource$sourceMethod == "SelfControlledCaseSeries") {
        counts <- tibble(
          outcomeSubjects = sumMinCellCount(subset$outcomeSubjects, minCellCount),
          outcomeEvents = sumMinCellCount(subset$outcomeEvents, minCellCount),
          outcomeObservationPeriods = sumMinCellCount(subset$outcomeObservationPeriods, 0),
          observedDays = sumMinCellCount(subset$observedDays, 0),
          covariateSubjects = sumMinCellCount(subset$covariateSubjects, minCellCount),
          covariateDays = sumMinCellCount(subset$covariateDays, minCellCount),
          covariateEras = sumMinCellCount(subset$covariateEras, minCellCount),
          covariateOutcomes = sumMinCellCount(subset$covariateOutcomes, minCellCount)
        )
      } else {
        stop(sprintf("Aggregating counts not implemented for source method '%s'", analysisSettings$evidenceSynthesisSource$sourceMethod))
      }

      if (nDatabases == 0) {
        estimate <- tibble(
          rr = as.numeric(NA),
          ci95Lb = as.numeric(NA),
          ci95Ub = as.numeric(NA),
          p = as.numeric(NA),
          oneSidedP = as.numeric(NA),
          logRr = as.numeric(NA),
          seLogRr = as.numeric(NA),
          i2 = as.numeric(NA),
          tau = as.numeric(NA),
          mdrr = as.numeric(Inf)
        )
      } else if (nDatabases == 1) {
        estimate <- tibble(
          rr = exp(subset$logRr),
          ci95Lb = subset$ci95Lb,
          ci95Ub = subset$ci95Ub,
          p = subset$p,
          oneSidedP = if ("oneSidedP" %in% colnames(subset)) subset$oneSidedP else NA,
          logRr = subset$logRr,
          seLogRr = subset$seLogRr,
          i2 = NA,
          tau = NA,
          mdrr = subset$mdrr
        )
      } else {
        if (is(analysisSettings, "FixedEffectsMetaAnalysis")) {
          args <- analysisSettings
          args$evidenceSynthesisAnalysisId <- NULL
          args$evidenceSynthesisDescription <- NULL
          args$evidenceSynthesisSource <- NULL
          args$controlType <- NULL
          args$data <- as.data.frame(llApproximations)
          estimate <- do.call(EvidenceSynthesis::computeFixedEffectMetaAnalysis, args)
          p <- EmpiricalCalibration::computeTraditionalP(
            logRr = estimate$logRr,
            seLogRr = estimate$seLogRr,
            twoSided = TRUE
          )
          oneSidedP <- EmpiricalCalibration::computeTraditionalP(
            logRr = estimate$logRr,
            seLogRr = estimate$seLogRr,
            twoSided = FALSE,
            upper = TRUE
          )
          estimate <- estimate |>
            as_tibble() |>
            rename(
              ci95Lb = lb,
              ci95Ub = ub
            ) |>
            mutate(
              i2 = NA,
              tau = NA,
              mdrr = computeMdrrFromSe(estimate$seLogRr),
              p = !!p,
              oneSidedP = !!oneSidedP
            )
        } else if (is(analysisSettings, "RandomEffectsMetaAnalysis")) {
          m <- meta::metagen(
            TE = llApproximations$logRr,
            seTE = llApproximations$seLogRr,
            studlab = rep("", nrow(llApproximations)),
            byvar = NULL,
            control = list(maxiter = 1000),
            sm = "RR",
            level.comb = 1 - analysisSettings$alpha
          )
          rfx <- summary(m)$random
          oneSidedP <- EmpiricalCalibration::computeTraditionalP(
            logRr = rfx$TE,
            seLogRr = rfx$seTE,
            twoSided = FALSE,
            upper = TRUE
          )
          estimate <- tibble(
            rr = exp(rfx$TE),
            ci95Lb = exp(rfx$lower),
            ci95Ub = exp(rfx$upper),
            p = rfx$p,
            oneSidedP = !!oneSidedP,
            logRr = rfx$TE,
            seLogRr = rfx$seTE,
            i2 = m$I2,
            tau = NA,
            mdrr = computeMdrrFromSe(rfx$seTE)
          )
        } else if (is(analysisSettings, "BayesianMetaAnalysis")) {
          args <- analysisSettings
          args$evidenceSynthesisAnalysisId <- NULL
          args$evidenceSynthesisDescription <- NULL
          args$evidenceSynthesisSource <- NULL
          args$controlType <- NULL
          args$data <- llApproximations
          estimate <- do.call(EvidenceSynthesis::computeBayesianMetaAnalysis, args)
          p <- EmpiricalCalibration::computeTraditionalP(
            logRr = estimate$logRr,
            seLogRr = estimate$seLogRr,
            twoSided = TRUE
          )
          oneSidedP <- EmpiricalCalibration::computeTraditionalP(
            logRr = estimate$logRr,
            seLogRr = estimate$seLogRr,
            twoSided = FALSE,
            upper = TRUE
          )
          estimate <- estimate |>
            as_tibble() |>
            transmute(
              rr = exp(.data$mu),
              ci95Lb = exp(.data$mu95Lb),
              ci95Ub = exp(.data$mu95Ub),
              p = !!p,
              oneSidedP = !!oneSidedP,
              logRr = .data$mu,
              seLogRr = .data$muSe,
              tau = .data$tau,
              i2 = NA,
              mdrr = computeMdrrFromSe(estimate$seLogRr)
            )
        }
      }
      estimate <- bind_cols(row, estimate, counts) |>
        mutate(nDatabases = nDatabases)
      return(estimate)
    },
    .hasUnblindForEvidenceSynthesisColumn = function(connection, databaseSchema, table) {
      row <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = "SELECT TOP 1 * FROM @database_schema.@table;",
        database_schema = databaseSchema,
        table = table,
        snakeCaseToCamelCase = TRUE
      )
      return("unlindForEvidenceSynthesis" %in% colnames(row))
    },
    .getPerDatabaseEstimates = function(connection, databaseSchema, evidenceSynthesisSource) {
      if (evidenceSynthesisSource$sourceMethod == "CohortMethod") {
        key <- c("targetId", "comparatorId", "outcomeId")
        databaseIds <- evidenceSynthesisSource$databaseIds
        analysisIds <- evidenceSynthesisSource$analysisIds
        if (private$.hasUnblindForEvidenceSynthesisColumn(connection, databaseSchema, "cm_diagnostics_summary")) {
          unblindColumn <- "unblind_for_evidence_synthesis"
        } else {
          unblindColumn <- "unblind"
        }
        # For backwards compatibility, when CohortMethod did not generate diagnostics
        # for negative controls: if negative control (outcome_of_interest = 0) then
        # still unblind.
        sql <- "SELECT cm_result.*,
        mdrr,
        CASE
          WHEN @unblind_column IS NULL THEN 1 - outcome_of_interest
          ELSE @unblind_column
        END AS unblind
      FROM @database_schema.cm_result
      INNER JOIN @database_schema.cm_target_comparator_outcome
        ON cm_result.target_id = cm_target_comparator_outcome.target_id
          AND cm_result.comparator_id = cm_target_comparator_outcome.comparator_id
          AND cm_result.outcome_id = cm_target_comparator_outcome.outcome_id
      LEFT JOIN @database_schema.cm_diagnostics_summary
      ON cm_result.target_id = cm_diagnostics_summary.target_id
        AND cm_result.comparator_id = cm_diagnostics_summary.comparator_id
        AND cm_result.outcome_id = cm_diagnostics_summary.outcome_id
        AND cm_result.analysis_id = cm_diagnostics_summary.analysis_id
        AND cm_result.database_id = cm_diagnostics_summary.database_id
      {@database_ids != ''| @analysis_ids != ''} ? {WHERE}
      {@database_ids != ''} ? {  cm_result.database_id IN (@database_ids)}
      {@analysis_ids != ''} ? {  {@database_ids != ''} ? {AND} cm_result.analysis_id IN (@analysis_ids)};
      "
        estimates <- DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = sql,
          database_schema = databaseSchema,
          unblind_column = unblindColumn,
          database_ids = if (is.null(databaseIds)) "" else private$.quoteSql(databaseIds),
          analysis_ids = if (is.null(analysisIds)) "" else analysisIds,
          snakeCaseToCamelCase = TRUE
        ) |>
          as_tibble()

        # Temp hack: detect NA values that have been converted to 0 in the DB:
        idx <- estimates$seLogRr == 0
        estimates$logRr[idx] <- NA
        estimates$seLogRr[idx] <- NA
        estimates$p[idx] <- NA

        if (evidenceSynthesisSource$likelihoodApproximation == "normal") {
          llApproximations <- estimates |>
            filter(.data$unblind == 1) |>
            select(
              "targetId",
              "comparatorId",
              "outcomeId",
              "analysisId",
              "databaseId",
              "logRr",
              "seLogRr"
            )
        } else if (evidenceSynthesisSource$likelihoodApproximation == "adaptive grid") {
          sql <- "SELECT cm_likelihood_profile.*
      FROM @database_schema.cm_likelihood_profile
      WHERE log_likelihood IS NOT NULL
      {@database_ids != ''} ? {  AND cm_likelihood_profile.database_id IN (@database_ids)}
      {@analysis_ids != ''} ? {  AND cm_likelihood_profile.analysis_id IN (@analysis_ids)};
      "
          llApproximations <- DatabaseConnector::renderTranslateQuerySql(
            connection = connection,
            sql = sql,
            database_schema = databaseSchema,
            database_ids = if (is.null(databaseIds)) "" else private$.quoteSql(databaseIds),
            analysis_ids = if (is.null(analysisIds)) "" else analysisIds,
            snakeCaseToCamelCase = TRUE
          ) |>
            inner_join(
              estimates |>
                filter(.data$unblind == 1) |>
                select(
                  "targetId",
                  "comparatorId",
                  "outcomeId",
                  "analysisId",
                  "databaseId",
                ),
              by = c("targetId", "comparatorId", "outcomeId", "analysisId", "databaseId")
            )
        } else {
          stop(sprintf("Unknown likelihood approximation '%s'.", evidenceSynthesisSource$likelihoodApproximation))
        }
        sql <- "SELECT *
      FROM @database_schema.cm_target_comparator_outcome;
    "
        trueEffectSizes <- DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = sql,
          database_schema = databaseSchema,
          snakeCaseToCamelCase = TRUE
        )
        trueEffectSizes <- trueEffectSizes |>
          mutate(trueEffectSize = ifelse(!is.na(.data$trueEffectSize) & .data$trueEffectSize == 0,
            NA,
            .data$trueEffectSize
          ))
      } else if (evidenceSynthesisSource$sourceMethod == "SelfControlledCaseSeries") {
        key <- c("exposureId", "nestingCohortId", "outcomeId", "exposuresOutcomeSetId", "covariateId")
        databaseIds <- evidenceSynthesisSource$databaseIds
        analysisIds <- evidenceSynthesisSource$analysisIds
        if (private$.hasUnblindForEvidenceSynthesisColumn(connection, databaseSchema, "sccs_diagnostics_summary")) {
          unblindColumn <- "unblind_for_evidence_synthesis"
        } else {
          unblindColumn <- "unblind"
        }
        sql <- "SELECT sccs_result.*,
        sccs_covariate.era_id AS exposure_id,
        outcome_id,
        nesting_cohort_id,
        mdrr,
        CASE
          WHEN @unblind_column IS NULL THEN CASE WHEN true_effect_size IS NULL THEN 0 ELSE 1 END
          ELSE @unblind_column
        END AS unblind
      FROM @database_schema.sccs_result
      INNER JOIN @database_schema.sccs_exposures_outcome_set
        ON sccs_result.exposures_outcome_set_id = sccs_exposures_outcome_set.exposures_outcome_set_id
      INNER JOIN @database_schema.sccs_covariate
        ON sccs_result.database_id = sccs_covariate.database_id
          AND sccs_result.exposures_outcome_set_id = sccs_covariate.exposures_outcome_set_id
          AND sccs_result.covariate_id = sccs_covariate.covariate_id
          AND sccs_result.analysis_id = sccs_covariate.analysis_id
      INNER JOIN @database_schema.sccs_exposure
        ON sccs_result.exposures_outcome_set_id = sccs_exposure.exposures_outcome_set_id
          AND sccs_covariate.era_id = sccs_exposure.era_id
      LEFT JOIN @database_schema.sccs_diagnostics_summary
      ON sccs_result.exposures_outcome_set_id = sccs_diagnostics_summary.exposures_outcome_set_id
        AND sccs_result.covariate_id = sccs_diagnostics_summary.covariate_id
        AND sccs_result.analysis_id = sccs_diagnostics_summary.analysis_id
        AND sccs_result.database_id = sccs_diagnostics_summary.database_id
      {@database_ids != ''| @analysis_ids != ''} ? {WHERE}
      {@database_ids != ''} ? {  sccs_result.database_id IN (@database_ids)}
      {@analysis_ids != ''} ? {  {@database_ids != ''} ? {AND} sccs_result.analysis_id IN (@analysis_ids)};
      "
        estimates <- DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = sql,
          database_schema = databaseSchema,
          unblind_column = unblindColumn,
          database_ids = if (is.null(databaseIds)) "" else private$.quoteSql(databaseIds),
          analysis_ids = if (is.null(analysisIds)) "" else analysisIds,
          snakeCaseToCamelCase = TRUE
        ) |>
          as_tibble()

        # Temp hack: detect NA values that have been converted to 0 in the DB:
        idx <- estimates$seLogRr == 0
        estimates$logRr[idx] <- NA
        estimates$seLogRr[idx] <- NA
        estimates$p[idx] <- NA

        if (evidenceSynthesisSource$likelihoodApproximation == "normal") {
          llApproximations <- estimates |>
            filter(.data$unblind == 1) |>
            select(
              "exposuresOutcomeSetId",
              "exposureId",
              "nestingCohortId",
              "outcomeId",
              "covariateId",
              "analysisId",
              "databaseId",
              "logRr",
              "seLogRr"
            )
        } else if (evidenceSynthesisSource$likelihoodApproximation == "adaptive grid") {
          sql <- "SELECT sccs_likelihood_profile.*
      FROM @database_schema.sccs_likelihood_profile
      WHERE log_likelihood IS NOT NULL
      {@database_ids != ''} ? {  AND sccs_likelihood_profile.database_id IN (@database_ids)}
      {@analysis_ids != ''} ? {  AND sccs_likelihood_profile.analysis_id IN (@analysis_ids)};
      "
          llApproximations <- DatabaseConnector::renderTranslateQuerySql(
            connection = connection,
            sql = sql,
            database_schema = databaseSchema,
            database_ids = if (is.null(databaseIds)) "" else private$.quoteSql(databaseIds),
            analysis_ids = if (is.null(analysisIds)) "" else analysisIds,
            snakeCaseToCamelCase = TRUE
          ) |>
            inner_join(
              estimates |>
                filter(.data$unblind == 1) |>
                select(
                  "exposuresOutcomeSetId",
                  "exposureId",
                  "nestingCohortId",
                  "outcomeId",
                  "covariateId",
                  "analysisId",
                  "databaseId",
                ),
              by = c("exposuresOutcomeSetId", "covariateId", "analysisId", "databaseId")
            )
        } else {
          stop(sprintf("Unknown likelihood approximation '%s'.", evidenceSynthesisSource$likelihoodApproximation))
        }
        sql <- "SELECT DISTINCT sccs_exposure.exposures_outcome_set_id,
            sccs_covariate.analysis_id,
            sccs_covariate.era_id AS exposure_id,
            nesting_cohort_id,
            outcome_id,
            sccs_covariate.covariate_id,
            true_effect_size
          FROM @database_schema.sccs_exposure
          INNER JOIN @database_schema.sccs_exposures_outcome_set
            ON sccs_exposure.exposures_outcome_set_id = sccs_exposures_outcome_set.exposures_outcome_set_id
          INNER JOIN @database_schema.sccs_covariate
            ON sccs_exposure.era_id = sccs_covariate.era_id
              AND sccs_exposure.exposures_outcome_set_id = sccs_covariate.exposures_outcome_set_id
          INNER JOIN @database_schema.sccs_covariate_analysis
            ON sccs_covariate.analysis_id = sccs_covariate_analysis.analysis_id
              AND sccs_covariate.covariate_analysis_id = sccs_covariate_analysis.covariate_analysis_id
          WHERE sccs_covariate_analysis.variable_of_interest = 1;
    "
        trueEffectSizes <- DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = sql,
          database_schema = databaseSchema,
          snakeCaseToCamelCase = TRUE
        )
        trueEffectSizes <- trueEffectSizes |>
          mutate(trueEffectSize = ifelse(!is.na(.data$trueEffectSize) & .data$trueEffectSize == 0,
            NA,
            .data$trueEffectSize
          ))
      } else {
        stop(sprintf("Evidence synthesis for source method '%s' hasn't been implemented yet.", evidenceSynthesisSource$sourceMethod))
      }
      return(list(
        key = key,
        estimates = estimates,
        llApproximations = llApproximations,
        trueEffectSizes = trueEffectSizes
      ))
    },
    .writeToCsv = function(data, fileName, append) {
      tableName <- gsub(".csv$", "", basename(fileName))
      names <- colnames(private$.createEmptyResult(tableName))
      data <- data[, names]
      data <- SqlRender::camelCaseToSnakeCaseNames(data)
      readr::write_csv(data, fileName, append = append)
    },
    .createEmptyResult = function(tableName = "") {
      columns <- private$.getResultsDataModelSpecification() |>
        filter(.data$tableName == !!tableName) |>
        pull(.data$columnName) |>
        SqlRender::snakeCaseToCamelCase()
      result <- vector(length = length(columns))
      names(result) <- columns
      result <- as_tibble(t(result), name_repair = "check_unique")
      result <- result[FALSE, ]
      return(result)
    },
    .quoteSql = function(values) {
      return(paste0("'", paste(values, collapse = "', '"), "'"))
    },
    .getResultsDataModelSpecification = function() {
      rdms <- CohortGenerator::readCsv(
        file = private$.getResultsDataModelSpecificationFileLocation()
      )
      return(rdms)
    },
    .getResultsDataModelSpecificationFileLocation = function() {
      return(system.file(
        file.path("csv", "evidenceSynthesisRdms.csv"),
        package = "Strategus"
      ))
    }
  )
)
