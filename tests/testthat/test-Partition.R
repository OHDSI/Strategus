analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
                         package = "Strategus"
  )
)
specificationFolder <- file.path(tempDir, "specificationFolderTest")
if (!dir.exists(specificationFolder)) {
  dir.create(specificationFolder, recursive = TRUE)
}

withr::defer(
  {
    unlink(specificationFolder, recursive = TRUE, force = TRUE)
  },
  testthat::teardown_env()
)

# call the function that partitions the spec                                                                                                                                                                                          c("CohortGeneratorModule", "PatientLevelPredictionModule")]
Strategus:::partitionModule(analysisSpecifications, specificationFolder)

test_that("Partition study has all modules", {
  
  # now check all the modules are in specificationFolder
  modulesInSpec <- unlist(lapply(analysisSpecifications$moduleSpecifications, function(ms) ms$module))
  testthat::expect_true(sum(modulesInSpec %in% dir(specificationFolder)) == length(modulesInSpec)) 
})

moduleNames <- c("CohortGeneratorModule","CharacterizationModule", "CohortIncidenceModule")
for(moduleName in moduleNames){
  
test_that(paste0("Partition study works for those that save single specs - ", moduleName), {
  # now check each module jsons combine to the original
  # ==========================================
  # CohortGenerator/Characterization/CohortIncidence
  #
  # modules that do not partition due to being database 
  # rather than cpu heavy
  # ==========================================
  modulesInSpec <- unlist(lapply(analysisSpecifications$moduleSpecifications, function(ms) ms$module))
  
    ind <- which(modulesInSpec == moduleName)
    originalSetting <- analysisSpecifications$moduleSpecifications[[ind]]$settings
    
    jsons <- dir(file.path(specificationFolder,moduleName))
    
    # check there is only one json
    testthat::expect_true(length(jsons) == 1)
    
    spec <- list()
    length(spec) <- length(jsons)
    for(i in 1:length(jsons)){
      spec[[i]] <- ParallelLogger::loadSettingsFromJson(
        file.path(
          specificationFolder,
          moduleName,
          jsons[i]
        )
      )
    }
    
    # check shared resources are the same and match the original
    sharedResources <- unique(lapply(spec, function(x) x$sharedResources))
    sharedResources[[length(sharedResources) + 1]] <- analysisSpecifications$sharedResources
    testthat::expect_true(length(unique(sharedResources)) == 1)
    
    # check the saved settings and the original
    settings <- list()
    for(i in 1:length(jsons)){
      settings <- append(settings, spec[[i]]$moduleSpecifications[[1]]$settings)
    }
    testthat::expect_true(identical(settings,originalSetting))
  
})
  
}

test_that("Partition study works for PLP", {
  # ==========================================
  # PatientLevelPrediction
  # ==========================================
  modulesInSpec <- unlist(lapply(analysisSpecifications$moduleSpecifications, function(ms) ms$module))
  
  plpInd <- which(modulesInSpec == "PatientLevelPredictionModule")
  originalModelDesignList <- analysisSpecifications$moduleSpecifications[[plpInd]]$settings$modelDesignList
  
  jsons <- dir(file.path(specificationFolder,"PatientLevelPredictionModule"))
  spec <- list()
  length(spec) <- length(jsons)
  for(i in 1:length(jsons)){
    spec[[i]] <- ParallelLogger::loadSettingsFromJson(
      file.path(
        specificationFolder,
        "PatientLevelPredictionModule",
        jsons[i]
      )
    )
  }
  
  # check there is a single module spec
  for(i in 1:length(jsons)){
    testthat::expect_true(length(spec[[i]]$moduleSpecifications) == 1)
  }
  
  # check shared resources are the same and match the original
  sharedResources <- unique(lapply(spec, function(x) x$sharedResources))
  sharedResources[[length(sharedResources) + 1]] <- analysisSpecifications$sharedResources
  testthat::expect_true(length(unique(sharedResources)) == 1)
  
  # now check the moduleSpecifications combine to original
  # for plp combine all the modelDesigns
  modelDesignList <- list()
  for(i in 1:length(jsons)){
    modelDesignList <- append(modelDesignList, spec[[i]]$moduleSpecifications[[1]]$settings$modelDesignList)
  }
  
  # check same size
  testthat::expect_true(length(modelDesignList) == length(originalModelDesignList))
  # check model designs the same 
  testthat::expect_true(sum(modelDesignList %in% originalModelDesignList) == length(originalModelDesignList))


})


test_that("Partition study works for CM", {
  # ==========================================
  # CohortMethod
  # ==========================================
  modulesInSpec <- unlist(lapply(analysisSpecifications$moduleSpecifications, function(ms) ms$module))
  
  moduleName <- "CohortMethodModule"
  
  ind <- which(modulesInSpec == moduleName)
  originalSettings <- analysisSpecifications$moduleSpecifications[[ind]]$settings
  
  jsons <- dir(file.path(specificationFolder,moduleName))
  spec <- list()
  length(spec) <- length(jsons)
  for(i in 1:length(jsons)){
    spec[[i]] <- ParallelLogger::loadSettingsFromJson(
      file.path(
        specificationFolder,
        moduleName,
        jsons[i]
      )
    )
  }
  
  # check there is a single module spec
  for(i in 1:length(jsons)){
    testthat::expect_true(length(spec[[i]]$moduleSpecifications) == 1)
  }
  
  # check shared resources are the same and match the original
  sharedResources <- unique(lapply(spec, function(x) x$sharedResources))
  sharedResources[[length(sharedResources) + 1]] <- analysisSpecifications$sharedResources
  testthat::expect_true(length(unique(sharedResources)) == 1)
  
  # now check the moduleSpecifications combine to original
  settingList <- lapply(spec, function(x) x$moduleSpecifications[[1]]$settings)
  
  
  
  # check cmAnalysisList,refitPsForEveryOutcome
  #       refitPsForEveryStudyPopulation
  #       cmDiagnosticThresholds
  testthat::expect_true(length(unique(lapply(settingList, function(x) x$cmAnalysisList))) == 1)
  testthat::expect_true(length(unique(lapply(settingList, function(x) x$refitPsForEveryOutcome))) == 1)
  testthat::expect_true(length(unique(lapply(settingList, function(x) x$refitPsForEveryStudyPopulation))) == 1)
  testthat::expect_true(length(unique(lapply(settingList, function(x) x$cmDiagnosticThresholds))) == 1)
  
  if(length(jsons) > 1 ){
    tcoList <- do.call(what = 'append', args = unique(lapply(settingList, function(x) x$targetComparatorOutcomesList)))
    testthat::expect_true(sum(unique(do.call('append',lapply(settingList, function(x) x$cmAnalysisList))) %in% originalSettings$cmAnalysisList) == length(originalSettings$cmAnalysisList))
    testthat::expect_true(sum(unique(do.call('append',lapply(settingList, function(x) x$refitPsForEveryOutcome))) %in% originalSettings$refitPsForEveryOutcome) == length(originalSettings$refitPsForEveryOutcome))
    testthat::expect_true(sum(unique(do.call('append',lapply(settingList, function(x) x$refitPsForEveryStudyPopulation))) %in% originalSettings$refitPsForEveryStudyPopulation) == length(originalSettings$refitPsForEveryStudyPopulation))
    testthat::expect_true(sum(unique(do.call('append',lapply(settingList, function(x) x$cmDiagnosticThresholds))) %in% originalSettings$cmDiagnosticThresholds) == length(originalSettings$cmDiagnosticThresholds))
  } else{
    testthat::expect_true(sum(settingList[[1]]$cmAnalysisList %in% originalSettings$cmAnalysisList) == length(originalSettings$cmAnalysisList))
    testthat::expect_true(sum(settingList[[1]]$refitPsForEveryOutcome %in% originalSettings$refitPsForEveryOutcome) == length(originalSettings$refitPsForEveryOutcome))
    testthat::expect_true(sum(settingList[[1]]$refitPsForEveryStudyPopulation %in% originalSettings$refitPsForEveryStudyPopulation) == length(originalSettings$refitPsForEveryStudyPopulation))
    testthat::expect_true(sum(settingList[[1]]$cmDiagnosticThresholds %in% originalSettings$cmDiagnosticThresholds) == length(originalSettings$cmDiagnosticThresholds))
    tcoList <- settingList[[1]]$targetComparatorOutcomesList
  }
  
  # check same length
  testthat::expect_true(length(tcoList) == length(originalSettings$targetComparatorOutcomesList))
  # check tco same
  testthat::expect_true(sum(tcoList %in% originalSettings$targetComparatorOutcomesList) == length(originalSettings$targetComparatorOutcomesList))
  
})



test_that("Partition study works for SCCS", {
  # ==========================================
  # SelfControlledCaseSeries
  # ==========================================
  modulesInSpec <- unlist(lapply(analysisSpecifications$moduleSpecifications, function(ms) ms$module))
  
  moduleName <- "SelfControlledCaseSeriesModule"
  
  ind <- which(modulesInSpec == moduleName)
  originalSettings <- analysisSpecifications$moduleSpecifications[[ind]]$settings
  
  jsons <- dir(file.path(specificationFolder,moduleName))
  spec <- list()
  length(spec) <- length(jsons)
  for(i in 1:length(jsons)){
    spec[[i]] <- ParallelLogger::loadSettingsFromJson(
      file.path(
        specificationFolder,
        moduleName,
        jsons[i]
      )
    )
  }
  
  # check there is a single module spec
  for(i in 1:length(jsons)){
    testthat::expect_true(length(spec[[i]]$moduleSpecifications) == 1)
  }
  
  # check shared resources are the same and match the original
  sharedResources <- unique(lapply(spec, function(x) x$sharedResources))
  sharedResources[[length(sharedResources) + 1]] <- analysisSpecifications$sharedResources
  testthat::expect_true(length(unique(sharedResources)) == 1)
  
  # now check the moduleSpecifications combine to original
  settingList <- lapply(spec, function(x) x$moduleSpecifications[[1]]$settings)
  
  # check sccsAnalysisList, combineDataFetchAcrossOutcomes, sccsDiagnosticThresholds
  testthat::expect_true(length(unique(lapply(settingList, function(x) x$combineDataFetchAcrossOutcomes))) == 1)
  testthat::expect_true(length(unique(lapply(settingList, function(x) x$sccsDiagnosticThresholds))) == 1)
  testthat::expect_true(length(unique(lapply(settingList, function(x) x$sccsAnalysisList))) == 1)
  
  testthat::expect_true(sum(unlist(lapply(settingList, function(x) identical(x$combineDataFetchAcrossOutcomes, originalSettings$combineDataFetchAcrossOutcomes)))) == length(settingList))
  testthat::expect_true(sum(unlist(lapply(settingList, function(x) identical(x$sccsDiagnosticThresholds, originalSettings$sccsDiagnosticThresholds)))) == length(settingList))
  testthat::expect_true(sum(unlist(lapply(settingList, function(x) identical(x$sccsAnalysisList, originalSettings$sccsAnalysisList)))) == length(settingList))
    
  if(length(jsons) > 1 ){ 
    eoList <- do.call(what = 'append', args = unique(lapply(settingList, function(x) x$exposuresOutcomeList)))
  } else{
    eoList <- settingList[[1]]$exposuresOutcomeList
  }
  
  # check same number of rows for components
  eoComponents <- .extractExposuresOutcomeComponents(eoList)
  eoOriginalComponents <- .extractExposuresOutcomeComponents(originalSettings$exposuresOutcomeList)
  testthat::expect_true(nrow(eoComponents) == nrow(eoOriginalComponents))
 
   # check tco same - paste rows into string and compare
  testthat::expect_true(sum(unlist(lapply(1:nrow(eoComponents), function(i){paste0(eoComponents[i,], collapse = '-')})) %in% 
    unlist(lapply(1:nrow(eoOriginalComponents), function(i){paste0(eoOriginalComponents[i,], collapse = '-')}))) == nrow(eoComponents))
  
})

# add test for .extractExposuresOutcomeComponents
test_that(".extractExposuresOutcomeComponents works", {
eo1 <- list(
  outcomeId = 10,
  nestingCohortId = 100,
  exposures = list(
    list(
      exposureId = 1,
      exposureIdRef = 1,
      trueEffectSize = NA
    )
  )
)

eo2 <- list(
  outcomeId = 11,
  nestingCohortId = 100,
  exposures = list(
    list(
      exposureId = 1,
      exposureIdRef = 1,
      trueEffectSize = NA
    ),
    list(
      exposureId = 2,
      exposureIdRef = 1,
      trueEffectSize = 1
    )
  )
)

exposuresOutcomeList <- list(
  eo1, eo2
)

dfEo <- .extractExposuresOutcomeComponents(exposuresOutcomeList)

testthat::expect_true(nrow(dfEo) == 3)
# test the first row is as expected by rbinding rows and uniqueing 
# then checking only one row 
testthat::expect_true(
  nrow(unique(rbind(
    dfEo[1,], 
    data.frame(
      outcomeId = 10,
      nestingCohortId = 100, 
      exposureId = 1,
      exposureIdRef = 1, 
      trueEffectSize = NA
      )
    )
  )) == 1)

# second row
testthat::expect_true(
  nrow(unique(rbind(
    dfEo[2,], 
    data.frame(
      outcomeId = 11,
      nestingCohortId = 100, 
      exposureId = 1,
      exposureIdRef = 1, 
      trueEffectSize = NA
    )
  )
  )) == 1)

# third row
testthat::expect_true(
  nrow(unique(rbind(
    dfEo[3,], 
    data.frame(
      outcomeId = 11,
      nestingCohortId = 100, 
      exposureId = 2,
      exposureIdRef = 1, 
      trueEffectSize = 1
    )
  )
  )) == 1)


})
