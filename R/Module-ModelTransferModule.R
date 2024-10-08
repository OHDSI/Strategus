# ModelTransferModule -------------
#' @title Model transfer module
#' @export
#' @description
#' This module contains functionality for moving plpModels to/from S3 buckets
#' and github repositories from your local file system.
ModelTransferModule <- R6::R6Class(
  classname = "ModelTransferModule",
  inherit = StrategusModule,
  public = list(
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Generates the cohorts
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      # TODO: Does this module connect to a DB? If not these
      # up-front validation steps done in super$.validateCdmExecutionSettings
      # and super$execute may not make sense here.
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      if (!dir.exists(resultsFolder)) {
        dir.create(resultsFolder, recursive = TRUE)
      }

      private$.message("Validating inputs")
      inherits(jobContext, 'list')

      if (is.null(jobContext$settings)) {
        stop("Analysis settings not found in job context")
      }
      if (is.null(jobContext$sharedResources)) {
        stop("Shared resources not found in job context")
      }
      if (is.null(jobContext$moduleExecutionSettings)) {
        stop("Execution settings not found in job context")
      }

      # workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      # resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      modelSaveLocation <- jobContext$moduleExecutionSettings$workSubFolder #jobContext$sharedResources$modelSaveLocation

      private$.message("Transfering models")
      #moduleInfo <- getModuleInfo()

      # finding S3 details
      s3Settings <- jobContext$settings$s3Settings
      # finding github details
      githubSettings <- jobContext$settings$githubSettings
      # finding localFile details
      localFileSettings <- jobContext$settings$localFileSettings

      modelLocationsS3 <- tryCatch({private$.getModelsFromS3(
        s3Settings = s3Settings,
        saveFolder = modelSaveLocation
      )}, error = function(e){ParallelLogger::logInfo(e); return(NULL)}
      )
      if(!is.null(modelLocationsS3)){
        readr::write_csv(modelLocationsS3, file = file.path(resultsFolder, 's3_export.csv'))
      }

      modelLocationsGithub <- tryCatch({private$.getModelsFromGithub(
        settings = githubSettings,
        saveFolder = modelSaveLocation
      )}, error = function(e){ParallelLogger::logInfo(e); return(NULL)}
      )
      if(!is.null(modelLocationsGithub)){
        readr::write_csv(modelLocationsGithub, file = file.path(resultsFolder, 'github_export.csv'))
      }

      modelLocationsLocalFiles <- tryCatch({private$.getModelsFromLocalFiles(
        localFileSettings = localFileSettings$locations,
        saveFolder = modelSaveLocation
      )}, error = function(e){ParallelLogger::logInfo(e); return(NULL)}
      )
      if(!is.null(modelLocationsS3)){
        readr::write_csv(modelLocationsS3, file = file.path(resultsFolder, 'local_export.csv'))
      }

      private$.message(paste("Results available at:", resultsFolder))
    },
    #' @description Creates the ModelTransferModule Specifications
    #' @param s3Settings description
    #' @param githubSettings description
    #' @param localFileSettings description
    #' include steps to compute inclusion rule statistics.
    createModuleSpecifications = function(s3Settings = NULL,
                                          githubSettings = NULL,
                                          localFileSettings = NULL) {
      analysis <- list()
      for (name in names(formals(self$createModuleSpecifications))) {
        analysis[[name]] <- get(name)
      }

      specifications <- super$createModuleSpecifications(
        moduleSpecifications = analysis
      )
      return(specifications)
    },
    #' @description Validate the module specifications
    #' @param moduleSpecifications The ModelTransfer module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  ),
  private = list(
    .getModelsFromLocalFiles = function(localFileSettings, saveFolder) {

      if (is.null(localFileSettings)) {
        return(NULL)
      }

      if (!dir.exists(file.path(saveFolder, "models"))) {
        dir.create(file.path(saveFolder, "models"), recursive = TRUE)
      }

      saveFolder <- file.path(saveFolder, "models")

      localFileSettings <- fs::path_expand(localFileSettings)
      saveFolder <- fs::path_expand(saveFolder)

      contents <- list.files(localFileSettings, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)

      for (item in contents) {
        relativePath <- fs::path_rel(item, start = localFileSettings)
        targetPath <- file.path(saveFolder, relativePath)
        targetDir <- dirname(targetPath)

        if (!dir.exists(targetDir)) {
          dir.create(targetDir, recursive = TRUE)
        }

        if (fs::dir_exists(item)) {
          if (!dir.exists(targetPath)) {
            dir.create(targetPath, recursive = TRUE)
          }
        } else {
          file.copy(item, targetPath, overwrite = TRUE)
        }
      }

      info <- data.frame()
      return(info)
    },
    # code that takes s3 details and download the models and returns the locations plus details as data.frame
    .getModelsFromS3 = function(settings, saveFolder){

      # need to have settings
      # AWS_ACCESS_KEY_ID=<my access key id>
      # AWS_SECRET_ACCESS_KEY=<my secret key>
      #  AWS_DEFAULT_REGION=ap-southeast-2

      if(is.null(settings)){
        return(NULL)
      }

      info <- data.frame()

      for(i in 1:nrow(settings)){

        modelSaved <- F
        saveToLoc <- ''

        validBucket <- aws.s3::bucket_exists(
          bucket = settings$bucket[i],
          region = settings$region[i]
        )

        if(validBucket){
          subfolder <- settings$modelZipLocation[i]
          bucket <- settings$bucket[i]
          region <- settings$region[i]

          result <- aws.s3::get_bucket_df(bucket = bucket, region = region, max = Inf)
          paths <- fs::path(result$Key)

          workDir <- private$.findWorkDir(bucket, subfolder, region)
          analyses <- private$.findAnalysesNames(bucket, workDir, region)

          if(length(analyses) > 0) {
            if(!fs::dir_exists(fs::path(saveFolder, "models"))){
              dir.create(fs::path(saveFolder, "models"), recursive = T)
            }
            saveToLoc <- fs::path(saveFolder, "models")

            for (analysis in analyses) {
              analysis_paths <- paths[fs::path_has_parent(paths, fs::path(workDir, analysis))]

              for(obj in analysis_paths) {
                # split work directory from path
                relative_paths <- fs::path_rel(obj, start = workDir)
                # remove artifacts created by current path location
                filtered_paths <- relative_paths[relative_paths != "."]
                # Construct the file path where you want to save the file locally
                local_file_path <- fs::path(saveToLoc, filtered_paths)

                # Download the file from S3
                aws.s3::save_object(obj, bucket, file = local_file_path)
              }
              ParallelLogger::logInfo(paste0("Downloaded: ", analysis, " to ", saveToLoc))
            }
          } else{
            ParallelLogger::logInfo(paste0("No ",settings$modelZipLocation[i]," in bucket ", settings$bucket[i], " in region ", settings$region[i] ))
          }
        }else{
          ParallelLogger::logInfo(paste0("No bucket ", settings$bucket[i] ," in region ", settings$region[i]))
        }

        info <- rbind(
          info,
          data.frame(
            originalLocation = "PLACEHOLDER",
            modelSavedLocally = TRUE,
            localLocation = saveToLoc
          )
        )

      }

      return(info)
    },
    # code that takes github details and download the models and returns the locations plus details as data.frame
    .getModelsFromGithub = function(settings, saveFolder) {

      if (is.null(settings)) {
        return(NULL)
      }

      info <- data.frame()

      for (i in 1:nrow(settings)) {

        user <- settings[i, ]$user
        repository <- settings[i, ]$repository
        ref <- settings[i, ]$ref

        downloadRepo <- tryCatch({
          utils::download.file(
            url = file.path("https://github.com", user, repository, "archive",
                            paste0(ref,".zip")),
            destfile = file.path(tempdir(), "tempGitHub.zip")
          )}, error = function(e) {
          ParallelLogger::logInfo('GitHub repository download failed')
            return(NULL)
            }
        )

        if(!is.null(downloadRepo)){
          # unzip into the workFolder
          OhdsiSharing::decompressFolder(
            sourceFileName = file.path(tempdir(), "tempGitHub.zip"),
            targetFolder = file.path(tempdir(), "tempGitHub")
          )
          for(j in 1:length(settings[i, ]$modelsFolder)) {
            modelsFolder <- settings[i, ]$modelsFolder[j]  #'models'
            modelFolder <- if (is.null(settings[i, ]$modelFolder[j])) {
              ""
            } else {
              settings[i, ]$modelFolder[j]
            }

            tempModelLocation <- file.path(
              tempdir(),
              "tempGitHub",
              paste0(repository, "-", ref),
              "inst",
              modelsFolder,
              modelFolder
            )

            if(!dir.exists(file.path(saveFolder,"models",paste0('model_github_', i, '_', j)))){
              dir.create(file.path(saveFolder,"models",paste0('model_github_', i, '_', j)), recursive = T)
            }
            for(dirEntry in dir(tempModelLocation)){
              file.copy(
                from = file.path(tempModelLocation, dirEntry),
                to = file.path(saveFolder,"models",paste0('model_github_', i, '_', j)), #issues if same modelFolder name in different github repos
                recursive = TRUE
              )
            }

            modelSaved <- TRUE
            saveToLoc <- file.path(saveFolder,"models",paste0('model_github_', i, '_', j))

            info <- rbind(
              info,
              data.frame(
                githubLocation = file.path("https://github.com", user, repository, "archive", paste0(ref,".zip")),
                githubPath = file.path('inst', modelsFolder, modelFolder),
                modelSavedLocally = modelSaved,
                localLocation = saveToLoc
              )
            )

          }

        } else{

          info <- rbind(
            info,
            data.frame(
              githubLocation = file.path("https://github.com", user, repository, "archive", paste0(ref,".zip")),
              githubPath = file.path('inst', settings[i, ]$modelsFolder, settings[i, ]$modelFolder),
              modelSavedLocally = FALSE,
              localLocation = ''
            )
          )
        }


      }

      return(info)
    },
    .findWorkDir = function(bucket, subfolder, region) {
      # list all content in the bucket
      result <- aws.s3::get_bucket_df(bucket = bucket, region = region, max = Inf)
      # extract paths of all content
      paths <- fs::path(result$Key)
      # split paths up for easier processing
      split_path <- fs::path_split(paths)

      # find the full path of the subfolder with models for validation
      results_sapply <- sapply(split_path, function(x) {
        identical(tail(x, 1), subfolder)
      })
      subfolder_path <- paths[results_sapply]

      return(subfolder_path)
    },
    .findAnalysesNames = function(bucket, workDir, region) {
      # list all content in the bucket
      result <- aws.s3::get_bucket_df(bucket = bucket, region = region, max = Inf)
      # extract paths of all content
      paths <- fs::path(result$Key)
      # filter for paths in work directory
      work_dir_paths <- paths[fs::path_has_parent(paths, workDir)]
      # split work directory from path
      relative_paths <- fs::path_rel(work_dir_paths, start = workDir)
      # remove artifacts created by current path location
      filtered_paths <- relative_paths[relative_paths != "."]
      # get only the top level directories
      top_level_dirs <- sapply(fs::path_split(filtered_paths), function(p) p[[1]])
      top_level_dirs <- unique(top_level_dirs)
      return(top_level_dirs)
    }
  )
)
