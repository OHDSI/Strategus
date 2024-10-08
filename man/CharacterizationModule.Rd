% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Module-Characterization.R
\name{CharacterizationModule}
\alias{CharacterizationModule}
\title{Characterize cohorts with the \href{https://ohdsi.github.io/Characterization/}{HADES Characterization Package}}
\description{
Computes cohort characterization information against
the OMOP Common Data Model
}
\section{Super class}{
\code{\link[Strategus:StrategusModule]{Strategus::StrategusModule}} -> \code{CharacterizationModule}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{tablePrefix}}{The table prefix to append to the results tables}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CharacterizationModule-new}{\code{CharacterizationModule$new()}}
\item \href{#method-CharacterizationModule-execute}{\code{CharacterizationModule$execute()}}
\item \href{#method-CharacterizationModule-createResultsDataModel}{\code{CharacterizationModule$createResultsDataModel()}}
\item \href{#method-CharacterizationModule-getResultsDataModelSpecification}{\code{CharacterizationModule$getResultsDataModelSpecification()}}
\item \href{#method-CharacterizationModule-uploadResults}{\code{CharacterizationModule$uploadResults()}}
\item \href{#method-CharacterizationModule-createModuleSpecifications}{\code{CharacterizationModule$createModuleSpecifications()}}
\item \href{#method-CharacterizationModule-clone}{\code{CharacterizationModule$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="Strategus" data-topic="StrategusModule" data-id="createSharedResourcesSpecifications"><a href='../../Strategus/html/StrategusModule.html#method-StrategusModule-createSharedResourcesSpecifications'><code>Strategus::StrategusModule$createSharedResourcesSpecifications()</code></a></span></li>
<li><span class="pkg-link" data-pkg="Strategus" data-topic="StrategusModule" data-id="validateModuleSpecifications"><a href='../../Strategus/html/StrategusModule.html#method-StrategusModule-validateModuleSpecifications'><code>Strategus::StrategusModule$validateModuleSpecifications()</code></a></span></li>
<li><span class="pkg-link" data-pkg="Strategus" data-topic="StrategusModule" data-id="validateSharedResourcesSpecifications"><a href='../../Strategus/html/StrategusModule.html#method-StrategusModule-validateSharedResourcesSpecifications'><code>Strategus::StrategusModule$validateSharedResourcesSpecifications()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CharacterizationModule-new"></a>}}
\if{latex}{\out{\hypertarget{method-CharacterizationModule-new}{}}}
\subsection{Method \code{new()}}{
Initialize the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CharacterizationModule$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CharacterizationModule-execute"></a>}}
\if{latex}{\out{\hypertarget{method-CharacterizationModule-execute}{}}}
\subsection{Method \code{execute()}}{
Execute characterization
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CharacterizationModule$execute(
  connectionDetails,
  analysisSpecifications,
  executionSettings
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{connectionDetails}}{An object of class \code{connectionDetails} as created by the
\code{\link[DatabaseConnector:createConnectionDetails]{DatabaseConnector::createConnectionDetails()}} function.}

\item{\code{analysisSpecifications}}{An object of type \code{AnalysisSpecifications} as created
by \code{\link[=createEmptyAnalysisSpecificiations]{createEmptyAnalysisSpecificiations()}}.}

\item{\code{analysisSpecifications}}{An object of type \code{AnalysisSpecifications} as created
by \code{\link[=createEmptyAnalysisSpecificiations]{createEmptyAnalysisSpecificiations()}}.}

\item{\code{executionSettings}}{An object of type \code{ExecutionSettings} as created
by \code{\link[=createCdmExecutionSettings]{createCdmExecutionSettings()}} or \code{\link[=createResultsExecutionSettings]{createResultsExecutionSettings()}}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CharacterizationModule-createResultsDataModel"></a>}}
\if{latex}{\out{\hypertarget{method-CharacterizationModule-createResultsDataModel}{}}}
\subsection{Method \code{createResultsDataModel()}}{
Create the results data model for the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CharacterizationModule$createResultsDataModel(
  resultsConnectionDetails,
  resultsDatabaseSchema,
  tablePrefix = self$tablePrefix
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{resultsConnectionDetails}}{The connection details to the results database which
is an object of class \code{connectionDetails} as created by the
\code{\link[DatabaseConnector:createConnectionDetails]{DatabaseConnector::createConnectionDetails()}} function.}

\item{\code{resultsConnectionDetails}}{The connection details to the results database which
is an object of class \code{connectionDetails} as created by the
\code{\link[DatabaseConnector:createConnectionDetails]{DatabaseConnector::createConnectionDetails()}} function.}

\item{\code{resultsDatabaseSchema}}{The schema in the results database that holds the results data model.}

\item{\code{tablePrefix}}{A prefix to apply to the database table names (optional).}

\item{\code{tablePrefix}}{A prefix to apply to the database table names (optional).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CharacterizationModule-getResultsDataModelSpecification"></a>}}
\if{latex}{\out{\hypertarget{method-CharacterizationModule-getResultsDataModelSpecification}{}}}
\subsection{Method \code{getResultsDataModelSpecification()}}{
Get the results data model specification for the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CharacterizationModule$getResultsDataModelSpecification(tablePrefix = "")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{tablePrefix}}{A prefix to apply to the database table names (optional).}

\item{\code{tablePrefix}}{A prefix to apply to the database table names (optional).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CharacterizationModule-uploadResults"></a>}}
\if{latex}{\out{\hypertarget{method-CharacterizationModule-uploadResults}{}}}
\subsection{Method \code{uploadResults()}}{
Upload the results for the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CharacterizationModule$uploadResults(
  resultsConnectionDetails,
  analysisSpecifications,
  resultsDataModelSettings
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{resultsConnectionDetails}}{The connection details to the results database which
is an object of class \code{connectionDetails} as created by the
\code{\link[DatabaseConnector:createConnectionDetails]{DatabaseConnector::createConnectionDetails()}} function.}

\item{\code{resultsConnectionDetails}}{The connection details to the results database which
is an object of class \code{connectionDetails} as created by the
\code{\link[DatabaseConnector:createConnectionDetails]{DatabaseConnector::createConnectionDetails()}} function.}

\item{\code{analysisSpecifications}}{An object of type \code{AnalysisSpecifications} as created
by \code{\link[=createEmptyAnalysisSpecificiations]{createEmptyAnalysisSpecificiations()}}.}

\item{\code{analysisSpecifications}}{An object of type \code{AnalysisSpecifications} as created
by \code{\link[=createEmptyAnalysisSpecificiations]{createEmptyAnalysisSpecificiations()}}.}

\item{\code{resultsDataModelSettings}}{The results data model settings as created using [@seealso \code{\link[=createResultsDataModelSettings]{createResultsDataModelSettings()}}]}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CharacterizationModule-createModuleSpecifications"></a>}}
\if{latex}{\out{\hypertarget{method-CharacterizationModule-createModuleSpecifications}{}}}
\subsection{Method \code{createModuleSpecifications()}}{
Creates the CharacterizationModule Specifications
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CharacterizationModule$createModuleSpecifications(
  targetIds,
  outcomeIds,
  outcomeWashoutDays = c(365),
  minPriorObservation = 365,
  dechallengeStopInterval = 30,
  dechallengeEvaluationWindow = 30,
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 365),
  endAnchor = c("cohort end", "cohort end"),
  minCharacterizationMean = 0.01,
  covariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsGender =
    T, useDemographicsAge = T, useDemographicsAgeGroup = T, useDemographicsRace = T,
    useDemographicsEthnicity = T, useDemographicsIndexYear = T, useDemographicsIndexMonth
    = T, useDemographicsTimeInCohort = T, useDemographicsPriorObservationTime = T,
    useDemographicsPostObservationTime = T, useConditionGroupEraLongTerm = T,
    useDrugGroupEraOverlapping = T, useDrugGroupEraLongTerm = T,
    useProcedureOccurrenceLongTerm = T, useMeasurementLongTerm = T, 
    
    useObservationLongTerm = T, useDeviceExposureLongTerm = T,
    useVisitConceptCountLongTerm = T, useConditionGroupEraShortTerm = T,
    useDrugGroupEraShortTerm = T, useProcedureOccurrenceShortTerm = T,
    useMeasurementShortTerm = T, useObservationShortTerm = T, useDeviceExposureShortTerm
    = T, useVisitConceptCountShortTerm = T, endDays = 0, longTermStartDays = -365,
    shortTermStartDays = -30),
  caseCovariateSettings =
    Characterization::createDuringCovariateSettings(useConditionGroupEraDuring = T,
    useDrugGroupEraDuring = T, useProcedureOccurrenceDuring = T, useDeviceExposureDuring
    = T, useMeasurementDuring = T, useObservationDuring = T, useVisitConceptCountDuring =
    T),
  casePreTargetDuration = 365,
  casePostOutcomeDuration = 365
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{targetIds}}{A vector of cohort IDs to use as the target(s) for the characterization}

\item{\code{outcomeIds}}{A vector of cohort IDs to use as the outcome(s) for the characterization}

\item{\code{outcomeWashoutDays}}{A vector of integers specifying the washout days for each outcome (same length as the outcomeIds)}

\item{\code{minPriorObservation}}{The number of days of minimum observation a patient in the target populations must have}

\item{\code{dechallengeStopInterval}}{description}

\item{\code{dechallengeEvaluationWindow}}{description}

\item{\code{riskWindowStart}}{The number of days after start anchor to start the time-at-risk (can be a vector for multiple TARS)}

\item{\code{startAnchor}}{The TAR starts relative to this either cohort start or cohort end (can be a vector for multiple TARS)}

\item{\code{riskWindowEnd}}{The number of days after end anchor to end the time-at-risk (can be a vector for multiple TARS)}

\item{\code{endAnchor}}{The TAR ends relative to this either cohort start or cohort end (can be a vector for multiple TARS)}

\item{\code{minCharacterizationMean}}{The minimum fraction patients in the target have a covariate for it to be included}

\item{\code{covariateSettings}}{Covariates for the database, cohort and risk factor characterization}

\item{\code{caseCovariateSettings}}{Covariates for the case-series characterization}

\item{\code{casePreTargetDuration}}{The number of days before target start to use for case-series}

\item{\code{casePostOutcomeDuration}}{The number of days after outcome start to use for case-series}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CharacterizationModule-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CharacterizationModule-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CharacterizationModule$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
