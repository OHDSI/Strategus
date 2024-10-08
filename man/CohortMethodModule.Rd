% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Module-CohortMethod.R
\name{CohortMethodModule}
\alias{CohortMethodModule}
\title{New-user cohort studies with the \href{https://ohdsi.github.io/CohortMethod/}{HADES CohortMethod Package}}
\description{
Module for performing new-user cohort studies against
the OMOP Common Data Model
}
\section{Super class}{
\code{\link[Strategus:StrategusModule]{Strategus::StrategusModule}} -> \code{CohortMethodModule}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CohortMethodModule-new}{\code{CohortMethodModule$new()}}
\item \href{#method-CohortMethodModule-execute}{\code{CohortMethodModule$execute()}}
\item \href{#method-CohortMethodModule-createResultsDataModel}{\code{CohortMethodModule$createResultsDataModel()}}
\item \href{#method-CohortMethodModule-getResultsDataModelSpecification}{\code{CohortMethodModule$getResultsDataModelSpecification()}}
\item \href{#method-CohortMethodModule-uploadResults}{\code{CohortMethodModule$uploadResults()}}
\item \href{#method-CohortMethodModule-createModuleSpecifications}{\code{CohortMethodModule$createModuleSpecifications()}}
\item \href{#method-CohortMethodModule-validateModuleSpecifications}{\code{CohortMethodModule$validateModuleSpecifications()}}
\item \href{#method-CohortMethodModule-clone}{\code{CohortMethodModule$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="Strategus" data-topic="StrategusModule" data-id="createSharedResourcesSpecifications"><a href='../../Strategus/html/StrategusModule.html#method-StrategusModule-createSharedResourcesSpecifications'><code>Strategus::StrategusModule$createSharedResourcesSpecifications()</code></a></span></li>
<li><span class="pkg-link" data-pkg="Strategus" data-topic="StrategusModule" data-id="validateSharedResourcesSpecifications"><a href='../../Strategus/html/StrategusModule.html#method-StrategusModule-validateSharedResourcesSpecifications'><code>Strategus::StrategusModule$validateSharedResourcesSpecifications()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CohortMethodModule-new"></a>}}
\if{latex}{\out{\hypertarget{method-CohortMethodModule-new}{}}}
\subsection{Method \code{new()}}{
Initialize the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CohortMethodModule$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CohortMethodModule-execute"></a>}}
\if{latex}{\out{\hypertarget{method-CohortMethodModule-execute}{}}}
\subsection{Method \code{execute()}}{
Executes the CohortMethod package
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CohortMethodModule$execute(
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

\item{\code{analysisSpecifications}}{The analysis specifications for the study}

\item{\code{executionSettings}}{An object of type \code{ExecutionSettings} as created
by \code{\link[=createCdmExecutionSettings]{createCdmExecutionSettings()}} or \code{\link[=createResultsExecutionSettings]{createResultsExecutionSettings()}}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CohortMethodModule-createResultsDataModel"></a>}}
\if{latex}{\out{\hypertarget{method-CohortMethodModule-createResultsDataModel}{}}}
\subsection{Method \code{createResultsDataModel()}}{
Create the results data model for the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CohortMethodModule$createResultsDataModel(
  resultsConnectionDetails,
  resultsDatabaseSchema,
  tablePrefix = ""
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
\if{html}{\out{<a id="method-CohortMethodModule-getResultsDataModelSpecification"></a>}}
\if{latex}{\out{\hypertarget{method-CohortMethodModule-getResultsDataModelSpecification}{}}}
\subsection{Method \code{getResultsDataModelSpecification()}}{
Get the results data model specification for the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CohortMethodModule$getResultsDataModelSpecification(tablePrefix = "")}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-CohortMethodModule-uploadResults"></a>}}
\if{latex}{\out{\hypertarget{method-CohortMethodModule-uploadResults}{}}}
\subsection{Method \code{uploadResults()}}{
Upload the results for the module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CohortMethodModule$uploadResults(
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

\item{\code{resultsDataModelSettings}}{The results data model settings as created using [@seealso \code{\link[=createResultsDataModelSettings]{createResultsDataModelSettings()}}]}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CohortMethodModule-createModuleSpecifications"></a>}}
\if{latex}{\out{\hypertarget{method-CohortMethodModule-createModuleSpecifications}{}}}
\subsection{Method \code{createModuleSpecifications()}}{
Creates the CohortMethod Module Specifications
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CohortMethodModule$createModuleSpecifications(
  cmAnalysisList,
  targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = TRUE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{cmAnalysisList}}{A list of objects of type \code{cmAnalysis} as created using
the `\link[CohortMethod:createCmAnalysis]{CohortMethod::createCmAnalysis} function.}

\item{\code{targetComparatorOutcomesList}}{A list of objects of type \code{targetComparatorOutcomes} as
created using the \link[CohortMethod:createTargetComparatorOutcomes]{CohortMethod::createTargetComparatorOutcomes}
function.}

\item{\code{analysesToExclude}}{Analyses to exclude. See the Analyses to Exclude section for details.}

\item{\code{refitPsForEveryOutcome}}{Should the propensity model be fitted for every outcome (i.e.
after people who already had the outcome are removed)? If
false, a single propensity model will be fitted, and people
who had the outcome previously will be removed afterwards.}

\item{\code{refitPsForEveryStudyPopulation}}{Should the propensity model be fitted for every study population
definition? If false, a single propensity model will be fitted,
and the study population criteria will be applied afterwards.}

\item{\code{cmDiagnosticThresholds}}{An object of type \code{CmDiagnosticThresholds} as created using
\code{\link[CohortMethod:createCmDiagnosticThresholds]{CohortMethod::createCmDiagnosticThresholds()}}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Run a list of analyses for the target-comparator-outcomes of interest. This function will run all
specified analyses against all hypotheses of interest, meaning that the total number of outcome
models is \code{length(cmAnalysisList) * length(targetComparatorOutcomesList)} (if all analyses specify an
outcome model should be fitted). When you provide several analyses it will determine whether any of
the analyses have anything in common, and will take advantage of this fact. For example, if we
specify several analyses that only differ in the way the outcome model is fitted, then this
function will extract the data and fit the propensity model only once, and re-use this in all the
analysis.

After completion, a tibble containing references to all generated files can be obtained using the
\code{\link[CohortMethod:getFileReference]{CohortMethod::getFileReference()}} function. A summary of the analysis results can be obtained using the
\code{\link[CohortMethod:getResultsSummary]{CohortMethod::getResultsSummary()}} function.
\subsection{Analyses to Exclude}{

Normally, \code{runCmAnalyses} will run all combinations of target-comparator-outcome-analyses settings.
However, sometimes we may not need all those combinations. Using the \code{analysesToExclude} argument,
we can remove certain items from the full matrix. This argument should be a data frame with at least
one of the following columns:
}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CohortMethodModule-validateModuleSpecifications"></a>}}
\if{latex}{\out{\hypertarget{method-CohortMethodModule-validateModuleSpecifications}{}}}
\subsection{Method \code{validateModuleSpecifications()}}{
Validate the module specifications
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CohortMethodModule$validateModuleSpecifications(moduleSpecifications)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{moduleSpecifications}}{The CohortMethod module specifications}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CohortMethodModule-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CohortMethodModule-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CohortMethodModule$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
