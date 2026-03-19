# Add shared resources (i.e. cohorts) to analysis specifications

Add shared resources (i.e. cohorts) to analysis specifications

## Usage

``` r
addSharedResources(analysisSpecifications, sharedResources)
```

## Arguments

- analysisSpecifications:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- sharedResources:

  An object of type `SharedResources`.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
