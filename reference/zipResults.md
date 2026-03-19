# Create a zip file with all study results for sharing with study coordinator

Create a zip file with all study results for sharing with study
coordinator

## Usage

``` r
zipResults(resultsFolder, zipFile)
```

## Arguments

- resultsFolder:

  The root folder holding the study results.

- zipFile:

  The path to the zip file to be created.

## Value

Does not return anything. Is called for the side-effect of creating the
zip file with results.

## Details

Creates a `.zip` file of the `.csv` files found in the `resultsFolder`.
The resulting `.zip` file will have relative paths to the root of the
`resultsFolder` which is generally found in
`executionSettings$resultsFolder`.
