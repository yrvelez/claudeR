## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local: [your OS and R version]
* GitHub Actions: Ubuntu 22.04, R 4.3.0
* R-hub: Windows Server, macOS, Ubuntu

## Notes

This is a new submission to CRAN.

### API Key Requirement

This package requires an Anthropic API key to function. The package provides
clear error messages when the API key is missing. All examples use `\dontrun{}`
to avoid attempting API calls during R CMD check.

### Internet Access

The package makes HTTP requests to the Anthropic API (api.anthropic.com).
This is the core functionality of the package.
