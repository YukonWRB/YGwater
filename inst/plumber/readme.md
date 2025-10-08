TL;DR: This directory contains files used to generate a {plumber} API, and any breaking changes to existing endpoints should prompt the coder to consider creating a new version of the API rather than changing the existing endpoints.


This directory (inst/api) contains files used to generate a {plumber} API.

Since many applications and processes may come to use this package's API, it is important to maintain backward compatibility; for that reason, any breaking changes to the functionality of any API endpoints (not additional endpoints, but changes to existing endpoints) should prompt the coder to consider creating a new version of the API (e.g., v2, v3, etc.) rather than changing the existing endpoints.

APIs are run using the exported api() R function (R/api.R). By default, this function will search for the latest version of the API (e.g., v1, v2, etc.) and run that version. To run a specific version of the API, use the version argument (e.g., api(version = "v1")).

Deployments of the API can therefore be made for multiple versions of the API concurrently, ensuring that existing applications and processes that rely on old API endpoints will continue to function as expected.