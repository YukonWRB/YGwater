TL;DR: This directory contains versioned API definitions. Any breaking change to
an existing endpoint should generally go into a new API version instead of
changing the existing one in place.


This directory (`inst/plumber`) contains files and directories used to generate
the package API. The current `v1` surface is built with {plumber}. The
in-progress `v2` surface is built with {plumber2}.

Since many applications and processes may come to use this package's API, it is important to maintain backward compatibility; for that reason, any breaking changes to the functionality of any API endpoints (not additional endpoints, but changes to existing endpoints) should prompt the coder to consider creating a new version of the API (e.g., v2, v3, etc.) rather than changing the existing endpoints.

APIs are run using the exported `api()` R function (`R/api.R`). To run a
specific version, use the `version` argument (for example, `api(version = 1)`
or `api(version = "v2")`). While newer versions are still incomplete, the
default `api()` entrypoint continues to use the latest complete file-based API
definition so the current production surface is not replaced unintentionally.

Deployments of the API can therefore be made for multiple versions of the API concurrently, ensuring that existing applications and processes that rely on old API endpoints will continue to function as expected.
