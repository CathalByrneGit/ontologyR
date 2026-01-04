# Resolve a Drift Event

Marks a drift event as resolved with a resolution note.

## Usage

``` r
ont_resolve_drift(
  drift_id,
  resolution,
  resolved_by,
  status = "resolved",
  con = NULL
)
```

## Arguments

- drift_id:

  Character. The drift event to resolve.

- resolution:

  Character. How the drift was resolved.

- resolved_by:

  Character. Who resolved it.

- status:

  Character. New status: "resolved" (drift was fixed) or "accepted"
  (drift is acceptable/expected).

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns `TRUE`.
