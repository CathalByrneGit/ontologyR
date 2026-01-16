# Request Approval

Creates an approval request for a concept version action.

## Usage

``` r
ont_request_approval(
  concept_id,
  scope,
  version,
  requested_action,
  requested_by = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  The concept.

- scope:

  The scope.

- version:

  The version.

- requested_action:

  The action: "activate", "deprecate", "retire".

- requested_by:

  User requesting.

- con:

  Optional DBI connection.

## Value

The request_id.
