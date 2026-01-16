# List Pending Approvals

Returns all pending approval requests.

## Usage

``` r
ont_list_pending_approvals(concept_id = NULL, con = NULL)
```

## Arguments

- concept_id:

  Optional filter by concept.

- con:

  Optional DBI connection.

## Value

A tibble of pending approvals.
