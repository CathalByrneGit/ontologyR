# List Governance Gates

Returns all defined governance gates.

## Usage

``` r
ont_list_gates(applies_to = NULL, enabled_only = TRUE, con = NULL)
```

## Arguments

- applies_to:

  Optional filter by action type.

- enabled_only:

  Only return enabled gates.

- con:

  Optional DBI connection.

## Value

A tibble of gates.
