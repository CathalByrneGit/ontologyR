# Get Dataset Provenance

Returns the complete provenance chain for a materialized dataset,
including the concept definition and all transformation steps.

## Usage

``` r
ont_get_provenance(dataset_id, con = NULL)
```

## Arguments

- dataset_id:

  The dataset to get provenance for.

- con:

  Optional DBI connection.

## Value

A list with provenance information.
