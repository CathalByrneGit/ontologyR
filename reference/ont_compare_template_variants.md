# Compare Template Variants

Compares how different concepts have customized a template's parameters.

## Usage

``` r
ont_compare_template_variants(template_id, con = NULL)
```

## Arguments

- template_id:

  Character. The template ID.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble showing parameter values across all variants.

## Examples

``` r
if (FALSE) { # \dontrun{
ont_compare_template_variants("ilo_unemployed")
} # }
```
