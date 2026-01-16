# Get Template Variants

Lists all concepts that inherit from a specific template.

## Usage

``` r
ont_get_template_variants(template_id, con = NULL)
```

## Arguments

- template_id:

  Character. The template ID.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of concepts that inherit from the template.

## Examples

``` r
if (FALSE) { # \dontrun{
ont_get_template_variants("ilo_unemployed")
} # }
```
