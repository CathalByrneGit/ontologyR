# Approve Request

Approves a pending approval request.

## Usage

``` r
ont_approve_request(request_id, decided_by, decision_notes = NULL, con = NULL)
```

## Arguments

- request_id:

  The request to approve.

- decided_by:

  User approving.

- decision_notes:

  Optional notes.

- con:

  Optional DBI connection.

## Value

Invisibly returns TRUE.
