# Reject Request

Rejects a pending approval request.

## Usage

``` r
ont_reject_request(request_id, decided_by, decision_notes, con = NULL)
```

## Arguments

- request_id:

  The request to reject.

- decided_by:

  User rejecting.

- decision_notes:

  Reason for rejection.

- con:

  Optional DBI connection.

## Value

Invisibly returns TRUE.
