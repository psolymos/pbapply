# Divide Tasks for Progress-bar Friendly Distribution in a Cluster

Divides up `1:nx` into approximately equal sizes (`ncl`) as a way to
allocate tasks to nodes in a cluster repeatedly while updating a
progress bar.

## Usage

``` r
splitpb(nx, ncl, nout = NULL)
```

## Arguments

- nx:

  Number of tasks.

- ncl:

  Number of cluster nodes.

- nout:

  Integer, maximum number of partitions in the output (must be \> 0).

## Value

A list of length `min(nout, ceiling(nx / ncl))`, each element being an
integer vector of length `ncl * k` or less, where `k` is a tuning
parameter constrained by the other arguments
(`k = max(1L, ceiling(ceiling(nx / ncl) / nout))` and `k = 1` if
`nout = NULL`).

## Author

Peter Solymos \<solymos@ualberta.ca\>

## See also

Parallel usage of [`pbapply`](pbapply.md) and related functions.

## Examples

``` r
## define 1 job / worker at a time and repeat
splitpb(10, 4)
#> [[1]]
#> [1] 1 2 3 4
#> 
#> [[2]]
#> [1] 5 6 7 8
#> 
#> [[3]]
#> [1]  9 10
#> 
## compare this to the no-progress-bar split
## that defines all the jubs / worker up front
parallel::splitIndices(10, 4)
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 4 5
#> 
#> [[3]]
#> [1] 6 7
#> 
#> [[4]]
#> [1]  8  9 10
#> 

## cap the length of the output
splitpb(20, 2, nout = NULL)
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 3 4
#> 
#> [[3]]
#> [1] 5 6
#> 
#> [[4]]
#> [1] 7 8
#> 
#> [[5]]
#> [1]  9 10
#> 
#> [[6]]
#> [1] 11 12
#> 
#> [[7]]
#> [1] 13 14
#> 
#> [[8]]
#> [1] 15 16
#> 
#> [[9]]
#> [1] 17 18
#> 
#> [[10]]
#> [1] 19 20
#> 
splitpb(20, 2, nout = 5)
#> [[1]]
#> [1] 1 2 3 4
#> 
#> [[2]]
#> [1] 5 6 7 8
#> 
#> [[3]]
#> [1]  9 10 11 12
#> 
#> [[4]]
#> [1] 13 14 15 16
#> 
#> [[5]]
#> [1] 17 18 19 20
#> 
```
