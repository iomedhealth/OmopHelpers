# buildCodelistTree

Traverses a nested list of concept sets (or codelists). For every node,
it recursively gathers all the concept IDs from its sub-nodes and
leaves, and injects a new \`omopgenerics::codelist\` object named
\`\$merged\` into each level.

## Usage

``` r
buildCodelistTree(node, node_name = "root")
```

## Arguments

- node:

  A list of lists or codelists, representing a semantic tree of
  concepts.

- node_name:

  Character. Name to assign to the merged codelist at the current node.

## Value

A nested list identical in structure to the input, but with \`\$merged\`
elements added.

## Examples

``` r
if (FALSE) { # \dontrun{
library(omopgenerics)
mock_list <- list(
  GroupA = list(Sub1 = newCodelist(list(a = 1:2))),
  GroupB = newCodelist(list(b = as.integer(3)))
)
res <- buildCodelistTree(mock_list)
print(res$merged)
print(res$GroupA$merged)
} # }
```
