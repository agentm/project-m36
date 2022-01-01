# Project:M36 DataFrames

Users of typical relational algebra engines have come to expect sorting and limiting of query results despite the fact that relations do not support ordering. To that end, Project:M36 supports converting relations to "data frames" to support sorting on attributes and limiting the maximum tuple count in the result set.

The equivalent feature in SQL is invoked by the `ORDER BY`, `LIMIT`, and `OFFSET` clauses.

## Usage

From within the `tutd` console, converting a relation to be sorted as a data frame is invoked using the `:showdataframe` command.

```
:showdataframe <relational expression> orderby {<attribute {ascending,descending}>} {offset <num>} {limit <num>}
```

The default sort order is `ascending`.

## Examples

```
TutorialD (master/main): :showdataframe s orderby {status}
┌──┬───────────┬─────────┬────────────┬────────────────┐
│DF│city::Text↕│s#::Text↕│sname::Text↕│status::Integer⬆│
├──┼───────────┼─────────┼────────────┼────────────────┤
│1 │"Paris"    │"S2"     │"Jones"     │10              │
│2 │"London"   │"S1"     │"Smith"     │20              │
│3 │"London"   │"S4"     │"Clark"     │20              │
│4 │"Athens"   │"S5"     │"Adams"     │30              │
│5 │"Paris"    │"S3"     │"Blake"     │30              │
└──┴───────────┴─────────┴────────────┴────────────────┘
TutorialD (master/main): :showdataframe s{status} orderby {status}
┌──┬────────────────┐
│DF│status::Integer⬆│
├──┼────────────────┤
│1 │10              │
│2 │20              │
│3 │30              │
└──┴────────────────┘
TutorialD (master/main): :showdataframe s{status} orderby {status descending} limit 1
┌──┬────────────────┐
│DF│status::Integer⬇│
├──┼────────────────┤
│1 │30              │
└──┴────────────────┘
TutorialD (master/main): :showdataframe s{status} orderby {status descending} offset 1 limit 3
┌──┬────────────────┐
│DF│status::Integer⬇│
├──┼────────────────┤
│1 │20              │
│2 │10              │
└──┴────────────────┘

```
The arrow in the attributes indicates the sort order while the DF column indicates the row number based on the sort order. Column ordering can also be arbitrary.