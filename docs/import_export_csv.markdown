# Project:M36 CSV Import/Export

Project:M36's `tutd` console supports the import and export of CSV files as a lowest-common denominator format for moving data. Because the CSV file format does not support all the features of Project:M36- most notably, nested relations- not all relation variables can be marshalled to-and-from CSV files.

## CSV Export

If a relation variable cannot be exported due to data types which are not supported by the CSV format- such as with a nested relation- then Project:M36 throws an error rather than export a partial file.

### Example Export

```
TutorialD (master/main): :importexample date
TutorialD (master/main): :exportcsv s "/tmp/csv"
```
```
$ cat /tmp/csv
s#,sname,status,city
S2,Jones,10,Paris
S1,Smith,20,London
S4,Clark,20,London
S5,Adams,30,Athens
S3,Blake,30,Paris
```

## CSV Import

An import is equivalent to an insert and does not replace the target relation variable.

Because relations do not have a natural ordering for their attributes, the CSV file to import must contain a header row which matches the names of attributes.

If a value in the CSV file cannot be converted to the expected type (determined by the existing relation attributes), then the entire import is aborted rather than perform a partial import.

### Example Import
```
$ cat /tmp/csv
s#,sname,status,city
S6,Samson,100,New York
```
```
TutorialD (master/main): :importexample date
TutorialD (master/main): :showexpr s
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Paris"   │"S2"    │"Jones"    │10             │
│"Paris"   │"S3"    │"Blake"    │30             │
│"London"  │"S4"    │"Clark"    │20             │
│"London"  │"S1"    │"Smith"    │20             │
│"Athens"  │"S5"    │"Adams"    │30             │
└──────────┴────────┴───────────┴───────────────┘
TutorialD (master/main): :importcsv "/tmp/csv" s
TutorialD (master/main): :showexpr s
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Paris"   │"S3"    │"Blake"    │30             │
│"London"  │"S4"    │"Clark"    │20             │
│"New York"│"S6"    │"Samson"   │100            │
│"Athens"  │"S5"    │"Adams"    │30             │
│"London"  │"S1"    │"Smith"    │20             │
│"Paris"   │"S2"    │"Jones"    │10             │
└──────────┴────────┴───────────┴───────────────┘
```

Note that relations do not have ordering, thus, no ordering from the CSV file is preserved.
