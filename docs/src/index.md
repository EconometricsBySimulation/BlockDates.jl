# BlockDates.jl
*A Context aware fuzzy date matching tool*

The date is often a critical piece of information for safety data analysis. It provides context and is necessary for measurement of event frequency and time-based trends. In some data sources, such as narrative information about an event or subject, the date is provided in various non-standardized formats. The Bureau of Transportation Statistics uses data provided in narrative, free-text format to validate and supplement reported safety event data.

We developed the open-source software package BlockDates using the Julia programming language to allow the extraction of fuzzy-matched dates from a block of text. The tool leverages contextual information and draws on external date data to find the best date matches.  For each identified date, multiple interpretations are proposed and scored to find the best fit. The output includes several record-level variables that help explain the result and prioritize error detection.

In a sample of 59,314 narrative records that include dates, the tool returned positive scores for 96.5% of records, meaning high confidence the selected date is valid.  Of those with no matching date, 77.9% were recognized correctly as having no viable match.


## Package Features
- Extract embedded dates and date ranges from text block

## Example

```julia
using BlockDates, Dates

txt = """
03/14/2022 went for a walk
and a stroll
03/15 went sky diving
03/16/2022 went fishing
"""

blockdate(txt; dtstart = Date("2022-03-14"), dtend = Date("2022-03-16"))

3×8 DataFrame
 Row │ date        mods     scoremod  score    order    text                               singleformat  rangeformat 
     │ Date        String?  Int64?    Float64  Array…?  String                             Int64         Int64       
─────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────
   1 │ 2022-03-14                  2      2.0  [1]      03/14/2022 went for a walk \n an…             5            1
   2 │ 2022-03-15                  2      2.0  [3]      03/15 went sky diving                         5            1
   3 │ 2022-03-16                  2      2.0  [4]      03/16/2022 went fishing                       5            1
```

## Function Documentation
```@docs
blockdate
dateformat2regex
replacemonth 
removedays 
removestopwords
removecommonprefix
removejoiner
singleGenerator
singleYearMissGen
cleanDate
DateMod
formatRange
andSplit
textToBlock
spreadOverNonDates
joinDuplicateDates
fillMissing
scoreDatesOutside!
dropBadMatches
fixBlock
scoreOutofOrder!
scoreBlock
```
