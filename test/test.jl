# cd("E:/WAR_processing/SoftDates")
# using Pkg
# Pkg.activate(".")

# include("../src/BlockDates.jl")

using Main.BlockDates, Dates, DataFrames

using Main.BlockDates: dateformat2regex, replacemonth, removedays, removestopwords, removecommonprefix, removejoiner, range1, range2, rangeSub1, rangeSub2, rangeYearMiss1, rangeYearMiss2, rangeMonthMiss1, rangeMissYearFirst1, rangeMissYearFirst2, rangeMissFirstMonth1, rangeInText1, rangeInText2, single0, single1, single2, single3, single4, singleInText1, singleInText2, cleanDate, DateMod, formatRange, formatSingle

using Test

@test dateformat2regex("m d y") == 
    "(?:[1][0-9]|[0]?[1-9]) (?:[1-3][0-9]|[0]?[1-9]) (?:[12][0-9]|20[12][0-9])"

@test dateformat2regex("y d m") == 
    "(?:[12][0-9]|20[12][0-9]) (?:[1-3][0-9]|[0]?[1-9]) (?:[1][0-9]|[0]?[1-9])"

@test dateformat2regex("y d m") == 
    "(?:[12][0-9]|20[12][0-9]) (?:[1-3][0-9]|[0]?[1-9]) (?:[1][0-9]|[0]?[1-9])"

# Requires exact number for match
@test dateformat2regex("y d m This is something") == 
    "(?:[12][0-9]|20[12][0-9]) (?:[1-3][0-9]|[0]?[1-9]) (?:[1][0-9]|[0]?[1-9]) This is something"

@test replacemonth("Jan") == "1"
@test replacemonth("June") == "6"
@test replacemonth("Feb") == "2"
@test replacemonth("Jul Aug Sep Oct Nov Dec") == "7 8 9 10 11 12"

@test removedays("Mon") == ""
@test removedays("tuesday June-02-2021") == "June-02-2021"

@test removestopwords("On Tuesday June-02-2021") == "Tuesday June-02-2021"

@test removecommonprefix("No Wireline Logs Run") == ""

@test removejoiner("Mon thru Fri") == "Mon   Fri"

@test replacemonth("Sept 7th 2019")    == "9 7th 2019"
@test replacemonth("February 14 2019") == "2 14 2019"
@test replacemonth("Mar 16 2019")      == "3 16 2019"

@test removedays("Monday mond test") == "test"
@test removedays(" mond test")       == "test"
@test removedays("Wednesday test")   == "test"

@test removestopwords("- on 12/12/12 dfvdfg.") == "12/12/12 dfvdfg."
@test removecommonprefix("No Wireline Logs Run: 07-31-2021") == ": 07-31-2021"
@test removejoiner("12 thru 13") == "12   13"

@test range1("07 25 2021 07 31 2021") == 
  (starter = Date("2021-07-25"), ender = Date("2021-07-31"), scoremod = 5)
@test range1("07 25 2021 07 31 2021", scoremod = -2) == 
  (starter = Date("2021-07-25"), ender = Date("2021-07-31"), scoremod = 3)
@test range1("06 25 2021 06 31 2021") == nothing # June does not have 31 days

@test range2("25 07 2021 31 07 2021") == 
  (starter = Date("2021-07-25"), ender = Date("2021-07-31"), scoremod = 5)
@test range2("25 06 2021 31 06 2021") == nothing # June does not have 31 days

@test rangeSub1("07 25 07 31 2020") == 
  (starter = Date("2020-07-25"), ender = Date("2020-07-31"), scoremod = 2)
@test rangeSub2("25 07 31 07 2020") == 
  (starter = Date("2020-07-25"), ender = Date("2020-07-31"), scoremod = 2)
@test rangeSub2("25 07 31 07 20") == 
  (starter = Date("0020-07-25"), ender = Date("0020-07-31"), scoremod = 2)

@test rangeYearMiss1("07 25 07 31", defaultyear = 2020) == 
  (starter = Date("2020-07-25"), ender = Date("2020-07-31"), scoremod = 1)

@test rangeYearMiss2("25 07 31 07", defaultyear = 2020) == 
  (starter = Date("2020-07-25"), ender = Date("2020-07-31"), scoremod = 1)

@test rangeYearMiss1("12 03 12 6", defaultyear = 2017) == 
  (starter = Date("2017-12-03"), ender = Date("2017-12-06"), scoremod = 1)

@test rangeMonthMiss1("1 17 20 2017 A non-standard range input") == 
  (starter = Date("2017-01-17"), ender = Date("2017-01-20"), scoremod = 0)

@test rangeMissYearFirst1("7 25 7 28 2021 A non-standard range input") == 
  (starter = Date("2021-07-25"), ender = Date("2021-07-28"), scoremod = 0)

@test rangeMissYearFirst2("25 7 28 7 2021 A non-standard range input") == 
  (starter = Date("2021-07-25"), ender = Date("2021-07-28"), scoremod = 0)

@test rangeMissFirstMonth1("14 20 07 21 20") == 
  (starter = Date("0020-07-14"), ender = Date("0020-07-21"), scoremod = -1)

@test rangeInText1("The event on 7 3 2020 7 10 2020.").starter == Date("2020-07-03")
@test rangeInText2("The event on 3 7 2020 10 7 2020.").scoremod == 4

@test single0("3. 6-5/8").scoremod == 0

@test single1("07 31 2021") == (starter = Date("2021-07-31"), scoremod = 5)
@test single1("06 31 2021")  == nothing # June does not have 31 days

@test single2("31 07 2021") == (starter = Date("2021-07-31"), scoremod = 5)
@test single2("31 06 2021") == nothing # June does not have 31 days

@test single3("2021 07 31") == (starter = Date("2021-07-31"), scoremod = 5)
@test single3("2021 06 31")  == nothing # June does not have 31 days

@test single4("2021 31 07") == (starter = Date("2021-07-31"), scoremod = 5)
@test single4("2021 31 06") == nothing # June does not have 31 days

@test singleInText1("The event on 7 3 2020.").starter == Date("2020-07-03")
@test singleInText2("The event on 3 7 2020.").scoremod == 4

@test cleanDate("12/06/2020 ipsum HAKLU") == "12 06 2020 ipsum haklu"
@test cleanDate("12/06/2020 - 12/12/2020 - Ipsum Actius") == 
                "12 06 2020 12 12 2020 ipsum actius"
@test cleanDate("12 06 2020     12 12 2020 - Ipsum Actius") ==  
                "12 06 2020 12 12 2020 ipsum actius"
@test cleanDate("12.06.20 thru 12.12.20 thru Ipsum Actius") == 
                "12 06 20 12 12 20 ipsum actius"

@test DateMod(single1("07 31 021")...).mods == "+2000yr"
@test DateMod(single1("07 31 3021")...).mods == "-1000yr"
@test DateMod(single1("07 31 2031")...).mods == "-10yr"
@test DateMod(single1("07 31 2201")...).mods == "-180yr"
@test DateMod(single1("07 31 2020")..., defaultdate = Date("2021-07-31")).mods == "+1yr"
@test DateMod(range1("07 31 221 8 7 2021")...).mods == "+1800yr"
@test DateMod(range1("04 30 2021 08 07 2021")...).mods == "interprettedRangeTooLarge"
@test DateMod(range1("07 31 0021 8 7 3021")...).mods == "+2000yr -1000yr"
@test DateMod(range1("8 7 3021 07 31 0021")...).mods == "-1000yr +2000yr startEndSwap"
@test DateMod(range1("8 7 3021 07 31 0022")..., defaultdate = Date("2021-07-31")).mods ==
    "-1000yr +2000yr -1yr startEndSwap"

x = formatRange("23213 1212 321 No range data found", rangeformat=1)
@test size(x) == (1, 4)

x = formatRange("11/29/20 thru 12/05/20 Ipsum Actius", rangeformat=1)
@test x[1, :mods] == "+2000yr +2000yr"

x = formatRange("11/29/20 thru 12/05/20 Ipsum Actius", rangeformat=1) 
@test x[!,:date] == Date("2020-11-29"):Day(1):Date("2020-12-05")

x = formatRange("11/29/20 thru 30 Ipsum Actius"      , rangeformat=1) 
@test x[1,:date] == Date(0) # Unsupported format

x = formatRange("12/05/20 to 11/29/20 Ipsum Actius"  , rangeformat=1) 
@test x[1,:scoremod] == 0 # Penalize date start and end swapped

x = formatRange("1   1 2015:5/1/2015 Date mismatch input", rangeformat=2)
@test x[1,:scoremod] == 5

x = formatRange("12/03-12/06: Something something something", rangeformat=5)
@test size(x,1) == 4

x = formatSingle("23213 1212 321 No date found", singleformat=1)
@test size(x) == (1, 4)

@test formatSingle("31 07 2021 Something", singleformat = 2) == 
  DataFrame(date = Date("2021-07-31"), text = "31 07 2021 Something", scoremod = 5, mods = "")

@test formatSingle("21 30 06 ", singleformat = 4)[1,:date] == Date("2021-06-30")

@test formatSingle("27-DEC-2020: ipsum lorem lorem", singleformat = 2)[1, :scoremod] ==  5

txt = """
03/14/2022 went for a walk
and a stroll

03/15 went sky diving

03/16/2022 went fishing
"""
txtout = blockdate(txt; dtstart = Date("2022-03-14"), dtend = Date("2022-03-16"))

@test size(txtout, 1)   == 3
@test txtout[1, :score] == 2.0

