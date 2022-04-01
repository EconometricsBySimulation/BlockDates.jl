module BlockDates

export blockdate,
       dateformat2regex,
       replacemonth, 
       removedays, 
       removestopwords,
       removecommonprefix,
       removejoiner,
       singleGenerator,
       singleYearMissGen,
       cleanDate,
       DateMod,
       formatRange,
       andSplit,
       textToBlock,
       spreadOverNonDates,
       joinDuplicateDates,
       fillMissing,
       scoreDatesOutside!,
       dropBadMatches,
       fixBlock,
       scoreOutofOrder!,
       scoreBlock
  

##############################################################################
##
## Dependencies
##
##############################################################################

using Dates,
    Distributions,
    StatsBase,
    IterTools,
    DataFramesMeta,
    Pipe,
    Test,
    NBInclude

##############################################################################
##
## Exported methods and types (in addition to everything reexported above)
##
##############################################################################

### Code Structure
# - text with dates are passed one by one to the softdate function
# - softdate passes txt to dt2block function which first passes txt to rangeformatter
# - if rangeformatter finds a range match then dates are spread over that range and returned to softdate
# - if rangeformatter does not find a match then singleformatter is attempted
# - if that finds a match then a single value is returned


# Previous dateformat2regex & rmwhitespace

"""dateformat2regex converts input dateformats into potential regex matches.
"""
function dateformat2regex(m::String)
    m = replace(m,   r"\bm\b" => "(?:[1][0-9]|[0]?[1-9])")
    m = replace(m,   r"\by\b" => "(?:[12][0-9]|20[12][0-9])")
    m = replace(m,   r"\bd\b" => "(?:[1-3][0-9]|[0]?[1-9])")
end

"Remove extra white space from x"
function rmwhitespace(x)
    strip(replace(x, r"\s+" => " "))
end

monthlist = [r"\bJanu?a?r?y?\b"i
            ,r"\bFebr?u?a?r?y?\b"i
            ,r"\bMarc?h?\b"i
            ,r"\bApri?l?\b"i
            ,r"\bMay\b"i
            ,r"\bJune?\b"i
            ,r"\bJuly?\b"i
            ,r"\bAugu?s?t?\b"i
            ,r"\bSept?e?m?b?e?r?\b"i
            ,r"\bOcto?b?e?r?\b"i
            ,r"\bNove?m?b?e?r?\b"i
            ,r"\bDece?m?b?e?r?\b"i]

"""
Replace month matches with numbers when found in text.
"""
function replacemonth(x, I...)
    for i in 1:12;
        x = replace(x, monthlist[i]=> string(i))
    end
    x
end

"""
Remove potential day name matches from text
"""
function removedays(x, I...)
    dayabrev = Regex("(?i)(^|\\b)(" * join(unique([[dn[1:min(i,end)]
      for dn in dayname.(now() + Day.(0:6)), i in 9:-1:1]...]),"|") * ")(\\b|\$)")
    replace(x, dayabrev => "") |> strip
end

"""
Remove potential stop words  and common words from x:
    "NOTE|Between|On|At|Continued|Date" and 
    "On|At|from"
"""
removestopwords(x) = replace(x, 
  r"^(?i)(-|:| )*(NOTE|Between|On|At|Continued|Date)\b(:| )*(On|At|from)*"=>"")

"""
Remove common prefixes in the text.
"""
removecommonprefix(x) = replace(x, "No Wireline Logs Run"=>"")

"""
Remove joiner text: "thru|through|to"
"""
removejoiner(x) = replace(x, r"\b(thru|through|to)\b" => " ")

monthN = "[0-9]{1,2}" # 02, 11, 10, 12, 4 , 3
dayN   = "[0-9]{1,2}" # 1 , 04, 15, 29, 30, 31
yearN  = "[0-9]{1,4}" # 2021, 2019, 2021

"""
    rangeGenerator(dateformat = "mm dd yyyy", scoremod = 5, mod = Day(0); dateformat2 = "", mod2 = Day(0))

Generates a function that will interpret the date range from text passed to it.

# Arguments
- `dateformat`:  date that the rangeInterpretor will attempt to match 
  dateformat to look for are of the type used with the DateFormat() function
- `score`:       base score to assign to this range interpretation
- `mod`:         number of Days, Years to add to the date after interpretation
- `dateformat2`: date that the rangeInterpretor will attempt to match for the second date
  if left empty it will use dateformat
- `mod2`:        number of Days, Years to add to the date2 after interpretation
"""
function rangeGenerator(dateformat = "mm dd yyyy", score = 5, mod = Day(0); dateformat2 = "", mod2 = Day(0))
    (dateformat2 == "") && (dateformat2 = dateformat; mod2 = mod)
    
    regmatcher = "^($dateformat) ($dateformat2)"
    
    for v in [r"\byyyy\b"=>yearN, r"\bdd\b"=>dayN , r"\bmm\b"=>monthN]
        regmatcher = replace(regmatcher, v)
    end
    
    function rangeInterpretor(textin; scoremod=0, defaultyear = year(now()))
        
        rangematch = match(Regex(regmatcher), textin)
        (rangematch === nothing) && return nothing
        
        try 
          starter  = Date(rangematch.captures[1], DateFormat(dateformat))  + mod
          ender    = Date(rangematch.captures[2], DateFormat(dateformat2)) + mod2
          return (
            starter  = starter  ,
            ender    = ender,
            scoremod = scoremod + score)
        catch
        end
    end
end

"""Interprets dates in the form "mm dd yyyy mm dd yyyy" and scores them as 5
"""
range1 = rangeGenerator("mm dd yyyy", 5)

"""Interprets dates in the form "dd mm yyyy dd mm yyyy" and scores them as 5
"""
range2 = rangeGenerator("dd mm yyyy", 5)

"""Allow for missing year in first date in range eg. `mm dd till mm dd yyyy` or 
  `dd mm till dd mm yyyy`
"""
function rangeSubGen(infunction::Function, penalty = -3)
    function(txtin;  scoremod=0, defaultyear = year(now()))
        txtout = replace(txtin, 
            r"([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{2,4})" => 
            s"\1 \2 \5 \3 \4 \5")
        infunction(txtout, scoremod= scoremod + penalty, defaultyear=defaultyear)
    end
end

"""Interprets dates in the form "mm dd mm dd yyyy" and scores them as 2
"""
rangeSub1 = rangeSubGen(range1)

"""Interprets dates in the form "dd mm dd mm yyyy" and scores them as 2
"""
rangeSub2 = rangeSubGen(range2)

"""
Missing Year in Date Range
Allow for missing years in date range eg. `mm dd mm dd` 
"""
function rangeYearMissGen(infunction::Function, penalty = -4)
    function(txtin;  scoremod=0, defaultyear = year(now()))
        txtout = replace(txtin, 
            r"([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,2})" => 
            SubstitutionString("\\1 \\2 $defaultyear \\3 \\4 $defaultyear"))
        infunction(txtout, scoremod= scoremod + penalty, defaultyear=defaultyear)
    end
end

"""Interprets dates in the form "mm dd mm dd" and scores them as 1
"""
rangeYearMiss1 = rangeYearMissGen(range1)

"""Interprets dates in the form "dd mm dd mm" and scores them as 1
"""
rangeYearMiss2 = rangeYearMissGen(range2)

"""
Missing Month in Part of Date Range
Allow for missing years in date range eg. `mm dd` through `dd yy` 
"""
function rangeMonthMissGen(infunction::Function, penalty = -5)
    function(txtin;  scoremod=0, defaultyear = year(now()))
        txtout = replace(txtin, 
            r"([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,4})" => 
            SubstitutionString("\\1 \\2 \\4 \\1 \\3 \\4"))
        infunction(txtout, scoremod= scoremod + penalty, defaultyear=defaultyear)
    end
end

"""Interprets dates in the form "mm dd dd yyyy" and scores them as 0
"""
rangeMonthMiss1 = rangeMonthMissGen(range1)

"""
Allow for missing year in first date `12/11 through 12/19/20` or 
    `11/12 through 19/12/20`
"""
function rangeMissYearFirstGen(infunction::Function, penalty = -5)
    function(txtin;  scoremod=0, defaultyear = year(now()))
        txtout = replace(txtin, 
            r"([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,4})" => 
            SubstitutionString("\\1 \\2 \\5 \\3 \\4 \\5"))
        infunction(txtout, scoremod= scoremod + penalty, defaultyear=defaultyear)
    end
end


"""Interprets dates in the form "mm dd mm dd yyyy" and scores them as 0
"""
rangeMissYearFirst1 = rangeMissYearFirstGen(range1)

"""Interprets dates in the form "dd mm dd mm yyyy" and scores them as 0
"""
rangeMissYearFirst2 = rangeMissYearFirstGen(range2)

"""
Allow for missing month in first date `14/20 through 07/21/20`.
"""
function rangeMissFirstMonthGen(infunction::Function, penalty = -6)
    function(txtin;  scoremod=0, defaultyear = year(now()))
        txtout = replace(txtin, 
            r"([0-9]{1,2}) ([0-9]{1,4}) ([0-9]{1,2}) ([0-9]{1,2}) ([0-9]{1,4})" => 
            SubstitutionString("\\3 \\1 \\2 \\3 \\4 \\5"))
        infunction(txtout, scoremod= scoremod + penalty, defaultyear=defaultyear)
    end
end

"""
Allow for missing month in first date `14/20 through 07/21/20`.
"""
rangeMissFirstMonth1 = rangeMissFirstMonthGen(range1)

"""
Allow for date to be found within the text `The event on July 3 2020 to July 10 2020.`
"""
function inTextGen(infunction::Function, penalty = -1)
    function(txtin;  scoremod=0, defaultyear = year(now()))
        m = match(r"([0-9]{1,4} [0-9]{1,4} [0-9]{1,4}\s*)+", txtin)
        (m != nothing) && (txtin = "$(String(m.match)) $txtin")
        infunction(txtin, scoremod= scoremod + penalty, defaultyear=defaultyear)
    end
end

"""
Allow for date to be found in text of the form 
    "On the date range mm dd yyyy mm dd yyyy something..."
"""
rangeInText1 = inTextGen(range1)

"""
Allow for date to be found in text of the form 
    "On the date dd mm yyyy dd mm yyyy something..."
"""
rangeInText2 = inTextGen(range2)

rangeset = [range1, range2 , rangeSub1 , rangeSub2 , 
    rangeYearMiss1, rangeYearMiss2, rangeMonthMiss1, 
    rangeMissYearFirst1, rangeMissYearFirst2,
    rangeMissFirstMonth1, rangeInText1, rangeInText2]

"""
    singleGenerator(dateformat = "mm dd yyyy", scoremod = 5, mod = Day(0))

Generates a function that will interprets a single date from text passed to it.

# Arguments
- `dateformat`:  date that the rangeInterpretor will attempt to match 
  dateformat to look for are of the type used with the DateFormat() function
- `scoremod`:    score to assign to this range interpretation
- `mod`:         number of Days, Years to add to the date after interpretation
"""
function singleGenerator(dateformat = "mm dd yyyy", score = 5, mod = Day(0))
    
    regmatcher = "^($dateformat)"
    
    for v in [r"\byyyy\b"=>yearN, r"\bdd\b"=>dayN, r"\bmm\b"=>monthN]
        regmatcher = replace(regmatcher, v)
    end
    
    function singleInterpretor(textin; scoremod = 0, defaultyear = now())
        
        singlematch = match(Regex(regmatcher), textin)
        
        (singlematch === nothing) && return nothing
        
        try 
#             @show regmatcher, textin, dateformat, singlematch.captures[1]
            starter  = Date(singlematch.captures[1], DateFormat(dateformat)) + mod
            return (starter  = starter  ,
                    scoremod = scoremod + score)
        catch
        end
    end
end

"""
Allow for date to be found in text of the form "mm dd yyyy something..."
"""
single0(x; scoremod = 0, defaultyear = now()) = (starter = Date(0), scoremod = 0)

"""
    single1(text, scoremod = 0, mod = Day(0))

Allow for date to be found in text of the form "mm dd yyyy something..."
"""
single1 = singleGenerator("mm dd yyyy", 5)

"""
    single2(text, scoremod = 0, mod = Day(0))

Allow for date to be found in text of the form "dd mm yyyy something..."
"""
single2 = singleGenerator("dd mm yyyy", 5)

"""
    single3(text, scoremod = 0, mod = Day(0))

Allow for date to be found in text of the form "yyyy mm dd something..."
"""
single3 = singleGenerator("yyyy mm dd", 5)

"""
    single4(text, scoremod = 0, mod = Day(0))

Allow for date to be found in text of the form "yyyy mm dd something..."
"""
single4 = singleGenerator("yyyy dd mm", 5)

"""
    singleYearMissGen(infunction::Function, penalty = -3)

Missing Year in Date Single
Allow for missing year in date eg. `mm dd` or `dd mm`
"""
function singleYearMissGen(infunction::Function, penalty = -3)
    function(txtin;  scoremod=0, defaultyear = year(now()))
        txtout = replace(txtin, 
            r"([0-9]{1,2}) ([0-9]{1,2})" => SubstitutionString("\\1 \\2 $defaultyear"))
        infunction(txtout, scoremod= scoremod + penalty, defaultyear=defaultyear)
    end
end

"""
    singleYearMiss1(text, scoremod = 0, defaultyear = year(now()))

Allow for date to be found in text of the form "mm dd something..."
"""
singleYearMiss1 = singleYearMissGen(single1)

"""
    singleYearMiss2(text, scoremod = 0, defaultyear = year(now()))

Allow for date to be found in text of the form "dd mm something..."
"""
singleYearMiss2 = singleYearMissGen(single2)

"""
    singleInText1(text, scoremod = 0, defaultyear = year(now()))

Allow for date to be found in text of the form "Something something mm dd yyyy something"
"""
singleInText1 = inTextGen(single1)

"""
    singleInText2(text, scoremod = 0, defaultyear = year(now()))

Allow for date to be found in text of the form "Something something mm dd yyyy something"
"""
singleInText2 = inTextGen(single2)

singleset = [single1, single2, single3, single4, 
             singleYearMiss1, singleYearMiss2,
             singleInText1, singleInText2, single0]

"""
    cleanDate(text)
    
Make text more readable by the date interpreter. (make lowercase, replace month with #,
remove days, remove common prefixes, remove stop words, remove joiners, replace markers used
to divide dates, and remove common numeric ordering tags (st, nd, rd).
"""
function cleanDate(txtin) 
    @pipe txtin |>
        lowercase |> 
        replacemonth |> removedays |> removecommonprefix |> 
        removestopwords |> removejoiner |>
        replace(_, r"[|./\-\\,\\:+]" => " ") |>
        replace(_, r"(st|nd|rd|th)\b"=>"") |>
        replace(_, r"[ ]+" => " ") |>
        strip
end


"""
    DateMod(inDate, scoremod; mods = "", defaultdate = Date(0))
    
Adjust dates to be within feasible range.
"""
function DateMod(inDate, scoremod; mods = "", defaultdate = Date(0))
    # If less than date(01-01-100) Add 2000 yrs
    (inDate < Date(100)) && 
         (inDate += Year(2000); scoremod -= 1; mods *= " +2000yr")
 
    # If someone swapped 220x when meant to write 202x
    (Date(2200) < inDate < Date(2220)) && 
       (inDate -= Year(180); scoremod -= 2; mods *= " -180yr")
     
    # If date 30xx when meant to write 20xx
    (Date(3000) < inDate < Date(3030)) && 
         (inDate -= Year(1000); scoremod -= 2; mods *= " -1000yr")
     
     # If date 203x when meant to write 202x
    (Date(2030) < inDate < Date(2040)) && 
         (inDate -= Year(10); scoremod -= 2; mods *= " -10yr")
     
     # If date 102x when meant to write 202x
    (Date(1000) < inDate < Date(1040)) && 
         (inDate -= Year(10); scoremod -= 2; mods *= " +1000yr")
     
     # If date 21x when meant to write 201x or 22x when meant 202x
    (Date(200) < inDate < Date(250)) && 
         (inDate += Year(1800); scoremod -= 2; mods *= " +1800yr")
     
     # In date is more than .5 years from the default date but no more than 1.5 years
     (defaultdate != Date(0)) && (Day(180) < defaultdate - inDate < Day(540)) &&
         (inDate += Year(1); scoremod -= 2; mods *= " +1yr")
     
     # In date is more than .5 years from the default date but no more than 1.5 years
     (defaultdate != Date(0)) && (Day(180) < inDate - defaultdate < Day(540)) &&
         (inDate -= Year(1); scoremod -= 2; mods *= " -1yr")
     
    (inDate=inDate, scoremod=scoremod, mods=strip(mods))
end

"""
    DateMod(inDate::Date, ender::Date, scoremod; mods = "", defaultdate = Date(0))

    Apply DateMod to inDate and ender separately then do two more modifications.
    If inDate is greater than ender swap.
    If the date range is too wide (greater than 30 days) force the end date to be
    only 6 days after the start date.
"""
function DateMod(inDate::Date, ender::Date, scoremod; 
    mods = "", defaultdate = Date(0))
    inDate, scoremod, mods = DateMod(inDate, scoremod, mods = mods, defaultdate=defaultdate)
    ender , scoremod, mods = DateMod(ender , scoremod, mods = mods, defaultdate=defaultdate)

    if inDate > ender; # Penalize ranges when start and end dates are swapped
        inDate , ender, scoremod = ender, inDate, scoremod -3; 
        mods *= " startEndSwap"
    end

    if (ender - inDate) > Day(30); # If date range is too wide "fix" ender
        inDate , ender, scoremod = inDate, inDate + Day(6), scoremod -5; 
        mods *= " interprettedRangeTooLarge"
    end

    (inDate=inDate, ender=ender, scoremod=scoremod, mods=strip(mods))
end

"""
    formatRange(textin; rangeformat = 1, defaultdate = Date(0))
    
Convert range from "11/29/2020 - 12/05/2020" to a dataframe with scoremods.
"""
function formatRange(textin; rangeformat = 1, defaultdate = Date(0))
    textCleaned = cleanDate(textin)
    
    r = rangeset[rangeformat](textCleaned, defaultyear=year(defaultdate))
    
    (r === nothing) &&
      (return DataFrame(date = Date(0), text = textin, scoremod = 0, mods = ""))
  
    starter , ender , scoremod, mods = DateMod(r..., defaultdate = defaultdate)
    return DataFrame(
        date     = starter:Day(1):ender, 
        text     = textin,
        scoremod = scoremod,
        mods     = String(mods))
end

"""
formatSingle(textin; singleformat = 1, defaultdate = Date(0))
    
Convert range from "11/29/2020 ..." to a dataframe with scoremods.
"""
function formatSingle(textin; singleformat = 1, defaultdate = Date(0))
    textclean = cleanDate(textin)
    r = singleset[singleformat](textclean, defaultyear = year(defaultdate))
    
    (r === nothing) && 
        (return DataFrame(date = Date(0), text = textin, scoremod = 0, mods = ""))

    starter , scoremod, mods = DateMod(r..., defaultdate = defaultdate)
        
    return DataFrame(date     = starter, 
                     text     = textin, 
                     scoremod = scoremod,
                     mods     = strip(String(mods)))
end

"""
    andSplit(txtin, splitters = "(?:\\/)", ands = "(?:and|,)")

A function to split dates that have groupings like "and" and ",".
"""
function andSplit(txtin, splitters = "(?:\\/)", ands = "(?:and|,)");
    andmatcher = r"^([a-zA-Z ]{0,5}((\d{1,4}(\/)\d{1,4})+\s*(,|and|\s)+\s*)+)"
    if occursin(andmatcher, txtin)
        m = match(andmatcher, txtin)
        txtdup = replace(txtin, m.captures[1] => "")
        x = [string(x[1]) for x in eachmatch(
            Regex("((?:[0-9]+" * splitters * ")+[0-9]+)"), m.captures[1])]
        return join(x .* " " .* txtdup, "\n")
    end
    return txtin
end

"""
    textToBlock(txtsplit::Array{String}; 
                    singleformat = 1, 
                    rangeformat  = 1,
                    defaultdate = Date(0))

Takes a date text string list and tries to match each element first as a range 
then as a single.
"""
function textToBlock(txtsplit::Array{String}; 
    singleformat = 1, 
    rangeformat  = 1,
    defaultdate  = Date(0))

    outframe = DataFrame()

    for tx in txtsplit;
        rangeattempt = formatRange(tx, 
            rangeformat = rangeformat, 
            defaultdate = defaultdate)

        if size(rangeattempt,1) > 1
            rangeattempt.mods .*= " rangefound"
            outframe = vcat(outframe, rangeattempt)
            continue
        end

        singleattempt = formatSingle(tx, 
                singleformat  = singleformat, 
                defaultdate   = defaultdate)
        outframe = vcat(outframe, singleattempt)

    end

    return outframe[strip.(outframe.text) .!= "", :]
end


"""
    spreadOverNonDates(inframe)

Merges rows with missing dates with previous row (unless first row then merges with next row).
"""
function spreadOverNonDates(inframe)
  
    # Create column to keep track of input dates == date(0)
    inframe[!, :date0] .= inframe.date
    
    # Track order that text is in
    inframe[!, :order] = 1:size(inframe,1)
      
    # Treat input text as a groupby unique ID to use for joining text fields without dates
    inframe[!, :block] .= inframe.text
  
    # Grab previous date if current date is missing
    for i in 2:size(inframe,1)
      (inframe[i, :date] == Date(0)) && 
          (inframe[i, [:block, :date]]  = inframe[i-1, [:block, :date]])
    end

    # Grab next date if current date is missing
    for i in (size(inframe,1)-1):-1:1
      (inframe[i, :date] == Date(0)) && 
          (inframe[i, [:block, :date]]  = inframe[i+1, [:block, :date]])
    end
      
    # Combine text across the block
    inframe = @pipe DataFramesMeta.groupby(inframe , :block) |> 
                @transform(_, :text = join(unique(:text), " \n "))
  
    # Drop anything that started with date == Date(0) ie missing
    inframe = inframe[(inframe.date0 .!= Date(0)) .| (inframe.date .== Date(0)), 
          Not([:date0, :block])]  
      
    inframe
end

"""
    joinDuplicateDates(inframe, penalty = 7)

Join any duplicate dates which have different text together.
Scoremod penalizes these observations by -7 number of duplicate dates.
"""
function joinDuplicateDates(inframe, penalty = 3)
  inframe = @pipe DataFramesMeta.groupby(inframe , :date) |>
    @combine(_, 
      :mods     = join(ifelse(length(:mods) > 1, 
                 vcat(unique(:mods)..., "duplicateDates"), unique(:mods)), " "),
      :scoremod = sum(:scoremod) - penalty*(length(:mods)-1),
      :order    = [[:order...]],
      :text     = join(unique(:text), " \n ")
  )
    
  (Date(0) in inframe.date) && (inframe.scoremod = [0]; inframe.mods = ["noDateFound"])
    
  inframe
end

"""
    fillMissing(inframe, dtstart = Date(0), dtend   = Date(0))

Join any duplicate dates which have different text together.
Scoremod penalizes these observations by -7 number of duplicate dates.
"""
function fillMissing(inframe, dtstart = Date(0), dtend   = Date(0))
  
    # Make sure all of the dates appear in the data
    (dtstart == Date(0)) && return inframe
      
    outframe = outerjoin(DataFrame(date = dtstart:Day(1):dtend), inframe, on = :date)
      
    outframe[ismissing.(outframe.text), :scoremod] .= -5
      
    outframe[!, :text] = 
      [coalesce(outframe.text[i], "<<NO TEXT FOUND FOR $(outframe.date[i])>>") 
          for i in 1:size(outframe,1)]
    
    outframe[ismissing.(outframe.mods), :mods] .= "filled"
      
    if (Date(0) in outframe.date)
        outframe.text = "Header Text: " .* outframe.text[outframe.date .== Date(0)] .* 
                        " " .* outframe.text
        outframe = outframe[outframe.date .!= Date(0), :]
    end
    
    outframe
end

"""
   scoreDatesOutside!(inblock; dtstart, dtend, penalty = 5)

Scores dates outside of the dtstart - dtend range. Dates which are outside 
get penalized by the penalty value. If they are outside by more than 1 2 4 8 16 25 100
weeks then then accumulate another penalty for each step and the "outOfRange" gets a "*"
appended to it for each step.

Parameters:
inblock: DataFrame containing date, mod, and scoremod columns
dtstart: the start of the interval of interest
dtend: the end of the interval of interest
penalty: the penalty to the scoremod for each step out of the dtstart/dtend range
"""
function scoreDatesOutside!(inblock; dtstart, dtend, penalty = 5)
    outOfRange = (inblock.date .> dtend) .| (inblock.date .< dtstart)
    inblock[outOfRange, :mods] = strip.(inblock[outOfRange, :mods] .* " outOfRange")
    inblock[outOfRange, :scoremod] .-= penalty
    
    step = Day(7)
    for i in [1 2 4 8 16 25 50 100]
        outOfRange = (inblock.date .> dtend + i*step) .| (inblock.date .< dtstart - i*step)
        inblock[outOfRange, :mods] = strip.(inblock[outOfRange, :mods] .* "*")
        inblock[outOfRange, :scoremod] .-= penalty
    end
    
    inblock
end

"""
   dropBadMatches(inframe::DataFrame, dropscore = -15)

Removes the date and score from any dates for which their interpretation is extremely
low scored. This is most often helpful when a non-date becomes interpetted as a date.
"""
function dropBadMatches(inframe::DataFrame, dropscore = -15)
    dropscores = (inframe.scoremod  .<= dropscore) .& (inframe.date .!= Date(0))
    any(dropscores) && (inframe[dropscores, :date]     .= Date(0); 
                        inframe[dropscores, :scoremod] .= 0)
    return inframe
end

"""
    fixBlock(inframe::DataFrame; dtstart = Date(0), dtend   = Date(0))

    inframe : a dataframe produced by `textToBlock` textToBlock
    dtstart : start date
    dtend   : end date

    Applies functions scoreDatesOutside!, dropBadMatches, spreadOverNonDates, joinDuplicateDates, and fillMissing to inframe.

"""
function fixBlock(inframe::DataFrame; dtstart = Date(0), dtend   = Date(0))

    outframe = copy(inframe)
      
    outframe = scoreDatesOutside!(outframe, dtstart = dtstart, dtend = dtend)
  # println("outframe = scoreDatesOutside!(outframe, dtstart = dtstart, dtend = dtend)")  
      
    outframe = dropBadMatches(outframe)
  # println("outframe = dropBadMatches(outframe)")  
      
    outframe = spreadOverNonDates(outframe)
  # println("outframe = spreadOverNonDates(outframe)")  
  
    outframe = joinDuplicateDates(outframe)
  # println("outframe = joinDuplicateDates(outframe)")  
      
    (dtstart != Date(0)) && (outframe = fillMissing(outframe, dtstart, dtend))
      
    outframe = sort(outframe[:, [:date, :mods, :scoremod, :order ,:text]], :date)
      
    return outframe
    
end

"""
    scoreOutofOrder!(inblock, dtstart, dtend)

penalizes dates out of order
"""
function scoreOutofOrder!(inblock; dtstart=now(), dtend=now()-Day(7))
    inview = view(inblock, .!ismissing.(inblock.order), :)
    for i in 2:size(inview, 1)
        ((maximum(inview.order[i-1]) > maximum(inview.order[i])) |
         (minimum(inview.order[i-1]) > minimum(inview.order[i]))) &&
            (inview[i, :mods]     = strip(inview[i, :mods] * " outOfOrder");
             inview[i, :scoremod] = inview[i, :scoremod] - 2)
    end
    inblock
end

"""
    scoreBlock(inframe; dtstart = Date(0), dtend = Date(0))

Score the passed block by first adjusting for out of order entries then
    taking the average of the scoremod values.

"""
function scoreBlock(inframe; dtstart = Date(0), dtend = Date(0))
    (dtstart == Date(0)) && (dtstart = minimum(inframe.date))
    (dtend   == Date(0)) && (dtend   = maximum(inframe.date))
    
#     inframe = scoreDatesOutside!(inframe, dtstart = dtstart, dtend = dtend)
    inframe = scoreOutofOrder!(inframe, dtstart = dtstart, dtend = dtend)

    inframe[!, :score] .= sum(inframe.scoremod)/size(inframe, 1)
    inframe = inframe[!, [:date, :mods, :scoremod, :score, :order, :text]]
    
    return inframe
end

"""
    blockdate(
        txtin; 
        dtstart         = Date(0), 
        dtend           = Date(0),
        splits          = r\"[\\n\\r]+\",
        singleformats   = length(singleset),
        rangeformats    = length(rangeset),
        targetScore     = 5,
        verbose         = false)

        txtin         : Text block unput into blockdates
        dtstart       : Start date of dates
        dtend         : End date of dates  
        splits        : Defines the character used to split the input text. Default is line breaks    
        singleformats : Single formats allowed integers
        rangeformats  : Range formats allowed integers
        targetScore   : Cut score considered good enough
        verbose       : Provide extra feedback
"""
function blockdate(txtin; dtstart = Date(0), dtend = Date(0),
    splits          = r"[\n\r]+",
    singleformats   = length(singleset),
    rangeformats    = length(rangeset),
    targetScore     = 5,
    verbose         = false)

    scoreMax, outframeMax = -999, DataFrame()

    txtSplit = String.([strip(tx) for tx in split(txtin, splits) if strip(tx) != ""])

    for s in 1:singleformats, r in 1:rangeformats

    outframe = @pipe textToBlock(txtSplit, singleformat = s, rangeformat  = r, defaultdate = dtstart) |>
                     fixBlock(  _, dtstart = dtstart, dtend = dtend)  |>
                     scoreBlock(_, dtstart = dtstart, dtend = dtend)

    if (outframe.score[1] > scoreMax)
        outframe[!, :singleformat] .= s
        outframe[!, :rangeformat]  .= r   
        scoreMax, outframeMax = outframe.score[1], outframe
        verbose && (@show outframeMax)

        (scoreMax >= targetScore) && verbose &&
        (println("targetScore ($targetScore) achieved."); break)
    end

    end

    (scoreMax < targetScore) && verbose &&
    println("targetScore ($targetScore) not achieved.")

    outframeMax
end

#############################################################
end # module

