
try
    cd("C:/Users/francis.smart.ctr/GitDir/SoftDates.jl")
catch
    cd("C:/Users/Francis Smart/Documents/GitHub/SoftDates.jl")
end
using Pkg
Pkg.activate(".")
using Pkg, Dates, DataFrames, Distributions, CSV, StatsBase, IterTools

#### Code Structure
## text with dates are passed one by one to the softdate function
## softdate passes txt to dt2block function which first passes txt to rangeformatter
## if rangeformatter finds a range match then dates are spread over that range
## and returned to softdate
## if rangeformatter does not find a match then singleformatter is attempted
## if that finds a match then a single value is returned

# This function converts input dateformats into potential regex matches
# It assumes years fall between 2000-29. Change this by changing yrpre or
# yrdec year decade restricts the decade of the year while year prefix restricts
# the melenium.

# Previous General Setup
##########################################################################################
##########################################################################################
# Previous dateformat2regex & rmwhitespace

function dateformat2regex(m::String; yrpre = "20", yrdec = "[0-2]")
    m = replace(m,   r"\bmm\b" => "[0-1]?[0-9]")
    m = replace(m, r"\byyyy\b" => "$yrpre$yrdec[0-9]")
    m = replace(m,   r"\byy\b" => "$yrdec[0-9]")
    m = replace(m,   r"\bdd\b" => "[0-3]?[0-9]")
end

dateformat2regex("yyyy dd mm")
dateformat2regex("yy dd mm")
# Requires exact number for match
dateformat2regex("yy d mm This is something")

rmwhitespace(x) = strip(replace(x, r"\s+" => " "))
rmwhitespace(" asdf  asdf as")
rmwhitespace.([" asdf  asdf as", " 1  2   3   4"])

# Previous dateformat2regex & rmwhitespace
##########################################################################################
##########################################################################################
# Previous rangeformatter

### Range Date Format
# Function for formatting ranged dates
function rangeformatter(txtin; rangeformat, seperator = "through|till| ", toomany = 30,
      trans1=(x,I...)->x, trans2=(x,I...)->x, dt1=Date(0), dt2=Date(0))
    txt = trans1(txtin, dt1, dt2)
    txt = trans2(txt,   dt1, dt2)
    txt = replace(txt, r"[|./\\-]" => " ")

    txt = replace(txt, r"\b([0-9])\b" => s"0\1") |> x->replace(x, r"[ ]+" => " ")

    # Input range formats
    rf1 = rangeformat[1]
    if length(rangeformat) == 1 ; rf2 = rangeformat[1]; end
    if length(rangeformat) >= 2 ; rf2 = rangeformat[2]; end

    left, right = dateformat2regex.([rf1,rf2])
    rangematch = match(Regex("^($left)([ ]*)($seperator)([ ]*)($right)"), txt)

    (rangematch === nothing) && (return [])

    m1 = rangematch.captures[1]
    m2 = rangematch.captures[5]

    # Interprettable range formats
    if length(rangeformat) == 4 ; rf1 = rangeformat[4]; rf2 = rangeformat[4]; end
    if length(rangeformat) == 5 ; rf1 = rangeformat[4]; rf2 = rangeformat[5]; end

    if length(rangeformat) >= 3 ; m1, m2 = rmwhitespace.(rangeformat[3](m1, m2)); end

    try
        dt1 = Date(m1, DateFormat(rf1))
        dt2 = Date(m2, DateFormat(rf2))

        # Define a place to start capturing the text input after the date
        starter = length(rangematch.match) + (length(txtin) - length(txt)) + 1

        #txtout = strip(txtin[starter:end])
        txtout = strip(txtin)
        # txtout = replace(txtin, Regex("^($left)([ ]*)($seperator)([ ]*)($right)")=>"")
        indt   = txtin[1:(starter - 1)]
        # indt   = rangematch.match


        (length(dt1:Day(1):dt2) > toomany) && return []

        return DataFrame(date = collect(dt1:Day(1):dt2),
        txt = fill(txtout, length(dt1:Day(1):dt2)),
        indt = fill(indt, length(dt1:Day(1):dt2)))
    catch
        return  []
    end
end

rangeformatter(;txtin::AbstractString, rangeformat) = rangeformatter(txtin, rangeformat = rangeformat)

# Extra spaces between dates are ignore (after the first)
rangeformatter(txtin = "1 1  2015 till   5/1/2015 Date match", rangeformat = ["dd mm yyyy"])

# fail from date misspecification, fits too many dates, second is misspecified and fits no dates
rangeformatter("2/1/2017 through 12/1/2017 Date mismatch", rangeformat = ["mm dd yyyy"])
rangeformatter("16/1/2017 through 19/1/2017 Date mismatch", rangeformat = ["mm dd yyyy"])

#### Some date ranges are so messy they have to borrow elements from each other
convertmdy2mdY(x, y) = ["$(x[1:(end - 2)])$(y[(end - 3):(end - 2)])$(x[(end - 1):end])", y]
convertmdy2mdY("11 12 17", "11 15 2017")

rg = rangeformatter("11 12 17 through 11 15 2017 ipsum tadsf",
  rangeformat = ["mm dd yy", "mm dd yyyy", convertmdy2mdY])

#### Some date ranges are so messy they have to borrow elements
convertmd_dy2mdy(x, y) = ["$x $(y[(end - 3):end])", "$(x[1:2]) $y"]
convertmd_dy2mdy("07 16", "19 2019")

# fail from insufficient date data
rangeformat = ("mm dd", "dd yyyy", convertmd_dy2mdy, "mm dd yyyy")
rangeformatter("1/10 - 15/2017 Range Match!", rangeformat = rangeformat)


# Previous rangeformatter
##########################################################################################
##########################################################################################
# Next singleformatter

#### Single Date Format
# Function for formatting single dates
function singleformatter(txtin; singleformat::Array = ["mm dd yyyy"],
      trans1=(x,I...)->x, trans2=(x,I...)->x, dt1=Date(0), dt2=Date(0))
    txt = trans1(txtin, dt1, dt2)
    txt = trans2(txt,   dt1, dt2)
    txt = replace(txt, r"[|./\\-]" => " ")
    txt = replace(txt, r"\b([0-9])\b" => s"0\1") |> x->replace(x, r"[ ]+" => " ")

    sf = singleformat[1]

    left = dateformat2regex.(sf)
    singlematch = match(Regex("^($left)"), txt)

    (singlematch === nothing) && (return DataFrame(date = Date(0), txt = txt, indt = ""))

    sm = singlematch.match

    if length(singleformat) >= 2 ; sm = rmwhitespace.(singleformat[2](sm)); end
    if length(singleformat) == 3 ; sf = singleformat[3]; end

    try
        dt1 = Date(sm, DateFormat(sf))
        # Define a place to start capturing the text input after the date
        starter = length(sm)  + (length(txtin) - length(txt)) +1
        # starter = length(rangematch.match) + (length(txtin) - length(txt))

        txtout = strip(txtin[starter:end])
        txtout = strip(txtin)
        indt   = strip(txtin[1:(starter)])

        return DataFrame(date = dt1, txt = txtout, indt = indt)
    catch
        return DataFrame(date = Date(0), txt = txtin, indt = "")
    end
end

sg1 = singleformatter("1 16 2017 A date", singleformat = ["mm dd yyyy"])
sg2 = singleformatter("443.43 Not a date", singleformat = ["mm dd yyyy"])
sg3 = singleformatter("16 1 2017 A date mismatch", singleformat = ["mm dd yyyy"])
sg4 = singleformatter("16  1  2017 A date match", singleformat = ["dd mm yyyy"])
sg5 = singleformatter("16.1.2017 A date match", singleformat = ["dd mm yyyy"])
sg6 = singleformatter("16/1/2017 A date match", singleformat = ["dd mm yyyy"])
sg7 = singleformatter("16-1-2017 A date match", singleformat = ["dd mm yyyy"])

sg8 = singleformatter("1 16 17 A date", singleformat = ["mm dd yy", (x->x[1:(end-2)] * "20" * x[(end-1):end]), "mm dd yyyy"])

# Previous singleformatter
##########################################################################################
##########################################################################################
# Next dropmerger

# With a dataframe
# dt | txt       | ...
# 5  | entry a   | ...
# 6  | entry a   | ...
# 0  | spread    | ...
function dropmerger(inframe::DataFrame; excludevalue = 0, exclude::Symbol=:date, merger::Symbol=:txt)
  outframe = inframe
  i = 2
  while i <= size(outframe)[1]
    if outframe[i, exclude] == excludevalue
      lastgroup(a) = (length(a)-length(collect(takewhile(isequal(last(a)),reverse(a))))+1):length(a)
      colsel = lastgroup(outframe[merger][1:(i-1)])
      outframe[merger][colsel] .= join(outframe[merger][(i - 1):i], "\n\r")
      outframe = outframe[i .!= 1:size(outframe)[1],:]
    else
      i += 1
    end
  end
  outframe
end

inframe=mydata = DataFrame(date = [0,1,2,3,4,4,4,0,0],
    txt = ["Some note", fill("Input repeated", 2)..., "Some input", fill("Input repeated", 3)..., "Belong with range", "Me too"],
    othervar = 1:9)
dropmerger(mydata, excludevalue = 0, exclude = :date, merger = :txt)

# Previous singleformatter
##########################################################################################
##########################################################################################
# Next dt2block

######
## Takes a date text string Array and tries to match first ranges then singles
## Uses dtstart and dtend to fill in missing
function dt2block(txtin;
  dtstart = Date(now() - Day(10)),
  dtend   = Date(now()),
  singleformat = ["mm dd yyyy"],
  rangeformat = ["mm dd yyyy"],
  seperator   = "through|till| |to",
  fillmissing = true,
  trans1=(x,I...)->x,
  trans2=(x,I...)->x)

  outframe = DataFrame()

  for tx in txtin;
  # global outframe
    rangeattempt = rangeformatter(tx, rangeformat = rangeformat, seperator = seperator,
      trans1=trans1, trans2=trans2, dt1=dtstart, dt2=dtend)

    if size(rangeattempt)[1] > 0
        outframe = vcat(outframe, rangeattempt)
        continue
    end
    singleattempt = singleformatter(tx, singleformat = singleformat,
      trans1=trans1, trans2=trans2, dt1=dtstart, dt2=dtend)
    outframe = vcat(outframe, singleattempt)
    continue
  end

  # Fill in any missing dates if dates exist in range but not in set
  if fillmissing; for dt in Date(dtstart):Day(1):Date(dtend);
    !(dt ∈ outframe[:date]) && (append!(outframe, DataFrame(date = dt, txt = "<<Input Missing>>", indt = "")))
  end; end

  outframe = dropmerger(outframe, excludevalue = Date(0), exclude = :date, merger = :txt)

  #outframe = outframe[outframekeep, :]

  outframe[:day]   = day.(outframe[:date])
  outframe[:month] = month.(outframe[:date])

  # If there are any fields which have been filled in but have the same day and month
  # as a missing field then replace dates from missing fields and drop filled
  # in dates. Fixes sloppy 12-30-2018, 12-31-2018, 01-01-2018
  keepframe = fill(true, size(outframe)[1])
  for i in 1:(size(outframe)[1] - 1), j in (i+1):size(outframe)[1]
      if outframe[i, :day] == outframe[j, :day] && outframe[j, :txt] == "<<Input Missing>>"
          outframe[i, :date] = outframe[j, :date]
          keepframe[j] = false
      elseif outframe[i, :day] == outframe[j, :day] && outframe[i, :txt] == "<<Input Missing>>"
          outframe[j, :date] = outframe[i, :date]
          keepframe[i] = false
      end
  end
  outframe = outframe[keepframe, :]

  outframe
end
################################################### - End dt2block

# Alternative specification of dt2block
dt2block(txt, dtstart, dtend, I...) = dt2block(txt, dtstart = dtstart, dtend = dtend, I...)

#### Test block 1
dtstart = Date("2017 16 01", dateformat"yyyy dd mm")
dtend   = Date("2017 25 01", dateformat"yyyy dd mm")

outframe = dt2block(["1 15 2017 an out of date date",
  "1 16 2017 A date",
  "1 18 2017 - 01 24 2017 A range",
  "A non-date"], dtstart = dtstart, dtend = dtend)

#### Test block 2
dtstart = Date("2015 16 11", dateformat"yyyy dd mm")
dtend   = Date("2015 20 11", dateformat"yyyy dd mm")

rangeformat_md_dy2mdy = ("mm dd", "dd yyyy", convertmd_dy2mdy, "mm dd yyyy")
    dt2block(["11 17 2015 a date in range","1 18 through 20 2015 A range match"],
    dtstart = dtstart, dtend = dtend, rangeformat = rangeformat_md_dy2mdy)

#### Test block 3
dt2block(["a non-date entry"], dtstart = dtstart, dtend = dtstart)

txtin = ["A note to the reader",
  "11 09 2017 through 11 11 2017 Range Nov 9 till 11th",
  "A line without any dates",
  "11 12 2017 through 11 15 2017 Range Nov 12 till 15th",
  "11 17 2017 Singleton",
  "443.43 Different line without any dates",
  "11 27 2017 Singleton Out of Range"]

dtstart = Date("2017 09 11", dateformat"yyyy dd mm")
dtend   = Date("2017 27 11", dateformat"yyyy dd mm")

dt2block(txtin, dtstart = dtstart, dtend = dtend)

# Previous dt2block
##########################################################################################
##########################################################################################
# Next scorer

# Function for scoring date block matches.
# Penalizes range misses
# Penalizes dates outside of range
# Rewards dates which increase
# Penalizes dates which are far from the center of date start and date end
function scorer(txtframe; scoreparameters = [1,1,1,1,1], dtstart, dtend = dtend)

    rngmiss    = [tx == "<<Input Missing>>" for tx in txtframe[:txt]] |> sum

    txtout     = [!(dt ∈ dtstart:Day(1):dtend) for dt in txtframe[:date]] |> sum

    gradient = [txtframe[i, :date] > txtframe[i - 1, :date]
   for i in 2:size(txtframe)[1] if txtframe[i, :txt] != "<<Input Missing>>" ]
    length(gradient) == 0 ? increasing = 0 : increasing = sum(gradient)

    txtframe = txtframe[txtframe[:date] .!= Date(0),:]
    distfrmmiddle = sum(Dates.value.(abs.(txtframe[:date] .- (dtstart + (dtend - dtstart) ÷ 2)))) / 10

    duplicatescount = length(txtframe[:date]) - length(unique(txtframe[:date]))

    α, β, γ, δ, ϕ = scoreparameters
    α * log(increasing + 1) - 10log(rngmiss + 1) - 5γ * log(txtout + 1) -
     γ * log(distfrmmiddle + 1) - ϕ * duplicatescount
end


### Score 1
dtstart = Date("2015 16 11", dateformat"yyyy dd mm")
dtend   = Date("2015 20 11", dateformat"yyyy dd mm")
rangeformat_md_dy2mdy = ("mm dd", "dd yyyy", convertmd_dy2mdy, "mm dd yyyy")

dtframe = dt2block(["11 17 2015 a date in range","11 18 through 20 2015 A range match"],
    dtstart = dtstart, dtend = dtend, rangeformat = rangeformat_md_dy2mdy)
scorer(dtframe, dtstart = dtstart, dtend = dtend) # Missing values

### Score get penalized for duplicate dates
dtframe = dt2block(["11 17 2015 a date in range","11 18 through 20 2015 A range match","11 17 2015 a duplicate"],
    dtstart = dtstart, dtend = dtend, rangeformat = rangeformat_md_dy2mdy)
scorer(dtframe, dtstart = dtstart, dtend = dtend) # Missing values


### Score 2
dtstart = Date("2017 16 01", dateformat"yyyy dd mm")
dtend   = Date("2017 25 01", dateformat"yyyy dd mm")

txtin = ["1 16 2017 A date"]
dtframe = dt2block(["1 15 2017 an out of date date","1 16 2017 A date", "1 18 2017 - 01 24 2017 A range"],
    dtstart = dtstart, dtend = dtend, singleformat = ["mm dd yyyy"], rangeformat = ["mm dd yyyy"])
scorer(dtframe, dtstart = dtstart, dtend = dtend)

### Score 3 - Format mm dd yyyy is a poor fit for dataset
txtin = ["11 16 2017 A date"]
dtframe = dt2block(["15 1 2017 an out of date date","16 1 2017 A date", "18 1 2017 - 24 1 2017 A range"],
    dtstart = dtstart, dtend = dtend, singleformat = ["mm dd yyyy"], rangeformat = ["mm dd yyyy"])
scorer(dtframe, dtstart = dtstart, dtend = dtend)

# Previous scorer
##########################################################################################
##########################################################################################
# Next softdate

# Some transformations which convert weird dates
makeyr3into4(x::AbstractString, I...) = replace(x, r"2(1[0-9])" => s"20\1")
makeyr3into4("10 12 219 a poorly entered date 219 notice that transformation converts all\n\r11 12 219")

set20xx_dtstart(x, I...) = replace(x, r"20[0-9]{2}" => year([I...][1]))
set20xx_dtend(x, I...)   = replace(x, r"20[0-9]{2}" => year([I...][2]))

Base.replace(x, y::Pair; which::Integer) =
  replace(x, r"\b(1[4-9])\b" => s"20\1", count=which) |>
  z -> (which > 1, replace(z, r"\b20(1[4-9])\b" => s"\1", count=which-1), z)

set1st1x_201x(x, I...) = replace(x, r"\b(1[4-9])\b" => s"20\1", count=1)
set2nd1x_201x(x, I...) = replace(replace(x, r"\b(1[4-9])\b" => s"20\1", count=2),
  r"\b20(1[4-9])\b" => s"\1", count=1)
set3rd1x_201x(x, I...) = replace(replace(x, r"\b(1[4-9])\b" => s"20\1", count=3),
  r"\b20(1[4-9])\b" => s"\1", count=2)
set4th1x_201x(x, I...) = replace(replace(x, r"\b(1[4-9])\b" => s"20\1", count=4),
  r"\b20(1[4-9])\b" => s"\1", count=3)

set1st3rd1x_201x(x, I...) = set1st1x_201x(set4th1x_201x(x, I...), I...)
set1st2nd1x_201x(x, I...) = set1st1x_201x(set2nd1x_201x(x, I...), I...)
set2nd3rd1x_201x(x, I...) = set2nd1x_201x(set3rd1x_201x(x, I...), I...)
set2nd4th1x_201x(x, I...) = set2nd1x_201x(set4th1x_201x(x, I...), I...)

set1st1x_201x("01 18 19")
set2nd1x_201x("12 18 19")
set3rd1x_201x("12 14 to 16 19")
set4th1x_201x("12 14 19 to 12 16 19")
set1st2nd1x_201x("12 9 19 to 12 11 19")
set1st3rd1x_201x("12 9 19 to 12 14 19")
set2nd3rd1x_201x("12 14 18 to 1 4 19")
set2nd4th1x_201x("12 14 19 to 12 16 19")

mytranformations = [((x, I...)->x), makeyr3into4, set20xx_dtstart, set20xx_dtend,
  set1st1x_201x, set2nd1x_201x, set3rd1x_201x, set4th1x_201x, set1st2nd1x_201x,
  set1st3rd1x_201x, set2nd3rd1x_201x, set2nd4th1x_201x]

################################################### - Begin softdate
function softdate(txt, dtstart::Date, dtend::Date;
   splits  = [r"\n\r|\n"],
   transformations = mytranformations,
   singleformats = [["mm dd yyyy"], ["dd mm yyyy"], ["yyyy dd mm"], ["yyyy mm dd"],
     ["mm dd yy", (x->x[1:(end-2)] * "20" * x[(end-1):end]), "mm dd yyyy"] ,
     ["dd mm yy", (x->x[1:(end-2)] * "20" * x[(end-1):end]), "dd mm yyyy"] ] ,
   rangeformats =
     [
     ["mm dd yyyy"], ["dd mm yyyy"], ["yyyy dd mm"], ["yyyy mm dd"],
     rangeformat_md_dy2mdy,
     ["dd mm yy", "dd mm yy", ((x,I...)->replace(x, r"([0-9]{2}).*?([0-9]{2}).*?(1[0-9])(.*)([0-9]{2}).*?([0-9]{2}).*?\3"=>s"\1 \2 20\3 \4 \5 \6 20\3")), "dd mm yyyy", "dd mm yyyy"],
     ["mm dd yy", "mm dd yy", ((x,I...)->replace(x, r"([0-9]{2}).*?([0-9]{2}).*?(1[0-9])(.*)([0-9]{2}).*?([0-9]{2}).*?\3"=>s"\1 \2 20\3 \4 \5 \6 20\3")), "mm dd yyyy", "mm dd yyyy"]
     ],
   seperators = ["through|till| |thru|to|-"],
   scoreparameters = [1,1,1,1,1],
   verbose = false,
   scorecut = 0)

  # Fill in some initial values
    scoremax = -999999
    dttxtout, combfull = fill([], 4)
    combset = fill(0, 5)
    framemax = DataFrame()

    if verbose
        txt2last = ""
        txtframelast = DataFrame()
        scoreilast = -999999
    end

    for s in 1:length(splits),
      t1 in 1:(length(transformations)-1),
      t2 in 1:length(transformations),
      F in 1:length(singleformats) ,
      r in 1:length(rangeformats)

        txtsplit = split(txt, splits[s])
        # txt2 = [transformations[t](x, dtstart, dtend) for x in txt1]
        # txt2 = txt2[txt2 .!= ""]

        # Send textsplit to be converted into dates
        txtframe = dt2block(txtsplit, dtstart = dtstart, dtend = dtend,
            singleformat = singleformats[F],
            rangeformat = rangeformats[r],
            seperator = seperators[s],
            trans1=transformations[t1],
            trans2=transformations[t2])

        scorei = scorer(txtframe, scoreparameters = scoreparameters, dtstart = dtstart, dtend = dtend)

      if scorei > scoremax
            scoremax = scorei
            dttxtout = txtframe[:txt]
            framemax = txtframe
            combset = [s, t1,t2, F, r]
            combfull = [splits[s], transformations[t1], transformations[t2], singleformats[F], rangeformats[r]]
            if verbose
                println("s=$s t=$t1 t=$t2 F=$F r=$r")
                (scoreilast != scorei) && println("Score $scorei")
                (txt2last != txt) && [println(tx) for tx in txtsplit]
                (txtframelast != txtframe) && println(txtframe)
                txt2last, txtframelast, scoreilast = txt, txtframe, scorei
            end
       if scorei > scorecut
           println("--------- $(round(scorei,2)) achieved!!")
           break
       end
      end
    end

    framemax[:score] = scoremax
    framemax[:s], framemax[:t1], framemax[:t2], framemax[:F], framemax[:r] = combset

    framemax
end

softdate(txt; dtstart::Date, dtend::Date, verbose=false) =
  softdate(txt, dtstart, dtend, verbose=verbose)

################################################### - End softdate

outframe = softdate("Missing Date\n\r10 12 219 a poorly entered date\n\r11 12 2019 ads",
  Date.(["2019 10 12","2019 12 12"], dateformat"yyyy dd mm")..., verbose = true)

# Forgot to update the date form 2018 to 2019
outframe = softdate("12-30-218 someth\n\r12-31-2018 someth\n\r01-01-2018 text",
  Date.(["2018 12 30","2019 01 01"], dateformat"yyyy mm dd")...)
