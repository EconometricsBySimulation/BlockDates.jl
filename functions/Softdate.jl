
try
    cd("C:/Users/francis.smart.ctr/GitDir/SoftDates.jl")
catch
   cd("C:/Users/Francis Smart/Documents/GitHub/SoftDates.jl")
end
using Pkg
Pkg.activate(".")
using Pkg, Dates, DataFrames, Distributions, CSV

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

function dateformat2regex(m::String; yrpre="20", yrdec="[0-2]")
    m = replace(m,   r"\bmm\b"=>"[0-1]?[0-9]")
    m = replace(m, r"\byyyy\b"=>"$yrpre$yrdec[0-9]")
    m = replace(m,   r"\byy\b"=>"$yrdec[0-9]")
    m = replace(m,   r"\bdd\b"=>"[0-3]?[0-9]")
end

dateformat2regex("yyyy dd mm")
dateformat2regex("yy dd mm")
# Requires exact number for match
dateformat2regex("yy d mm This is something")

rmwhitespace(x) = strip(replace(x, r"\s+"=>" "))
rmwhitespace(" asdf  asdf as")
rmwhitespace.([" asdf  asdf as", " 1  2   3   4"])


### Range Date Format
# Function for formatting ranged dates
function rangeformatter(txtin; rangeformat, seperator="through|till| ", toomany=30)

  txt = replace(txtin, r"[|./\\-]"=>" ")
  txt = replace(txt, r"\b([0-9])\b"=>s"0\1") |> x->replace(x, r"[ ]+"=>" ")

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
    starter = length(rangematch.match)+1 + (length(txtin) - length(txt))

    txtout = strip(txtin[starter:end])
    indt   = txtin[1:(starter-1)]

    (length(dt1:Day(1):dt2) > toomany) && return []

    return DataFrame(date=collect(dt1:Day(1):dt2),
               txt=fill(txtout, length(dt1:Day(1):dt2)),
               indt=fill(indt, length(dt1:Day(1):dt2)))
  catch
    return  []
  end
end

rangeformatter(;txtin::AbstractString, rangeformat) = rangeformatter(txtin, rangeformat=rangeformat)

# Extra spaces between dates are ignore (after the first)
rangeformatter(txtin = "1 1  2015 till   12/1/2015 Date   match", rangeformat = ["dd mm yyyy"])

# fail from date misspecification, fits too many dates, second is misspecified and fits no dates
rangeformatter("2/1/2017 through 12/1/2017 Date mismatch" , rangeformat = ["mm dd yyyy"])
rangeformatter("16/1/2017 through 19/1/2017 Date mismatch", rangeformat =["mm dd yyyy"])

#### Some date ranges are so messy they have to borrow elements from each other
convertmdy2mdY(x,y) = ["$(x[1:(end-2)])$(y[(end-3):(end-2)])$(x[(end-1):end])", y]
convertmdy2mdY("11 12 17", "11 15 2017")

rg = rangeformatter("11 12 17 through 11 15 2017 ipsum tadsf",
  rangeformat =["mm dd yy", "mm dd yyyy", convertmdy2mdY])

#### Some date ranges are so messy they have to borrow elements
convertmd_dy2mdy(x,y) = ["$x $(y[(end-3):end])", "$(x[1:2]) $y"]
convertmd_dy2mdy("07 16", "19 2019")

# fail from insufficient date data
rangeformat=("mm dd", "dd yyyy", convertmd_dy2mdy, "mm dd yyyy")
rangeformatter("1/10 - 15/2017 Range Match!", rangeformat = rangeformat)


#### Single Date Format
# Function for formatting single dates
function singleformatter(txtin; singleformat::Array{String,1}=["mm dd yyyy"])
  txt = replace(txtin, r"[|./\\-]"=>" ")
  txt = replace(txt, r"\b([0-9])\b"=>s"0\1") |> x->replace(x, r"[ ]+"=>" ")

  left = dateformat2regex.(singleformat[1])
  singlematch = match(Regex("^($left)"), txt)

  (singlematch === nothing) && (return DataFrame(date = Date(0), txt= txt, indt=""))

  try
    dt1 = Date(singlematch.captures[1], DateFormat(singleformat[1]))
    # Define a place to start capturing the text input after the date
    starter = length(singlematch.match)+1 + (length(txtin) - length(txt))

    txtout = strip(txtin[starter:end])
    indt   = txtin[1:(starter-1)]

    return DataFrame(date=dt1, txt=txtout, indt = indt)
  catch
    return DataFrame(date=Date(0), txt=txt, indt = "")
  end
end

sg1 = singleformatter("1 16 2017 A date", singleformat=["mm dd yyyy"])
sg2 = singleformatter("443.43 Not a date", singleformat=["mm dd yyyy"])
sg3 = singleformatter("16 1 2017 A date mismatch", singleformat=["mm dd yyyy"])
sg4 = singleformatter("16  1  2017 A date match", singleformat=["dd mm yyyy"])
sg5 = singleformatter("16.1.2017 A date match", singleformat=["dd mm yyyy"])
sg6 = singleformatter("16/1/2017 A date match", singleformat=["dd mm yyyy"])
sg7 = singleformatter("16-1-2017 A date match", singleformat=["dd mm yyyy"])


for i = 2:size(outframe)[1]
  if outframe[i,1] ==  Date(0)
     outframemap = collect(1:(i-1))[outframe[1:(i-1),3] .== outframe[(i-1),3]]
     outframe[outframemap,:txt] .= join(outframe[(i-1):i,2], "---")
     outframekeep[i] = false
  end
end


function dropmerge(inframe::DataFrame; excludevalue=Date(0), exclude::Symbol, merge::Symbol, verbose=false)
  framekeep = fill(true, size(inframe)[1])
  for i in 2:size(inframe)[1]
    if inframe[i, exclude] == excludevalue
     collapser = collect(1:(i-1))[inframe[1:(i-1),merge] .== inframe[(i-1),merge]]
     verbose && println(collapser)
     inframe[collapser, merge] .= join(inframe[(i-1):i,2], "---")
     verbose && println(join(inframe[(i-1):i,2], "---"))
     framekeep[i] = false
    end
  end
  inframe[framekeep, :]
end

mydata = DataFrame(date=[(today() .- Day.([1,2,2,2]))..., Date(0)],
  txt=["Some input", fill("Input repeated",3)..., "Spread me"],
  othervar = 1:5)

dropmerge(mydata, excludevalue = Date(0), exclude=:date, merge=:txt, verbose=true)

######
## Takes a date text string Array and tries to match first ranges then singles
## Uses dtstart and dtend to fill in missing
function dt2block(txtin;
    dtstart = Date(now()-Day(10)),
    dtend   = Date(now()),
    singleformat = ["mm dd yyyy"],
    rangeformat = ["mm dd yyyy"],
    seperator   = "through|till| |to",
    fillmissing = true)

  outframe = DataFrame()

  for tx in txtin;
     # global outframe
    rangeattempt = rangeformatter(tx, rangeformat=rangeformat, seperator=seperator)

    if size(rangeattempt)[1] > 0
        outframe= vcat(outframe, rangeattempt)
        continue
    end
    singleattempt = singleformatter(tx, singleformat=singleformat)
    outframe = vcat(outframe, singleattempt)
    continue
  end

  # Fill in any missing dates if dates exist in range but not in set
  if fillmissing; for dt in Date(dtstart):Day(1):Date(dtend);
      !(dt ∈ outframe[:date]) &&
        (append!(outframe, DataFrame(date=dt, txt="<<Input Missing>>", indt = "")))
  end; end

  # Merge any txt fields which do not have dates associated with them except for the first.
  outframekeep = fill(true, size(outframe)[1])

  for i = 2:size(outframe)[1]
    if outframe[i,1] ==  Date(0)
       outframemap = collect(1:(i-1))[outframe[1:(i-1),3] .== outframe[(i-1),3]]
       outframe[outframemap,:txt] .= join(outframe[(i-1):i,2], "---")
       outframekeep[i] = false
    end
  end

  #outframe = outframe[outframekeep, :]

  outframe[:day]   = day.(outframe[:date])
  outframe[:month] = month.(outframe[:date])

  # If there are any fields which have been filled in but have the same day and month
  # as a missing field then replace dates from missing fields and drop filled
  # in dates. Fixes sloppy 12-30-2018, 12-31-2018, 01-01-2018
  keepframe = fill(true,size(outframe)[1])
  for i in 1:(size(outframe)[1]-1), j in i:size(outframe)[1]
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
#
 # X = DataFrame(a=1:99, b=repeat(collect(1:3),33))
 # for i in 3:5
 #    if i-1 ∈ X[:b]
 #       X[:a][X[:b] .== i,] .= X[:a][X[:b] .== i,] .^ (i-1)
 #    end
 # end

# Alternative specification of dt2block
dt2block(txt, dtstart, dtend, I...) = dt2block(txt, dtstart=dtstart, dtend=dtend, I...)

#### Test block 1
dtstart = Date("2017 16 01", dateformat"yyyy dd mm")
dtend   = Date("2017 25 01", dateformat"yyyy dd mm")

outframe = dt2block(["1 15 2017 an out of date date",
    "1 16 2017 A date",
    "1 18 2017 - 01 24 2017 A range",
    "A non-date"], dtstart=dtstart, dtend=dtend)

#### Test block 2
dtstart = Date("2015 16 11", dateformat"yyyy dd mm")
dtend   = Date("2015 20 11", dateformat"yyyy dd mm")

rangeformat_md_dy2mdy=("mm dd", "dd yyyy", convertmd_dy2mdy, "mm dd yyyy")
dt2block(["11 17 2015 a date in range","1 18 through 20 2015 A range match"],
  dtstart=dtstart, dtend=dtend, rangeformat = rangeformat_md_dy2mdy)

#### Test block 3
dt2block(["a non-date entry"], dtstart=dtstart, dtend=dtstart)


txtin = ["A note to the reader",
  "11 09 2017 through 11 11 2017 Range Nov 9 till 11th",
  "A line without any dates",
  "11 12 2017 through 11 15 2017 Range Nov 12 till 15th",
  "11 17 2017 Singleton",
  "443.43 Different line without any dates",
  "11 27 2017 Singleton Out of Range"]

dtstart = Date("2017 09 11", dateformat"yyyy dd mm")
dtend   = Date("2017 27 11", dateformat"yyyy dd mm")

dt2block(txtin, dtstart=dtstart, dtend=dtend)




# Function for scoring date block matches.
# Penalizes range misses
# Penalizes dates outside of range
# Rewards dates which increase
# Penalizes dates which are far from the center of date start and date end
function scorer(txtframe; scoreparameters=[1,1,1,1], dtstart, dtend=dtend)

    rngmiss    = [tx == "<<Input Missing>>" for tx in txtframe[:txt]] |> sum

    txtout     = [!(dt ∈ dtstart:Day(1):dtend) for dt in txtframe[:date]] |> sum

    gradient = [txtframe[i, :date] > txtframe[i-1, :date]
     for i in 2:size(txtframe)[1] if txtframe[i, :txt] != "<<Input Missing>>" ]
    length(gradient)==0 ? increasing = 0 : increasing = sum(gradient)

    txtframe = txtframe[txtframe[:date] .!= Date(0),:]
    distfrmmiddle = sum(Dates.value.(abs.(txtframe[:date] .- (dtstart+(dtend-dtstart)÷2))))/10

    α, β, γ, δ = scoreparameters
    α*log(increasing+1) - 10log(rngmiss+1) - γ*log(txtout+1) - γ*log(distfrmmiddle+1)
end

### Score 1
dtstart = Date("2015 16 11", dateformat"yyyy dd mm")
dtend   = Date("2015 20 11", dateformat"yyyy dd mm")
rangeformat_md_dy2mdy=("mm dd", "dd yyyy", convertmd_dy2mdy, "mm dd yyyy")

dtframe = dt2block(["11 17 2015 a date in range","11 18 through 20 2015 A range match"],
  dtstart=dtstart, dtend=dtend, rangeformat = rangeformat_md_dy2mdy)
scorer(dtframe, dtstart=dtstart, dtend=dtend)

### Score 2
dtstart = Date("2017 16 01", dateformat"yyyy dd mm")
dtend   = Date("2017 25 01", dateformat"yyyy dd mm")

txtin = ["1 16 2017 A date"]
dtframe = dt2block(["1 15 2017 an out of date date","1 16 2017 A date", "1 18 2017 - 01 24 2017 A range"],
  dtstart=dtstart, dtend=dtend)
scorer(dtframe, dtstart=dtstart, dtend=dtend)

### Score 3 - Poor fit for dataset
txtin = ["11 16 2017 A date"]
dtframe = dt2block(["15 1 2017 an out of date date","16 1 2017 A date", "18 1 2017 - 24 1 2017 A range"],
  dtstart=dtstart, dtend=dtend)
scorer(dtframe, dtstart=dtstart, dtend=dtend)


# Some transformations which convert weird dates
makeyr3into4(x::AbstractString, I...) = replace(x, r"2(1[0-9])"=>s"20\1")
makeyr3into4("10 12 219 a poorly entered date 219 notice that transformation converts all\n\r11 12 219")

set20xx_dtstart(x, I...) = replace(x, r"20[0-9]{2}"=>year([I...][1]) )
set20xx_dtend(x, I...)   = replace(x, r"20[0-9]{2}"=>year([I...][2]) )

mytranformations = [((x, I...) -> x), makeyr3into4, set20xx_dtstart, set20xx_dtend]

function softdate(txt, dtstart::Date, dtend::Date;
     splits  = [r"\n\r|\n"],
     transformations = mytranformations,
     singleformats = [["mm dd yyyy"], ["dd mm yyyy"], ["yyyy dd mm"], ["yyyy mm dd"]],
     rangeformats =
       [["mm dd yyyy"], ["dd mm yyyy"], ["yyyy dd mm"], ["yyyy mm dd"], rangeformat_md_dy2mdy],
     seperators = ["through|till| |thru|to"],
     scoreparameters = [1,1,1,1],
     verbose = false)

    # Fill in some initial values
    scoremax = -999999
    dttxtout, combfull = fill([], 3)
    combset = fill(0,4)
    framemax = DataFrame()

    if verbose
      txt2last = ""
      txtframelast = DataFrame()
      scoreilast = -999999
    end

    #s,t,F,r = fill(1, 4)

    for s in 1:length(splits),
        t in 1:length(transformations),
        F in 1:length(singleformats) ,
        r in 1:length(rangeformats)

        verbose && println("s=$s t=$t F=$F r=$r")

#       global scoremax, dttxtout, combset, combfull, s0, t0, f0, r0, framemax

        txt1 = split(txt, splits[s])
        txt2 = [transformations[t](x,dtstart,dtend) for x in txt1]
        txt2 = txt2[txt2 .!= ""]


        txtframe = dt2block(txt2, dtstart=dtstart, dtend=dtend, singleformat=singleformats[F],
          rangeformat=rangeformats[r], seperator=seperators[s])

#        txtframe = dt2block(txt2, dtstart, dtend, singleformats[F], rangeformats[r], seperators[s])
        #return(txtframe, scoreparameters, dtstart, dtend)
        scorei = scorer(txtframe, scoreparameters = scoreparameters, dtstart = dtstart, dtend = dtend)


        if scorei > scoremax
         # global scoremax, dttxtout, combset, combfull, soremax, framemax
          scoremax = scorei
          dttxtout = txtframe[:txt]
          framemax = txtframe
          combset = [s, t, F, r]
          combfull = [splits[s], transformations[t], singleformats[F], rangeformats[r]]
        end

        if verbose
          (scoreilast != scorei) && println("Score $scorei")
          (txt2last != txt2) && [println(tx) for tx in txt2]
          (txtframelast != txtframe) && println(txtframe)
          txt2last, txtframelast, scoreilast = txt2, txtframe, scorei
        end
    end

   # return(framemax, [scoremax, combset...])
    framemax[:score] = scoremax
    framemax[:s], framemax[:t], framemax[:F], framemax[:r] = combset

   framemax
end

txt = txtin[1]

outframe = softdate("Missing Date\n\r10 12 219 a poorly entered date\n\r11 12 219 ads",
  Date.(["2019 10 12","2019 12 12"], dateformat"yyyy dd mm")..., verbose = true)

# Forgot to update the date form 2018 to 2019
outframe = softdate("12-30-2018 someth\n\r12-31-2018 someth\n\r01-01-2018 text",
  Date.(["2018 12 30","2019 01 01"], dateformat"yyyy mm dd")...)
