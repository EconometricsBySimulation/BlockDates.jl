#### Code Structure
## text with dates are passed one by one to the softdate function
## softdate passes txt to dt2block2 function which first passes txt to rangeformatter
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
  txt = replace(txt, r"\b([0-9])\b"=>s"0\1")

  # Input range formats
  rf1 = rangeformat[1]
  if length(rangeformat) == 1 ; rf2 = rangeformat[1]; end
  if length(rangeformat) >= 2 ; rf2 = rangeformat[2]; end

  left, right = dateformat2regex.([rf1,rf2])
  matchcheck = match(Regex("^($left)([ ]*)($seperator)([ ]*)($right)"), txt)

  (matchcheck === nothing) && (return [])

  m1 = matchcheck.captures[1]
  m2 = matchcheck.captures[5]

  # Interprettable range formats
  if length(rangeformat) == 4 ; rf1 = rangeformat[4]; rf2 = rangeformat[4]; end
  if length(rangeformat) == 5 ; rf1 = rangeformat[4]; rf2 = rangeformat[5]; end

  if length(rangeformat) >= 3 ; m1, m2 = rmwhitespace.(rangeformat[3](m1, m2)); end

  try
    dt1 = Date(m1, DateFormat(rf1))
    dt2 = Date(m2, DateFormat(rf2))

    starter = sum(length.(matchcheck.captures))+1

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

rangeformatter("26/10/2015 till 01/11/2015 Date match", rangeformat = ["dd mm yyyy"])

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
  txt = replace(txt, r"\b([0-9])\b"=>s"0\1")

  left = dateformat2regex.(singleformat[1])
  matchcheck = match(Regex("^($left)"), txt)

  (matchcheck === nothing) && (return DataFrame(date = Date(0), txt= txt, indt=""))

  try
    dt1 = Date(matchcheck.captures[1], DateFormat(singleformat[1]))
    starter = sum(length.(matchcheck.captures[1]))+1

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
sg4 = singleformatter("16.1.2017 A date match", singleformat=["dd mm yyyy"])


######
## Takes a date text string Array and tries to match first ranges then singles
## Uses dtstart and dtend to fill in missing
function dt2block2(txtin::Array{String,1};
    dtstart = Date(now()-Day(10)),
    dtend   = Date(now()),
    singeformat = ["mm dd yyyy"],
    rangeformat = ["mm dd yyyy"],
    seperator = "through|till| |to",
    fillmissing = true)

  outframe = DataFrame(date = Date(0), txt= "", indt="")

  for tx in txtin;
    rangeattempt = rangeformatter(tx, rangeformat=rangeformat, seperator=seperator)

    if size(rangeattempt)[1] > 0
        outframe= vcat(outframe, rangeattempt)
        continue
    end
    singleattempt = singleformatter(tx, singlefmt)
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
       outframe[i-1,2] = join(outframe[(i-1):i,2], "\n\r")
       outframekeep[i] = false
    end
  end

  outframe = outframe[outframekeep, :]

  outframe[2:end,:]
end

dtstart = Date("2017 16 01", dateformat"yyyy dd mm")
dtend   = Date("2017 25 01", dateformat"yyyy dd mm")

txtin = ["1 16 2017 A date"]
dt2block2(["1 15 2017 an out of date date","1 16 2017 A date", "1 18 2017 - 01 24 2017 A range"],
  dtstart=dtstart, dtend=dtend)


dtstart = Date("2015 16 11", dateformat"yyyy dd mm")
dtend   = Date("2015 20 11", dateformat"yyyy dd mm")
dt2block2(["11 17 2015 a date in range","1 18 through 20 2015 A range match"],
  dtstart=dtstart, dtend=dtend, rangeformat = rangeformat)

function softdate(txt, dtstart::Date, dtend::Date;
     splits  = [r"\n\r|\n"],
     transformations = [(x -> x)],
     singlefmts = ["yyyy mm dd"],
     rangeformats = [("mm dd yyyy", "mm dd yyyy"), ("dd mm yyyy", "dd mm yyyy")],
     seperators = [r"through|till"],
     scoreparameters = [1,1,1,1])

    # Fill in some initial values
    scoremax = -999
    dttxtout, combset, combfull = fill([], 3)
    s0, t0, f0, r0 = fill(0,4)
    framemax = DataFrame()

    s,t,F,r = fill(1, 4)

    for s in 1:length(splits),
        t in 1:length(transformations),
        F in 1:length(singlefmts) ,
        r in 1:length(rangeformats)

        global scoremax, dttxtout, combset, combfull, s0, t0, f0, r0, framemax

        txt1 = split(txt, splits[s])
        txt2 = [transformations[t](x,dtstart,dtend) for x in txt1]
        txt2 = txt2[txt2 .!= ""]

        txtframe = dt2block2(txt2, dtstart, dtend, singlefmts[F], rangeformats[r], seperators[s])
        #return(txtframe, scoreparameters, dtstart, dtend)
        scorei = scorer(txtframe, scoreparameters, dtstart, dtend)

        if scorei > scoremax
         # global scoremax, dttxtout, combset, combfull, soremax, framemax
          scoremax = scorei
          dttxtout = txtframe[:txt]
          framemax = txtframe
          combset = [s, t, F, r]
          combfull = [splits[s], transformations[t], singlefmts[F], rangeformats[r]]
        end
    end

   return(framemax, [scoremax, combset...])
    framemax[:score] = scoremax
    framemax[:s], framemax[:t], framemax[:F], framemax[:r] = combset

   framemax
end

txtin = txt2
tx = txtin[1]







vcat(rg, sg1, sg2)

dtstart = Date("2017 09 11", dateformat"yyyy dd mm")
dtend   = Date("2017 18 11", dateformat"yyyy dd mm")

txtin = ["A note to the reader",
    "11 09 2017 through 11 11 2017 Range Nov 9 till 11th",
    "A line without any dates",
    "11 12 2017 through 11 15 2017 Range Nov 12 till 15th",
    "11 17 2017 Singleton",
    "443.43 Different line without any dates",
    "11 27 2017 Singleton Out of Range"]

outframe = dt2block2(
  ["A note to the reader",
  "11 09 2017 through 11 11 2017 Range Nov 9 till 11th",
  "A line without any dates",
  "11 12 2017 through 11 15 2017 Range Nov 12 till 15th",
  "11 17 2017 Singleton",
  "443.43 Different line without any dates",
  "11 27 2017 Singleton Out of Range"],
  dtstart,
  dtend,
  "mm dd yyyy",
  ("mm dd yyyy", "mm dd yyyy"),
  "through")

outframe = dt2block2(
    ["11 08 2017 through 11 09 2017 Two Dates", "11 09 not date",
     "11 12 2017 Single Date", "33 PSI -not date"],
    Date("2017 07 11", dateformat"yyyy dd mm"),
    Date("2017 13 11", dateformat"yyyy dd mm"),
    "mm dd yyyy",
    ("mm dd yyyy", "mm dd yyyy"),
    "through")

for i in outframe[:txt]; println(i); end

txtframe = outframe

function scorer(txtframe, scoreparameters, dtstart, dtend)
  rngmiss    = [tx == "<<Input Missing>>" for tx in txtframe[:txt]] |> sum
  txtout     = [!(dt ∈ dtstart:Day(1):dtend) for dt in txtframe[:date]] |> sum
  gradient = [txtframe[i, :date] > txtframe[i-1, :date]
   for i in 2:size(txtframe)[1] if txtframe[i, :txt] != "<<Input Missing>>" ]
  length(gradient)==0 ? increasing = 0 : increasing = sum(gradient)

  distfrmmiddle = sum(Dates.value.(abs.(txtframe[:date] .- (dtstart+(dtend-dtstart)÷2))))/10

  α, β, γ, δ = scoreparameters
  α*increasing - β*rngmiss - γ*txtout - γ*distfrmmiddle
end

txt = """A note to the reader
  11 09 2017 through 11 11 2017 Range Nov 9 till 11th
  A line without any dates
  11 12 2017 through 11 15 2017 Range Nov 12 till 15th
  11 17 2017 Singleton
  443.43 Different line without any dates
  11 27 2017 Singleton Out of Range]"""

dtstart = Date("2017 09 11", dateformat"yyyy dd mm")
dtend   = Date("2017 18 11", dateformat"yyyy dd mm")

makeyr3into4(x::AbstractString, I...) = replace(x, r"2(1[0-9])"=>s"20\1")
makeyr3into4("10 12 219 asdlkjfdsalkf 219 adsf\n\r11 12 219")
singlefmts= ["mm dd yyyy", "dd mm yyyy"]
rangeformats = [("mm dd yyyy", "mm dd yyyy"),
             ("dd mm yyyy", "dd mm yyyy")]



transformations = [((x, I...) -> x), makeyr3into4]

dtmatch = softdate(txt, dtstart, dtend,
  singlefmts = singlefmts,
  rangeformats  = rangeformats,
  transformations = transformations)
