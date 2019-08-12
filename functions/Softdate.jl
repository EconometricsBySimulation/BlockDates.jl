#### Code Structure
## text with dates are passed one by one to the softdate function
## softdate passes txt to dt2block2 function which first passes txt to rangeformatter
## if rangeformatter finds a range match then dates are spread over that range
## and returned to softdate
## if rangeformatter does not find a match then singleformatter is attempted
## if that finds a match then a single value is returned


function softdate(txt, dtstart::Date, dtend::Date;
     splits  = [r"\n\r|\n"],
     transformations = [(x -> x)],
     singlefmts = ["yyyy mm dd"],
     rangefmts = [("mm dd yyyy", "mm dd yyyy"), ("dd mm yyyy", "dd mm yyyy")],
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
        r in 1:length(rangefmts)

        global scoremax, dttxtout, combset, combfull, s0, t0, f0, r0, framemax

        txt1 = split(txt, splits[s])
        txt2 = [transformations[t](x,dtstart,dtend) for x in txt1]
        txt2 = txt2[txt2 .!= ""]

        txtframe = dt2block2(txt2, dtstart, dtend, singlefmts[F], rangefmts[r], seperators[s])
        #return(txtframe, scoreparameters, dtstart, dtend)
        scorei = scorer(txtframe, scoreparameters, dtstart, dtend)

        if scorei > scoremax
         # global scoremax, dttxtout, combset, combfull, soremax, framemax
          scoremax = scorei
          dttxtout = txtframe[:txt]
          framemax = txtframe
          combset = [s, t, F, r]
          combfull = [splits[s], transformations[t], singlefmts[F], rangefmts[r]]
        end
    end

   return(framemax, [scoremax, combset...])
    framemax[:score] = scoremax
    framemax[:s], framemax[:t], framemax[:F], framemax[:r] = combset

   framemax
end

txtin = txt2
tx = txtin[1]

function dt2block2(txtin,
    dtstart = now()-Day(10),
    dtend   = now(),
    singlefmt = "mm dd yyyy",
    rangefmt = ("dd mm yyyy", "dd mm yyyy"), seperator = r"through|till",
    fillmissing = true)

  outframe = DataFrame(date = Date(0), txt= "", indt="")

  for tx in txtin;
    rangeattempt = rangeformatter(tx, rangefmt, seperator)

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


function dateformat2regex(m::String)
    m = replace(m,   r"\bmm\b"=>"[0-1][0-9]")
    m = replace(m, r"\byyyy\b"=>"201[0-9]")
    m = replace(m,   r"\byy\b"=>"1[0-9]")
    m = replace(m,   r"\bdd\b"=>"[0-3][0-9]")
end

# Function for formatting ranged dates
function rangeformatter(txt, rangeformat, seperator)
  txt = replace(txt, r"[|./\\-]"=>" ")
  #println(txt)
  left, right = dateformat2regex.(rangeformat)
  matchcheck = match(Regex("^($left)([ ]*)($seperator)([ ]*)($right)"), txt)

  (matchcheck === nothing) && (return [])
  try
    dt1 = Date(matchcheck.captures[1], DateFormat(rangeformat[1]))
    dt3 = Date(matchcheck.captures[5], DateFormat(rangeformat[2]))

    starter = sum(length.(matchcheck.captures))+1

    txtout = strip(txt[starter:end])
    indt   = txt[1:(starter-1)]

    DataFrame(date=collect(dt1:Day(1):dt3),
               txt=fill(txtout, length(dt1:Day(1):dt3)),
               indt=fill(indt, length(dt1:Day(1):dt3)))
  catch
    return  []
  end
end

rangeformatter("26/10/2015 till 01/11/2015 Nunc rhoncus dictum", ("dd mm yyyy", "dd mm yyyy"), "through|till")
txt = "26/10/2015 till 01/11/2015 Nunc rhoncus dictum"
rangeformat = ("dd mm yyyy", "dd mm yyyy")
seperator = "(through|till)"

# Function for formatting single dates
function singleformatter(txt, singleformat::String="mm dd yyyy")
  txt = replace(txt, r"[|./\\-]"=>" ")
  println(txt)

  left = dateformat2regex.(singleformat)
  matchcheck = match(Regex("^($left)"), txt)

  (matchcheck === nothing) && (return DataFrame(date = Date(0), txt= txt, indt=""))

  try
    dt1 = Date(matchcheck.captures[1], DateFormat(rangeformat[1]))
    starter = sum(length.(matchcheck.captures[1]))+1

    txtout = strip(txt[starter:end])
    indt   = txt[1:(starter-1)]

    return DataFrame(date=dt1, txt=txtout, indt = indt)
  catch
    return DataFrame(date=Date(0), txt=txt, indt = "")
  end
end

rangeformat = ("mm dd yyyy", "mm dd yyyy")
#txt         = replace(txt, "/"=>" ")
seperator   = "through"

rg = rangeformatter("11 12 2017 through 11 15 2017 ipsum tadsf", ("mm dd yyyy", "mm dd yyyy"), "through")
sg1 = singleformatter("11 16 2017 ipsum tadsf", "mm dd yyyy")
sg2 = singleformatter("443.43 ipsum tadsf", "mm dd yyyy")

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
rangefmts = [("mm dd yyyy", "mm dd yyyy"),
             ("dd mm yyyy", "dd mm yyyy")]



transformations = [((x, I...) -> x), makeyr3into4]

dtmatch = softdate(txt, dtstart, dtend,
  singlefmts = singlefmts,
  rangefmts  = rangefmts,
  transformations = transformations)
