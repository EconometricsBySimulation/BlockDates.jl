cd("C:/Users/francis.smart.ctr/GitDir/SoftDatesJL/SoftDatesJL")

using Pkg, Dates, DataFrames, Distributions
Pkg.activate(".")

# used lipsum.com to grab 2000 year old document (https://www.lipsum.com/)
lipsum = read("sampledata/Lipsum.txt", String)

lip_par = [replace(i, r"\r|\n" => "") for i in split(lipsum, "\n\r") if i!=""]
lip_sen  = [replace(i, r"\r|\n|^ " => "") for i in split(lipsum, ".") if i!=""]
lip_wrd  = [replace(i, r"\r|\n|^ " => "") for i in split(lipsum, r"\n\r|\s+") if i!=""]

# DataGenerators
# Type1
# Number of dates to generate
n = 1000

dtrange = (Date(now()) - Year(5)):Day(1):Date(now())

startdt = firstdayofweek.(rand(dtrange, n)) + Day.(rand([-2,-1,0,0,0,0,1], n))
enddt  = startdt .+ Day.(rand([4,5,6,6,7,7,7,7,7,8], n))

# Number of days covered from the start to the end
span = Dates.value.(enddt-startdt)

# The reporting rate by date -> I set at .9
coverage = [rand(Binomial(span[i], .9),1)[1] for i in 1:n]

daydates = [sort(sample(startdt[i]:Day(1):enddt[i], coverage[i], replace = false)) for i in 1:n]

notes =  [join(sample(lip_sen, rand(Binomial(3, .25),1)[1]), " ") for i in 1:n]

txtfield = [[join(sample(lip_par, rand([1,1,2,3])), "\n\r") for j in 1:length(daydates[i])]  for i in 1:n]


sep = sample(split(raw".\/- ","") , n)

txtblocks = [join((notes[i], , ), )]

# We have start dates and end dates and where things get tricky is that we
# have individual records that start at Start and end at End. They are listed
# at the beginning of lines.

# Some lines start with a note before the first date is entered.
# Not every date is covered

# Information to be spread accross the dates
V0 = collect(1:n)
V1 = [join(sample(lip_wrd, rand(Binomial(9, .25),1)[1]), " ") for i in 1:n]
V2 = [sample(lip_sen, 1)[1] for i in 1:n]
V3 = [sample(1:(n*5), 1)[1] for i in 1:n]

txtblocks =
   [ begin
   x = [join([Dates.format.(daydates[i][j],"yyyy$(sep[i])mm$(sep[i])dd"), txtfield[i][j]]," ") for j in 1:length(daydates[i])]
   replace(join([notes[i], x...], "\n\r"), r"^\n\r" => "")
   end
   for i = 1 : n ]

dt1 = DataFrame(Start = startdt ,
            End = enddt,
            Dates = daydates,
            Text = txtblocks,
            V0=V0, V1=V1, V2=V2, V3=V3,
            type = 1)
