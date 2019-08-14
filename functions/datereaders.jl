
try
    cd("C:/Users/francis.smart.ctr/GitDir/SoftDates.jl")
catch
   cd("C:/Users/Francis Smart/Documents/GitHub/SoftDates.jl")
end
using Pkg
Pkg.activate(".")
using Pkg, Dates, DataFrames, Distributions, CSV

##############################################################################
# Read in Dates

# Load the dates without errors first
dtrange = CSV.read("sampledata/SampleDateRanges.csv")

DTStart = dtrange[:Start]
DTEnd   = dtrange[:End]
DTtext  = dtrange[:Text]

x = softdate(DTtext[1], Date(DTStart[1]), Date(DTEnd[1]))
[println(y) for y in x[:txt]]

function trunk(x,n=60)
 x = replace(x, "\n\r"=>" ")
 join([x[1:min(length(x), n)], repeat(" ", n-min(length(x), n))])
end
trunk(x[:txt][1])

i = 1
while i <= length(DTtext)
  println("------Input $i -- Start Date $(Date(DTStart[i])) till $(Date(DTEnd[i]))------")
  [println(trunk(tx)) for tx in split(DTtext[i], r"\n\r|\r") if strip(tx) != ""]
  println("------SoftDates-------")
  x = softdate(DTtext[i], Date(DTStart[i]), Date(DTEnd[i]))
  [println("$(x[:date][j]) || $(trunk(x[:txt][j])) || $(x[:indt][j]) || $(x[:score][j])") for j in 1:size(x)[1]]
  println()
  print("Type \"break\" or enter to continue:")
  userinput = readline(stdin)
  (length(userinput)>0) && (userinput[1:1] ∈ ["b"]) && break
  global i
  i += 1
end
