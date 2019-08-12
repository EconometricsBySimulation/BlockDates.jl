
cd("C:/Users/francis.smart.ctr/GitDir/SoftDatesJL/SoftDatesJL")

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

x = softdate(DTtext[1], Date(DTStart[1]), Date(DTEnd[1]),
  singlefmts = singlefmts,
  rangefmts  = rangefmts,
  transformations = transformations)
