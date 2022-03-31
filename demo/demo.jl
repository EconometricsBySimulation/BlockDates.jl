using Pkg
Pkg.activate(".")

include("../src/BlockDates.jl")

# Load Packages
using Main.BlockDates, Dates, DataFrames, CSV
using Main.BlockDates: blockdate

# Read CSV file as DataFrame
inDF = CSV.read("demo/data.csv", DataFrame)

# Convert String Dates to Date format dates
# They have to be new columns as the old ones were string columns
inDF[:, "startDate"] = [Date(v, dateformat"mm/dd/yy") for v in inDF[:, "start date"]]
inDF[:, "endDate"  ] = [Date(v, dateformat"mm/dd/yy") for v in inDF[:, "end date"  ]]

dfout = DataFrame()
for i in 1:size(inDF, 1)
    dftemp = blockdate(inDF[i, "text remark"], dtstart = inDF[i, "startDate"], dtend = inDF[i, "endDate"])

    println("$(i). $(size(dftemp, 1)) rows score $(round(dftemp.score[1], digits = 1))")

    dfout = [dfout; dftemp]
end