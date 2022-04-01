# Inside make.jl
push!(LOAD_PATH,"../src/")
using BlockDates
using Documenter

makedocs(
         sitename = "BlockDates.jl",
         modules  = [BlockDates],
         pages=[
                "Home" => "index.md"
               ])
deploydocs(;
    repo="github.com/EconometricsBySimulation/BlockDates.jl",
)