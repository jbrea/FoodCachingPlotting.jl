# FoodCachingPlotting

The code in this repository defines some plotting methods to visualize the results of
fitting and simulating food caching experiments. This repository is a submodule of [FoodCaching](https://github.com/jbrea/FoodCaching).

Run the [scripts](scripts) `fig1.jl`, `figappendix.jl` and `prediction.jl` to reproduce the figures in the paper.

Run the [scripts](scripts) `loss_dashboard.jl` to monitor log files produced during
fitting with [FoodCachingFitting](https://github.com/jbrea/FoodCachingFitting.jl) and `sim_dashboard.jl` to
visualize simulated experiments.

To run the code in this repository, download [julia 1.6](https://julialang.org/downloads/)
and activate and instantiate this project. This can be done in a julia REPL with the
following lines of code:
```julia
using Pkg
# download code
Pkg.develop(url = "https://github.com/jbrea/FoodCachingFitting.jl")
# activate project
cd(joinpath(Pkg.devdir(), "FoodCachingFitting"))
Pkg.activate(".")
# install dependencies
Pkg.instantiate()

# run a script
include(joinpath("scripts", "sim_dashboard.jl"))
```

