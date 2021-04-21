using Pkg
Pkg.activate(joinpath(@__DIR__, "..", "..", "FoodCachingFitting"))
using FoodCachingModels, Distances, FoodCachingExperiments, Statistics
import FoodCachingFitting: run_experiments, simname

include(joinpath(@__DIR__, "paths.jl"))

p1 = load(joinpath(datapath, "Baseline_deKort07_exp1_4_a7af080"))
p2 = load(joinpath(datapath, "Baseline_deKort07_exp1_4_a7af080"))

function mean_and_best(model, id, rev; N = 20000)
    tmp = run_experiments(simname(model, [:deKort07_exp1], id, rev),
                          [:deKort07_exp1], N)
    metric = Distances.Euclidean()
    x = target(:deKort07_exp1)
    d = metric.(Ref(x), tmp.results)
    mean(tmp.results), tmp.results[argmin(d)]
end

m1, b1 = mean_and_best(Baseline, 4, "a7af080", N = 20000)
m2, b2 = mean_and_best(Baseline, 1, "8c1439a", N = 20000)

Pkg.activate(joinpath(@__DIR__, ".."))
using FoodCachingPlotting, PGFPlotsX

function deKort07_exp1_plot(m, b)
    p = plot_compare(:deKort07_exp1, m, b, backend = :pgf)
    p.elements[1] = raw"\begin{longtable}{c@{}c@{}c}"
    p.elements[4].elements[1].options[:yticklabels] = "none"
    p.elements[6].elements[1].options[:yticklabels] = "none"
    p
end
p1 = deKort07_exp1_plot(m1, b1)
p2 = deKort07_exp1_plot(m2, b1)
p2 = plot_compare(:deKort07_exp1, m2, b2, backend = :pgf)
pgfsave(joinpath(figpath, "variability1a.tikz"), p1)
pgfsave(joinpath(figpath, "variability1b.tikz"), p2)
