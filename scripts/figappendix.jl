using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
using PGFPlotsX, FoodCachingPlotting, DataFrames, DataFramesMeta
import FoodCachingExperiments: bload, CLAYTON0103_EXPERIMENTS, EXPERIMENTS, summarize

include(joinpath(@__DIR__, "utils.jl"))

###
### Appendix result figures
###
results = bload(joinpath(datapath, "run_indi"))[:results]

sort!(results, :logp_hat)
best = combine(groupby(results, [:model, :experiment]),
               names(results) .=> x -> [last(x)], renamecols = false)

n = 0
for row in eachrow(best)
    @show row.model row.experiment
    f = plot_compare(row.experiment, row.avg, row.best, backend = :pgf)
    if row.experiment == :Clayton0103
        for (ei, fi) in zip(CLAYTON0103_EXPERIMENTS, f)
            n += length(filter(x -> isa(x, TikzPicture), fi.elements)) ÷ 3
            pgfsave(joinpath(figpath, "$(row.model)_$(ei).tikz"), fi)
        end
    else
        n += length(filter(x -> isa(x, TikzPicture), f.elements)) ÷ 3
        pgfsave(joinpath(figpath, "$(row.model)_$(row.experiment).tikz"), f)
    end
end

###
### Appendix document
###

length_without_missing(x) = sum(1 .- ismissing.(x))
process_testval(val) = match(r"[0-9.]", string(val[1])) !== nothing ? "=" * val : val
texstring(s) = reduce(replace, ["_" => " ", "&" => "\\&", "×" => "\$\\times\$", "Western Scrub Jays" => "California scrub-jays", "Jays" => "jays"], init = string(s))
figstring(m, s) = "\\begin{center}\\textbf{$(modelname(m))}\\end{center}\\input{$(m)_$s.tikz}"
function modelname(m)
    m == "ReplayAndPlan" && return raw"\mplan{}"
    m == "PlasticCaching" && return raw"\mplastic{}"
    m == "MotivationalControl" && return raw"\mspec{}"
    m == "EpisodicLikeMemory" && return raw"\mmem{}"
    raw"\msimple{}"
end
function process_pval(val)
    pval = string(round(val, sigdigits = 2))
    m = match(r"(.*)e(.*)", pval)
    if m !== nothing
        "$(m.captures[1])\\cdot 10^{$(m.captures[2])}"
    else
        pval
    end
end
function experiment_summary()
    result = DataFrame(key = Symbol[],
                       ntests = Int[],
                       summary = String[],
                       comments = String[],
                       nvals = Int[],
                       atests = Array{String}[])
    for (key, experiment) in EXPERIMENTS
        key == :Clayton0103 && continue
        ntests = sum(length(test.pvalues) for (_, test) in experiment.tests)
        atests = vcat([["$k ($(test.keys[i])): \$$(test.statistic[i])($(test.df[i])) $(process_testval(test.value[i])), p = $(process_pval(test.pvalues[i]))\$"
                        for i in eachindex(test.labels) if test.labels[i] == "A"]
                       for (k, test) in experiment.tests]...)
        data = experiment.data
        if key ∈ (:Cheke11_specsat, :Cheke11_planning)
            data = summarize(experiment, data)
        end
        nvals = length_without_missing(data.μ)
        hasproperty(data, :sem) && (nvals += length_without_missing(data.sem))
        hasproperty(data, :firstinspection) && (nvals += length_without_missing(data.firstinspection))
        push!(result, [key, ntests, experiment.major_finding,
                       experiment.comments, nvals, atests])
    end
    result
end
function section(row)
    key = row.key
    s = "\\subsection{$(texstring(key))}\n"
    s *= "Published in \\cite{$(split(string(key), '_')[1])}."
    s *= "\\subsubsection*{Major Finding}\n$(texstring(row.summary))\n\n"
    if row.comments != ""
        s *= "\\subsubsection*{Comments}\n$(texstring(row.comments))\n"
    end
    s *= "\\subsubsection*{Most Important Tests}\n"
    s *= join(texstring.(row.atests), "\\newline\n")
    s *= "\\subsubsection*{Total Number of Statistics}\n$(row.ntests) p-values, $(row.nvals) means or sems.\n"
    s *= "\\subsubsection*{Figures}\\begin{center}"
    for model in ["Baseline", "MotivationalControl", "EpisodicLikeMemory",
                  "PlasticCaching", "ReplayAndPlan"]
        s *= figstring(model, key)
    end
    s *= raw"\end{center}"
    s
end

summary = experiment_summary()
summary = @transform summary ntotal = :ntests .+ :nvals
summary = @transform summary nAtests = length.(:atests)

sort!(summary, :key, by = x -> findfirst(==(x), experiment_order))
summary = join([section(summary[i, :]) for i in 1:nrow(summary)], "\n\n");
open(joinpath(figpath, "experiment_summary.tex"), "w") do io
    write(io, summary)
end


