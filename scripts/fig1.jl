using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
using PGFPlotsX, BSON, CodecZstd, DataFrames

push!(PGFPlotsX.CUSTOM_PREAMBLE, raw"""
\usepackage{xcolor}
\definecolor{mblue}{HTML}{0992F2}
\definecolor{morange}{HTML}{ff7f0e}
\definecolor{mgreen}{HTML}{2ca02c}
\definecolor{mred}{HTML}{d62728}
""")

include(joinpath(@__DIR__, "paths.jl"))

results = open(joinpath(datapath, "results.bson.zstd")) do f
    s = ZstdDecompressorStream(f)
    res = BSON.load(s)[:results]
    close(s)
    res
end

include(joinpath(@__DIR__, "order.jl"))

sort!(results, :logp_hat)
sort!(results, :experiment, by = x -> findfirst(==(x), experiment_order))
sort!(results, :model, by = x -> findfirst(==(x), model_order))
best = combine(groupby(results, [:model, :experiment]),
               names(results) .=> x -> [last(x)], renamecols = false)
best.logp_hat_rel = best.logp_hat .- repeat(best.logp_hat[best.model .== :Baseline],
                                            length(union(best.model)))


groups = groupby(best[best.model .!= :Baseline, :], :model)
y = [g.logp_hat_rel for g in groups]

function toname(e)
    e == :Raby07_planningforbreakfast && return "Raby07 planning"
    e == :Raby07_breakfastchoice && return "Raby07 choice"
    replace(string(e), "_" => " ")
end

f1 = @pgf Axis({legend_entries =["Replay-and-Plan", "Plastic Caching",
                                 "Episodic-Like Memory", "Motivational Control"],
           legend_columns = 2,
           font = raw"\scriptsize", axis_lines = "left",
           x_axis_line_style = "white", ymax = 150, ymin = -2,
           y_axis_line_style = "-",
           "every axis legend/.append style" = {at = "{(0.01, 1)}",
                                                anchor = "south west"},
           xtick = 1:28, xmin = .5, xmax = 23.5,
           cycle_list = ["{thick, morange, mark = square}",
                         "{thick, mred, mark = square}",
                         "{thick, mblue, mark = square}",
                         "{thick, mgreen, mark = square}"],
           xticklabels = toname.(groups[1].experiment),
           width = "9cm", height = "6cm",
           ylabel = raw"$\Delta \log\hat p$",
           xticklabel_style = {rotate = 60, anchor = "east"}},
           "\\draw[draw = none, fill = mred!12] (-.5, 0) rectangle (11.5, 150);",
           "\\draw[draw = none, fill = mblue!12] (11.5, 0) rectangle (17.5, 150);",
           "\\draw[draw = none, fill = mgreen!12] (17.5, 0) rectangle (23, 150);",
           raw"\node[anchor = center] at (6, 135) {\color{black}planning experiments};",
           raw"\node[anchor = center] at (14.5, 135) {\parbox{2cm}{\centering memory experiments}};",
           raw"\node[anchor = center] at (20, 135) {\parbox{2cm}{\centering satiety experiments}};",
          [Plot(Coordinates(1:length(yi), yi)) for yi in y]...,
           Plot({no_marks, dashed, domain = "{0:28}"}, Expression("0")),
         )
pgfsave(joinpath(figpath, "logp_hat_rel_experiments.tikz"), f1)

pe = 1:11
me = 12:17
se = 18:22
ce = 23
ys = [sum(yi[e]) for yi in y, e in (ce, se, me, pe)]
f2 = @pgf Axis({ybar_stacked, xtick = 1:4, font = raw"\scriptsize", axis_lines =
                "left", x_axis_line_style = "white", xmin = 0.5, xmax = 4.5,
                height = "6cm", width = "6cm", ymax = 820, ymin = 0,
                y_axis_line_style = "-", "thin",
                cycle_list = ["fill = gray", "fill = mgreen!12",
                              "fill = mblue!12", "fill = mred!12"],
                ylabel = raw"$\Delta \log\hat p$",
                xticklabels =
                [raw"\parbox{3cm}{\color{morange}\raggedleft Replay-and-Plan\\\&
                                  Episodic-Like Memory\\\& Motivational Control}",
                 raw"\parbox{3.4cm}{\color{mred}\raggedleft Plastic Caching\\\&
                                  Episodic-Like Memory\\\& Motivational Control}",
                 raw"\parbox{3cm}{\color{mblue}\raggedleft Episodic-Like Memory\\\&
                                  Motivational Control}",
                 raw"\color{mgreen} Motivational Control"],
                xticklabel_style = {rotate = 60, anchor = "east"}},
               [Plot(Coordinates(1:4, ys[:, i])) for i in 1:4]...)
pgfsave(joinpath(figpath, "logp_hat_rel_summary.tikz"), f2)
