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

include(joinpath(@__DIR__, "utils.jl"))

res = loadresult(isempty(ARGS) ? "run_indi.bson.zstd" : ARGS[1])

baseline = length(ARGS) > 1 ? prepare_plot(loadresult(ARGS[2]))[1] : :self
best, groups, y, err = prepare_plot(res; baseline)

resnew = loadresult("run_nocachemodulation_db2221c.bson.zstd");
nr = prepare_plot(resnew; baseline = best);

resnew2 = loadresult("run_nomodulation_8918427.bson.zstd");
nr2 = prepare_plot(resnew2; baseline = best);

pe = 1:11
me = 12:17
se = 18:22
ce = 23
reverse(x) = 27 - x
xtick = reverse.([pe; me .+ 1; se .+ 2; ce .+ 3])
legend_entries = ["Plastic Caching", "No-Plasticity", "No-Plasticity-No-Memory",
                  "\\parbox{3.4cm}{No-Plasticity-No-Memory-\\\\No-Motivational-Control}" ]
cycle_list = ["{thick, mred, mark = *, mark options = {fill = white}}",
              "{thick, mblue, mark = *, mark options = {fill = white}}",
              "{thick, mgreen, mark = *,  mark options = {fill = white}}",
              "{thick, gray!60!black, mark = *, mark options = {fill = white}}"]
if length(ARGS) > 1
    prepend!(legend_entries, ["Planning-By-Replay"])
    prepend!(cycle_list, ["{thick, morange, mark = *, mark options = {fill = white}}"])
end
f1 = @pgf Axis({legend_entries = legend_entries,
           legend_columns = 2, #grid = "none",
           font = raw"\scriptsize", axis_lines = "left",
           x_axis_line_style = "white", ymax = 30, ymin = -130,
           y_axis_line_style = "-",
           "every axis legend/.append style" = {at = "{(0.01, 1)}",
                                                anchor = "south west"},
           xtick = 27:-1:1, xmin = .5, xmax = 27.,
           cycle_list = cycle_list,
           xticklabels = toname.(groups[1].experiment) |>
                         l -> insert!(l, 23, "") |> l -> insert!(l, 18, "") |>
                         l -> insert!(l, 12, ""),
           width = "9cm", height = "6cm",
           ylabel = raw"$\Delta \log\hat p$",
           grid = "major",
           "error bars/y dir=both",
           "error bars/y explicit",
           "error bars/error mark = .",
           xticklabel_style = {xshift = "-.5mm", rotate = 60, anchor = "east"}},
#                "\\draw[draw = none, fill = mred!12] ($(reverse(-.5)), -125) rectangle ($(reverse(11.5)), 150);",
#                "\\draw[draw = none, fill = mblue!12] ($(reverse(12.5)), -125) rectangle ($(reverse(18.5)), 150);",
#                "\\draw[draw = none, fill = mgreen!12] ($(reverse(19.5)), -125) rectangle ($(reverse(25)), 150);",
"\\node[anchor = center, fill = white] at ($(reverse(5.5)), -124) {\\parbox{2.6cm}{\\centering planning}};",
               "\\node[anchor = center, fill = white] at ($(reverse(15)), -124) {\\parbox{1.4cm}{\\centering memory}};",
               "\\node[anchor = center, fill = white] at ($(reverse(21.5)), -124) {\\parbox{1.1cm}{\\centering satiety}};",
               [Plot(Coordinates(xtick .+ (i)/(length(y) + 1), yi, yerror = err[i])) for (i, yi) in enumerate(y)]...,
           Plot({no_marks, dashed, domain = "{0:28}"}, Expression("0")),
         )

pgfsave(joinpath(figpath, "logp_hat_rel_experiments.tikz"), f1)

ys = [sum(yi[e]) for yi in y, e in (ce, se, me, pe)]
if length(ARGS) < 1
    ys = vcat(fill(eps(), 4)', ys) # to see something on the plot
end
cycle_list = collect(Iterators.reverse(["",
                                        "{thick, dotted, mark = square*, mark options = {solid, fill = white}, }",
                                        "{thick, dashed, mark = diamond*, mark options = {solid}, fill = white}",
                                        "{thick, solid, mark = triangle*, mark options = {fill = white}}"]))
f2 = @pgf Axis({ybar_stacked, xtick = 1:5, font = raw"\scriptsize",
                xmin = 0.5, xmax = 5.5,
                height = "6cm", width = "3.8cm",
                ymax = 120, #ymin = -1350,
                y_axis_line_style = "-", "thin",
                cycle_list = cycle_list,
                ylabel = raw"$\Delta \log\hat p$",
                xticklabels =
                [raw"\parbox{3cm}{\color{morange}\raggedleft Planning-By-Replay}",
                 raw"\parbox{3.4cm}{\color{mred}\raggedleft Plastic Caching}",
                 raw"\parbox{3cm}{\color{mblue}\raggedleft No-Plasticity}",
                 raw"\color{mgreen} No-Plasticity-No-Memory",
                 raw"\parbox{3.4cm}{\color{gray!60!black}\raggedleft No-Plasticity-No-Memory-\\No-Motivational-Control}"],
                xticklabel_style = {rotate = 60, anchor = "east"}},
               [Plot(Coordinates(1:5, ys[:, i])) for i in 4:-1:1]...)

pgfsave(joinpath(figpath, "logp_hat_rel_summary_all.tikz"), f2)


