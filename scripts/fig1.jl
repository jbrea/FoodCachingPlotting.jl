using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
using PGFPlotsX, BSON, CodecZstd, DataFrames

include(joinpath(@__DIR__, "utils.jl"))

res = loadresult(isempty(ARGS) ? "run_indi.bson.zstd" : ARGS[1])

baseline = length(ARGS) > 1 ? prepare_plot(loadresult(ARGS[2]))[1] : :self
best, groups, y, err = prepare_plot(res; baseline)

REV1 = "be96fd7" # new results
REV2 = "f5ec1d5" # with maxN = 2000
REV3 = "01135d8" # new CacheModulatedCaching2
resnew = vcat(loadresult("run_HungermodulatedCaching_$REV1.bson.zstd"),
              loadresult("run_HungermodulatedCaching_$REV2.bson.zstd"));
nr = prepare_plot(resnew; baseline = best);

resnew2 = vcat(loadresult("run_UnmodulatedCaching_$REV1.bson.zstd"),
               loadresult("run_UnmodulatedCaching_$REV2.bson.zstd"));
nr2 = prepare_plot(resnew2; baseline = nr.best, baselinemodel = :self);

resnew3 = vcat(loadresult("run_CachemodulatedCaching_$REV1.bson.zstd"),
               loadresult("run_CachemodulatedCaching_$REV2.bson.zstd"))
nr3 = prepare_plot(resnew3, baseline = nr.best, baselinemodel = :self);

resnew6 = vcat(loadresult("run_CacheModulatedCaching2_$REV3.bson.zstd")[:, ["model", "experiment", "logp_hat", "logp_hat_std"]],
               loadresult("run_CacheModulatedCaching2_COPY.bson.zstd")) # for MotivationalControl Clayton99C exp2 the optimizer did not find a good result, but copying parameters from the hungermodulated fit works.
nr6 = prepare_plot(resnew6, baseline = nr.best[nr.best.experiment .∈ Ref(union(resnew6.experiment)), :], baselinemodel = :self);

resnew4 = loadresult("run_unmodulatedcachingX.bson.zstd")
nr4 = prepare_plot(resnew4, baseline = best);

resnew5 = loadresult("run_X.bson.zstd")
nr5 = prepare_plot(resnew5, baseline = best);

cols = [:model, :experiment, :logp_hat]
comparebest = leftjoin(best[best.model .∈ Ref((:MotivationalControl, :EpisodicLikeMemory, :PlasticCaching)), [cols; :logp_hat_std; :logp_hat_rel]],
                       nr.best[:, cols], on = [:model, :experiment], makeunique = true)
comparebest = leftjoin(comparebest, nr2.best[:, cols], on = [:model, :experiment], makeunique = true)
comparebest = leftjoin(comparebest, nr3.best[:, cols], on = [:model, :experiment], makeunique = true)
comparebest.dh_o = comparebest.logp_hat_1 - comparebest.logp_hat
comparebest.du_o = comparebest.logp_hat_2 - comparebest.logp_hat
comparebest.dc_o = comparebest.logp_hat_3 - comparebest.logp_hat
comparebest.dc_h = comparebest.logp_hat_3 - comparebest.logp_hat_1
comparebest.du_h = comparebest.logp_hat_2 - comparebest.logp_hat_1

pe = 1:11
me = 12:17
se = 18:22
ce = 23
reverse_axis_dir(x) = 27 - x
xtick = reverse_axis_dir.([pe; me .+ 1; se .+ 2; ce .+ 3])
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
function plot_logp_hat(y, err;
                       ymax = 30, ymin = -130,
                       xmin = .5, xmax = 27,
                       width = "9cm", legend_entries = legend_entries)
    @pgf Axis({legend_entries = legend_entries,
           legend_columns = 2, #grid = "none",
           font = raw"\scriptsize", axis_lines = "left",
           x_axis_line_style = "white", ymax = ymax, ymin = ymin,
           y_axis_line_style = "-",
           "every axis legend/.append style" = {at = "{(0.01, 1)}",
                                                anchor = "south west"},
           xtick = 27:-1:1, xmin = xmin, xmax = xmax,
           cycle_list = cycle_list,
           xticklabels = toname.(groups[1].experiment) |>
                         l -> insert!(l, 23, "") |> l -> insert!(l, 18, "") |>
                         l -> insert!(l, 12, ""),
           width = width, height = "6cm",
           ylabel = raw"$\Delta \log\hat p$",
           grid = "major",
           "error bars/y dir=both",
           "error bars/y explicit",
           "error bars/error mark = .",
           xticklabel_style = {xshift = "-.5mm", rotate = 60, anchor = "east"},
          },
#                "\\draw[draw = none, fill = mred!12] ($(reverse(-.5)), -125) rectangle ($(reverse(11.5)), 150);",
#                "\\draw[draw = none, fill = mblue!12] ($(reverse(12.5)), -125) rectangle ($(reverse(18.5)), 150);",
#                "\\draw[draw = none, fill = mgreen!12] ($(reverse(19.5)), -125) rectangle ($(reverse(25)), 150);",
"\\node[anchor = center, fill = white] at ($(reverse_axis_dir(5.5)), -124) {\\parbox{2.6cm}{\\centering planning}};",
               "\\node[anchor = center, fill = white] at ($(reverse_axis_dir(15)), -124) {\\parbox{1.4cm}{\\centering memory}};",
               "\\node[anchor = center, fill = white] at ($(reverse_axis_dir(21.5)), -124) {\\parbox{1.1cm}{\\centering satiety}};",
               [Plot(Coordinates(xtick .+ (i)/(length(y) + 1), yi, yerror = err[i])) for (i, yi) in enumerate(y)]...,
           Plot({no_marks, dashed, domain = "{0:28}"}, Expression("0")),
         )
end
f1 = plot_logp_hat(y, err)

pgfsave(joinpath(figpath, "logp_hat_rel_experiments.tikz"), f1)

ys = [sum(yi[e]) for yi in y, e in (ce, se, me, pe)]
if length(ARGS) < 1
    ys = vcat(fill(eps(), 4)', ys) # to see something on the plot
end
cycle_list = collect(Iterators.reverse(["",
                                        "{thick, dotted, mark = none, mark options = {solid, fill = white}, }",
                                        "{thick, dashed, mark = none, mark options = {solid}, fill = white}",
                                        "{thick, solid, mark = none, pattern = crosshatch}"]))
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

f3 = plot_logp_hat(nr2.y, nr2.err, ymin = -50, ymax = 20, xmin = 2.5, xmax = 8.5,
                   width = "4cm", legend_entries = [legend_entries[1:3]; "hunger-modulated caching"]);
f3.options["legend_to_name"] = "alt-mod-legend"
f4 = plot_logp_hat(nr3.y, nr3.err, ymin = -50, ymax = 20, xmin = 2.5, xmax = 8.5,
                   width = "4cm", legend_entries = []);
pgfsave(joinpath(figpath, "logp_hat_rel_unmodcaching.tikz"), f3)
pgfsave(joinpath(figpath, "logp_hat_rel_cachemodcaching.tikz"), f4)

# push dummy
tmpy = [[fill(0, 17); y; 0] for y in nr6.y]
tmperr =fill(fill(0, 23), 3)
f5 = plot_logp_hat(tmpy, tmperr, ymin = -50, ymax = 20, xmin = 2.5, xmax = 8.5,
                   width = "4cm", legend_entries = []);
f5
pgfsave(joinpath(figpath, "logp_hat_rel_cachemodcaching2.tikz"), f5)
