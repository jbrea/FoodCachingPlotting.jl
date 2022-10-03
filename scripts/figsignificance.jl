include(joinpath(@__DIR__, "fig1.jl"))
using FoodCachingExperiments, Random, FoodCachingModels, Statistics, DataFrames
# This is a hack, because the labeling in Clayton99C_exp1 is wrong
FoodCachingExperiments.EXPERIMENTS[:Clayton99C_exp1].tests["interactions"].labels[2] = "missing"
import FoodCachingExperiments: EXPERIMENTS, bsave, bload


function plot_sigresults(sigresults;
                         ylabel = "reproduced significant p-values in \\%",
                         measure = :ratio_same_rejects_mean)
    tmp = combine(groupby(sigresults, [:model, :experiment]),
                  [:samebinarysignificance, :meansamebinarysigA,
                   :meansamebinarysig, :atleasttwo, :ratio_same_rejects] .=> mean,
                 )
    sort!(tmp, :experiment, by = x -> findfirst(==(x), experiment_order))
    sort!(tmp, :model, by = x -> findfirst(==(x), model_order))
    groups = groupby(tmp, :model)
    ys = [getproperty(g, measure)  for g in groups]
    ns = [sum(rejects(EXPERIMENTS[e])) for e in experiment_order
          if e ∉ (:Clayton0103, :Watanabe05)]
    pe = 1:11
    me = 12:22
    se = 23:27
    ce = 28
    reverse(x) = 32 - x
    xtick = reverse.([pe; me .+ 1; se .+ 2; ce .+ 3])
    f1 = @pgf Axis({xtick = 32:-1:1, xticklabels = (toname.(groups[1].experiment)|>
                         l -> insert!(l, 28, "") |> l -> insert!(l, 23, "") |>
                         l -> insert!(l, 12, "")),
               ylabel = ylabel,
               xticklabel_style = {xshift = "-.5mm", rotate = 60, anchor = "east"},
               width = "16cm", height = "6cm", xmin = .5, xmax = 32.5,
               grid = "major", axis_line_style = "{draw = white}",
               legend_entries = "{Planning-By-Replay, Plastic Caching, No-Plasticity, No-Plasticity-No-Memory, No-Plasticity-No-Memory-No-Motivational-Control}", legend_columns="{3}", font="{\\scriptsize}",
                "every axis legend/.append style={at={{(0.01, 1)}}, anchor={south west}}",
               cycle_list = [
                             "{thick, morange, mark = *, mark options = {fill = white}}",
                             "{thick, mred, mark = *, mark options = {fill = white}}",
                             "{thick, mblue, mark = *, mark options = {fill = white}}",
                             "{thick, mgreen, mark = *, mark options = {fill = white}}",
                             "{thick, gray!60!black, mark = *, mark options = {fill = white}}"
                            ],
              },
                   [Plot(Coordinates(xtick .+ (i)/(length(ys) + 1), 100*y)) for (i, y) in pairs(ys)]...)

    f2 = @pgf Axis({cycle_list = [
                                 "{thick, mark = square*, dotted, mark size = 3,
                                 mark options = {solid, fill = white}}",
                                 "{thick, dashed, mark = diamond*,
                                   mark options = {solid, fill =  white}, mark size = 3}",
                                 "{thick, mark = triangle*, mark size = 3,
                                   mark options = {fill = white}}",
                                ],
                   ylabel = "avg. reproducibility [\\%]",
               xticklabel_style = {rotate = 60, anchor = "east"},
               height = "4.8cm", width = "3.8cm",
               ymax = 105,
               ytick = 0:20:100,
               legend_entries = {"satiety experiments", "memory experiments",
                                 "planning experiments"},
               "every axis legend/.append style" = {at = "{(-.4, 1)}",
                                                    anchor = "south west"},
               xtick = 1:5, font = raw"\scriptsize",
               xticklabels = ["\\color{morange}Planning-By-Replay",
                              "\\color{mred}Plastic Caching",
                              "\\color{mblue}No-Plasticity",
                              "\\color{mgreen}No-Plasticity-No-Memory",
                              "\\parbox{3.4cm}{\\color{gray!60!black}\\raggedleft No-Plasticity-No-Memory-\\\\No-Motivational-Control}"],
              },
                  [Plot(Coordinates(1:5, [100*sum(y[x] .* 1/length(x)) for y in ys]))
               for x in Iterators.reverse([pe, me, se])]...)
    (f1, f2)
end

rm_test_locals.(values(EXPERIMENTS))

sigresults1 = run_sigtests(best, Ninner = 1, Nouter = 50)
bsave(joinpath(datapath, "sigresults1"), Dict(:sigresults1 => sigresults1))
sigresults10 = run_sigtests(best, Ninner = 10, Nouter = 50)
bsave(joinpath(datapath, "sigresults10"), Dict(:sigresults10 => sigresults10))

sigresults1 = bload(joinpath(datapath, "sigresults1"))[:sigresults1]
sigresults10 = bload(joinpath(datapath, "sigresults10"))[:sigresults10]

p1indi, p1all = plot_sigresults(sigresults1)
p10indi, p10all = plot_sigresults(sigresults10)

combine(groupby(sigresults10, :model), :samebinarysignificance => mean)

pgfsave(joinpath(figpath, "sig_summary_all1.tikz"), p1all)
pgfsave(joinpath(figpath, "sig_summary_all10.tikz"), p10all)
pgfsave(joinpath(figpath, "sig_summary_indi10.tikz"), p10indi)
