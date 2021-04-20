using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
using FoodCachingExperiments, FoodCachingModels, Statistics, DataFrames, PGFPlotsX,
      FoodCachingPlotting
import FoodCachingExperiments: Experiment, _abc
import PlanningVsCompensatoryCaching
const PC = PlanningVsCompensatoryCaching
import StatsBase: sample

postprocess(::Any, data) = data
function postprocess(::Experiment{:Amodio20_exp2}, data)
    cached = []
    for row in 1:nrow(data)
        push!(cached, circshift([data.A[row], data.B[row], data.C[row]],
                                -(_abc(row, 1)-1)))
    end
    data, cached
end
function postprocess(::Experiment{:Amodio20_exp1}, data)
    cached = []
    for row in 1:nrow(data) ÷ 2
        push!(cached, hcat([circshift([data.A[2*row-i],
                                       data.B[2*row-i],
                                       data.C[2*row-i]],
                                      -(_abc(row, 1)-1))
                            for i in (isodd(row) ? (1, 0) : (0, 1))]...))
    end
    data, cached
end
function predict(fp; N = 500, experiment = :Amodio20_exp2)
    ms = rand(fp, N)
    expe = Experiment{experiment}(nothing, nothing, "", "", DataFrame(), Dict(), 0, [])
    data = run!(expe, ms)
    postprocess(expe, data)
end

function posterior(cached, K, ms; only_data = false)
    N = length(cached)
    d_fn = cached[sample(2:2:N, K, replace = false)]
    only_data || (lp_fn = PC.compute_posterior(d_fn, ms, group = :FN, unnormalizedlog = true))
    d_nf = cached[sample(1:2:N, K, replace = false)]
    only_data ||  (lp_nf = PC.compute_posterior(d_nf, ms, group = :NF, unnormalizedlog = true))
    only_data ? 0 : (x -> x ./ sum(x))(exp.(lp_fn .+ lp_nf)), mean(d_fn), mean(d_nf)
end
function posterior2(cached, K, ms)
    N = length(cached)
    d = cached[sample(1:N, K, replace = false)]
    p = PC.compute_posterior(d, ms)
    p, mean(d)
end

function plotcachingpattern(res)
    @pgf Axis({xticklabels = [raw"$K_1$", raw"$K_2$", raw"$K_3$"],
                xtick = 1:3, height = "3cm", width = "3cm",
                font = raw"\scriptsize",
                ylabel = "items cached",
#                 legend_entries = ["group 1", "group 2"]
},
               Plot({"mblue", mark = "square"}, Table(1:3, mean(x[2] for x in res[3]))),
               Plot({"mbrown", mark = "square"}, Table(1:3, mean(x[3] for x in res[3]))))
end
function plot_bayesratioh(bf; colors = ["red", "green!60!black", "blue"],
                              data = nothing,
                              legend = ["6 birds", "12 birds", "18 birds"],
                              opt = @pgf({}),
    )
    p1 = boxplot([bf],
            options = [PGFPlotsX.Options(:draw => c) for c in colors],
            labels = [],
            axisopt = merge(@pgf({ylabel = "Bayes Factor",
#                             ymin = 1e-8, ymax = 1e3,
#                             ytick = [1e-6, 1e-3, 1, 1e3],
                            height = "4.5cm", xtick = [.75, 1, 1.25],
                            font = raw"\scriptsize",
                            width = "3cm",
                            "legend image code/.code = {\\draw [#1,only marks] plot coordinates { (0cm, 0cm) (0.1cm, 0cm) (0.2cm, 0cm)};}",
                            legend_entries = legend
                           }), opt)
           );
    push!(p1.contents, raw"\draw[dotted] (0, 1) -- (4, 1);");
    data === nothing || push!(p1.contents, "\\draw[] (0, $data) -- (4, $data);");
    p1
end
function plotcachingpattern1(res)
    @pgf Axis({xticklabels = [raw"$K_1$", raw"$K_2$", raw"$K_3$"],
                xtick = 1:3, height = "3cm", width = "3cm",
                font = raw"\scriptsize",
                ylabel = "items cached",
#                 legend_entries = ["group 1", "group 2"]
},
              Plot({"mblue", mark = "square"}, Table(1:3, mean(res)[:, 1])),
              Plot({"mbrown", mark = "square"}, Table(1:3, mean(res)[:, 2])))
end
function plotcachingpattern_hyp(res)
    @pgf Axis({xticklabels = [raw"$K_1$", raw"$K_2$", raw"$K_3$"],
                xtick = 1:3, height = "3cm", width = "3cm",
                ymin = 0, ymax = 7,
                font = raw"\scriptsize",
                ylabel = "items cached",
#                 legend_entries = ["group 1", "group 2"]
},
               Plot({"mblue", mark = "square"}, Table(1:2, mean(x[2] for x in res[3]))),
              )
end
function plot_bayesratio(bf;
        colors = ["red", "green!60!black", "blue"],
        labels = [raw"$M_0/M_1$", raw"$M_0/M_2$", raw"$M_1/M_2$"],
        legend = ["6 birds", "12 birds", "18 birds"],
        opt = @pgf({}),
    )
    p1 = boxplot(bf,
                 options = [PGFPlotsX.Options(:draw => c) for c in colors],
                 labels = [],
            axisopt = merge(@pgf({ylabel = "Bayes Factor",
#                             ymin = 1e-6, ymax = 1e5,
#                             xmax = 2.5,
#                             xmin = 1.5,
                            height = "4.5cm", xtick = [.75, 1, 1.25],
                            font = raw"\scriptsize",
                            width = "3cm",
#                             ytick = [1e-12, 1e-3, 1e-0, 1e3],
                            "legend image code/.code = {\\draw [#1,only marks] plot coordinates { (0cm, 0cm) (0.1cm, 0cm) (0.2cm, 0cm)};}",
                            legend_entries = legend
                           }), opt));
    push!(p1.contents, raw"\draw[dotted] (0, 1) -- (4, 1);");
    push!(p1.contents, raw"\draw[] (0, 80) -- (4, 80);");
    p1
end

include(joinpath(@__DIR__, "paths.jl"))
model1 = load(joinpath(datapath, "ReplayAndPlan_Raby07_breakfastchoice"))
model2 = load(joinpath(datapath, "PlasticCaching_Raby07_breakfastchoice"))

data11, cached11 = predict(model1, N = 5000, experiment = :Amodio20_exp1);
data21, cached21 = predict(model2, N = 5000, experiment = :Amodio20_exp1);
data1, cached1 = predict(model1, N = 5000, experiment = :Amodio20_exp2);
data2, cached2 = predict(model2, N = 5000, experiment = :Amodio20_exp2);
datah1 = predict(model1, N = 5000, experiment = :Hypothetical);
datah2 = predict(model2, N = 5000, experiment = :Hypothetical);

ms = [
      (bird_indep = true, compartement_indep = true,
       food_indep = true, hyp = nothing),
      (bird_indep = true, compartement_indep = false,
       food_indep = true, hyp = :cch),
      (bird_indep = true, compartement_indep = false,
       food_indep = true, hyp = :fph1),
      (bird_indep = true, compartement_indep = false,
       food_indep = true, hyp = :fph2),
     ]
ms_hypothetical = [(bird_indep = true, compartement_indep = true,
                    food_indep = true),
                   (bird_indep = true, compartement_indep = false,
                    food_indep = true, constraints = ((1, >, 2),))]
ms_1 = [(bird_indep = true, compartement_indep = false, food_indep = false,
         hyp = :cch),
        (bird_indep = true, compartement_indep = false, food_indep = false,
         hyp = :fph1)]

N = 500
res1 = [[posterior(cached1, n, ms, only_data = false) for _ in 1:N] for n in [3, 6, 9]]
res2 = [[posterior(cached2, n, ms, only_data = false) for _ in 1:N] for n in [3, 6, 9]]
res11 = [[posterior2(cached11, n, ms_1) for _ in 1:N] for n in [6, 12, 18]]
res21 = [[posterior2(cached21, n, ms_1) for _ in 1:N] for n in [6, 12, 18]]
resh1 = [[posterior2([Array(datah1[i, 3:4]) for i in 1:nrow(datah1)],
                                 n, ms_hypothetical) for _ in 1:N] for n in [6, 12, 18]]
resh2 = [[posterior2([Array(datah2[i, 3:4]) for i in 1:nrow(datah1)],
                                 n, ms_hypothetical) for _ in 1:N] for n in [6, 12, 18]]


p1 = plotcachingpattern(res1)
p2 = plotcachingpattern(res2)
pgfsave(joinpath(figpath, "prediction_Amodio_exp2_1.tikz"), p1)
pgfsave(joinpath(figpath, "prediction_Amodio_exp2_2.tikz"), p2)

bayesfactors(res, models = [(1, 2), (1, 4), (2, 4), (2, 3)]) = [[r[1][i] ./ r[1][j] for r in res] for (i, j) in models]
cchconsistency(res) = [r[2][2] > r[2][1] && r[2][2] > r[2][3] &&
                       r[3][2] < r[3][1] && r[3][2] < r[3][3] for r in res]
fph1consistency(res) = [r[2][1] > r[2][2] && r[2][1] > r[2][3] &&
                        r[3][2] > r[3][1] && r[3][2] > r[3][3] for r in res]
fph2consistency(res) = [r[2][1] > r[2][2] && r[2][3] > r[2][2] &&
                        r[3][2] > r[3][1] && r[3][2] > r[3][3] for r in res]

bayesfactors2(res) = [r[1][1] / r[1][2] for r in res]
bfh1 = bayesfactors2.(resh1)
bfh2 = bayesfactors2.(resh2)

p1 = plot_bayesratioh(bfh1, legend = [], opt = @pgf({ymin = 1e-25, ymax = 1e2}))
p2 = plot_bayesratioh(bfh2, opt = @pgf({ymin = 1e-25, ymax = 1e2, legend_pos = "outer north east"}))
f1 = joinpath(figpath, "prediction_bayesfactor_hyp1.tikz")
pgfsave(f1, p1)
f2 = joinpath(figpath, "prediction_bayesfactor_hyp2.tikz")
pgfsave(f2, p2)

p1 = plotcachingpattern_hyp(resh1)
p2 = plotcachingpattern_hyp(resh2)
f1 = joinpath(figpath, "prediction_hyp_1.tikz")
pgfsave(f1, p1)
f2 = joinpath(figpath, "prediction_hyp_2.tikz")
pgfsave(f2, p2)


replnan(x) = isnan(x) ? rand() : x
replnan(x::AbstractVector) = replnan.(x)
bf11 = bayesfactors2.(res11)
bf21 = bayesfactors2.(res21)
p1 = plot_bayesratioh(replnan.(bf11), data = 0.00001/0.0014,
                      legend = [], opt = @pgf({ymin = 1e-25, ymax = 1e4}))
p2 = plot_bayesratioh(replnan.(bf21), data = 0.00001/0.0014,
                      opt = @pgf({ymin = 1e-25, ymax = 1e4, legend_pos = "outer north east"})
                     )
f1 = joinpath(figpath, "prediction_bayesfactor11.tikz")
pgfsave(f1, p1)
f2 = joinpath(figpath, "prediction_bayesfactor12.tikz")
pgfsave(f2, p2)

p1 = plotcachingpattern1(cached11)
p2 = plotcachingpattern1(cached21)
f1 = joinpath(figpath, "prediction_Amodio_exp1_1.tikz")
pgfsave(f1, p1)
f2 = joinpath(figpath, "prediction_Amodio_exp1_2.tikz")
pgfsave(f2, p2)
