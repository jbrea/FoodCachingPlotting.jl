using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
using FoodCachingPlotting, Stipple, StippleUI, StipplePlotly, Genie, DataFrames,
      Random, PlotlyJS, FoodCachingExperiments, FoodCachingModels
import FoodCachingExperiments: EXPERIMENTS, CLAYTON0103_EXPERIMENTS
const M = FoodCachingModels

Base.@kwdef mutable struct Model <: ReactiveModel
    plot::R{Bool} = false
    birdid::R{Int} = 0
    birdid_options::R{Vector{Int}} = []
    imgurl::R{String} = ""
    filter::R{String} = ""
    seed::R{String} = "random"
    models::Vector{Any} = []
    layout::R{Any} = nothing
    plot_data::R{Vector{PlotData}} = []
    dist_layout::R{Any} = nothing
    dist_plots::R{Any} = nothing
    summary1_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary1_plots::R{Any} = nothing
    summary2_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary2_plots::R{Any} = nothing
    summary3_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary3_plots::R{Any} = nothing
    summary4_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary4_plots::R{Any} = nothing
    summary5_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary5_plots::R{Any} = nothing
    summary6_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary6_plots::R{Any} = nothing
    summary7_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary7_plots::R{Any} = nothing
    summary8_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary8_plots::R{Any} = nothing
    summary9_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary9_plots::R{Any} = nothing
    summary10_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary10_plots::R{Any} = nothing
    summary11_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary11_plots::R{Any} = nothing
    summary12_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary12_plots::R{Any} = nothing
    summary13_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary13_plots::R{Any} = nothing
    summary14_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary14_plots::R{Any} = nothing
    summary15_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary15_plots::R{Any} = nothing
    summary16_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary16_plots::R{Any} = nothing
    summary17_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary17_plots::R{Any} = nothing
    summary18_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary18_plots::R{Any} = nothing
    summary19_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary19_plots::R{Any} = nothing
    summary20_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary20_plots::R{Any} = nothing
    summary21_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary21_plots::R{Any} = nothing
    summary22_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary22_plots::R{Any} = nothing
    summary23_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary23_plots::R{Any} = nothing
    summary24_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary24_plots::R{Any} = nothing
    summary25_layout::R{Any} = PlotlyJS.Layout(height = 10, width = 10)
    summary25_plots::R{Any} = nothing
    experiment::R{String} = ""
    datadir::R{String} = joinpath(@__DIR__, "..", "data")
    files::R{Vector{String}} = []
    opt_experiment::Vector{String} = string.(setdiff(collect(keys(EXPERIMENTS)),
                                                     CLAYTON0103_EXPERIMENTS))
    model::R{String} = ""
    opt_models::Vector{String} = ["Baseline", "MotivationalControl",
                                  "EpisodicLikeMemory", "PlasticCaching",
                                  "ReplayAndPlan"]
    file::R{String} = ""
    opt_file::R{Vector{String}} = [""]
end

function updatefiles(model, dir)
    if ispath(dir)
        model.files[] = readdir(dir)
    end
end

function runmodel(m, experiment, trackedfields, seed)
    p = M.parameters(M.Model{:euler};
                      m.p.fixed...,
                      tracker = () -> M.Tracker(trackedfields = trackedfields))
    m = M.Population(m.m, m.s, m.l, m.u, p, m.dist, M.init(p))
    Random.seed!(seed)
    ms = rand(m, nbirds(experiment))
    data = run!(experiment, ms)
    ms, data
end

function default_trackedfields(p)
    pd = Dict(p...)
    res = [(:cage, :cacheableitems),
     (:cage, :eatableitems),
     (:cage, :ismdpresent),
     (:cage, :trays),
     :actions]
    if pd[:agent] ∈ [M.SpecSatAgent, M.PlasticCachingAgent]
        push!(res, (:agent, :hungermodel, :hunger))
    end
#     if haskey(pd, :cacheparams) && pd[:cacheparams] <: M.PlanningAgentParams
#         push!(res, (:agent, :cacheparams, :current))
#         push!(res, (:agent, :cacheparams, :memory))
#     end
    if haskey(pd, :cacheparams) && pd[:cacheparams] <: M.PlasticCachingAgentParams
        push!(res, (:agent, :cacheparams, :trayweights))
    end
    if pd[:hungermodel] <: M.Hunger{M.CacheModulatedCaching}
        push!(res, (:agent, :hungermodel, :cachemodulation, :cachemotivation))
    end
    res
end

function parse_seed(seed)
    seed == "best" && return 0
    seed == "random" && return UInt(time_ns())
    parse(UInt, seed)
end

function to_plotdata(x)
    PlotData(; filter(x -> x.first ∉ (:type, :xaxis), x.fields)...)
end
function plot_bird(ms, id)
    println("plotting bird $id")
    tmp = FoodCachingPlotting.plotexperiment(ms[id])
    model.plot_data[] = to_plotdata.(filter(x -> !any(ismissing.(x.fields[:y])), tmp.plot.data))
    model.layout[] = tmp.plot.layout
#     model.plot_data[] = [PlotData(; filter(e -> first(e) != :type, d.fields)...)
#                          for d in tmp.plot.data]
end

function titles(m)
    vcat([fill(string(k), M.NestedStructInitialiser.free_param_length(v))
          for (k, v) in m.p.free]...)
end

function plot_dists(m; x = 0.01:.01:.99)
    n = length(m.m)
    n1 = ceil(Int, sqrt(n))
    n2 = ceil(Int, n / n1)
    xaxis = Dict([Symbol("xaxis", i) => attr(automargin = false,
                                             title = attr(text = n, standoff = 0))
                  for (i, n) in enumerate(titles(m))]...)
    model.dist_layout[] = PlotlyJS.Layout(; grid = attr(ygap = .6,
                                                        rows = n1,
                                                        columns = n2,
                                                        pattern = "independent"),
                                           showlegend = false,
                                           height = 900,
                                           xaxis...
                                          )
    model.dist_plots[] = [PlotData(x = (u - l) * x .+ l,
                                   y = M.Distributions.pdf(m.dist(d, s), x),
                                   yaxis = "y$i", xaxis = "x$i"
                                  )
                          for (i, u, l, d, s) in zip(1:length(m.u), m.u, m.l, m.m, m.s)]
end

function plot_summary(e, data)
    summary_plots = plot_compare(e, FoodCachingExperiments.summarize(e, data))
    for i in 1:25
        if i <= length(summary_plots)
            getproperty(model, Symbol("summary$(i)_plots"))[] = summary_plots[i]
            getproperty(model, Symbol("summary$(i)_layout"))[] = PlotlyJS.Layout(height = 300, width = 600)
        else
            getproperty(model, Symbol("summary$(i)_layout"))[] = PlotlyJS.Layout(height = 10, width = 10)
        end
    end
end

model = Stipple.init(Model())


on(model.birdid) do id
    isempty(id) || plot_bird(model.models, id)
end

on(model.datadir) do dir
    updatefiles(model, dir)
end

function filter_files()
    updatefiles(model, model.datadir[])
    model.opt_file[] = filter(x -> match(Regex(model.model[]), x) !== nothing &&
                                 match(Regex(model.experiment[]), x) !== nothing,
                            model.files[])
end

on(model.model) do _
    filter_files()
end
on(model.experiment) do _
    filter_files()
end

on(model.plot) do plot
    println("plot changed $plot")
    plot || return
    f = model.file[]
    println("Loading $f ...")
    m = load(joinpath(model.datadir[], f))
    plot_dists(m)
    println("loaded")
    e = Symbol(model.experiment[])
    println("running")
    ms, data = runmodel(m, e,
                        default_trackedfields(m.p.fixed),
                        parse_seed(model.seed[]))
    model.models = ms
    plot_summary(e, data)
    model.birdid_options[] = 1:length(ms)
    model.birdid[] = 1
    model.plot[] = false
end


function ui()
    page(vm(model), class = "container", [input("", @bind(:datadir),
                                                  style = "width: 500px"),
         row(class = "st-br",
             [cell(class = "st-module", [
                   h6("model"),
                   quasar(:select, "", @bind(:model), options = :opt_models)]),
              cell(class = "st-module", [
                   h6("experiment"),
                   quasar(:select, "", @bind(:experiment), options = :opt_experiment)]),
              cell(class = "st-module", [
                   h6("file"),
                   quasar(:select, "", @bind(:file), options = :opt_file)]),
              cell(class = "st-module", [
                   h6("bird"),
                   quasar(:select, "", @bind(:birdid), options = :birdid_options)]),
              cell(class = "st-module", [
                   h6("seed"), "<br>",
                      input("", @bind(:seed), style = "width: 75px")]),
                     row(class = "justify-end",
                              btn("plot", @click("plot = true"),
                                  style = "background: #FF0000; color: white")
                ),
               ]),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:plot_data, layout = :layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary1_plots, layout = :summary1_layout),
            StipplePlotly.plot(:summary2_plots, layout = :summary2_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary3_plots, layout = :summary3_layout),
            StipplePlotly.plot(:summary4_plots, layout = :summary4_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary5_plots, layout = :summary5_layout),
            StipplePlotly.plot(:summary6_plots, layout = :summary6_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary7_plots, layout = :summary7_layout),
            StipplePlotly.plot(:summary8_plots, layout = :summary8_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary9_plots, layout = :summary9_layout),
            StipplePlotly.plot(:summary10_plots, layout = :summary10_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary11_plots, layout = :summary11_layout),
            StipplePlotly.plot(:summary12_plots, layout = :summary12_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary13_plots, layout = :summary13_layout),
            StipplePlotly.plot(:summary14_plots, layout = :summary14_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary15_plots, layout = :summary15_layout),
            StipplePlotly.plot(:summary16_plots, layout = :summary16_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary17_plots, layout = :summary17_layout),
            StipplePlotly.plot(:summary18_plots, layout = :summary18_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary19_plots, layout = :summary19_layout),
            StipplePlotly.plot(:summary20_plots, layout = :summary20_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary21_plots, layout = :summary21_layout),
            StipplePlotly.plot(:summary22_plots, layout = :summary22_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary23_plots, layout = :summary23_layout),
            StipplePlotly.plot(:summary24_plots, layout = :summary24_layout)
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:summary25_plots, layout = :summary25_layout),
           ])),
         row(cell(class = "st-module", [
            StipplePlotly.plot(:dist_plots, layout = :dist_layout)
           ])),
         ]) |> html
end

route("/", ui)
Genie.config.server_host = "127.0.0.1"
up()
