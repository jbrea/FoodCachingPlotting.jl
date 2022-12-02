module FoodCachingPlotting

using DataFrames, DataFramesMeta, Unitful, Random, StatsBase, Statistics
import PGFPlotsX, PlotlyJS, FoodCachingExperiments, FoodCachingModels
import PGFPlotsX: @pgf, Coordinates, Plot, Axis, Options
import FoodCachingExperiments: Experiment, EXPERIMENTS, CLAYTON0103_EXPERIMENTS,
       asdataframe

export plot_experiment, plot_compare, boxplot

function __init__()
    push!(PGFPlotsX.CUSTOM_PREAMBLE, "\\usepackage{longtable}\\pgfplotsset{width=5cm}")
    PGFPlotsX.CLASS_OPTIONS[1] = "crop = false"
    push!(PGFPlotsX.CUSTOM_PREAMBLE, raw"""
    \usepackage{xcolor}
    \definecolor{mblue}{HTML}{0992F2}
    \definecolor{morange}{HTML}{ff7f0e}
    \definecolor{mgreen}{HTML}{2ca02c}
    \definecolor{mred}{HTML}{d62728}
    \pagestyle{empty}
    """)
end

function plotdf(::Val{:pgf}, df, x, y; yerr, group, kwargs...)
    df = copy(df)
    gs = groupby(df, group)
    legend_entries = (x -> join(x, " ")).(collect(keys(gs.keymap)))
    xlabs = union(getproperty(df, x))
    try
        xlabs = Int.(xlabs)
    catch
    end
    xcoords = haskey(kwargs, :xtick) ? Dict(x => i for (i, x) in zip(kwargs[:xtick], xlabs)) : Dict(x => i - 1 for (i, x) in enumerate(xlabs))
    df.xcoords = map(x -> xcoords[x], getproperty(df, x))
    ymax = maximum(getproperty(df, y) .+ (yerr === nothing ? 0 : getproperty(df, yerr)))
    ymin = minimum(getproperty(df, y) .- (yerr === nothing ? 0 : getproperty(df, yerr)))
    ymax += .05 * (ymax - ymin)
    ymin -= .05 * (ymax - ymin)
    options = merge(@pgf({xtick = 0:length(xlabs)-1,
              xticklabels = xlabs, ymax = ymax, ymin = ymin,
               "no marks", xlabel = string(x),
               "error bars/error bar style = very thick", very_thick,
               axis_line_style = {draw = "none"},
               "error bars/error mark = |",
               legend_to_name = randstring(),
               legend_entries = legend_entries, legend_columns = 2,
               legend_style = {draw = "none", very_thick},
               "error bars/error mark options = {very thick, mark size = 3}",
               grid = "major", tick_style = "gray!50",
               ylabel = "items",
               "error bars/y explicit",
               "error bars/y dir=both"}), Options(kwargs...))
    Axis(options,
         [Plot(Coordinates(getproperty(d, :xcoords),
                           getproperty(d, y),
                           yerror = yerr === nothing ? nothing : getproperty(d, yerr)))
          for d in gs]...)
end
ticks(x::AbstractVector{Int}) = sort(union(x))
ticks(x) = typeof(x[1]) <: Int ? sort(union(x)) : 0:length(union(x))-1
function plotdf(::Val{:plotly}, df, x, y; yerr, group,
                  color = [:red, :blue, :green, :orange, :cyan, :brown],
                  kwargs...)
    layout = merge(Dict(:xaxis => Dict(:title => string(x),
                                       :zeroline => false,
                                       :tickvals => ticks(getproperty(df, x))),
                        :yaxis => Dict(:zeroline => false),
                        :legend => Dict(:xanchor => "center", :yanchor => "top",
                                        :x => .5, :y => -.3)), kwargs)
    p = PlotlyJS.Plot(df, PlotlyJS.Layout(; layout...); x, y, group, yerr)
    for (i, t) in enumerate(p.data)
        t[:error_y] = Dict(:array => t[:yerr])
        t[:line] = Dict(:color => color[i])
    end
    PlotlyJS.plot(p)
end

function plotdf(df, x, y = :μ;
                yerr = hasproperty(df, :sem) ? :sem : nothing,
                group = setdiff(Symbol.(names(df)), [x, y, yerr, :n]),
                backend = :plotly,
                kwargs...)
    plotdf(Val(backend), df, x, y; yerr, group, renamekw(Val(backend), kwargs)...)
end
function plotdfgroupby(df, group, x, y = :μ; backend = :plotly, kwargs...)
    [plotdf(d, x, y; backend, kwargs...) for d in groupby(df, group)]
end

plot_experiment(e::Symbol; kwargs...) = plot_experiment(EXPERIMENTS[e]; kwargs...)
plot_experiment(e::Symbol, data; kwargs...) = plot_experiment(EXPERIMENTS[e], data; kwargs...)
function plot_experiment(e::Symbol, data::AbstractVector; kwargs...)
    if e == :Clayton0103
        dfs = asdataframe(e, data)
        [plot_experiment(ei, di[1]; kwargs...)
         for (ei, di) in zip(CLAYTON0103_EXPERIMENTS, dfs)]
    else
        plot_experiment(e, asdataframe(e, data)[1]; kwargs...)
    end
end
plot_experiment(e::Experiment; kwargs...) = plot_experiment(e, e.data; kwargs...)

function renamekw(::Val{:plotly}, kwargs)
    rename = Dict(:ylabel => :yaxis_title,
                  :xtick => :xaxis_tickvals,
                  :xticklabels => :xaxis_ticktext)
    [(x -> haskey(rename, x) ? rename[x] : x)(k) => v for (k, v) in kwargs]
end
renamekw(::Any, kwargs) = kwargs
pushorappend!(x, v::AbstractVector) = append!(x, v)
pushorappend!(x, v) = push!(x, v)
function share_yaxis(::Val{:plotly}, plots)
    ps = hcat(plots...)
    y = ps.plot.data[1][:yaxis]
    for p in ps.plot.data
        PlotlyJS.restyle!(p, yaxis = y,
                          legendgroup = p[:name],
                          showlegend = p[:xaxis] == "x1")
    end
    PlotlyJS.plot(ps.plot)
end
function share_yaxis(::Val{:pgf}, plots)
    ymax = maximum([p.options.dict["ymax"] for p in plots])
    ymin = minimum([p.options.dict["ymin"] for p in plots])
    for p in plots
        p.options.dict["ymax"] = ymax
        p.options.dict["ymin"] = ymin
    end
    plots
end
arrange(::Any, ps) = ps
function arrange(::Val{:pgf}, ps)
    m = length(ps[1])
    d = PGFPlotsX.TikzDocument(raw"\begin{longtable}{" * "c"^m * "}")
    for row in ps
        for fig in row
            push!(d, PGFPlotsX.TikzPicture(fig))
            push!(d, " & ")
        end
        pop!(d.elements); push!(d, "\\\\")
        push!(d, "\\multicolumn{$m}{c}{\\ref{$(row[1].options.dict["legend_to_name"])}}\\\\")
    end
    push!(d, raw"\end{longtable}")
end
function plot_compare(e::Symbol, data1, data2 = nothing;
                      backend = :plotly, title1 = "Data",
                      title2 = data2 === nothing ? "Model" : "Model mean",
                      title3 = "Model closest",
                      color = [:red, :blue, :green, :orange, :cyan, :brown])
    if e == :Clayton0103
        dfs1 = asdataframe(e, data1)
        dfs2 = data2 === nothing ? fill(nothing, length(dfs1)) : asdataframe(e, data2)
        return [plot_compare(ei, di1[1], di2[1]; backend, title1, title2, title3, color)
                for (ei, di1, di2) in zip(CLAYTON0103_EXPERIMENTS, dfs1, dfs2)]
    end
    plots = []
    if data2 !== nothing
        pushorappend!(plots, plot_experiment(e, data2; title = title3, backend))
    end
    pushorappend!(plots, plot_experiment(e; title = title1, backend))
    pushorappend!(plots, plot_experiment(e, data1; title = title2, backend))
    m = 2 + (data2 !== nothing)
    n = length(plots) ÷ m
    ps = [share_yaxis(Val(backend), plots[collect(i:n:end)]) for i in 1:n]
    arrange(Val(backend), ps)
end

plot_experiment(::Experiment{:deKort07_exp1}, data; kwargs...) =
    plotdf(data, :trial; kwargs...)

plot_experiment(::Experiment{:deKort07_exp2}, data; kwargs...) =
    plotdf(data, :trial; kwargs...)

plot_experiment(::Experiment{:deKort07_exp3}, data; kwargs...) =
    plotdf(data, :trial; kwargs...)

plot_experiment(::Experiment{:deKort07_exp4}, data; kwargs...) =
    plotdf(data, :tray; kwargs...)

plot_experiment(::Experiment{:Raby07_planningforbreakfast}, data; kwargs...) =
    plotdf(data, :tray; kwargs...)

plot_experiment(::Experiment{:Raby07_breakfastchoice}, data; kwargs...) =
    plotdf(data, :tray; kwargs...)

function plot_experiment(::Experiment{:deKort05}, data; kwargs...)
    [plotdfgroupby(@select(data, :group, :set, :RI, :μ, :sem), :group, :set;
                   kwargs...);
     plotdf(@select(@where(data, :set .== 8), :group, :RI, :set, :firstinspection),
            :RI, :firstinspection;
            ymin = 0, ymax = 1, xtick = [4, 28],
            ytick = [0, 1], ylabel = "first inspection waxworm", kwargs...)]
end

plot_experiment(::Experiment{:Correia07_exp1}, data; kwargs...) =
    plotdf(data, :prefed; ylabel = "average items eaten", kwargs...)

plot_experiment(::Experiment{:Correia07_exp2}, data; kwargs...) =
    plotdfgroupby(data, :action, :trial; xtick = 1:3, kwargs...)

function plot_experiment(e::Experiment{:Cheke11_specsat}, data; kwargs...)
    data = hasproperty(data, :μ) ? data : FoodCachingExperiments.summarize(e, data)
    plotdf(data, :prefed; xtick = 0:1, xticklabels = ["peanut", "suet"], kwargs...)
end

function plot_experiment(e::Experiment{:Cheke11_planning}, data; kwargs...)
    data = hasproperty(data, :μ) ? data : FoodCachingExperiments.summarize(e, data)
    plotdf(data, :trial; xtick = 1:3, kwargs...)
end

plot_experiment(::Experiment{:Clayton99A_exp1}, data; kwargs...) =
    plotdfgroupby(data, :action, :tray; kwargs...)

plot_experiment(::Experiment{:Clayton99A_exp2}, data; kwargs...) =
    plotdf(data, :tray; kwargs...)

function plot_experiment(::Experiment{:Clayton99B_exp1}, data; kwargs...)
    [plotdfgroupby(@select(data, :group, :trial, :RI, :action, :foodtype, :μ, :sem),
                   [:group, :RI], :trial; kwargs...);
     plotdf(@select(@where(data, :trial .== 5,
                           :action .== "inspect", :foodtype .== "peanut"),
                    :group, :RI, :firstinspection),
            :RI, :firstinspection; ylabel = "first inspection peanut", ymin = 0, ymax = 1,
            kwargs...)]
end

function plot_experiment(::Experiment{:Clayton99B_exp2}, data; kwargs...)
    [plotdfgroupby(@select(@where(data, :action .== "inspect"),
                           :group, :trial, :order, :foodtype, :μ, :sem),
                   [:trial, :group], :order; kwargs...);
     plotdfgroupby(@select(@where(data, :action .== "inspect", :foodtype .== "peanut"),
                           :group, :trial, :order, :firstinspection),
                   [:trial], :order, :firstinspection;
                   ylabel = "first inspection peanuts", ymin = 0, ymax = 1, kwargs...)]
end

plot_experiment(::Experiment{:Clayton99C_exp1}, data; kwargs...) =
    plotdfgroupby(data, :foodtype, :group; kwargs...)

plot_experiment(::Experiment{:Clayton99C_exp2}, data; kwargs...) =
    plotdfgroupby(data, :action, :group; kwargs...)

plot_experiment(::Experiment{:Clayton99C_exp3}, data; kwargs...) =
    plotdfgroupby(data, :action, :group; xticklabel_style = @pgf({rotate = 45}),
                  kwargs...)

function plot_experiment(::Experiment{:Clayton01_exp1}, data; kwargs...)
    [plotdfgroupby(@select(@where(data, :set .== 5, :action .== "inspect"),
                           :foodtype, :group, :RI, :trial, :μ, :sem),
                     :RI, :foodtype; kwargs...);
     plotdfgroupby(@select(@where(data, :foodtype .== "peanut",
                                  :action .== "inspect", 0 .< :set .<= 6),
                           :group, :RI, :trial, :set, :firstinspection),
                   :RI, :set, :firstinspection; ylabel = "first inspection peanut",
                   ymin = 0, ymax = 1, kwargs...);
     plotdf(@select(@where(data, :foodtype .== "other", :action .== "inspect",
                           :set .> 4, :trial .== "pc"),
                    :group, :RI, :firstinspection),
            :RI, :firstinspection; ylabel = "first inspection cricket",
            ymin = 0, ymax = 1, kwargs...);
    ]
end

function plot_experiment(::Experiment{:Clayton01_exp2}, data; kwargs...)
    [plotdfgroupby(@select(@where(data, :set .== 6, :action .== "inspect"),
                           :foodtype, :group, :RI, :trial, :μ, :sem),
                   :RI, :foodtype; kwargs...);
     plotdf(@select(@where(data, :foodtype .== "mealworm",
                           :action .== "inspect", :set .== 6),
                    :group, :RI, :trial, :set, :firstinspection),
            :RI, :firstinspection;
            ylabel = "first inspection mealworm", ymin = 0, ymax = 1, kwargs...)]
end

function plot_experiment(::Experiment{:Clayton01_exp3}, data; kwargs...)
    ps = Vector{Any}(undef, 3)
    ps[1] = plotdf(@select(@where(data, :action .== "inspect"),
                           :foodtype, :group, :trial, :μ, :sem),
                     :foodtype; ylabel = "inspect", kwargs...)
    ps[2] = plotdf(@select(@where(data, :action .== "cache"),
                    :foodtype, :group, :μ, :sem),
            :foodtype; ylabel = "cache", kwargs...)
    ps[3] = plotdf(@select(@where(data, :foodtype .== "mealworm",
                                  :action .== "inspect"),
                           :group, :trial, :firstinspection),
                   :trial, :firstinspection; ylabel = "first inspection mealworm",
                   ymin = 0, ymax = 1, kwargs...)
    ps
end

function plot_experiment(::Experiment{:Clayton01_exp4}, data; kwargs...)
    ps = Vector{Any}(undef, 2)
    ps[1] = plotdf(@select(@where(data, :action .== "inspect"),
                           :foodtype, :group, :RI, :μ, :sem),
                     :foodtype; kwargs...);
    ps[2] = plotdf(@select(@where(data, :foodtype .== "peanut",
                                  :action .== "inspect"),
                           :group, :RI, :firstinspection),
                   :RI, :firstinspection; ylabel = "first inspection peanut",
                   ymin = 0, ymax = 1, kwargs...)
    ps
end

function plot_experiment(::Experiment{:Clayton03_exp1}, data; kwargs...)
    ps = Vector{Any}(undef, 2)
    ps[1] = plotdf(@select(@where(data, :foodtype .== "other", :action .== "inspect",
                           :set .> 4, :trial .== "pc"),
                    :group, :RI, :firstinspection),
            :RI, :firstinspection; ylabel = "first inspection cricket",
            ymin = 0, ymax = 1, kwargs...)
    ps[2] = plotdf(@select(@where(dropmissing(data, :foodtype, disallowmissing = true),
                           :foodtype .== "cricketproportion"),
                    :group, :RI, :μ, :sem),
            :RI; ylabel = "first inspection cricket", ymin = 0, ymax = 1, kwargs...)
    ps
end

function plot_experiment(exp::Experiment{:Clayton03_exp2}, data; kwargs...)
    [plotdfgroupby(@select(@where(data, :action .== "inspect"),
                          :group, :foodtype, :condition, :RI, :μ, :sem),
                  [:group, :RI], :foodtype; kwargs...);
     plotdfgroupby(@select(@where(data, :action .== "inspect", :foodtype .== "peanut"),
                           :group, :condition, :RI, :firstinspection),
                   [:group], :RI, :firstinspection; ylabel = "first inspection crickets",
                   ymin = 0, ymax = 1, kwargs...);
     plotdf(@select(@where(data, :action .== "cache"),
                    :group, :foodtype, :μ, :sem),
            :foodtype; kwargs...)]
end

function plot_experiment(e::Experiment{:Clayton0103}; kwargs...)
    plot_experiment(e, [FoodCachingExperiments.EXPERIMENTS[x].data
                        for x in FoodCachingExperiments.CLAYTON0103_EXPERIMENTS];
                    kwargs...)
end
function plot_experiment(::Experiment{:Clayton0103}, data; kwargs...)
    vcat([plot_experiment(e, d; kwargs...)
          for (e, d) in zip(FoodCachingExperiments.CLAYTON0103_EXPERIMENTS, data)]...)
end

plot_experiment(::Experiment{:Clayton05_exp1}, data; kwargs...) =
    plotdfgroupby(data, :group, :trial; kwargs...)

plot_experiment(::Experiment{:Clayton05_exp2}, data; kwargs...) =
    plotdfgroupby(data, :group, :trial; kwargs...)

plot_experiment(exp::Experiment{:Clayton05_exp3}, data; kwargs...) =
    plotdfgroupby(@where(data, :n .< 80), :group, :trial; kwargs...)

plot_experiment(exp::Experiment{:Clayton05_exp4}, data; kwargs...) =
    plotdf(data, :trial; kwargs...)


function trayprocessing(trays, i, f = nothing)
    trays = filter(x -> x.position == i, trays)
    length(trays) > 1 && @warn "multiple trays at position $i"
    isempty(trays) && return 0.
    if f === nothing
        trays[1].closed ? .75 : 1.5
    else
        FoodCachingModels.countfooditems(trays[1], f)
    end
end
function preprocess(field, t, v, foodtypes)
    if field == :ismdpresent
        [(t, v, "maintenance diet")]
    elseif field ∈ (:actions_eat, :actions_cache, :actions_inspect, :actions_other)
        [(t, first.(v), replace(string(field), "actions_" => ""))]
    elseif field ∈ (:eatableitems, :cacheableitems)
        [(t, FoodCachingModels.countfooditems.(v, f), "$field $f")
         for f in foodtypes]
    elseif field ∈ (:hunger, :stomach, :cachemotivation)
        [(t, getindex.(v, i), "$field $(foodtypes[i])")
         for i in eachindex(foodtypes)]
    elseif field == :trays
        trays = union((x -> x.position).(vcat(v...)))
        [[(t, trayprocessing.(v, i), "tray $i presence")
          for i in trays]...;
         [(t, trayprocessing.(v, i, f), "tray $i $f")
          for i in trays, f in foodtypes]...]
    elseif field == :trayweights
        ks = union(keys.(union(v))...)
        [(t, (x -> haskey(x, k) ? x[k] : missing).(v), "$(k[1]) $(k[2])")
         for k in ks]
    end
end
function traces(model; timeunit = 1.0u"hr")
    data = model.tracker.data
    foodtypes = union((x -> x.id).([vcat(data[:eatableitems].v...); vcat(data[:cacheableitems].v...)]))
    plotsdict = Dict()
    for (field, x) in data
        plotsdict[field] = preprocess(field,
                                      uconvert.(NoUnits, x.t ./ timeunit),
                                      x.v,
                                      foodtypes)
    end
    actions = intersect(keys(plotsdict),
                        [:actions_eat, :actions_cache,
                         :actions_inspect, :actions_other])
    plotsdict[:actions] = vcat([plotsdict[a] for a in actions]...)
    for a in actions
        pop!(plotsdict, a)
    end
    plotsdict
end
function titles(k)
    k == :trays && return "cached items in trays"
    k == :ismdpresent && return "maintentance diet availability"
    k == :cacheableitems && return "cacheable items"
    k == :eatableitems && return "eatable items"
    k == :trayweights && return "tray weights"
    string(k)
end
function plotexperiment(model; order = nothing)
    plotsdict = traces(model)
    if order !== nothing
        plotsdict = sort(plotsdict, by = x -> findfirst(a -> x == a, order))
    end
    data = PlotlyJS.GenericTrace{Dict{Symbol,Any}}[]
    layout = Dict{Symbol, Any}(:xaxis_domain => [0., 1.])
    layout[:annotations] = Dict{Symbol, Any}[]
    n = length(plotsdict)
    h0 = .04
    h = (1 - (n-1)*h0)/n
    i = n + 1
    for (k, v) in plotsdict
        i -= 1
        for (t, d, li) in v
            push!(data, PlotlyJS.scatter(x = t, y = d,
                                         mode = k == :actions ? "markers" : "lines",
                                         name = li, xaxis = "x",
                                         yaxis = "y$i"))
        end
        layout[Symbol(:yaxis, i, :_domain)] = [(i - 1)*h, i*h] .+ (i - 1)*h0
        push!(layout[:annotations],
              Dict(:yanchor => "bottom",
                   :xanchor => "center",
                   :y => (i-.5)*h + (i - 1)*h0,
                   :bgcolor => "#ffffff",
                   :font => Dict(),
                   :showarrow => false,
                   :yref => "paper",
                   :text => titles(k),
                   :xref => "paper",
                   :x => 0.8))
    end
    l = PlotlyJS.Layout(; xaxis = PlotlyJS.attr(title="time [hr]"), layout...)
    PlotlyJS.plot(data, l)
end

function prepare_boxplot(x; options = @pgf({}))
    opt = @pgf {mark_size = ".5pt",
                "every mark/.append style = {fill = white, line width = .2pt}"}
    opt = merge(opt, options)
    uq = quantile(x, .75)
    lq = quantile(x, .25)
    IQR = uq - lq
    lw = minimum(filter(x -> x > lq - 1.5 * IQR, x))
    uw = maximum(filter(x -> x < uq + 1.5 * IQR, x))
    bopt = @pgf {median = median(x), lower_quartile = lq,
                 upper_quartile = uq, lower_whisker = lw,
                 upper_whisker = uw}
    opt, bopt, filter(x -> x > uq + 1.5 * IQR || x < lq - 1.5 * IQR, x)
end
function boxplot(data; labels, axisopt = @pgf({}), options = @pgf({}))
    plots = []
    for (i, group) in enumerate(data)
        N = length(group)
        box_extend = .6/N
        offsets = N == 1 ? [0.] : range(-.35 + box_extend/2,
                                        stop = .35 - box_extend/2,
                                        length = N)
        for (k, x) in enumerate(group)
            opt, bopt, outliers = prepare_boxplot(x; options = options[k])
            bopt = merge(@pgf({box_extend = box_extend, draw_position =
                               offsets[k] + i}), bopt)
            opt = merge(@pgf({boxplot_prepared = bopt, thick}), opt)
            p = @pgf Plot(opt, Coordinates(fill(offsets[k] + i,
                                                          length(outliers)),
                                                         outliers))
            push!(plots, p)
        end
    end
    Axis(merge(@pgf({"boxplot/draw direction = y",
                     xtick = 1:length(labels),
                     ymode = "log",
                     ylabel = raw"$\Delta$",
                     xticklabels = labels,
                     xticklabel_style = {rotate = 70, anchor = "east"}}),
                axisopt), plots...)
end

end
