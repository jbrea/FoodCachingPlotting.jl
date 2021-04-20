using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
using Stipple, StippleUI, StipplePlotly, Genie, DataFrames

Base.@kwdef mutable struct Model <: ReactiveModel
    plot::R{Bool} = false
    filter::R{String} = ""
    datadir::R{String} = ""
    files::R{Vector{String}} = []
    list::R{String} = ""
    options::R{Vector{String}} = []
    xaxis::R{String} = "iter"
    yaxis::R{String} = "f"
    axisopt::Vector{String} = ["iter", "feval", "f", "sigma", "aratio", "t"]
    layout::R{PlotLayout} = PlotLayout(plot_bgcolor = "#333",
                                       title_text = "",
                                       width = 1000,
                                       hoverlabel_namelength = 200,
                                       legend_itemwidth = 200)
    plot_data::R{Vector{PlotData}} = []
end

const model = Stipple.init(Model())

fn(f) = filter(x -> match(Regex(f), x) !== nothing, model.files[])

function updatelist(model)
    model.options[] = fn(model.filter[])
    model.list[] = join(model.options[], " ")
    nothing
end

function parsefile(s)
    result = DataFrame(iter = Int[], feval = Int[],
                       f = Float64[], sigma = Float64[],
                       aratio = Float64[], t = Float64[])
    lines = readlines(joinpath(model.datadir[], s))
    reading = false
    for line in lines
        if reading
            line[1] != ' ' && continue
            elems = strip.(split(line))
            length(elems) != 6 && continue
            push!(result, parse.(Float64, elems))
        elseif match(r"fevals", line) !== nothing || match(r"^ *$", line) !== nothing
            reading = true
        end
    end
    result
end

on(model.filter) do _
    updatelist(model)
end


on(model.plot) do plot
    plot || return
    model.plot_data[] = [begin
                             data = parsefile(f)
                             PlotData(x = data[:, model.xaxis[]],
                                      y = data[:, model.yaxis[]],
                                      name = f)
                         end
                         for f in model.options[]]
    model.plot[] = false
end

on(model.datadir) do dir
    if ispath(dir)
        model.files[] = readdir(dir)
        updatelist(model)
    end
end

function ui()
    page(vm(model), class = "container", [
         plot(:plot_data, layout = :layout),
         Genie.Renderer.Html.p(["log path", input("", @bind(:datadir),
                                                  style = "width: 500px"),
                                "filter ",
                                 input("", @bind(:filter),
                                       style = "width: 500px"),
                                 btn("plot", @click("plot = true")),
                                 quasar(:select, "", @bind(:xaxis),
                                        options = :axisopt),
                                 quasar(:select, "", @bind(:yaxis),
                                        options = :axisopt),
                                ]),
         Genie.Renderer.Html.small([span("", @text(:list))]),
         ]) |> html
end

route("/", ui)
up(8001)
