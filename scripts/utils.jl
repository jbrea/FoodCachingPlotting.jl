datapath = joinpath(@__DIR__, "..", "data")
figpath = joinpath(@__DIR__, "..", "figures")

experiment_order = [:Raby07_breakfastchoice, :Raby07_planningforbreakfast,
                    :Cheke11_planning, :Correia07_exp2, :deKort07_exp2,
                    :deKort07_exp3, :deKort07_exp4, :Clayton05_exp1,
                    :Clayton05_exp2, :Clayton05_exp3, :Clayton05_exp4,
                    :Clayton99A_exp1, :Clayton99A_exp2, :Clayton99B_exp1,
                    :Clayton99B_exp2, :Clayton0103, :Clayton01_exp1,
                    :Clayton01_exp2, :Clayton01_exp3, :Clayton01_exp4,
                    :Clayton03_exp1, :Clayton03_exp2, :deKort05,
                    :Cheke11_specsat, :Correia07_exp1, :Clayton99C_exp1,
                    :Clayton99C_exp2, :Clayton99C_exp3, :Watanabe05,
                    :deKort07_exp1]
model_order = [:ReplayAndPlan, :PlasticCaching,
               :EpisodicLikeMemory, :MotivationalControl, :Baseline]

function loadmodel(model, experiment, id, rev; datapath = datapath)
    load(joinpath(datapath, join([model, experiment, id, rev], "_")))
end

function loadresult(filename)
    open(joinpath(datapath, filename)) do f
        s = ZstdDecompressorStream(f)
        res = BSON.load(s)[:results]
        close(s)
        res
    end
end

function prepare_plot(res; baseline = :self, baselinemodel = :ReplayAndPlan)
    sort!(res, :logp_hat)
    sort!(res, :experiment, by = x -> findfirst(==(x), experiment_order))
    sort!(res, :model, by = x -> findfirst(==(x), model_order))
    best = combine(groupby(res, [:model, :experiment]),
                   names(res) .=> x -> [last(x)], renamecols = false)
    if baseline != :none
        baseline = baseline == :self ? best : baseline
        nmodels = length(union(best.model))
        idxs = baseline.model .== baselinemodel
        best.logp_hat_rel = best.logp_hat .- repeat(baseline.logp_hat[idxs], nmodels)
        best.logp_hat_std_rel = sqrt.(best.logp_hat_std.^2 .+ repeat(baseline.logp_hat_std[idxs], nmodels).^2)
        best.logp_hat_std_rel[idxs] .= 0
        groups = groupby(best[(baseline == best ? best.model .!= baselinemodel : :), :], :model)
        y = [g.logp_hat_rel for g in groups]
        err = [g.logp_hat_std_rel ./ sqrt.(g.rep) for g in groups]
    else
        groups = groupby(best, :model)
        y = [g.logp_hat for g in groups]
        err = [g.logp_hat_std ./ sqrt.(g.rep) for g in groups]
    end
    (best = best, groups = groups, y = y, err = err)
end

function toname(e)
    e == :Raby07_planningforbreakfast && return "Raby07 planning"
    e == :Raby07_breakfastchoice && return "Raby07 choice"
    e == :Clayton0103 && return "Clayton01\\&Clayton03"
    replace(string(e), "_" => " ")
end

function labelaccessor(e, tests)
    vcat([FoodCachingExperiments.EXPERIMENTS[e].tests[test.id].labels .== "A"
          for test in tests.tests]...)
end
samebinarysignificance(x::Number, y::Number) = (x < 4 && y < 4) || (x > 3 && y > 3)
function samebinarysignificance(e, data)
    tests = FoodCachingExperiments.statistical_tests(e, data)
    l = labelaccessor(e, tests)
    tt = FoodCachingExperiments.asdataframe(e, target(e))[2]
    t = FoodCachingExperiments.significancecode.(tests.pvalues)
    s = samebinarysignificance.(t, tt)
    (all(s[l]), mean(s[l]), mean(s), s, l, tests.pvalues)
end

function runbestparams(e, model, bestseed; reseed = true)
    Random.seed!(bestseed)
    ms = rand(model, nbirds(e))
    reseed && Random.seed!(time_ns())
    run!(e, ms)
end

function concat_experiments(e)
    if hasproperty(e[1], :id)
        for i in eachindex(e)
            e[i].id .+= 100 * i
        end
    end
    vcat(e...)
end
function run_sigtests(data; loadmodel = loadmodel, Ninner = 1, Nouter = 10^3)
    sigresults = DataFrame(model = [], experiment = [],
                           samebinarysignificance = [],
                           meansamebinarysigA = [],
                           meansamebinarysig = [],
                           s = [], l = [], pvalues = [])
    for row in eachrow(data)
        model, e = row.model, row.experiment
        id = row.id
        rev = hasproperty(row, :rev) ? row.rev : "indi"
        @show model, e
        m = loadmodel(model, e, id, rev)
        for k in 1:Nouter
    #         data = runbestparams(e, m, s, reseed = k != 1)
            d = [runbestparams(e, m, rand(UInt)) for _ in 1:Ninner]
            if e == :Clayton0103
                data = [concat_experiments([d[i][j] for i in 1:Ninner]) for j in 1:6]
                for (i, ei) in enumerate(FoodCachingExperiments.CLAYTON0103_EXPERIMENTS)
                    push!(sigresults, [model, ei, samebinarysignificance(ei, data[i])...])
                end
            else
                data = concat_experiments(d)
                s = samebinarysignificance(e, data)
                push!(sigresults, [model, e, s...])
            end
        end
    end
    sigresults.ratio_same_rejects = ratio_same_rejects.(eachrow(sigresults))
    sigresults.atleasttwo = [mean(s[l]) > .5 for (s, l) in zip(sigresults.s, sigresults.l)]
    sigresults
end

function rm_test_locals(e)
    for (k, c) in e.tests
        e.tests[k] = FoodCachingExperiments.TestSummary(c.pvalues,
                                 c.keys,
                                 c.df,
                                 c.labels,
                                 c.statistic,
                                 c.value,
                                 Base.RefValue{Any}(),
                                 Base.RefValue{Any}())
    end
    e
end
function BY(α, p; α₁ = (p, m, α) -> .05, arbitrary_dependence = false)
    p = sort(p)
    m = length(p)
    c = arbitrary_dependence ? 1 : sum(1/i for i in 1:m)
    for k in 1:m
        if p[k] > k/(m * c) * α
            if k == 1
                return α₁(p[1], m, α)
            else
                return p[k-1]
            end
        end
    end
    p[end]
end
function rejects(e; α = .05)
    α != .05 && error("Not implemented for $α ≂̸ 0.05.")
    FoodCachingExperiments.asdataframe(e, target(e))[2] .< 4
end
same_rejects(e, pvalues; α = .05) = pvalues[rejects(e; α)[1:length(pvalues)]] .<= α
function ratio_same_rejects(row; corrected = false)
    e = EXPERIMENTS[row.experiment]
    sr = same_rejects(e, row.pvalues,
                      α = corrected ? BY(.2, pvalues(e)) : .05)
    length(sr) > 0 ? mean(sr) : 1.
end
