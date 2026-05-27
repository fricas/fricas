# Test Symbolics.jl
# Test it using ')read examples/Symbolics.jl' or 'jlInclude("examples/Symbolics.jl")'
# Official example from the documentation of Symbolics.jl, see:
# https://docs.sciml.ai/Symbolics/stable/tutorials/converting_to_C/

using Pkg
Pkg.add("Symbolics")
using Symbolics

function lotka_volterra!(du, u, p, t)
  x, y = u
  α, β, δ, γ = p
  du[1] = dx = α*x - β*x*y
  du[2] = dy = -δ*y + γ*x*y
end

@variables t du[1:2] u[1:2] p[1:4]
du = collect(du)
lotka_volterra!(du, u, p, t)
println(du)

println(build_function(du, u, p, t, target=Symbolics.CTarget()))

f = build_function(du, u, p, t, target=Symbolics.CTarget(), expression=Val{false})
println(f)

du = rand(2); du2 = rand(2)
u = rand(2)
p = rand(4)
t = rand()
f(du, u, p, t)
lotka_volterra!(du2, u, p, t)
println(du == du2) # true
println(du)
println(du2)