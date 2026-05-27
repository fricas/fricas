# Test it in jlFriCAS using the )read command: )read Pari.jl
# libpari is required on your system. See your package manager.
# AI helped generated. Will possibly break jlFriCAS (segmentation fault).

module LibPari

# Use the Libdl module to find and open shared libraries
using Libdl

# --- Library and Type Definitions ---

# Global variable to hold the path to the libpari shared library
const libpari = find_library(["libpari", "pari"])

# Check if the library was found
if isempty(libpari)
    error("libpari not found. Make sure it is installed and in your library path.")
end

# Define a type alias for the PARI GEN type.
const GEN = Ptr{Cvoid}
#const PariStackPos = Clong

# --- Safe Wrapper for Off-Stack PARI Objects ---

"""
    PariGen

A safe Julia wrapper for a PARI GEN object. This object is allocated
off-stack (on the heap) and is managed by the Julia garbage collector.
A finalizer is attached to automatically call `gunclone` when the
object is no longer reachable, preventing memory leaks without
interfering with the main PARI stack.
"""
mutable struct PariGen
    ptr::GEN
    function PariGen(ptr::GEN)
        # Create a new PariGen object and attach a finalizer to it.
        obj = new(ptr)
        finalizer(destroy!, obj)
        return obj
    end
end

"""
    destroy!(g::PariGen)

The finalizer function for a PariGen object. It calls the C function
`gunclone` to free the heap-allocated PARI object created by `gclone`.
"""
function destroy!(g::PariGen)
    # Check if the pointer is not null before destroying
    if g.ptr != C_NULL
        gunclone(g.ptr)
        # Set the pointer to null to prevent double-freeing
        g.ptr = C_NULL
    end
    return
end


# --- Initialization and State Management ---

"""
    pari_init(size, maxprime)

Initialize the PARI library.
"""
function pari_init(size::Int, maxprime::Int)
    ccall((:pari_init, libpari), Cvoid, (Csize_t, Cuint), size, maxprime)
end

"""
    pari_close()

Close the PARI library and free all associated memory.
"""
function pari_close()
    ccall((:pari_close, libpari), Cvoid, ())
end


# --- Type Conversions and String Operations ---

"""
    GEN_to_str(g::PariGen) -> String

Converts a wrapped PariGen object to its string representation.
"""
function GEN_to_str(g::PariGen)
    # We must preserve the Julia object `g` to ensure its pointer `g.ptr`
    # is valid during the C call.
    GC.@preserve g begin
        c_str_ptr = ccall((:GENtostr, libpari), Cstring, (GEN,), g.ptr)
        return unsafe_string(c_str_ptr)
    end
end

# --- Core GP Functions ---

# Raw C function wrappers
gp_read_str_raw(s::String) = ccall((:gp_read_str, libpari), GEN, (Cstring,), s)
gclone_raw(g::GEN) = ccall((:gclone, libpari), GEN, (GEN,), g)
gunclone(g::GEN) = ccall((:gunclone, libpari), Cvoid, (GEN,), g)
stoi_raw(i::Int) = ccall((:stoi, libpari), GEN, (Clong,), i)
itos_raw(g::GEN) = ccall((:itos, libpari), Clong, (GEN,), g)
dtor_raw(d::Float64) = ccall((:dbltor, libpari), GEN, (Cdouble,), d)
rtod_raw(g::GEN) = ccall((:rtodbl, libpari), Cdouble, (GEN,), g)
add_raw(g1::GEN, g2::GEN) = ccall((:gadd, libpari), GEN, (GEN, GEN), g1, g2)
sub_raw(g1::GEN, g2::GEN) = ccall((:gsub, libpari), GEN, (GEN, GEN), g1, g2)
mul_raw(g1::GEN, g2::GEN) = ccall((:gmul, libpari), GEN, (GEN, GEN), g1, g2)
divii_raw(g1::GEN, g2::GEN) = ccall((:divii, libpari), GEN, (GEN, GEN), g1, g2)
# avma/set_avma are removed from use to prevent crashes in host environments
# avma_raw() = ccall((:avma, libpari), PariStackPos, ())
# set_avma_raw(pos::PariStackPos) = ccall((:set_avma, libpari), Cvoid, (PariStackPos,), pos)


# --- High-Level Safe Wrappers ---

# Conversion functions
from_int(i::Int) = PariGen(gclone_raw(stoi_raw(i)))
from_float(d::Float64) = PariGen(gclone_raw(dtor_raw(d)))

to_int(g::PariGen) = GC.@preserve g itos_raw(g.ptr)
to_float(g::PariGen) = GC.@preserve g rtod_raw(g.ptr)

# Arithmetic operations
function add(g1::PariGen, g2::PariGen)
    GC.@preserve g1 g2 begin
        result_tmp = add_raw(g1.ptr, g2.ptr)
        return PariGen(gclone_raw(result_tmp))
    end
end

function sub(g1::PariGen, g2::PariGen)
    GC.@preserve g1 g2 begin
        result_tmp = sub_raw(g1.ptr, g2.ptr)
        return PariGen(gclone_raw(result_tmp))
    end
end

function mul(g1::PariGen, g2::PariGen)
    GC.@preserve g1 g2 begin
        result_tmp = mul_raw(g1.ptr, g2.ptr)
        return PariGen(gclone_raw(result_tmp))
    end
end

function divii(g1::PariGen, g2::PariGen) # Integer division
    GC.@preserve g1 g2 begin
        result_tmp = divii_raw(g1.ptr, g2.ptr)
        return PariGen(gclone_raw(result_tmp))
    end
end


"""
    eval_str(command::String) -> Union{PariGen, Nothing}

Safely evaluates a GP command string and returns the result as a
safe, garbage-collected `PariGen` object.
"""
function eval_str(command::String)
    GC.@preserve command begin
        result_gen_tmp = gp_read_str_raw(command)
        if result_gen_tmp == C_NULL
            @warn "PARI/GP error evaluating command: $command"
            return nothing
        end
        return PariGen(gclone_raw(result_gen_tmp))
    end
end


# --- Example Usage ---
function run_examples()
    println("Initializing PARI...")
    pari_init(4 * 1024 * 1024, 500000)

    try
        # --- Example 1: `eval_str` ---
        println("\n--- `eval_str` example (success) ---")
        command_ok = "factor(2024)"
        println("Executing: '$command_ok'")
        pari_result = eval_str(command_ok)
        if pari_result !== nothing
            str_result = GEN_to_str(pari_result)
            println("Result: $str_result")
        else
            println("Failed to evaluate command.")
        end

        # --- Example 2: Conversions and Arithmetic (Integers Only) ---
        println("\n--- Conversion and Arithmetic Examples (Integers Only) ---")
        a_int = 100
        c_int = 7

        pari_a = from_int(a_int)
        pari_c = from_int(c_int)

        println("Created PARI objects:")
        println("  a = $(GEN_to_str(pari_a))")
        println("  c = $(GEN_to_str(pari_c))")

        pari_diff = sub(pari_a, pari_c)
        pari_prod = mul(pari_c, pari_c)
        pari_quot = divii(pari_a, pari_c)

        println("\nArithmetic Results:")
        println("  a - c = $(GEN_to_str(pari_diff))")
        println("  c * c = $(GEN_to_str(pari_prod))")
        println("  a \\ c = $(GEN_to_str(pari_quot))")

        diff_int = to_int(pari_diff)
        prod_int = to_int(pari_prod)
        quot_int = to_int(pari_quot)

        println("\nResults converted back to Julia types:")
        println("  Difference (Int): $diff_int")
        println("  Product (Int): $prod_int")
        println("  Quotient (Int): $quot_int")

        # --- Example 3: Conversions and Arithmetic (Floats Only) ---
        println("\n--- Conversion and Arithmetic Examples (Floats Only) ---")
        f1_float = 123.45
        f2_float = 0.55

        pari_f1 = from_float(f1_float)
        pari_f2 = from_float(f2_float)

        println("Created PARI objects:")
        println("  f1 = $(GEN_to_str(pari_f1))")
        println("  f2 = $(GEN_to_str(pari_f2))")

        pari_f_sum = add(pari_f1, pari_f2)

        println("\nArithmetic Results:")
        println("  f1 + f2 = $(GEN_to_str(pari_f_sum))")

        sum_float = to_float(pari_f_sum)

        println("\nResults converted back to Julia types:")
        println("  Sum (Float64): $sum_float")


        # Force a GC run to demonstrate finalizers (for testing purposes)
        println("\nForcing garbage collection to test finalizers...")
        GC.gc()
        println("GC run complete.")

    finally
        println("\nClosing PARI library...")
        #pari_close()
    end
end

end # module LibPari

# Run the examples
LibPari.run_examples()

