#!/usr/bin/env python3
"""
generate_docs.py  –  Build Markdown documentation for all jlFriCAS constructors.

Strategy
--------
For each constructor we run FriCAS in batch mode:
  1.  constructorDocumentation('Name)$SpadDoc  → description
  2.  )show Name                               → operation signatures
  3.  For each operation in the )show output, we call
      operationDocumentation('op)$SpadDoc and filter to only the
      blocks whose "From: <Constructor>" matches one of the jlFriCAS
      constructors (JL/NM/WS prefix), avoiding thousands of irrelevant
      FriCAS core entries.
"""

import subprocess, re, os, textwrap

OUT_DIR = "docs/constructors"
os.makedirs(OUT_DIR, exist_ok=True)

GITHUB_BASE = "https://github.com/gvanuxem/jlfricas/blob/master/"

# ── Full constructor lists ────────────────────────────────────────────────────

JL_CATEGORIES = [
    "JLArbitraryPrecision", "JLCommutativeRing", "JLMachineFloat",
    "JLMachineType", "JLMatrixCategory", "JLObjectAggregate",
    "JLObjectRing", "JLObjectType", "JLRing", "JLType", "JLVectorCategory",
]

JL_DOMAINS = [
    "JLComplexF32", "JLComplexF32Matrix", "JLComplexF32SquareMatrix",
    "JLComplexF32Vector", "JLComplexF64", "JLComplexF64Matrix",
    "JLComplexF64SquareMatrix", "JLComplexF64Vector", "JLComplexFloat",
    "JLDataFrame", "JLF32SquareMatrix", "JLF64SquareMatrix",
    "JLFloat", "JLFloat32", "JLFloat32Matrix", "JLFloat32Vector",
    "JLFloat64", "JLFloat64Matrix", "JLFloat64Vector",
    "JLInt64", "JLInt64Vector", "JLMatrix",
    "JLObjAnonymousFunction", "JLObjBigInt", "JLObjBool",
    "JLObjComplexF32", "JLObjComplexF64", "JLObjDict",
    "JLObjDynamicLinker", "JLObjFloat32", "JLObjFloat64",
    "JLObjFunction", "JLObjInt64", "JLObjNamedTuple",
    "JLObjPair", "JLObjPy", "JLObjR", "JLObjRational",
    "JLObjTuple", "JLObjUInt64", "JLObject", "JLSymbol", "JLVector",
]

JL_PACKAGES = [
    "JLCF32LinearAlgebra", "JLCF64LinearAlgebra",
    "JLComplexF64MatrixTranscendentalFunctions",
    "JLComplexFloatSpecialFunctions", "JLDrawFunctions",
    "JLF32ArrayFunctions", "JLF32LinearAlgebra",
    "JLF64ArrayFunctions", "JLF64LinearAlgebra",
    "JLF64MatrixTranscendentalFunctions",
    "JLFloat32SpecialFunctions", "JLFloat32SpecialFunctions2",
    "JLFloat32VectorFunctions2", "JLFloat64SpecialFunctions",
    "JLFloat64SpecialFunctions2", "JLFloat64VectorFunctions2",
    "JLFloatSpecialFunctions", "JLFloatSpecialFunctions2",
    "JLPlotFunctions", "JLStringUtilities", "JLUtilityFunctions",
    "JLVectorFunctions2",
]

NM_CATEGORIES = [
    "NMCommutativeRing", "NMField", "NMPadicNumberCategory", "NMRing", "NMType",
]

NM_DOMAINS = [
    "NMAcbField", "NMAlgebraicNumber", "NMArbField", "NMComplexBall",
    "NMComplexField", "NMExactCalciumField", "NMExtendedPadicInteger",
    "NMExtendedPadicRational", "NMFactored", "NMFiniteField",
    "NMFraction", "NMInteger", "NMIntegerMod",
    "NMMultivariateLaurentPolynomial", "NMMultivariatePolynomial",
    "NMPadicInteger", "NMPadicRational", "NMPolynomial", "NMPrimeField",
    "NMRealBall", "NMRealField", "NMUnivariateLaurentPolynomial",
    "NMUnivariateLaurentSeries", "NMUnivariatePolynomial",
    "NMUnivariatePowerSeries", "NMUnivariatePuiseuxSeries",
]

WS_CATEGORIES = ["WSAggregate", "WSNumber", "WSObject", "WSRing"]

WS_DOMAINS = [
    "WSAPComplex", "WSAPReal", "WSComplex", "WSExpression",
    "WSGaussianInteger", "WSInteger", "WSList", "WSMatrix",
    "WSRational", "WSReal", "WSString", "WSSymbol", "WSVector",
]

WS_PACKAGES = ["WSNumericalSpecialFunctions", "WSUtilityFunctions"]

ALL_JLFRICAS = set(
    JL_CATEGORIES + JL_DOMAINS + JL_PACKAGES +
    NM_CATEGORIES + NM_DOMAINS +
    WS_CATEGORIES + WS_DOMAINS + WS_PACKAGES
)

# Prefixes that strip type-params for matching  e.g. "NMAcbField(p)" → "NMAcbField"
def bare_name(s):
    return s.split("(")[0].strip()

# ── Source Discovery ──────────────────────────────────────────────────────────

CONSTRUCTOR_SOURCES = {}  # name -> (file_path, line_number)

def discover_sources():
    print("Scanning src/algebra for constructor definitions …")
    algebra_dir = "src/algebra"
    if not os.path.isdir(algebra_dir):
        print(f"  !! Algebra directory not found: {algebra_dir}")
        return

    # Grep for )abbrev lines: )abbrev <type> <abbrev> <Name>
    # We use a regex to capture the file, line, and constructor name
    cmd = ["grep", "-rnE", r"^\)abbrev", algebra_dir]
    try:
        proc = subprocess.run(cmd, capture_output=True, text=True, check=True)
        for line in proc.stdout.splitlines():
            # Format: path/to/file.spad:line:)abbrev category/domain/package ABBREV Name
            parts = line.split(":", 2)
            if len(parts) < 3: continue
            file_path, line_num, content = parts
            m = re.search(r"\)abbrev\s+\w+\s+\w+\s+(\w+)", content)
            if m:
                name = m.group(1)
                CONSTRUCTOR_SOURCES[name] = (file_path, int(line_num))
    except Exception as e:
        print(f"  !! Error during source discovery: {e}")

def find_op_line(file_path, op_name, start_line):
    """
    Heuristic to find the export line of an operation in a SPAD file.
    """
    if not os.path.exists(file_path): return None
    try:
        with open(file_path, "r") as f:
            lines = f.readlines()
        
        # Search for "op_name :" or "op_name:" or "op_name  :"
        # starting from start_line
        pattern = re.compile(r"^\s*" + re.escape(op_name) + r"\s*:")
        for i in range(start_line - 1, len(lines)):
            if pattern.search(lines[i]):
                return i + 1
    except:
        pass
    return None

discover_sources()

# ── Helper: run a FriCAS batch session ───────────────────────────────────────

BANNER_END_RE = re.compile(r"^-+$")

def run_fricas(evals: list[str], timeout: int = 60) -> str:
    args = ["fricas", "-nosman"]
    for e in evals:
        args += ["-eval", e]
    try:
        r = subprocess.run(
            args,
            stdin=subprocess.DEVNULL,
            capture_output=True, text=True, timeout=timeout
        )
        return r.stdout
    except subprocess.TimeoutExpired:
        return "<<TIMEOUT>>"
    except Exception as exc:
        return f"<<ERROR: {exc}>>"


def strip_banner(text: str) -> str:
    lines = text.splitlines()
    dash_count = 0
    start = 0
    for i, ln in enumerate(lines):
        if BANNER_END_RE.match(ln.strip()) and len(ln.strip()) > 10:
            dash_count += 1
            if dash_count == 2:
                start = i + 1
                break
    return "\n".join(lines[start:])


def clean(text: str, unwrap: bool = False) -> str:
    """
    Clean up FriCAS documentation text:
    - Strip leading/trailing blank lines
    - Strip trailing whitespace from lines
    - If unwrap=True, join lines that were wrapped by FriCAS
    - Format examples (Example:) into code blocks
    """
    if not text:
        return ""
    lines = [l.rstrip() for l in text.splitlines()]
    while lines and not (lines[0].strip()):
        lines.pop(0)
    while lines and not (lines[-1].strip()):
        lines.pop()
    
    if not lines:
        return ""

    if not unwrap:
        return "\n".join(lines)

    # Unwrapping: join consecutive non-blank lines.
    processed_lines = []
    current_para = []
    for i, ln in enumerate(lines):
        # FriCAS doc lines usually start with 2 spaces
        content = ln[2:] if ln.startswith("  ") else ln.lstrip()
        if not content.strip():
            if current_para:
                processed_lines.append(" ".join(current_para))
                current_para = []
            processed_lines.append("")
        else:
            if current_para and i > 0:
                prev_ln = lines[i-1]
                # Improved heuristic: if prev line was long and didn't end in space,
                # and current line doesn't start with space (beyond indentation), join without space.
                if len(prev_ln) >= 77 and not prev_ln.endswith(" ") and not ln.startswith("   "):
                    current_para[-1] += content.strip()
                else:
                    current_para.append(content.strip())
            else:
                current_para.append(content.strip())
    if current_para:
        processed_lines.append(" ".join(current_para))
    
    final_text = "\n".join(processed_lines)
    
    # Normalize "Example:" (in case it was wrapped and joined)
    final_text = re.sub(r"Ex\s*ample:", "Example:", final_text)
    
    # Format examples as markers and code blocks
    # We split by "Example:" and handle each block
    parts = re.split(r"(Example:)", final_text)
    new_parts = []
    i = 0
    while i < len(parts):
        if parts[i] == "Example:":
            if i + 1 < len(parts):
                # The next part is the code until the next "Example:" or a double newline
                code_parts = re.split(r"(\n\n)", parts[i+1])
                code = code_parts[0].strip()
                new_parts.append(f"\n\n**Example**:\n```fricas\n{code}\n```\n")
                if len(code_parts) > 1:
                    new_parts.append("".join(code_parts[1:]))
                i += 2
            else:
                new_parts.append(parts[i])
                i += 1
        else:
            new_parts.append(parts[i])
            i += 1
            
    final_text = "".join(new_parts)
    # Cleanup extra newlines
    final_text = re.sub(r"\n{3,}", "\n\n", final_text)
    
    return final_text.strip()


# ── Extract operations list from )show output ────────────────────────────────

OP_RE = re.compile(r"\b([a-zA-Z?][a-zA-Z0-9_?!]*)\s*:")

def extract_operations(show_output: str) -> list[str]:
    ops = set()
    in_ops = False
    for line in show_output.splitlines():
        if "Operations" in line and "---" in line:
            in_ops = True
            continue
        if in_ops:
            for m in OP_RE.finditer(line):
                name = m.group(1)
                if name not in {"%", "Type"}:
                    ops.add(name)
    return sorted(ops)


# ── Filter operation doc blocks to only jlFriCAS constructors ────────────────

FROM_RE = re.compile(r"From:\s*(\S+)")

def filter_op_blocks(raw: str) -> list[dict]:
    """
    Split raw operationDocumentation output by blank separator lines
    and return only the blocks that come from a jlFriCAS constructor.
    Each block becomes {"signature": str, "description": str, "from": str}.
    """
    # Split on double-newlines (blank lines between entries)
    raw = re.sub(r"\s*Type:\s*Void\s*", "", raw)
    # Each entry is separated by a blank line following "From: ..."
    # We split by the pattern "   \n\n" or just accumulate by From: markers
    lines = raw.splitlines()
    results = []
    current = []

    def flush(block_lines):
        block = "\n".join(block_lines).strip()
        if not block:
            return
        m = FROM_RE.search(block)
        if m:
            from_name = bare_name(m.group(1))
            if from_name in ALL_JLFRICAS:
                # Extract signature
                sig_m = re.search(r"Signature:\s*(.+)", block)
                sig = sig_m.group(1).strip() if sig_m else ""
                # Description = everything before "Signature:" line
                desc_lines = []
                for ln in block.splitlines():
                    if ln.strip().startswith("Signature:") or ln.strip().startswith("From:"):
                        break
                    desc_lines.append(ln)
                desc = clean("\n".join(desc_lines), unwrap=True)
                results.append({
                    "from": from_name,
                    "signature": sig,
                    "description": desc,
                })

    for ln in lines:
        current.append(ln)
        if FROM_RE.search(ln):
            flush(current)
            current = []
    if current:
        flush(current)

    return results


# ── Fetch docs for one constructor ───────────────────────────────────────────

def fetch_constructor_doc(name: str) -> dict:
    print(f"  → {name}")

    # Step 1: constructor description + )show signatures
    raw = run_fricas([
        f"constructorDocumentation('{name})$SpadDoc",
        f")show {name}",
        ")quit",
    ])
    body = strip_banner(raw)

    # Split description from )show block
    show_marker_re = re.compile(
        r"(?m)^\s*" + re.escape(name) + r"(?:\([^)]*\))?\s+is a\s+(domain|category|package)"
    )
    m = show_marker_re.search(body)
    if m:
        description_raw = body[: m.start()]
        show_block = body[m.start():]
    else:
        description_raw = body
        show_block = ""

    description = clean(description_raw, unwrap=True)
    description = re.sub(r"\s*Type:\s*Void\s*", "", description).strip()

    # Extract constructor signature (first few lines of )show)
    # until the dashed line or Operations marker
    constructor_sig = ""
    if show_block:
        show_lines = show_block.splitlines()
        sig_lines = []
        op_start = 0
        for i, ln in enumerate(show_lines):
            if "---" in ln or "Operations" in ln:
                op_start = i
                break
            if ln.strip():
                sig_lines.append(ln.strip())
        
        if sig_lines:
            constructor_sig = "  \n".join(sig_lines)
            show_block = "\n".join(show_lines[op_start:])

    # Shorten the long dashed line
    show_block = re.sub(r"-+\s*Operations\s*-+", "--- Operations ---", show_block)

    ops_names = extract_operations(show_block)

    # Step 2: fetch operation docs (one FriCAS session per constructor)
    op_entries: dict[str, list[dict]] = {}

    if ops_names:
        evals = [f"operationDocumentation('{op})$SpadDoc" for op in ops_names]
        evals.append(")quit")
        raw_ops = run_fricas(evals, timeout=180)
        raw_ops = strip_banner(raw_ops)

        # Split the entire output by "Type: Void" boundaries to get per-op sections
        sections = re.split(r"\s*Type:\s*Void\s*", raw_ops)
        for op_name, section in zip(ops_names, sections):
            blocks = filter_op_blocks(section)
            if blocks:
                op_entries[op_name] = blocks

    return {
        "description": description,
        "constructor_sig": constructor_sig,
        "show_block": clean(show_block, unwrap=False),
        "ops": op_entries,
    }


# ── Render one Markdown file ──────────────────────────────────────────────────

def render_md(name: str, kind: str, group: str, info: dict) -> str:
    kind_str = f"**Kind**: {kind}"
    source_info = CONSTRUCTOR_SOURCES.get(name)
    if source_info:
        rel_path, line = source_info
        source_url = f"{GITHUB_BASE}{rel_path}#L{line}"
        kind_str += f" &nbsp;|&nbsp; \\[[Source]({source_url})\\]"

    lines = [
        f"# {name}",
        "",
        f"> {kind_str} &nbsp;|&nbsp; **Group**: {group}",
        "",
    ]

    if info["description"]:
        lines += ["## Description", "", info["description"], ""]

    if info["constructor_sig"]:
        lines += ["**" + info["constructor_sig"].replace("  \n", "**  \n**") + "**", ""]

    if info["show_block"]:
        lines += ["## Signatures", "", "```fricas", info["show_block"], "```", ""]

    ops = info["ops"]
    if ops:
        lines += ["## Operations added", ""]
        for op_name, blocks in sorted(ops.items()):
            # Filter blocks to only those from this constructor
            my_blocks = [b for b in blocks if b["from"] == name]
            if not my_blocks:
                continue
            
            # Title for the operation name with source link
            op_line = None
            if source_info:
                op_line = find_op_line(source_info[0], op_name, source_info[1])
            
            op_title = f"### `{op_name}`"
            if op_line:
                op_url = f"{GITHUB_BASE}{source_info[0]}#L{op_line}"
                op_title += f" &nbsp; \\[[source]({op_url})\\]"
            lines.append(op_title)
            lines.append("")

            # Group signatures by their description to avoid redundant text
            by_desc = {} # description -> list of signatures
            for b in my_blocks:
                desc = b["description"]
                if desc not in by_desc:
                    by_desc[desc] = []
                if b["signature"]:
                    by_desc[desc].append(b["signature"])
            
            for desc, sigs in by_desc.items():
                if desc:
                    lines.append(desc)
                    lines.append("")
                for sig in sigs:
                    lines.append(f"- **Signature**: `{sig}`")
                lines.append("")

        # Remove the last empty line if it exists to avoid a blank line before ---
        if lines and lines[-1] == "":
            lines.pop()

    lines += ["---", "[Back to Index](../index.md)", ""]
    return "\n".join(lines)


# ── Main loop ─────────────────────────────────────────────────────────────────

CONSTRUCTORS = [
    *[(n, "Category", "JL — Native Julia") for n in JL_CATEGORIES],
    *[(n, "Domain",   "JL — Native Julia") for n in JL_DOMAINS],
    *[(n, "Package",  "JL — Native Julia") for n in JL_PACKAGES],
    *[(n, "Category", "NM — Nemo (FLINT)") for n in NM_CATEGORIES],
    *[(n, "Domain",   "NM — Nemo (FLINT)") for n in NM_DOMAINS],
    *[(n, "Category", "WS — Wolfram/MathLink") for n in WS_CATEGORIES],
    *[(n, "Domain",   "WS — Wolfram/MathLink") for n in WS_DOMAINS],
    *[(n, "Package",  "WS — Wolfram/MathLink") for n in WS_PACKAGES],
]

print(f"Generating documentation for {len(CONSTRUCTORS)} constructors …\n")

failed = []
for name, kind, group in CONSTRUCTORS:
    try:
        info = fetch_constructor_doc(name)
        md = render_md(name, kind, group, info)
        path = os.path.join(OUT_DIR, f"{name}.md")
        with open(path, "w") as f:
            f.write(md)
    except Exception as exc:
        print(f"  !! FAILED {name}: {exc}")
        failed.append(name)

print(f"\nDone. {len(CONSTRUCTORS) - len(failed)} files written to {OUT_DIR}/")
if failed:
    print(f"Failed: {failed}")
