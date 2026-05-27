# jlFriCAS MCP Server Design Specification

## Overview
This document outlines the design for an integrated Model Context Protocol (MCP) server embedded within the `jlFriCAS` environment. It enables LLMs to leverage the symbolic algebra of FriCAS and the numerical performance of Julia through a standardized protocol. The server is fully compatible with both **SBCL** and **Clozure CL (CCL)**, with specific optimizations for multi-threaded operation on both platforms. It is also used by the `jlFriCAS` VS Code extension to provide IDE features ([fricas-vscode](https://github.com/gvanuxem/fricas-vscode))

## 🧱 Architecture
The server follows a three-layer bridge architecture and supports multiple transport modes:

1.  **Protocol Layer (Common Lisp - SBCL / CCL)**:
    *   Hosted within the FriCAS image.
    *   Uses `yason` for JSON-RPC 2.0 handling.
    *   Runs in a background thread to maintain responsiveness (in Socket mode).
    *   Supports two transport modes:
        *   **StdIO Mode**: Uses `fd 3` for protocol output and `fd 0` for input. `fd 1` (stdout) is redirected to `fd 2` (stderr) at the C level (`dup2`) to prevent non-protocol output from leaking into the JSON stream.
        *   **Socket Mode**: Supports both TCP (127.0.0.1) and Unix sockets. Uses LSP-style `Content-Length` framing for robust streaming.
    *   Includes specific concurrency fixes for CCL to share database file streams across threads.
2.  **Engine Layer**:
    *   **FriCAS (SPAD/Lisp)**: Symbolic engine and main entry point.
    *   **Julia (LibJulia)**: High-performance numerical engine integrated via FriCAS.
    *   Routes all evaluations through a unified handler that supports multi-line code splitting and system commands.

## 🛠️ MCP Tools

### 1. `evaluate`
*   **Input**: `expression` (String), `format` (Optional String: `"text"` or `"markdown"`).
*   **Behavior**: Evaluates expressions in the FriCAS environment.
    *   Supports symbolic algebra, calculus, and numerical operations (including Julia calls, but only in StdIO Mode).
    *   Splits multi-line input into individual statements for sequential evaluation.
    *   Echoes the input code and evaluation result to the terminal REPL (`*error-output*`).
    *   Sends `repl/starteval` and `repl/finisheval` notifications for progress tracking.
*   **Output**: Returns the evaluation result as a string, optionally wrapped in a markdown code block if requested.

### 2. `get-documentation`
*   **Input**: 
    *   `name` (String): The name of the constructor or operation.
    *   `type` (String): One of `"constructor"` or `"operation"`.
*   **Behavior**: Retrieves documentation using the internal `SpadDoc` package.
*   **Output**: Formatted and cleaned documentation text.

### 3. `list-constructors`
*   **Input**: `pattern` (String).
*   **Behavior**: Searches for Categories, Domains, and Packages matching the pattern.

## 🖥️ REPL Custom Methods
The server extends the standard MCP with custom methods for IDE integration (specifically VS Code):
*   `repl/runcode`: Directly executes code from the IDE editor.
*   `repl/getDocFromWord` / `repl/getDocAt`: Contextual documentation retrieval for the editor.
*   Notifications: `repl/starteval` and `repl/finisheval` allow the client to show a progress indicator during heavy computations.

## 📊 Plotting & Resource Management (work in progress)
High-resolution plots (SVG/PNG) generated via `jlPlot` are handled through a dedicated pipeline:
1.  **Notification**: The server sends a `display` notification containing the MIME type and raw data (SVG XML or Base64 PNG).
2.  **History**: All plots sent during a session are stored in a history list.
3.  **Resources**: All plots are exposed as MCP Resources via `plot://<index>` URIs.
4.  **Retrieval**: Clients can use `resources/list` to see available plots and `resources/read` to fetch them.

## 🚀 Operational Workflow
1.  **Startup**: 
    *   **Automated**: Launch via shell with the `--mcp` flag (StdIO mode) or `--mcp-port <port>` (Socket mode).
    *   **Manual**: Initiated via integrated system command `)mcp start` or `)lisp |mcp| 'start`.
2.  **Concurrency**: 
    *   Uses thread-safe locks (`*fricas-eval-lock*`, `*fricas-db-lock*`).
    *   Mathematical execution is serialized via a global mutex to protect engine state, while protocol handling and resource listing remain concurrent.
3.  **Stream Redirection**: To ensure protocol integrity, the server aggressively redirects standard output streams (*standard-output*, *trace-output*, etc.) to *error-output* (stderr).

## 📝 Decision Log
| Decision | Rationale |
| :--- | :--- |
| **Lisp-based Bridge** | Provides native JSON-RPC and stable threading missing in SPAD. |
| **fd 3 for StdIO** | Allows using a clean file descriptor for the protocol while redirecting the noisy stdout (fd 1) to stderr (fd 2). |
| **LSP-style Framing** | Adopted for Socket mode to handle large payloads and avoid "message-in-message" parsing errors common with simple line-based JSON. |
| **Global Eval Lock** | Necessary because the underlying FriCAS and Julia engines are largely single-threaded and use global state. |
| **SVG over PNG** | Preferred for plots to ensure infinity scalability and smaller footprint in notification payloads. |

## 💡 Assumptions
*   The environment has `yason` installed and available to the Lisp implementation.
*   The user has Julia configured with the `GR` backend for reliable SVG generation.
*   The `SpadDoc` package is compiled and exposed in FriCAS for documentation tools.
