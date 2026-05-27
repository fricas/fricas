import subprocess
import json
import os
import sys
import time

def main():
    #target_root = "/home/greg/Git/jlfricas/target/x86_64-linux-gnu"
    #target_root = "/home/greg/.local"
    target_root = "/usr/local"
    fricas_path = f"{target_root}/bin/jlfricas"
    
    env = os.environ.copy()
    env["FRICAS"] = target_root
    
    print(f"Starting MCP server with: {fricas_path} -nosman --mcp")
    
    # Start the process
    process = subprocess.Popen(
        [fricas_path, "-nosman", "--mcp"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        env=env,
        text=True,
        bufsize=1
    )
    
    print("Waiting for server to start (5s)...")
    time.sleep(5)
    
    # 1. Initialize
    init_request = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        }
    }
    
    print("SENDING: initialize")
    process.stdin.write(json.dumps(init_request) + "\n")
    
    # Read the response
    line = process.stdout.readline()
    print(f"RECEIVED: {line}")
    
    # 2. List tools
    list_request = {
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list",
        "params": {}
    }
    
    print("SENDING: tools/list")
    process.stdin.write(json.dumps(list_request) + "\n")
    
    # Read the response
    line = process.stdout.readline()
    print(f"RECEIVED: {line}")
    
    # 3. Call evaluate (FriCAS)
    eval_request = {
        "jsonrpc": "2.0",
        "id": 3,
        "method": "tools/call",
        "params": {
            "name": "evaluate",
            "arguments": {
                "expression": "integrate(sin(x),x)"
            }
        }
    }
    
    print("SENDING: tools/call (evaluate FriCAS)")
    process.stdin.write(json.dumps(eval_request) + "\n")
    line = process.stdout.readline()
    print(f"RECEIVED: {line}")

    # 4. Call evaluate (Julia)
    eval_request_jl = {
        "jsonrpc": "2.0",
        "id": 4,
        "method": "tools/call",
        "params": {
            "name": "evaluate",
            "arguments": {
                "expression": "sqrt(jf64 2.0)"
            }
        }
    }
    
    print("SENDING: tools/call (evaluate Julia)")
    process.stdin.write(json.dumps(eval_request_jl) + "\n")
    line = process.stdout.readline()
    print(f"RECEIVED: {line}")
    
    # Terminate
    process.terminate()

if __name__ == "__main__":
    main()
