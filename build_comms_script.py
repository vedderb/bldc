import os
import json

compile_entries = []

with open("build_commands.txt", "r") as f:
    for line in f:
        line = line.strip()
        if not line:
            continue

        # Extract source file (assumes it ends with .c or .cpp)
        parts = line.split()
        src_file = next((p for p in parts if p.endswith(".c") or p.endswith(".cpp")), None)
        if not src_file:
            continue

        entry = {
            "directory": os.getcwd(),
            "command": line,
            "file": os.path.abspath(src_file)
        }
        compile_entries.append(entry)

with open("compile_commands.json", "w") as out:
    json.dump(compile_entries, out, indent=2)

print(f"Wrote {len(compile_entries)} entries to compile_commands.json")
