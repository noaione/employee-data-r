import os
from pathlib import Path
from typing import List

ci_file = Path(__file__)
base_dirs = [
    ci_file.parent,
    ci_file.parent.parent,
    ci_file.parent.parent.parent,
]

base_dir: Path = None
for base in base_dirs:
    if os.path.isfile(base / "main.r"):
        base_dir = base
        break

main_r = base_dir / "main.r"
main_ci_r = base_dir / "main_ci.r"

print(f"📜 Working directory: {base_dir}")

print("📍 Reading main.r")
with open(main_r, "r") as f:
    lines = f.readlines()

print("💉 Injecting function...")
new_format: List[str] = []
for line in lines:
    if line.strip() == "":
        new_format.append(line)
    else:
        new_format.append((" " * 4) + line)
new_format.insert(0, "main <- function() {\n")
new_format.append("}\n")
new_format.append("\nmain()\n")

print("🤖 Writing back file...")
with open(main_ci_r, "w") as fp:
    fp.writelines(new_format)
print("✨ Done!")
