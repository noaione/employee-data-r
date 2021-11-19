from pathlib import Path

base_dir = Path(__file__).parent.parent
main_r = base_dir / "main.r"
main_ci_r = base_dir / "main_ci.r"

print("📍 Reading main.r")
with open(main_r, "r") as f:
    lines = f.readlines()

print("💉 Injecting function...")
lines.insert(0, "main <- function(){")
lines.append("}")
lines.append("main()")

print("🤖 Writing back file...")
with open(main_ci_r, "w") as fp:
    fp.writelines(lines)
print("✨ Done!")
