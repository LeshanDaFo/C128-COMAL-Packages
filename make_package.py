#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#  make_package.py – Hauptprogramm zum erstellen von COMAL-kompatiblen .seq-Dateien im MOS-Technik-Format
#
#  Copyright (c) 2025 Claus Schlereth
#
#  Dieses Projekt steht unter der MIT-Lizenz.
#  Weitere Details siehe LICENSE-Datei im Repository oder:
#  https://opensource.org/licenses/MIT
#
#  Weitere Informationen zum MOS-Format:
#  https://srecord.sourceforge.net/man/man5/srec_mos_tech.5.html

import sys
import os
import subprocess
import platform

TEXTS_EN = {
    "no_filename": "Error: No filename given.\nUsage: {} <filename> [--overwrite]",
    "source_missing": "Error: Source file '{}' does not exist!",
    "building": "Building '{}' → '{}'",
    "build_success": "Done: {} has been created.",
    "build_error": "Error during assembly!",
    "running_hex2mos": "Running Python script hex2mos.py with '{}'...",
    "hex2mos_done": "Python script hex2mos executed successfully.",
    "hex2mos_failed": "Error while running hex2mos.py!",
    "unsupported_os": "Unsupported OS: '{}'"
}

try:
    import check_lang
    language = check_lang.get_system_language()
except Exception:
    language = "en"

# load localization if available
try:
    import locales
    TEXTS = locales.get_texts(language)
except Exception as e:
    print(f"[INFO] could not load locales.py ({e}), using English as default language.")
    TEXTS = TEXTS_EN

# fallback function for safe access to texts
def t(key, *args):
    text = TEXTS.get(key, TEXTS_EN.get(key, key))
    return text.format(*args)

def main():
    if len(sys.argv) < 2:
        print(t("no_filename", sys.argv[0]))
        sys.exit(1)

    input_file = sys.argv[1]
    option = sys.argv[2] if len(sys.argv) > 2 else None

    root_dir = os.getcwd()
    build_dir = os.path.join(root_dir, "build")
    bin_dir = os.path.join(build_dir, "bin")
    seq_dir = os.path.join(build_dir, "seq")

    # Plattformabhängige Auswahl der ACME-Binärdatei
    system = platform.system()
    if system == "Windows":
        acme_subdir = "win"
        acme_executable = "acme.exe"
    elif system == "Darwin":
        acme_subdir = "mac"
        acme_executable = "acme"
    elif system == "Linux":
        acme_subdir = "linux"
        acme_executable = "acme"
    else:
        print(t("unsupported_os", system))
        sys.exit(1)

    acme_path = os.path.join(root_dir, "bin", acme_subdir, acme_executable)

    source_path = os.path.join(root_dir, input_file)

    if not os.path.isfile(source_path):
        print(t("source_missing", source_path))
        sys.exit(1)

    base_name = os.path.splitext(os.path.basename(input_file))[0].replace("_", ".")
    output_bin = os.path.join(bin_dir, f"{base_name}.bin")
    output_seq = os.path.join(seq_dir, f"{base_name}.seq")

    os.makedirs(bin_dir, exist_ok=True)
    os.makedirs(seq_dir, exist_ok=True)

    print(t("building", source_path, output_bin))

    try:
        subprocess.run([acme_path, "-f", "cbm", "-o", output_bin, source_path], check=True)
        print(t("build_success", output_bin))
    except subprocess.CalledProcessError:
        print(t("build_error"))
        sys.exit(1)

    print(t("running_hex2mos", output_bin))

    cmd = ["python3", "hex2mos.py", output_bin, output_seq]
    if option == "--overwrite":
        cmd.append(option)

    try:
        subprocess.run(cmd, check=True)
        print(t("hex2mos_done"))
    except subprocess.CalledProcessError:
        print(t("hex2mos_failed"))
        sys.exit(1)

if __name__ == "__main__":
    main()