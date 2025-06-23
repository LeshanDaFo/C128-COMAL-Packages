#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#  make_all.py – Startprogramm zum erstellen von COMAL-kompatiblen .seq-Dateien im MOS-Technik-Format
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

TEXTS_EN = {
    "usage_makeall": "Usage: {} [Directory] [--overwrite]",
    "not_a_directory": "Error: '{}' is not a valid directory.",
    "no_asm_found": "No .asm files found in directory '{}'.",
    "processing_file": "Processing: {}",
    "hex2mos_error": "Error while processing '{}': exited with status {}",
}

try:
    import check_lang
    language = check_lang.get_system_language()
except Exception:
    language = "en"

try:
    import locales
    TEXTS = locales.get_texts(language)
except Exception as e:
    print(f"[INFO] could not load locales.py ({e}), using English as fallback.")
    TEXTS = TEXTS_EN

def t(key, *args):
    text = TEXTS.get(key, TEXTS_EN.get(key, key))
    return text.format(*args)

def main():
    print(f"{'Detected language' if not language.startswith('de') else 'Erkannte Sprache'}: {language}")

    if len(sys.argv) < 2:
        print(t("usage_makeall", sys.argv[0]))
        sys.exit(1)

    directory = sys.argv[1]
    option = sys.argv[2] if len(sys.argv) > 2 else None

    if not os.path.isdir(directory):
        print(t("not_a_directory", directory))
        sys.exit(1)

    asm_files = [f for f in os.listdir(directory) if f.endswith('.asm')]
    if not asm_files:
        print(t("no_asm_found", directory))
        sys.exit(0)

    for file in asm_files:
        filepath = os.path.join(directory, file)
        if os.path.isfile(filepath):
            print(t("processing_file", filepath))
            cmd = [sys.executable, "make_package.py", filepath]
            if option == "--overwrite":
                cmd.append(option)
            try:
                subprocess.run(cmd, check=True)
            except subprocess.CalledProcessError as e:
                print(t("hex2mos_error", filepath, e.returncode))

if __name__ == "__main__":
    main()