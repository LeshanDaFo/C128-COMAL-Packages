#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#  hex2mos.py – Wandelt ein .bin-File in eine COMAL-kompatible .seq-Datei im MOS-Technik-Format
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
from pathlib import Path
import os

TEXTS_EN = {
    "creating_dir": "The target directory '{}' does not exist. Creating it now.",
    "prompt_overwrite": "File '{}' already exists. Overwrite? (y/n): ",
    "yes": "y",
    "no": "n",
    "prompt_new_name": "Please enter a new output filename (current: {}): ",
    "invalid_input": "Please enter 'y' for yes or 'n' for no.",
    "file_created": "File successfully created: {}",
    "usage_hex2mos": "Usage: {} <inputfile> [<outputfile>] [--overwrite]",
    "file_not_found": "File not found: {}",
    "no_output_name": "Error: No output filename given.",
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

def read_start_address(filename):
    with open(filename, 'rb') as f:
        start_bytes = f.read(2)
        low = start_bytes[0]
        high = start_bytes[1]
        return (high << 8) | low

def compute_checksum(line):
    return sum(line) & 0xFFFF

def create_line(address, data_block):
    length = bytes([len(data_block)])
    address_bytes = address.to_bytes(2, byteorder='big')
    payload = length + address_bytes + data_block
    checksum = compute_checksum(payload)
    checksum_bytes = bytes([(checksum >> 8) & 0xFF, checksum & 0xFF])
    return payload + checksum_bytes

def create_final_line(line_count):
    fill = bytes([0x00])
    count_bytes = bytes([(line_count >> 8) & 0xFF, line_count & 0xFF])
    return fill + count_bytes + count_bytes

def confirm_output_file(template, overwrite=False):
    path = Path(template)

    if not path.parent.exists():
        print(t("creating_dir", path.parent))
        os.makedirs(path.parent)

    while path.exists() and not overwrite:
        answer = input(t("prompt_overwrite", path)).strip().lower()
        if answer == TEXTS.get("yes", "y"):
            break
        elif answer == TEXTS.get("no", "n"):
            new_name = input(t("prompt_new_name", path.name)).strip()
            path = path.parent / new_name
        else:
            print(t("invalid_input"))

    return str(path)

def main(input_file, output_file):
    try:
        address = read_start_address(input_file)
        with open(input_file, 'rb') as f:
            data = f.read()
    except IOError as e:
        print(t("file_not_found", input_file))
        sys.exit(1)

    block_size = 24
    offset = 2  # skip start address
    lines = []

    while offset < len(data):
        block = data[offset:offset + block_size]
        lines.append(create_line(address, block))
        offset += block_size
        address += len(block)

    final_line = create_final_line(len(lines))

    with open(output_file, "w") as out:
        for line in lines:
            out.write(";" + line.hex().upper() + "\n")
        out.write(";" + final_line.hex().upper() + "\n")

    print(t("file_created", output_file))

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(t("usage_hex2mos", sys.argv[0]))
        sys.exit(1)

    input_file = sys.argv[1]
    overwrite = "--overwrite" in sys.argv
    output_file = next((arg for arg in sys.argv[2:] if not arg.startswith("--")), None)

    if not Path(input_file).exists():
        print(t("file_not_found", input_file))
        sys.exit(1)

    if output_file:
        output_file = confirm_output_file(output_file, overwrite)
    else:
        print(t("no_output_name"))
        output_file = confirm_output_file(f"{Path(input_file).stem}.seq", overwrite)

    main(input_file, output_file)