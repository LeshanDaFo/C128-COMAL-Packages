#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#  locales.py – Sprachdatei für das Mos2Hex-Paket
#
#  Copyright (c) 2025 Claus Schlereth
#
#  Dieses Projekt steht unter der MIT-Lizenz.
#  Weitere Details siehe LICENSE-Datei im Repository oder:
#  https://opensource.org/licenses/MIT
#
#  Weitere Informationen zum MOS-Format:
#  https://srecord.sourceforge.net/man/man5/srec_mos_tech.5.html

  
def get_texts(lang):
    if lang.startswith("de"):
        return {
#hex2mos.py
            "creating_dir": "Das Zielverzeichnis '{}' existiert nicht. Es wird nun erstellt.",
            "prompt_overwrite": "Die Datei '{}' existiert bereits. Überschreiben? (j/n): ",
            "yes": "j",
            "no": "n",
            "prompt_new_name": "Bitte neuen Ausgabedateinamen eingeben (aktuell: {}): ",
            "invalid_input": "Bitte 'j' für ja oder 'n' für nein eingeben.",
            "file_created": "Datei erfolgreich erstellt: {}",
            "usage_hex2mos": "Verwendung: {} <eingabedatei> [<ausgabedatei>] [--overwrite]",
            "file_not_found": "Datei nicht gefunden: {}",
            "no_output_name": "Kein Zielname angegeben. Bitte Zielname eingeben.",
#make_all
            "usage_makeall": "Usage: {} [Verzeichnis] [--overwrite]",
            "not_a_directory": "Fehler: '{}' ist kein gültiges Verzeichnis.",
            "no_asm_found": "Keine .asm-Dateien im Verzeichnis '{}' gefunden.",
            "processing_file": "Verarbeite: {}",
            "hex2mos_error": "Fehler beim Ausführen von hex2mos.py! {} {}",
#make_package            
            "build_success": "Fertig: {} wurde erstellt.",
            "no_filename": "Fehler: Kein Dateiname angegeben.\nVerwendung: {} <Dateiname> [--overwrite]",
            "source_missing": "Error: Qelldatei '{}' existiert nicht!",
            "building": "Baue '{}' → '{}'",
            "build_error": "Fehler beim Assemblieren!",
            "running_hex2mos": "Starte Python-Script hex2mos.py mit '{}'...",
            "hex2mos_done": "Python-Script hex2mos.py wurde erfolgreich ausgeführt.",
            "hex2mos_failed": "Fehler beim Ausführen von hex2mos.py!",
#check_lang
            "lang_detected": "Erkannte Sprache: {}",
        }
    else:
        return {
#hex2mos.py
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
#make_all
            "usage_makeall": "Usage: {} [Directory] [--overwrite]",
            "not_a_directory": "Error: '{}' is not a valid directory.",
            "no_asm_found": "No .asm files found in directory '{}'.",
            "processing_file": "Processing: {}",
            "hex2mos_error": "Error while running {} {}",
#make_package
            "no_filename": "Error: No filename given.\nUsage: {} <filename> [--overwrite]",
            "source_missing": "Error: Source file '{}' does not exist!",
            "building": "Building '{}' → '{}'",
            "build_success": "Done: {} has been created.",
            "build_error": "Error during assembly!",
            "running_hex2mos": "Running Python script hex2mos.py with '{}'...",
            "hex2mos_done": "Python script hex2mos.py executed successfully.",
            "hex2mos_failed": "Error while running hex2mos.py!",
#check_lang
            "lang_detected": "Detected system language: {}",
        }
