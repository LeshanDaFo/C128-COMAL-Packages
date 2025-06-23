#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#  check_lang.py – Prüft die eingestellte Systemsprache
#
#  Copyright (c) 2025 Claus Schlereth
#
#  Dieses Projekt steht unter der MIT-Lizenz.
#  Weitere Details siehe LICENSE-Datei im Repository oder:
#  https://opensource.org/licenses/MIT
#
#  Weitere Informationen zum MOS-Format:
#  https://srecord.sourceforge.net/man/man5/srec_mos_tech.5.html

import locale
import sys

# Sprachpaket laden
TEXTS_EN = {
    "lang_detected": "Detected system language: {}",
    "lang_not_supported": "Language '{}' is not supported. Defaulting to English.",
}

try:
    import locales

    def t(key, *args):
        lang = get_system_language()  # Die vollständige Sprache mit Region ermitteln
        texts = locales.get_texts(lang)
        text = texts.get(key, TEXTS_EN.get(key, key))
        return text.format(*args)

    # Unterstützte Sprachen: Nur Sprachcodes wie "de" und "en"
    SUPPORTED_LANGUAGES = ["de", "en"]  # Du kannst hier auch weitere Sprachen hinzufügen

except Exception as e:
    def t(key, *args):
        return TEXTS_EN.get(key, key).format(*args)
    SUPPORTED_LANGUAGES = ["en"]

# Sprache aus dem System ermitteln
def get_system_language():
    lang_tuple = locale.getlocale()  # z.B. ('de_DE', 'UTF-8')
    lang = lang_tuple[0] if lang_tuple and lang_tuple[0] else 'en'
    return lang.split("_")[0].lower()  # Nur den Sprachcode extrahieren (z.B. 'de' oder 'en')

# Nur wenn direkt aufgerufen
if __name__ == "__main__":
    language = get_system_language()

    # Extrahiere den Sprachcode und prüfe, ob er in der Liste der unterstützten Sprachen ist
    language_code = language.split("_")[0]  # Nur den Sprachcode extrahieren (z.B. 'de' oder 'en')

    # Prüfen, ob der Sprachcode in der Liste der unterstützten Sprachen ist
    if language_code in SUPPORTED_LANGUAGES:
        print(t("lang_detected", language_code))
    else:
        print(t("lang_not_supported", language_code))