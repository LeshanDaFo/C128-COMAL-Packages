# COMAL80-Pakete für den C128

Dieses Repository enthält eine gepflegte Sammlung von **COMAL80-Paketen** für den Commodore 128, darunter Originaldateien und rekonstruierte Erweiterungen. Ziel ist es, COMAL80-kompatible Maschinensprache-Erweiterungen zu erhalten und nutzbar zu machen.

## Inhalt

Die Pakete liegen in unterschiedlichen Formen vor:

- 🟢 **Quellcode** im ACME-Format (*.asm), vorbereitet für den Einsatz mit dem [ACME Assembler](https://github.com/meonwax/acme)
- 🔵 **Binärdateien** im `.seq`-Format, direkt aus COMAL80 heraus ladbar
- 🔁 **Rekonstruierte Quellen**, z. B. aus disassemblierten .seq-Dateien oder Listings aus COMAL-Today

## Herkunft der Dateien

Die Ursprungsquellen umfassen:

- Disketten-Images aus historischen COMAL-Publikationen (z. B. **COMAL-Today**,**DUTCH COMAL USERS GROUP**)
- Fertige `.seq`-Dateien von solchen Disketten
- Maschinencode, der in COMAL80 geladen und mit Monitorprogrammen abgespeichert wurde
- Bereits vorhandene Quellcodes, angepasst an den ACME-Assembler

Falls kein Quellcode vorhanden war, wurde die Binärdatei disassembliert, dokumentiert und kommentiert. Solche Dateien enthalten einen Hinweis im Kopfbereich.

## Werkzeuge und Umwandlung

Das Projekt enthält Python-Skripte zur Umwandlung von Quellcode in COMAL-kompatible Binärdateien:

- `make_package.py`: Erstellt aus einer `.asm`-Datei eine `.prg`- und `.seq`-Datei
- Unterstützte Plattformen: **Windows**, **macOS**, **Linux**
- Die benötigten ACME-Binaries befinden sich im Verzeichnis `bin/`

### Voraussetzungen

- **Python 3** (bitte als `python3` aufrufen)
- Keine weiteren Abhängigkeiten erforderlich
- Optional: [VS Code](https://code.visualstudio.com/) zur Bearbeitung

### Paketerzeugung

- In Visual Studio Code:

Öffne das Projekt in VS Code und führe die definierte Task über das Menü aus:
Terminal → Run Task → Build COMAL80 Package (sofern eine Task in tasks.json definiert ist).

- Alternativ über die Kommandozeile:

Verwende je nach Betriebssystem den passenden Befehl:

```bash
# Linux/macOS
python3 make_package.py src/dein_package.asm

# Windows (PowerShell)
python make_package.py src\dein_package.asm
```

Erzeugte Dateien:

- `build/prg/DEIN.PACKAGE.prg`: Ausführbares PRG
- `build/seq/DEIN.PACKAGE.seq`: In COMAL80 per `LINK "DEIN.PACKAGE"` ladbar


## Erweiterbarkeit

Dieses Projekt wird fortlaufend erweitert:

- Neue Pakete aus Archiven
- Eigene COMAL80-Entwicklungen
- Werkzeuge zur Analyse und Disassemblierung

Beiträge und Hinweise auf weitere COMAL80-Ressourcen sind willkommen!

## Lizenz

- Disassembliertes Material enthält Copyright-Vermerke
- Python-Tools und neue Inhalte stehen unter der **MIT-Lizenz**, sofern nicht anders angegeben

---

**COMAL lebt weiter!** ✨ Dieses Projekt möchte zur Bewahrung und Erweiterung der COMAL80-Welt beitragen.
