# COMAL80 Packages for C128

This repository provides a curated collection of **COMAL80 Packages** for the Commodore 128, including both original and reconstructed packages. Its purpose is to preserve and share COMAL80 extensions that can be linked and executed within the COMAL80 environment.

## Contents

The repository contains packages in various forms:

- 🟢 **Source code** in ACME format (*.asm) – adapted for use with the [ACME Assembler](https://github.com/meonwax/acme)
- 🔵 **Binary files** in `.seq` format – directly executable from COMAL80
- 🔁 **Reconstructed sources** – disassembled from `.seq` files or derived from printed listings (e.g., COMAL-Today magazine)

## Source Origins

Files in this repository originate from:

- Disk images of historical publications such as **COMAL-Today**, **DUTCH COMAL USERS GROUP**
- Precompiled `.seq` files found on such disks
- Machine code extracted via monitor tools in COMAL80 and then disassembled
- ACME-compatible source files adapted or rewritten from original listings

When the original source was unavailable, files were disassembled and annotated appropriately. In such cases, source files include comments indicating their reconstructed status and any applicable copyright notices.

## Tools and Conversion

This project includes Python tools to convert ACME source files into `.prg` and `.seq` files:

- `make_package.py`: compiles `.asm` files using ACME and produces COMAL80-linkable `.seq` files
- Platform support: **Windows**, **macOS**, and **Linux**
- Platform-specific ACME binaries included under `bin/`

### Requirements

- **Python 3**
- No external dependencies beyond the included `acme` binaries
- Optionally: [VS Code](https://code.visualstudio.com/) for editing

### Building Packages

- In Visual Studio Code:

Open the project in VS Code and execute the defined task from the menu:
Terminal → Run Task → Build COMAL80 Package
(if a tasks.json task is configured)

- Alternatively via command line:

Use the appropriate command depending on your operating system:

```bash
# Linux/macOS
python3 make_package.py src/your_package.asm

# Windows (PowerShell)
python make_package.py src\your_package.asm
```

Output:

- `build/prg/YOUR.PACKAGE.prg`: the assembled binary file
- `build/seq/YOUR.PACKAGE.seq`: loadable in COMAL80 via `LINK "your.package"`


## Extensibility

This project is actively maintained and extended:

- New packages from archives
- Personal COMAL80 developments
- Tools for analyzing and reconstructing machine-code packages

Community contributions and tips on additional COMAL80 resources are welcome!

## License

- Reconstructed sources include appropriate copyright notes
- Python tooling and new contributions are licensed under the **MIT License**, unless otherwise noted

---

**COMAL lives on!** ✨ This project aims to preserve, understand, and expand the world of COMAL80.
