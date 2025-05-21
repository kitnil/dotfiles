# Dotfiles

This repository contains my dotfiles, configuration files for various
applications and tools used to customize my environment.  It's a work in
progress and may change frequently.

## Structure

*   **AUTHORS:** List of contributors.
*   **bootstrap:** Scripts or instructions for setting up the initial environment.
*   **ChangeLog:** Record of changes made to the dotfiles themselves.
*   **configure.ac:** Autoconf input file, likely used for building tools within this repository.
*   **COPYING:**  License information.
*   **doc/:** Documentation related to the dotfiles and their usage.
*   **flux/:** Configuration files or scripts specific to Flux Kubernetes continues delivery tools.
*   **guix/:** Configuration files for Guix, a functional package manager.
*   **Makefile.am:** Autotools Makefile fragment.  Used for building tools within this repository.
*   **NEWS:** Announcements of new features or changes.
*   **nix/:** Configuration files for Nix, another functional package manager.
*   **README.md:** This file!
*   **src/:** Source code for any custom scripts or tools included in the dotfiles.

## Usage

1.  Clone the repository: `git clone <repository_url> ~/.dotfiles`
2.  Link necessary files and directories (e.g., using symbolic links or other
methods).  The specific commands will depend on your operating system and
desired configuration.
3.  Consult the `doc/` directory for more detailed instructions and
explanations of each file's purpose.

## Dependencies

*   Guix (for `guix/`)
*   Nix (for `nix/`)
*   Flux (for `flux/`)
*   Autotools (`autoconf`, `automake`, `make` - likely needed for building tools in `src/`)
*   Password-store

## Contributing

Contributions are welcome! Please submit pull requests with clear descriptions
of the changes.

## License

This project is licensed under GPLv3 - see the `COPYING` file for details.

## Thanks

*   [Alex Kost configs](https://github.com/alezost)
