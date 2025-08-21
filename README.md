# Debian Dotfiles Management

This repository contains a set of scripts and configuration files to manage a Debian environment, including dotfiles and packages.

## Prerequisites

Before you begin, ensure you have Guile installed on your system:

```bash
sudo apt install guile
```

## Overview

This project provides two main functionalities:

1.  **Dotfile Management**: Symlink dotfiles from the `files` directory to your home directory.
2.  **Package Management**: Install and uninstall Debian packages.

These functionalities are exposed through the `bin/dotfiles` and `bin/pkg-manager` scripts, respectively. The `Makefile` provides a convenient wrapper around these scripts.

## Configuration

-   `config/dotfiles.scm`: This file specifies which dotfiles to manage. Each entry should be a list of two strings: the source file in the `files` directory and the destination path in your home directory.
-   `config/packages.scm`: This file lists the Debian packages to be managed.

## Usage

### Makefile

The `Makefile` is the recommended way to interact with this project. Here are the main targets:

-   `make help`: Display the help message.
-   `make install`: Install dotfiles.
-   `make uninstall`: Uninstall dotfiles.
-   `make list`: List managed dotfiles.
-   `make pkg-install`: Install Debian packages.
-   `make pkg-uninstall`: Uninstall Debian packages.
-   `make pkg-list`: List managed Debian packages.
-   `make test`: Run the test suite.

### Direct Script Usage

You can also call the scripts directly:

#### Dotfile Management

-   `bin/dotfiles list`: List the dotfiles specified in `config/dotfiles.scm`.
-   `bin/dotfiles install`: Create symlinks for the specified dotfiles.
-   `bin/dotfiles uninstall`: Remove the symlinks for the specified dotfiles.

#### Package Management

-   `bin/pkg-manager list`: List the packages specified in `config/packages.scm`.
-   `bin/pkg-manager install`: Install the specified packages.
-   `bin/pkg-manager uninstall`: Uninstall the specified packages.

