# Requirements

This document describes the requirements for the project.

## Software Versions

- GHC: 9.6.6
- HLS: 2.9.0.1
- Cabal: 3.10.3.0
- Stack: 3.1.1


# System Requirements

*table of contents*
- Linux
    - [Debian](#linux-debian)
    - [Ubuntu](#linux-ubuntu)
    - [Fedora](#linux-fedora)
    - [CentOS](#linux-centos)
    - [Alpine](#linux-alpine)
    - [(generic)](#linux-generic)
- [FreeBSD](#freebsd)
- [Windows](#windows)

## Linux Debian

The following distro packages are required:
`build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`
Version >= 11 && <= 12

The following distro packages are required: `build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`
Version >= 12

The following distro packages are required: `build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`
## Linux Ubuntu

The following distro packages are required: `build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`
Version >= 20.04 && < 20.10

The following distro packages are required: `build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`
Version >= 20.10 && < 23

The following distro packages are required: `build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`
Version >= 23

The following distro packages are required: `build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev`
## Linux Fedora

The following distro packages are required: `gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs xz perl`
## Linux CentOS

The following distro packages are required: `gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs xz perl`
Version >= 7 && < 8

The following distro packages are required: `gcc gcc-c++ gmp gmp-devel make ncurses xz perl`
## Linux Alpine

The following distro packages are required: `binutils-gold curl gcc g++ gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl tar xz`
## Linux (generic)

You need the following packages: `curl g++ gcc gmp make ncurses realpath xz-utils`. Consult your distro documentation on the exact names of those packages.

## FreeBSD

The following distro packages are required: `curl gcc gmp gmake ncurses perl5 libffi libiconv`
## Windows

On Windows, `msys2` should already have been set up during the installation, so most users should just proceed. If you are installing manually, make sure to have a working mingw64 toolchain and shell.