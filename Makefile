GUILE ?= guile
LOAD_PATH := src

.PHONY:  test install uninstall list help pkg-install pkg-uninstall pkg-list pkg-help

test:
	@GUILE_LOAD_PATH=$(LOAD_PATH) ./src/tests/run.scm

install:
	@GUILE_LOAD_PATH=$(LOAD_PATH) bin/dotfiles --install

uninstall:
	@GUILE_LOAD_PATH=$(LOAD_PATH) bin/dotfiles --uninstall

list:
	@GUILE_LOAD_PATH=$(LOAD_PATH) bin/dotfiles --list

help:
	@GUILE_LOAD_PATH=$(LOAD_PATH) bin/dotfiles --help

pkg-install:
	@GUILE_LOAD_PATH=$(LOAD_PATH) bin/pkg-manager --install

pkg-uninstall:
	@GUILE_LOAD_PATH=$(LOAD_PATH) bin/pkg-manager --uninstall

pkg-list:
	@GUILE_LOAD_PATH=$(LOAD_PATH) bin/pkg-manager --list

pkg-help:
	@GUILE_LOAD_PATH=$(LOAD_PATH) bin/pkg-manager --help
