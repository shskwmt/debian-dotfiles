#!/usr/bin/guile \
--no-auto-compile -s
!#
;; Run all tests. Usage: GUILE_LOAD_PATH=src ./tests/run.scm
(use-modules (tests test-pkgs)
	     (tests test-core)
	     (tests test-fs)
	     (tests test-spec))
;; Each imported module runs its tests at load time via srfi-64
