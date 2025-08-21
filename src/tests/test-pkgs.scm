(define-module (tests test-pkgs)
  #:use-module (srfi srfi-64)
  #:use-module (dotfiles pkgs)
  #:use-module (dotfiles core)
  #:use-module (ice-9 popen))

(test-begin "pkgs")

;; Test for installed?
(test-group "installed?"
  ;; This test assumes 'bash' is installed on a Debian-like system.
  ;; If not, this test might fail or need adjustment.
  (test-assert "bash should be installed" (installed? "bash"))
  (test-assert "non-existent-package should not be installed" (not (installed? "non-existent-package-12345"))))

;; Test for install (simulated)
(test-group "install (simulated)"
  ;; This test will only check the simulation output, not actual installation.
  (let* ((output (with-output-to-string
                   (lambda ()
                     (install '("test-package-a" "test-package-b") #:simulate? #t)))))
    (test-assert "simulated output contains planned installs" (string-contains output "Planned installs: test-package-a, test-package-b"))
    (test-assert "simulated output indicates no commitment" (string-contains output "--simulate set: not committing changes."))))

;; Test for uninstall (simulated)
(test-group "uninstall (simulated)"
  (let* ((output (with-output-to-string
                   (lambda ()
                     (uninstall '("test-package-c" "test-package-d") #:simulate? #t)))))
    (test-assert "simulated uninstall output contains apt-get remove" (string-contains output "+ apt-get remove --simulate test-package-c test-package-d"))))

;; Test for list-packages
(test-group "list-packages"
  (let* ((output (with-output-to-string
                   (lambda ()
                     (list-packages '("bash" "non-existent-package-xyz"))))))
    (test-assert "list output contains bash installed status" (string-contains output "bash: installed"))
    (test-assert "list output contains non-existent package status" (string-contains output "non-existent-package-xyz: not installed"))))

(test-end)
