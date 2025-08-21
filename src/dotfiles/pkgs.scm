(define-module (dotfiles pkgs)
  #:use-module (dotfiles core)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (list-packages uninstall installed?
            install))

(define (installed? pkg)
  (call-with-values
      (lambda () (read-cmd-output "dpkg-query" "-W" "-f=${Status}" pkg))
    (lambda (out status)
      (and (eqv? status 0)
	   (string=? out "install ok installed")))))

(define* (install packages #:key (simulate? #f) (no-update? #f))
  (let* ((to-install (filter (lambda (p)
                               (if (installed? p)
                                   (begin (format #t "Already installed: ~a~%" p)
                                          #f)
                                   #t))
                             packages)))
    (when (null? to-install)
      (format #t "Nothing to do.~%")
      (exit 0))

    (unless no-update?
      (if simulate?
	  (format #t "(simulate) apt-get update~%")
	  (unless (= 0 (run! "apt-get" "update"))
	    (stderr "apt-get update failed.")
	    (exit 1))))

    (format #t "Planned installs: ~a~%" (string-join to-install ", "))

    (if simulate?
	(format #t "--simulate set: not committing changes.~%")
	(let ((rc (apply run! "apt-get" (append '("install" "-y") to-install))))
	  (exit rc)))))

(define* (uninstall pkgs #:key (simulate? #f))
  (let ((command (if simulate?
                     '("apt-get" "remove" "--simulate")
                     '("apt-get" "remove"))))
    (apply run! (append command pkgs))))

(define (list-packages pkgs)
  (for-each
   (lambda (pkg)
     (let ((status (if (installed? pkg) "installed" "not installed")))
       (format #t "~a: ~a\n" pkg status)))
   pkgs))
