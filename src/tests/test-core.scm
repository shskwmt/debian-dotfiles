(define-module (tests test-core)
  #:use-module (srfi srfi-64)
  #:use-module (dotfiles core))

(test-begin "core")

(test-equal "path-join relative"
  "home/user/.bashrc"
  (path-join "home" "user" ".bashrc"))

(test-equal "path-join absolute resets"
  "/etc/opt/bin"
  (path-join "/etc" "/opt" "bin"))

(test-equal "path-join trims slashes"
  "/etc/bashrc"
  (path-join "/etc/" "bashrc"))

(let* ((txt "Hello {{USER}} on {{HOST}} using {{EDITOR}}!")
       (ctx (let ((c (context)))
              (hash-set! c 'USER "alice")
              (hash-set! c 'HOST "lab")
              (hash-set! c 'EDITOR "emacs")
              c)))
  (test-equal "render-template"
    "Hello alice on lab using emacs!"
    (render-template txt ctx)))

(test-end)
