(define-module (dotfiles core)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:export (read-cmd-output run! stderr info home repo-root
            path-join render-template
            context abs-target git-repo?))

(define (stderr fmt . args) (apply format (current-error-port) fmt args))
(define (info   fmt . args) (apply format (current-output-port) fmt args))

(define (home)
  (or (getenv "HOME")
      (begin (stderr "HOME is not set~%") (exit 1))))

(define (repo-root)
  (or (getenv "DOTFILES_REPO_ROOT") (getcwd)))

;; Fixed and reset-on-absolute join
(define (path-join . parts)
  (let loop ((ps (filter (lambda (x) (and x (string? x) (not (string=? x ""))))
                         parts))
             (acc ""))
    (if (null? ps)
        acc
        (let* ((p (car ps))
               (acc* (cond
                      ((string=? acc "") p)
                      ((string-suffix? "/" acc) (string-append acc p))
		      ((string-prefix? "/" p) (string-append acc p))
                      (else (string-append acc "/" p)))))
          (loop (cdr ps) acc*)))))

;; Mini mustache: {{KEY}} => from ctx or env
(define (render-template text ctx)
  (let* ((rx (make-regexp "\\{\\{[[:space:]]*([A-Za-z0-9_]+)[[:space:]]*\\}\\}"))
         (out (open-output-string)))
    (let loop ((start 0) (m (regexp-exec rx text)))
      (if (not m)
          (begin
            (display (substring text start (string-length text)) out)
            (get-output-string out))
          (let* ((m-start (match:start m))
                 (m-end   (match:end m))
                 (key     (match:substring m 1))
                 (sym     (string->symbol key))
                 (val     (or (hash-ref ctx sym #f) (getenv key) "")))
            (display (substring text start m-start) out)
            (display val out)
            (loop m-end (regexp-exec rx text m-end)))))))

(define (context)
  (let ((ctx (make-hash-table)))
    (hash-set! ctx 'EDITOR (or (getenv "EDITOR") "vim"))
    ctx))

(define (abs-target t)
  (if (string-prefix? "/" t) t (path-join (home) t)))

(define (read-cmd-output cmd . args)
  "Run CMD with ARGS; return its stdout as a string (trimmed)."
  (let* ((p (apply open-pipe* OPEN_READ cmd args))
	 (out (read-string p))
	 (status (close-pipe p)))
    (values (string-trim-both (or out "")) status)))

(define (run! cmd . args)
  (format #t "+ ~a~{ ~a~}~%" cmd args)
  (apply system* cmd args))

(define (git-repo? path)
  "Returns #t if PATH is a git repository, otherwise #f."
  (file-exists? (path-join path ".git")))
