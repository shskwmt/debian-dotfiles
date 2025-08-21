(define-module (dotfiles fs)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (dotfiles core)
  #:export (ensure-dir force-unlink backup-if-exists
            symlink? readlink*
            chmod-str make-symlink file->string))

(define (ensure-dir p)
  (let ((d (dirname p)))
    (unless (and (access? d F_OK) (eq? 'directory (stat:type (stat d))))
      (system* "mkdir" "-p" d))))

(define (symlink? p)
  (and
   (file-exists? p)
   (eq? 'symlink (stat:type (lstat p)))))

(define (make-symlink src dst)
  (ensure-dir dst)
  (symlink src dst))

(define readlink* readlink)

(define (chmod-str p mode-str)
  (system* "chmod" mode-str p))

(define (force-unlink path)
  (when (file-exists? path) (delete-file path)))

(define (backup-if-exists target)
  (when (file-exists? target)
    (let ((bak (string-append target ".bak")))
      (info "Backing up ~a -> ~a~%" target bak)
      (when (file-exists? bak) (delete-file bak))
      (rename-file target bak))))

;; Read entire file into a string (no reliance on get-string-all)
(define (file->string path)
  (call-with-input-file path
    (lambda (p)
      (let ((out (open-output-string)))
        (let loop ((ch (read-char p)))
          (if (eof-object? ch)
              (get-output-string out)
              (begin
                (write-char ch out)
                (loop (read-char p)))))))))
