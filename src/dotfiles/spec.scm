(define-module (dotfiles spec)
  #:use-module (dotfiles core)
  #:use-module (dotfiles fs)
  #:export (dotfiles install-one uninstall-one))

;; Your declarative spec
;; Load dotfiles from external Scheme file
(define dotfiles
  (call-with-input-file (path-join (repo-root) "config" "dotfiles.scm") read))

(define* (install-one entry #:key (dry? #f))
  (let* ((action   (assq-ref entry 'action))
         (source   (assq-ref entry 'source))
         (target-r (assq-ref entry 'target))
         (mode     (assq-ref entry 'mode))
         (tmpl?    (eq? #t (assq-ref entry 'template?)))
         (ctx      (context))
         (target   (abs-target target-r)))
    (case action
      ((link)
       (let* ((src (path-join (repo-root) source)))
         (unless (file-exists? src)
           (stderr "Source missing for link: ~a~%" src) (exit 1))
         (let ((same? (and (symlink? target)
                           (string=? (readlink* target) src))))
           (if same?
               (info "OK (link exists): ~a -> ~a~%" target src)
               (begin
                 (info "~a ~a -> ~a~%" (if dry? "[DRY] link" "link") target src)
                 (unless dry?
                   (backup-if-exists target)
                   (force-unlink target)
                   (make-symlink src target)))))))
      ((copy)
       (let* ((src (path-join (repo-root) source)))
         (unless (file-exists? src)
           (stderr "Source missing for copy: ~a~%" src) (exit 1))
         (if tmpl?
             (let* ((text (file->string src))
                    (rendered (render-template text ctx)))
               (info "~a ~a (templated)~%"
                     (if dry? "[DRY] write" "write") target)
               (unless dry?
                 (backup-if-exists target)
                 (ensure-dir target)
                 (call-with-output-file target
                   (lambda (out) (display rendered out)))
                 (when mode (chmod-str target mode))))
             (begin
               (info "~a ~a~%" (if dry? "[DRY] copy" "copy") target)
               (unless dry?
                 (backup-if-exists target)
                 (copy-file src target)
                 (when mode (chmod-str target mode)))))))
      ((write)
       (let* ((text (if (string? source) source
                        (begin (stderr "write action expects string source~%")
                               (exit 1))))
              (data (if tmpl? (render-template text ctx) text)))
         (info "~a ~a~%" (if dry? "[DRY] write" "write") target)
         (unless dry?
           (backup-if-exists target)
           (ensure-dir target)
           (call-with-output-file target
             (lambda (out) (display data out)))
           (when mode (chmod-str target mode)))))
      ((clone)
       (let ((url (assq-ref entry 'source))
             (target (abs-target (assq-ref entry 'target))))
         (if (file-exists? target)
             (if (git-repo? target)
                 (begin
                   (info "~a Updating git repository: ~a~%" (if dry? "[DRY]" "") target)
                   (unless dry?
                     (run! "git" "-C" target "pull")))
                 (begin
                   (info "~a Target ~a exists but is not a git repository. Backing up and cloning.~%" (if dry? "[DRY]" "") target)
                   (unless dry?
                     (backup-if-exists target)
                     (run! "git" "clone" url target))))
             (begin
               (info "~a Cloning ~a into ~a~%" (if dry? "[DRY]" "") url target)
               (unless dry?
                 (ensure-dir (dirname target))
                 (run! "git" "clone" url target))))))
      (else
       (stderr "Unknown action: ~a~%" action) (exit 1)))))

(define* (uninstall-one entry #:key (dry? #f))
  (let* ((action   (assq-ref entry 'action))
         (target-r (assq-ref entry 'target))
         (target   (abs-target target-r)))
    (case action
      ((clone)
       (info "~a Skipping uninstall for git repository: ~a~%" (if dry? "[DRY]" "") target))
      (else
       (when (file-exists? target)
         (info "~a ~a~%" (if dry? "[DRY] remove" "remove") target)
         (unless dry? (delete-file target)))))))
