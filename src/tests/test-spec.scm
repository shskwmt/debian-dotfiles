(define-module (tests test-spec)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (dotfiles fs)
  #:use-module (dotfiles spec))

(test-begin "spec")

;; Use a temp $HOME and temp repo for safe IO
(let* ((tmp-home (mkdtemp "/tmp/df-home.XXXXXX"))
       (tmp-repo (mkdtemp "/tmp/df-repo.XXXXXX"))
       (files-dir (string-append tmp-repo "/files")))
  (setenv "HOME" tmp-home)
  (setenv "DOTFILES_REPO_ROOT" tmp-repo)
  (mkdir files-dir)
  ;; prepare files
  (call-with-output-file (string-append files-dir "/gitconfig")
    (lambda (o) (display "[user]\n\tname = Test\n" o)))
  (call-with-output-file (string-append files-dir "/bashrc")
    (lambda (o) (display "user = \"{{USER}}\"\n" o)))

  ;; install two entries explicitly
  (for-each
   (lambda (name)
     (let* ((entry (car (filter (lambda (e) (eq? (assq-ref e 'name) name)) dotfiles))))
       (install-one entry)))
   '(gitconfig bashrc))

  (test-assert "gitconfig link exists"
    (let* ((dst (string-append tmp-home "/.gitconfig")))
      (and (symlink? dst)
           (string-suffix? "/files/gitconfig" (readlink* dst)))))

  (test-assert "bashrc copied and templated"
    (let* ((dst (string-append tmp-home "/.bashrc")))
      (and (symlink? dst)
           (string-suffix? "/files/bashrc" (readlink* dst)))))

  ;; uninstall
  (for-each
   (lambda (name)
     (let* ((entry (car (filter (lambda (e) (eq? (assq-ref e 'name) name)) dotfiles))))
       (uninstall-one entry)))
   '(gitconfig bashrc))

  (test-assert "gitconfig removed"
    (not (access? (string-append tmp-home "/.gitconfig") F_OK)))

  (test-assert "bashrc removed"
    (not (access? (string-append tmp-home "/.bashrc") F_OK))))

(test-end)
