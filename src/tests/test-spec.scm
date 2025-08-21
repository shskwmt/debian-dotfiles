(define-module (tests test-spec)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (dotfiles core)
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

(test-group "install-one with clone action"
	    (let* ((tmp-dir (mkdtemp "/tmp/dotfiles.XXXXXX"))
		   (repo-path (path-join tmp-dir "test-source-repo"))
		   (clone-target (path-join tmp-dir "clone-target"))
		   (dotfile `((name . "test-clone")
			      (action . clone)
			      (source . ,repo-path)
			      (target . ,clone-target))))
	      ;; Setup a bare git repo to clone
	      (mkdir repo-path)
	      (run! "git" "-C" repo-path "init")
	      (run! "git" "-C" repo-path "config" "user.email" "test@example.com")
	      (run! "git" "-C" repo-path "config" "user.name" "Test User")
	      (run! "git" "-C" repo-path "commit" "--allow-empty" "-m" "initial commit")

	      (install-one dotfile)
	      (test-assert "clones into a non-existent directory" (git-repo? clone-target))

	      ;; Add a new commit to the source repo
	      (run! "git" "-C" repo-path "commit" "--allow-empty" "-m" "second commit")
	      (install-one dotfile)

	      ;; A simple way commit to the source repo
	      (let ((log (read-cmd-output "git" "-C" clone-target "log")))
		(test-assert "pulls into an existing repository" (string-contains log "second commit")))

              (system* "rm" "-rf" clone-target) ;; Ensure it's gone
              (mkdir clone-target)
              (call-with-output-file (path-join clone-target "some-file")
		(lambda (p) (display "hello" p)))
              (install-one dotfile)
	      (test-assert "backs up if target is not a git repo" (file-exists? (string-append clone-target ".bak")))
	      (test-assert "clone if target is not a git repo" (git-repo? clone-target))))

(test-end)
