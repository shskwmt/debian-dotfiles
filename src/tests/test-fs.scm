(define-module (tests test-fs)
  #:use-module (srfi srfi-64)
  #:use-module (dotfiles core)
  #:use-module (dotfiles fs))


(test-begin "fs")

;; Use a temporary directory for all file system tests
(let ((tmp-dir (mkdtemp "/tmp/dotfiles-test-XXXXXX")))
  (test-group "ensure-dir"
	      (let* ((new-sub-dir (path-join tmp-dir "new-dir"))
		     (file-path (path-join new-sub-dir "file.txt")))
		(ensure-dir file-path)
		(test-assert "directory was created" (file-exists? new-sub-dir))
		(test-equal "is a directory" 'directory (stat:type (stat new-sub-dir)))))

  (test-group "symlink?, make-symlink, readlink"
	      (let* ((source-path (path-join tmp-dir "source.file"))
		     (link-path (path-join tmp-dir "link.file")))
		(call-with-output-file source-path (lambda (p) (display "data" p)))
		(make-symlink source-path link-path)
		(test-assert "is a symlink" (symlink? link-path))
		(test-assert "source is not a symlink" (not (symlink? source-path)))
		(test-equal "readlink* resolves correctly"
			    source-path
			    (readlink* link-path))))

  (test-group "chmod-str"
	      (let ((file-path (path-join tmp-dir "script.sh")))
		(call-with-output-file file-path (lambda (p) (display "#!/bin/sh" p)))
		(test-assert "is not executable initially" (not (access? file-path X_OK)))
		(chmod-str file-path "u+x")
		(test-assert "is executable after chmod" (access? file-path X_OK))))

  (test-group "file->string and force-unlink"
	      (let* ((file-path (path-join tmp-dir "test.txt"))
		     (content "hello world\nline 2"))
		(call-with-output-file file-path
		  (lambda (p) (display content p)))
		(test-equal "file->string reads content"
			    content
			    (file->string file-path))
		(force-unlink file-path)
		(test-assert "file was unlinked" (not (file-exists? file-path)))))

  (test-group "backup-if-exists"
	      (let* ((file-path (path-join tmp-dir "original.txt"))
		     (backup-path (string-append file-path ".bak")))
		;; Test 1: Backup a file that exists
		(call-with-output-file file-path (lambda (p) (display "v1" p)))
		(backup-if-exists file-path)
		(test-assert "original file is gone" (not (file-exists? file-path)))
		(test-assert "backup file was created" (file-exists? backup-path))
		(test-equal "backup content is correct" "v1" (file->string backup-path))

		;; Test 2: Overwrite an existing backup
		(call-with-output-file file-path (lambda (p) (display "v2" p)))
		(backup-if-exists file-path)
		(test-equal "backup was overwritten" "v2" (file->string backup-path)))))

(test-end)
