;; ~/debian-dotfiles/config/dotfiles.scm
;; Specification for each dotfile or configuration directory.
(
  ((name . bashrc)
   (action . link)
   (source . "files/bashrc")
   (target . ".bashrc")
   (mode . #o644) ;; Using octal literal for permissions
   (template? . #f))

  ((name . gitconfig)
   (action . link)
   (source . "files/gitconfig")
   (target . ".gitconfig")
   (mode . #o644)
   (template? . #f))

  ((name . emacs-config)
   (action . clone)
   (source . "https://github.com/shskwmt/hikizan-emacs")
   (target . ".emacs.d"))
)
