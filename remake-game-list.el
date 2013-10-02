#! /bin/sh
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-
(package-initialize)
(load-file "jumblr.el")
(setq jumblr-precompute-file "~/jumblr-games.el")
(jlr-recompute-games 500 5 9)
