;;; jumblr.el -- an anagram game for emacs

;; Copyright (C) 2013 Mike McCourt
;;
;; Authors: Mike McCourt <mkmcc@astro.berkeley.edu>
;; URL: https://github.com/mkmcc/jumblr
;; Version: 0.0.1
;; Keywords: anagram, word game, games
;; Package-Requires: ((s "1.8.0") (dash "2.2.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a function `jumblr-new-game' (aliased to `jumblr') which
;; launches a word game closely based on the old TextTwist by Yahoo.

;; The interface should be pretty intuitive: it displays a sequence of
;; letters, along with a series of blanks.  The blanks represent words
;; which can be made from the letters.  You type a word and "submit"
;; it by either hitting SPC or RET; the word replaces one of the
;; blanks if it fits.  Hitting SPC or RET with an empty guess
;; reshuffles the letters.

;; I tried to make the interface pretty responsive; take a look at the
;; screenshots in the github repository for a sense of how it works.

;;; Notes about the implementation:

;; The implementation is pretty straightforward and mostly consists of
;; functions to find permutations and subsets of words; I'm sure this
;; could be made much faster.

;; The most important aspect for game play is actually the word list:
;; in order for the game to be fun it must know all the words you can
;; think of, but not have too many obscure words.  It turns out to be
;; surprisingly difficult to find that balance.  My approach is as
;; follows:
;;
;; 1. Take the SIL english word list and intersect it with the New
;;    Oxford English Dictionary which comes with mac osx.  I then
;;    removed the three letter words which were obviously acronyms,
;;    abbreviations for longer words, or racial slurs.  This is the
;;    "expert" word list and contains about 85,000 words.
;;    (source: http://www-01.sil.org/linguistics/wordlists/english/)
;;
;; 2. Take the "8" word list from
;;    http://www.keithv.com/software/wlist/ and intersect it with the
;;    "expert" list.  This becomes the "hard" list and contains about
;;    47,000 words.
;;
;; 3. Take the "9" word list from the above link and intersect with
;;    with the "expert" list.  This is the "medium" list and contains
;;    about 36,000 words.
;;
;; 4. Take the "10" word list from the above link and intersect it
;;    with the "expert" list.  This contains 21,000 words and is the
;;    "easy" list.
;;
;; Let me know how you find the difficulty levels -- I'd really
;; appreciate the feedback!

;;; Installation:

;; Use package.el. You'll need to add MELPA to your archives:

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Alternatively, you can just save this file and do the standard
;; (add-to-list 'load-path "/path/to/jumblr.el")
;; (require 'jumblr)

;;; Customization:

;; See the defvar definitions at the beginning of the source code.
;; For example, to switch the dictionary, add the following to your
;; .emacs:
;;
;; (setq jumblr-dict-file "dict/easy.txt")

;;; TODO:
;;  1. will this know where to find dict/*.txt???
;;  2. submit to MELPA
;;  3. pre-compute games and store them in a database?
;;  4. speed up computation of new games?

;;; Acknowledgements

;; Thanks to Meredith, Brittney, Kathara, and Anna for suggesting the
;; name jumblr!

;;; Code:
(require 's)
(require 'dash)
(require 'cl)


;;; customizable properties
;; TODO: add colors?
(defvar jumblr-col-width 15
  "Column width for the jumblr game.")

(defvar jumblr-col-height 20
  "Column height for the jumblr game.")

(defvar jumblr-dict-file "dict/hard.txt"
  "Dictionary file for the jumblr game.  Choices are either
dict/easy.txt   (21k words)
dict/medium.txt (36k words)
dict/hard.txt   (47k words)
dict/expert.txt (85k words)")



;;; faces
(make-face 'jlr-scrable-face)
(make-face 'jlr-correct-face)
(make-face 'jlr-cheat-face)
(make-face 'jlr-blank-face)
(make-face 'jlr-guess-face)

(set-face-attribute 'jlr-correct-face nil
                    :inherit 'fixed-pitch
                    :weight 'bold
                    :foreground "#859900"
                    :height 1.5)

(set-face-attribute 'jlr-blank-face nil
                    :inherit 'fixed-pitch
                    :height 1.5)

(set-face-attribute 'jlr-cheat-face nil
                    :inherit 'fixed-pitch
                    :slant 'italic
                    :foreground "#d33682"
                    :height 1.5)

(set-face-attribute 'jlr-scrable-face nil
                    :inherit 'variable-pitch
                    :weight 'bold
                    :foreground "#dc322f"
                    :height 4.0)

(set-face-attribute 'jlr-guess-face nil
                    :inherit 'variable-pitch
                    :weight 'bold
                    :foreground "#6c71c4"
                    :height 3.0)



;;; set-theory stuff:
;; difference lists
(defun jlr-difference (list1 list2)
  "like -difference, but works with repeated entries."
  (let ((newlist list1))
    (loop for item in list2
          do (setq newlist
                   (remove* item newlist :count 1)))
    newlist))

;; scramble a word
(defun jlr-scramble-word (word)
  "return a random permutation of the letters in WORD."
  (let* ((lst (string-to-list word))
         (num (length lst))
         (rnd (--map (list (random (* 10 num)) it) lst)))
    (apply #'string
           (--map (cadr it) (--sort (< (car it) (car other)) rnd)))))

;; permutations (fixed length)
(defun jlr-list-permutations (list)
  "return all permutations of the entries in LIST."
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (jlr-list-permutations
                                 (remove* element list :count 1)))))))

(defun jlr-word-permutations (word)
  "return a list of all permutations of characters in WORD."
  (let* ((lst (string-to-list word))
         (data (jlr-list-permutations lst)))
    (--map (apply #'string it) data)))

;; subsets (smaller length)
(defun jlr-get-sublists (lst)
  "return all subsets of LST as a nested list."
  (cond
   ((null lst) nil)
   ((null (cdr lst)) lst)
   (t (cons lst (loop for element in lst
                      collect (jlr-get-sublists (remove* element lst :count 1)))))))

(defun jlr-flatten1 (l)
  "flatten a nested list into two levels."
  (if (and (listp l) (listp (cadr l)))
      (-mapcat 'jlr-flatten1 l)
    (list l)))

(defun jlr-sublists (lst)
  "return a list of all distinct subsets of LST with length > 2."
  (-distinct
   (--filter (and (listp it) (> (length it) 2))
             (jlr-flatten1 (jlr-get-sublists lst)))))

(defun jlr-substrings (str)
  "return all distinct substrings of STR with length > 2."
  (let* ((lst (string-to-list str))
         (data (jlr-sublists lst)))
    (--map (apply #' string it) data)))

;; get subwords
(defun jlr-get-subwords (word)
  "return a list of all permutations and sub-permutations of WORD
which are words."
  (--filter (-contains? word-list it)
            (-distinct
             (-flatten (-map 'jlr-word-permutations (jlr-substrings word))))))



;;; manage the word list
(defun jlr-slurp-dictionary (fname)
  "read a file into a list of words.
presume the file is properly formatted (lower case, etc.)"
  (let ((word-list '()))
    (when (file-readable-p fname)
      (with-temp-buffer
        (insert-file-contents fname)
        (goto-char (point-min))
        (while (re-search-forward "\\sw+" nil t)
          (setq word-list (cons (match-string 0) word-list)))))
    word-list))

(defvar word-list)
(setq word-list
      (--sort (s-less? it other)
              (jlr-slurp-dictionary jumblr-dict-file)))



;;; define a major mode for playing the game
;; remap letter keys and backspace to edit the *guess*, not the buffer
(defun jlr-backspace ()
  "Remove the last letter from the word guess."
  (interactive)
  (let ((scr (caar jlr-game-data))
        (guess (cadr (car jlr-game-data)))
        (data (cadr jlr-game-data)))
    (when (> (length guess) 0)
      (setq jlr-game-data
            (list (list scr
                        (s-left (- (length guess) 1) guess))
                  data))
      (jlr-draw-game))))

(defmacro jlr-define-key (key)
  "This macro writes a function which adds the letter KEY to the
current guess."
  `(defun ,(intern (concat "jlr-insert-" key)) ()
     ,(concat "Insert the letter \"" key "\" into the word guess.")
     (interactive)
     (let ((scr (caar jlr-game-data))
           (guess (cadr (car jlr-game-data)))
           (data (cadr jlr-game-data)))
       (let* ((scrlist (string-to-list scr))
              (guesslist (string-to-list guess))
              (remaining (jlr-difference scrlist guesslist)))
         (when (-contains? remaining (string-to-char ,key))
           (setq jlr-game-data
                 (list (list scr (s-append ,key guess))
                       data))
           (jlr-draw-game))))))

;; TODO: is there a way to clean this up?
(jlr-define-key "a")
(jlr-define-key "b")
(jlr-define-key "c")
(jlr-define-key "d")
(jlr-define-key "e")
(jlr-define-key "f")
(jlr-define-key "g")
(jlr-define-key "h")
(jlr-define-key "i")
(jlr-define-key "j")
(jlr-define-key "k")
(jlr-define-key "l")
(jlr-define-key "m")
(jlr-define-key "n")
(jlr-define-key "o")
(jlr-define-key "p")
(jlr-define-key "q")
(jlr-define-key "r")
(jlr-define-key "s")
(jlr-define-key "t")
(jlr-define-key "u")
(jlr-define-key "v")
(jlr-define-key "w")
(jlr-define-key "x")
(jlr-define-key "y")
(jlr-define-key "z")

;; key bindings
(defvar jumblr-mode-map
  "Keymap for jumblr mode."
  (let ((map (make-keymap)))
    (suppress-keymap map)

    (define-key map (kbd "RET") 'jlr-send-guess)
    (define-key map (kbd "SPC") 'jlr-send-guess)

    (define-key map (kbd "C-c C-q") 'jlr-solve-game)
    (define-key map (kbd "C-c C-r") 'jumblr-new-game)

    (define-key map (kbd "a") 'jlr-insert-a)
    (define-key map (kbd "b") 'jlr-insert-b)
    (define-key map (kbd "c") 'jlr-insert-c)
    (define-key map (kbd "d") 'jlr-insert-d)
    (define-key map (kbd "e") 'jlr-insert-e)
    (define-key map (kbd "f") 'jlr-insert-f)
    (define-key map (kbd "g") 'jlr-insert-g)
    (define-key map (kbd "h") 'jlr-insert-h)
    (define-key map (kbd "i") 'jlr-insert-i)
    (define-key map (kbd "j") 'jlr-insert-j)
    (define-key map (kbd "k") 'jlr-insert-k)
    (define-key map (kbd "l") 'jlr-insert-l)
    (define-key map (kbd "m") 'jlr-insert-m)
    (define-key map (kbd "n") 'jlr-insert-n)
    (define-key map (kbd "o") 'jlr-insert-o)
    (define-key map (kbd "p") 'jlr-insert-p)
    (define-key map (kbd "q") 'jlr-insert-q)
    (define-key map (kbd "r") 'jlr-insert-r)
    (define-key map (kbd "s") 'jlr-insert-s)
    (define-key map (kbd "t") 'jlr-insert-t)
    (define-key map (kbd "u") 'jlr-insert-u)
    (define-key map (kbd "v") 'jlr-insert-v)
    (define-key map (kbd "w") 'jlr-insert-w)
    (define-key map (kbd "x") 'jlr-insert-x)
    (define-key map (kbd "y") 'jlr-insert-y)
    (define-key map (kbd "z") 'jlr-insert-z)

    (define-key map (kbd "DEL") 'jlr-backspace)
    map))

(define-derived-mode jumblr-mode fundamental-mode "jumblr-mode"
  "Major mode for playing the anagram game.

\\<jumblr-mode-map>"
  (use-local-map jumblr-mode-map)
  (read-only-mode t))



;;; data structure for the game
;; The structure is
;;       ((scramble guess) ((subword1 nil) (subword2 nil)...))
;; where scramble is a random permutation of the word, guess is the
;; user's current guess, and the following list contains all of the
;; subwords which must be guessed.  The second element may either be
;; nil (meaning the word hasn't been guessed), t (meaning the word has
;; been guessed), or -1 (meaning the user has given up and used
;; jlr-solve-game to show the answer.
;;
(defvar jlr-game-data nil
  "Data for the jumblr game.")

(defun jlr-create-game-data (word)
  "Populate `jlr-game-data'.  See comments in source above its definition."
  (let* ((words (--sort (< (length it) (length other))
                        (jlr-get-subwords word)))
         (data (--map (list it nil) words))
         (datasrt (--map (--sort (string< (car it) (car other)) it)
                         (--partition-by (length (car it)) data))))
    (list (list (jlr-scramble-word word) "")
          (loop for element in datasrt
                append element))))

;; printing and formatting
(defun jlr-pad-column (str)
  (s-pad-right jumblr-col-width " " str))

(defun jlr-prettify (data)
  (--map (cond ((equal (cadr it) t)
                (propertize (car it) 'face 'jlr-correct-face))
               ((equal (cadr it) -1)
                (propertize (car it) 'face 'jlr-cheat-face))
               (t
                (propertize (make-string (length (car it)) ?-) 'face 'jlr-blank-face)))
         data))

(defun jlr-format-data (data)
  "print the words in neat columns with fixed width."
  (let* ((entries (jlr-prettify data))
         (cols (-partition-all jumblr-col-height (-flatten entries)))
         rows)
    (setq rows
          (loop for n to jumblr-col-height
                collect (mapconcat 'jlr-pad-column
                                   (loop for col in cols
                                         collect (nth n col)) " ")))
    (mapconcat 'identity rows "\n")))

(defun jlr-draw-game ()
  "print the game data structure for each step in the loop."
  (let* ((inhibit-read-only t)
         (jlr-scramble-word (caar jlr-game-data))
         (guess (cadr (car jlr-game-data)))
         (data (jlr-format-data (cadr jlr-game-data)))
         (remaining (jlr-difference
                     (string-to-list jlr-scramble-word)
                     (string-to-list guess))))
    (with-current-buffer (get-buffer-create "Jumblr")
      (erase-buffer)
      (insert
       (propertize (concat (mapconcat 'identity (-map 'string remaining) "") " ")
                   'face 'jlr-scrable-face)
       "\n\n" "SPC to shuffle "
       "\n"   "type a guess, RET or SPC to submit "
       "\n"   "C-c C-q to give up, C-c C-r to start a new game"
       "\n\n" data
       "\n\n" (propertize guess 'face 'jlr-guess-face)))))



;;; game functions
(defun jlr-try-word (word)
  "check whether word exists in the game data structure.  update
if so."
  (let ((scr (caar jlr-game-data))
        (data (cadr jlr-game-data))
        (try (list word nil)))
    (when (-contains? data try)
      (let ((ind (-elem-index try data)))
        (setq data (remove try data))
        (setq data (-insert-at ind (list word t) data))))
    (setq jlr-game-data
          (list (list (jlr-scramble-word scr) "")
                data))))

(defun jlr-send-guess ()
  (interactive)
  (let ((word (cadr (car jlr-game-data))))
    (jlr-try-word word)
    (jlr-try-word (s-append "s" word))
    (jlr-try-word (s-chop-suffix "s" word))
    (jlr-draw-game)))

;;;###autoload
(defalias 'jumblr 'jumblr-new-game)

;;;###autoload
(defun jumblr-new-game ()
  "Launch a new jumblr game."
  (interactive)
  (let* ((words (--filter (and (>= (length it) 5) (<= (length it) 7)) word-list))
         (n (random (length words)))
         (word (nth n words)))
    (setq jlr-game-data (jlr-create-game-data word))
    (jlr-draw-game)
    (switch-to-buffer "Jumblr")
    (jumblr-mode)))

(defun jlr-solve-game ()
  "End the game and fill in the remaining blanks on the board."
  (interactive)
  (let* ((scr (caar jlr-game-data))
         (data (cadr jlr-game-data))
         (newdata (--map (list (car it) (if (cadr it) t -1)) data)))
    (setq jlr-game-data (list (list scr "") newdata)))
  (jlr-draw-game))

(provide 'jumblr)

;;; jumblr.el ends here
