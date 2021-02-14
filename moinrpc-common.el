;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Common codes

(defvar moinrpc-buffer-name-wiki-format )


(defvar moinrpc-regex-bracket-wikilink
  "\\[\\[[^|]+?\\]\\]")


(defvar moinrpc-regex-wikilink
  "\\([A-Z][a-z]+\\)\\{2,\\}?")


(defun moinrpc-wiki-name (wiki)
  (cdr (assoc 'wiki-alias wiki)))


(defun moinrpc-buffer-name (pagename &optional wiki)
  "Construct a buffer name of wiki PAGENAME."
  (cond ((and (null wiki) (null pagename))
         "*moin*")
        ((and wiki (null pagename))
         (format "*moin:%s*" (moinrpc-wiki-name wiki)))
        ((and pagename (null wiki))
         (format "*moin:%s*" pagename))
        (t
         (format "*moin:%s:%s*" (moinrpc-wiki-name wiki) pagename))))


(defun moinrpc-strip-text-properties (txt)
  "Remove all text properties of TXT."
  (set-text-properties 0 (length txt) nil txt)
  txt)


(defun moinrpc-get-overlay-text (overlay)
  (buffer-substring (overlay-start overlay)
                    (overlay-end overlay)))


(defun moinrpc-wikilink-p (&optional word)
  "."
  (if (null word)
      (thing-at-point-looking-at moinrpc-regex-wikilink
                                 100)
    (string-match-p (format "^%s$" moinrpc-regex-wikilink)
                    word)))
  

(defun moinrpc-bracket-wikilink-p (&optional word)
  "."
  (if (null word)
      (thing-at-point-looking-at moinrpc-regex-bracket-wikilink
                                 100)
    (string-match-p (format "^%s$" moinrpc-regex-bracket-wikilink)
                    word)))


(provide 'moinrpc-common)
