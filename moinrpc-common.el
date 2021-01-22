;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Common codes

(defvar moinrpc-buffer-name-wiki-format )


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


(provide 'moinrpc-common)
