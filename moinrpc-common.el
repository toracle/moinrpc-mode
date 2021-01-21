;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Common codes

(defvar moinrpc-buffer-name-format "*moin: %s*")

(defun moinrpc-buffer-name (pagename)
  "Construct a buffer name of wiki PAGENAME."
  (format moinrpc-buffer-name-format pagename))

(defun moinrpc-strip-text-properties (txt)
  "Remove all text properties of TXT."
  (set-text-properties 0 (length txt) nil txt)
  txt)

(provide 'moinrpc-common)
