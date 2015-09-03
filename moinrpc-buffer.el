;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Buffer related


(defvar moinrpc-buffer-prefix "*moin: ")
(defvar moinrpc-buffer-postfix "*")

(defun moinrpc-buffer-name (pagename)
  "Return moin buffer name for a PAGENAME."
  (concat moinrpc-buffer-prefix pagename moinrpc-buffer-postfix))

(defun moinrpc-strip-text-properties (txt)
  "TXT."
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun moinrpc-create-page-buffer (wiki pagename)
  "WIKI PAGENAME."
  (with-current-buffer
      (get-buffer-create (moinrpc-buffer-name pagename))
    (make-variable-buffer-local 'current-wiki)
    (make-variable-buffer-local 'current-pagename)
    (setq current-wiki wiki)
    (setq current-pagename pagename)
    (erase-buffer)
    (moinrpc-page-mode)
    (switch-to-buffer (current-buffer))
    (current-buffer)))

(defun moinrpc-fill-page-buffer-content (buffer)
  "BUFFER."
  (with-current-buffer
      buffer
    (insert (moinrpc-get-page-content current-wiki current-pagename))
    (set-buffer-modified-p nil)))

(defun moinrpc-create-list-buffer (wiki)
  "WIKI"
  (with-current-buffer
      (get-buffer-create (moinrpc-buffer-name "List Pages"))
    (read-only-mode -1)
    (erase-buffer)
    (make-variable-buffer-local 'current-wiki)
    (setq current-wiki wiki)
    (insert
     (mapconcat 'concat (moinrpc-get-list-content current-wiki) "\n"))
    (read-only-mode)
    (current-buffer)))

(defun moinrpc-create-main-buffer ()
  "."
  (with-current-buffer
      (get-buffer-create "*moinrpc*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (insert (format "%S" moinrpc-wiki-settings))
    (read-only-mode)
  ))

(defun moinrpc-save-current-buffer ()
  "."
  (interactive)
  (moinrpc-save-page-content current-wiki
			  current-pagename
			  (moinrpc-strip-text-properties (buffer-string)))
  (set-buffer-modified-p nil)
  (current-buffer))

(defun moinrpc-get-or-create-page-buffer (pagename)
  "PAGENAME."
  (let
      ((buffer-name (moinrpc-buffer-name pagename))
       (buffer nil))
    (if
	(null (get-buffer buffer-name))
	(progn
	  (setq buffer (moinrpc-create-page-buffer current-wiki pagename))
	  (moinrpc-fill-page-buffer-content buffer))
      (setq buffer (get-buffer (moinrpc-buffer-name pagename))))
    (switch-to-buffer buffer)))

(defun moinrpc-find-page ()
  "."
  (interactive)
  (let
      ((pagename (read-string "Find page: ")))
    (moinrpc-get-or-create-page-buffer pagename)))

(defun helm-moinrpc-get-or-create-page-buffer (candidate)
  "CANDIDATE."
  (stringp candidate)
  (moinrpc-get-or-create-page-buffer candidate)
  )

(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
