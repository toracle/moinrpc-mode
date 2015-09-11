;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Buffer related

(require 'moinrpc-xmlrpc)

(defvar moinrpc-buffer-prefix "*moin: ")
(defvar moinrpc-buffer-postfix "*")

(defvar moinrpc-xmlrpc-content-provider
  '((:initialize . nil)
    (:get-page . moinrpc-get-page-content)
    (:get-list . moinrpc-get-list-content)
    (:save-page . moinrpc-save-page-content)))

(defvar moinrpc-dummy-content-provider
  '((:initialize . nil)
    (:get-page . nil)
    (:get-list . nil)
    (:save-page . nil)))

(defvar moinrpc-content-provider moinrpc-xmlrpc-content-provider)

(defun moinrpc-buffer-name (pagename)
  "Construct a buffer name of wiki PAGENAME."
  (concat moinrpc-buffer-prefix pagename moinrpc-buffer-postfix))

(defun moinrpc-strip-text-properties (txt)
  "Remove all text properties of TXT."
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun moinrpc-create-page-buffer (wiki pagename)
  "Create a buffer for a WIKI page which has a PAGENAME."
  (with-current-buffer
      (get-buffer-create (moinrpc-buffer-name pagename))
    (setq moinrpc-buffer-local-current-wiki wiki)
    (setq moinrpc-buffer-local-current-pagename pagename)
    (erase-buffer)
    (moinrpc-page-mode)
    (switch-to-buffer (current-buffer))
    (current-buffer)))

(defun moinrpc-fill-page-buffer-content (buffer current-wiki current-pagename)
  "Fill a BUFFER with content of CURRENT-WIKI CURRENT-PAGENAME."
  (with-current-buffer
      buffer
    (let
	((get-page-function (cdr (assoc :get-page moinrpc-content-provider)))
	 (content nil))
      (setq content (funcall get-page-function current-wiki current-pagename))
      (insert content)
      (set-buffer-modified-p nil))))

(defun moinrpc-create-list-buffer (wiki)
  "Create WIKI page list buffer."
  (with-current-buffer
      (get-buffer-create (moinrpc-buffer-name "List Pages"))
    (let
	((list-function (cdr (assoc :list-page moinrpc-content-provider))))
      (read-only-mode -1)
      (erase-buffer)
      (setq moinrpc-buffer-local-current-wiki wiki)
      (insert
       (mapconcat 'concat (funcall list-function moinrpc-buffer-local-current-wiki) "\n"))
      (read-only-mode)
      (current-buffer))))

(defun moinrpc-create-main-buffer ()
  "Create main page buffer.  List up wiki list."
  (with-current-buffer
      (get-buffer-create "*moinrpc*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (insert (format "%S" moinrpc-wiki-settings))
    (read-only-mode)
  ))

(defun moinrpc-save-current-buffer ()
  "Save current buffer to remote wiki."
  (interactive)
  (let
      ((save-page-function (cdr (assoc :save-page moinrpc-content-provider))))
    (funcall save-page-function
	     moinrpc-buffer-local-current-wiki
	     moinrpc-buffer-local-current-pagename
	     (moinrpc-strip-text-properties (buffer-string)))
    (set-buffer-modified-p nil)
    (current-buffer)))

(defun moinrpc-get-or-create-page-buffer (pagename)
  "Get a page buffer which have a PAGENAME or create one if there is not exist."
  (let
      ((buffer-name (moinrpc-buffer-name pagename))
       (buffer nil))
    (if
	(null (get-buffer buffer-name))
	(progn
	  (setq buffer (moinrpc-create-page-buffer moinrpc-buffer-local-current-wiki pagename))
	  (moinrpc-fill-page-buffer-content buffer moinrpc-buffer-local-current-wiki pagename))
      (setq buffer (get-buffer (moinrpc-buffer-name pagename))))
    (switch-to-buffer buffer)))

(defun moinrpc-find-page ()
  "Find a page with name."
  (interactive)
  (let
      ((pagename (read-string "Find page: ")))
    (moinrpc-get-or-create-page-buffer pagename)))

(defun moinrpc-create-page-buffer-helm (pagename)
  "Create a buffer for a wiki page of PAGENAME."
  (let
      ((buffer nil))
    (setq buffer (moinrpc-create-page-buffer moinrpc-buffer-local-current-wiki pagename))
    (moinrpc-fill-page-buffer-content buffer)))

(defun helm-moinrpc-find-page ()
  "Find page using helm."
  (interactive)
  (let
      ((get-list-function (cdr (assoc :get-list moinrpc-content-provider)))
       (all-pages nil))
    (setq all-pages (funcall get-list-function moinrpc-buffer-local-current-wiki))
    (helm :sources '(
		     ((name . "All wiki pages")
		      (candidates . all-pages)
		      (action . (("Open" . moinrpc-get-or-create-page-buffer))))
		     ((name . "fallback")
		      (dummy)
		      (action . (("Create" . moinrpc-create-page-buffer-helm)))))
	  :prompt "Find Page: "
	  :buffer "*helm-moinrpc-find-pages*"
	  )))

(defun moinrpc-helm-find-page (button)
  "BUTTON."
  (let
      (
       (wiki-alias (button-label button))
       (current-wiki nil))
    (setq moinrpc-buffer-local-current-wiki (cdr (assoc wiki-alias moinrpc-wiki-settings)))
    (helm-moinrpc-find-page)))

(defun helm-moinrpc-get-or-create-page-buffer (candidate)
  "Get or create a page buffer which have a name equals to CANDIDATE."
  (stringp candidate)
  (moinrpc-get-or-create-page-buffer candidate)
  )

(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
