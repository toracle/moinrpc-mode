;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Buffer related

(require 'moinrpc-common)
(require 'moinrpc-xmlrpc)

(defun print-current-buffer-local (location)
  "Print current buffer local var values of LOCATION for debug."
  ; (message (format "%s: current-pagename=%s, current-wiki=%s" location moinrpc-buffer-local-current-pagename moinrpc-buffer-local-current-wiki)))
  )

(defun moinrpc-create-page-buffer (wiki pagename)
  "Create a buffer for a WIKI page which has a PAGENAME."
  (with-current-buffer (get-buffer-create (moinrpc-buffer-name pagename))
    (moinrpc-page-mode)
    (erase-buffer)
    (setq moinrpc-buffer-local-current-wiki wiki)
    (setq moinrpc-buffer-local-current-pagename pagename)
    (switch-to-buffer (current-buffer))
    (print-current-buffer-local "create-page-buffer")
    (current-buffer)))

(defun moinrpc-fill-page-buffer-content (buffer current-wiki current-pagename)
  "Fill a BUFFER with content from CURRENT-WIKI CURRENT-PAGENAME."
  (with-current-buffer buffer
    (let ((get-page-function (cdr (assoc :get-page *moinrpc-content-provider*)))
          (content nil))
      (setq content (funcall get-page-function current-wiki current-pagename))
      (insert content)
      (setq moinrpc-buffer-local-current-wiki current-wiki)
      (setq moinrpc-buffer-local-current-pagename current-pagename)
      (print-current-buffer-local "fill-page-buffer-content")
      (set-buffer-modified-p nil))))

(defun moinrpc-get-or-create-page-buffer (pagename)
  "Get a page buffer which have a PAGENAME or create one if there is not exist."
  (let ((buffer-name (moinrpc-buffer-name pagename))
        (buffer nil))
    (if (not (setq buffer (get-buffer buffer-name)))
	(progn (setq buffer (moinrpc-create-page-buffer moinrpc-buffer-local-current-wiki pagename))
               (moinrpc-fill-page-buffer-content buffer moinrpc-buffer-local-current-wiki pagename))
      (setq buffer (get-buffer (moinrpc-buffer-name pagename))))
      (print-current-buffer-local "get-or-create-page-buffer")
    (switch-to-buffer buffer)))

(defun moinrpc-create-list-buffer (wiki)
  "Create WIKI page list buffer."
  (with-current-buffer (get-buffer-create (moinrpc-buffer-name "List Pages"))
    (let ((list-function (cdr (assoc :list-page
                                     *moinrpc-content-provider*))))
      (read-only-mode -1)
      (erase-buffer)
      (setq moinrpc-buffer-local-current-wiki wiki)
      (insert (mapconcat 'concat
                         (funcall list-function moinrpc-buffer-local-current-wiki)
                         "\n"))
      (read-only-mode)
      (current-buffer))))

(defun moinrpc-save-current-buffer ()
  "Save current buffer to remote wiki."
  (interactive)
  (let ((save-page-function (cdr (assoc :save-page
                                        *moinrpc-content-provider*))))
    (funcall save-page-function
	     moinrpc-buffer-local-current-wiki
	     moinrpc-buffer-local-current-pagename
	     (moinrpc-strip-text-properties (buffer-string)))
    (set-buffer-modified-p nil)
    (print-current-buffer-local "save-current-buffer")
    (current-buffer)))

(defun moinrpc-create-main-buffer ()
  "Create main page buffer.  List up wiki list."
  (with-current-buffer (get-buffer-create "*moinrpc*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (insert (format "%S" *moinrpc-wiki-settings*))
    (read-only-mode)
    (print-current-buffer-local "create-main-buffer")))

(defun moinrpc-find-page ()
  "Find a page with name."
  (interactive)
  (let ((pagename (read-string "Find page: ")))
    (moinrpc-get-or-create-page-buffer pagename)))

(defun helm-moinrpc-find-page ()
  "Find page using helm."
  (interactive)
  (let ((get-list-function (cdr (assoc :get-list
                                       *moinrpc-content-provider*)))
        (all-pages nil))
    (print-current-buffer-local "helm-moinrpc-find-page")
    (setq all-pages (funcall get-list-function
                             moinrpc-buffer-local-current-wiki))
    (helm :sources '(
		     ((name . "All wiki pages")
		      (candidates . all-pages)
		      (action . (("Open" . moinrpc-get-or-create-page-buffer))))
		     ((name . "fallback")
		      (dummy)
		      (action . (("Create" . moinrpc-get-or-create-page-buffer)))))
	  :prompt "Find Page: "
	  :buffer "*helm-moinrpc-find-pages*"
	  )))

(defun moinrpc-helm-find-page (button)
  "BUTTON."
  (let ((wiki-alias (button-label button)))
    (setq moinrpc-buffer-local-current-wiki (cdr (assoc wiki-alias *moinrpc-wiki-settings*)))
    (print-current-buffer-local "helm-find-page")
    (helm-moinrpc-find-page)))

(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
