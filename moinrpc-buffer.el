;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Buffer related

(require 'moinrpc-common)
(require 'moinrpc-xmlrpc)
(require 'moinrpc-buffer-helper)


(defun moinrpc-recent-changes ()
  (interactive)
  (let* ((wiki moinrpc-buffer-local-current-wiki)
         (content (moinrpc-xmlrpc-recent-changes wiki))
         (buffer (moinrpc-buffer-name "RecentChanges")))
    (switch-to-buffer buffer)
    (moinrpc-buffer-recent-changes buffer content wiki)))


(defun moinrpc-fill-list-attachment ()
  (interactive)
  (let ((wiki moinrpc-buffer-local-current-wiki)
        (pagename moinrpc-buffer-local-current-pagename))
    (with-current-buffer
        (get-buffer-create (moinrpc-buffer-name (format "%s:attachments"
                                                        pagename)))
        (read-only-mode -1)
      (erase-buffer)
      (print-current-buffer-local "create-attachment-list-buffer")
      (let ((entries (moinrpc-get-attachment-list moinrpc-buffer-local-current-wiki
                                                  moinrpc-buffer-local-current-pagename)))
        (insert "Attachment List:")
        (newline)
        (newline)
        (dolist (entry entries)
          (insert " * ")
          (insert-button entry)
          (newline)))
      (goto-char 1)
      (read-only-mode))))


(defun moinrpc-list-attachment ()
  (interactive)
  (let ((wiki moinrpc-buffer-local-current-wiki)
        (pagename moinrpc-buffer-local-current-pagename))
    (with-current-buffer
        (get-buffer-create (moinrpc-buffer-name (format "%s:attachments"
                                                        pagename)))
      (switch-to-buffer (current-buffer))
      (moinrpc-attachment-mode)
      (setq-local moinrpc-buffer-local-list-type :attachment-list)
      (setq-local moinrpc-buffer-local-current-wiki wiki)
      (setq-local moinrpc-buffer-local-current-pagename pagename)
      (moinrpc-fill-list-attachment))))


(defun moinrpc-read-file-as-base64 (filename)
  "Read a file from PATH and encode it to base64."
  (with-temp-buffer
    (insert-file-contents filename nil nil nil t)
    (buffer-string)))


(defun moinrpc-upload-attachment ()
  (interactive)
  (let* ((filename (read-file-name "Select a file to upload:"))
         (name (file-name-nondirectory filename))
         (content (moinrpc-read-file-as-base64 filename)))
    (moinrpc-put-attachment moinrpc-buffer-local-current-wiki
                            moinrpc-buffer-local-current-pagename
                            name
                            content)
    (moinrpc-fill-list-attachment)))


(defun moinrpc-delete-attachment ()
  (interactive)
  (let* ((overlay (car (overlays-at (point))))
         (name (buffer-substring (overlay-start overlay)
                                 (overlay-end overlay))))
    (moinrpc-xmlrpc-delete-attachment moinrpc-buffer-local-current-wiki
                                      moinrpc-buffer-local-current-pagename
                                      name)
    (moinrpc-fill-list-attachment)))


(defun moinrpc-find-page ()
  "Find a page with name."
  (interactive)
  (let
      ((pagename (read-string "Find page: ")))
    (moinrpc-get-or-create-page-buffer pagename)))

(defun helm-moinrpc-find-page ()
  "Find page using helm."
  (interactive)
  (let
      ((all-pages (moinrpc-get-list-content moinrpc-buffer-local-current-wiki)))
    (print-current-buffer-local "helm-moinrpc-find-page")
    (helm :sources
          '(((name . "All wiki pages")
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
  (let
      ((wiki-alias (button-label button)))
    (setq moinrpc-buffer-local-current-wiki
          (cdr (assoc wiki-alias *moinrpc-wiki-settings*)))
    (print-current-buffer-local "helm-find-page")
    (helm-moinrpc-find-page)))

(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
