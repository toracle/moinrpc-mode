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
    (moinrpc-render-recent-changes buffer content wiki)))


(defun moinrpc-list-attachments ()
  (interactive)
  (let* ((wiki moinrpc-buffer-local-current-wiki)
         (pagename moinrpc-buffer-local-current-pagename)
         (content (moinrpc-xmlrpc-list-attachments wiki pagename))
         (buffer-name (moinrpc-buffer-name (format "%s:attachments"
                                                   pagename)))
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (moinrpc-buffer-list-attachment buffer pagename content wiki)))


(defun moinrpc-read-file-as-base64 (filename)
  "Read a file from PATH and encode it to base64."
  (with-temp-buffer
    (insert-file-contents filename nil nil nil t)
    (buffer-string)))


(defun moinrpc-upload-attachment ()
  (let* ((filename (read-file-name "Select a file to upload:"))
         (name (file-name-nondirectory filename))
         (content (moinrpc-read-file-as-base64 filename)))
    (moinrpc-xmlrpc-put-attachment moinrpc-buffer-local-current-wiki
                            moinrpc-buffer-local-current-pagename
                            name
                            content)
    (moinrpc-list-attachments)))


(defun moinrpc-delete-attachment ()
  (let* ((overlay (car (overlays-at (point))))
         (name (moinrpc-get-overlay-text overlay)))
    (moinrpc-xmlrpc-delete-attachment moinrpc-buffer-local-current-wiki
                                      moinrpc-buffer-local-current-pagename
                                      name)
    (moinrpc-list-attachments)))


(defun moinrpc-open-page ()
  "Find a page with name."
  (interactive)
  (let
      ((pagename (read-string "Open page: ")))
    (moinrpc-get-or-create-page-buffer pagename)))


(defun moinrpc-helm-find-page ()
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


(defun moinrpc-buffer-enter-wiki (button)
  "BUTTON."
  (let
      ((wiki-alias (button-label button)))
    (setq-local moinrpc-buffer-local-current-wiki
                (cdr (assoc wiki-alias *moinrpc-wiki-settings*)))
    (print-current-buffer-local "helm-find-page")
    (moinrpc-helm-find-page)))

(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
