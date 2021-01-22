;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Buffer related

(require 'moinrpc-common)
(require 'moinrpc-conf)
(require 'moinrpc-xmlrpc)
(require 'moinrpc-render)


(defun moinrpc-main-page ()
  "Create a wiki list buffer."
  (interactive)
  (let ((buffer (get-buffer-create (moinrpc-buffer-name nil)))
        (content (moinrpc-get-keys *moinrpc-wiki-settings*)))
    (switch-to-buffer buffer)
    (moinrpc-render-main-page buffer content)
    t))


(defun moinrpc-wiki-front (button)
  "Create a wiki front buffer."
  (interactive)
  (let* ((wiki-name (button-label button))
         (wiki (cdr (assoc *moinrpc-current-wiki* *moinrpc-wiki-settings*)))
         (buffer (get-buffer-create (moinrpc-buffer-name wiki-name))))
    (switch-to-buffer buffer)
    (moinrpc-render-wiki-front buffer wiki)))


(defun moinrpc-recent-changes ()
  (interactive)
  (let* ((wiki moinrpc-buffer-local-current-wiki)
         (content (moinrpc-xmlrpc-recent-changes wiki))
         (buffer (moinrpc-buffer-name "RecentChanges" wiki)))
    (switch-to-buffer buffer)
    (moinrpc-render-recent-changes buffer content wiki)))


(defun moinrpc-list-attachments ()
  (interactive)
  (let* ((wiki moinrpc-buffer-local-current-wiki)
         (pagename moinrpc-buffer-local-current-pagename)
         (content (moinrpc-xmlrpc-list-attachments wiki pagename))
         (buffer-name (moinrpc-buffer-name (format "%s:attachments"
                                                   pagename) wiki))
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (moinrpc-render-list-attachment buffer pagename content wiki)))


(defun moinrpc-open-page (pagename)
  (let* ((wiki moinrpc-buffer-local-current-wiki)
         (buffer-name (moinrpc-buffer-name pagename wiki))
         (buffer (get-buffer-create buffer-name))
         (content (moinrpc-get-page-content wiki
                                            pagename)))
    (switch-to-buffer buffer)
    (moinrpc-render-page buffer pagename content wiki)))


(defun moinrpc-save-page ()
  "Save current buffer to remote wiki."
  (interactive)
  (moinrpc-save-page-content moinrpc-buffer-local-current-wiki
                             moinrpc-buffer-local-current-pagename
                             (moinrpc-strip-text-properties (buffer-string)))
  (set-buffer-modified-p nil)
  (current-buffer))


(defun moinrpc-read-file (filename)
  "Read a file from PATH and encode it to base64."
  (with-temp-buffer
    (insert-file-contents filename nil nil nil t)
    (buffer-string)))


(defun moinrpc-upload-attachment ()
  (let* ((filename (read-file-name "Select a file to upload:"))
         (name (file-name-nondirectory filename))
         (content (moinrpc-read-file filename)))
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


(defun moinrpc-find-page ()
  "Find a page with name."
  (interactive)
  (let
      ((pagename (read-string "Open page: ")))
    (moinrpc-open-page pagename)))


(defun moinrpc-helm-find-page ()
  "Find page using helm."
  (interactive)
  (let
      ((all-pages (moinrpc-get-list-content moinrpc-buffer-local-current-wiki)))
    (helm :sources
          '(((name . "All wiki pages")
	     (candidates . all-pages)
	     (action . (("Open" . moinrpc-open-page))))
	    ((name . "fallback")
	     (dummy)
	     (action . (("Create" . moinrpc-open-page)))))
	  :prompt "Find Page: "
	  :buffer "*helm-moinrpc-find-pages*"
	  )))


(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
