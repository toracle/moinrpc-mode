;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Buffer related

(require 'moinrpc-common)
(require 'moinrpc-conf)
(require 'moinrpc-xmlrpc)
(require 'moinrpc-render)


(defun moinrpc-create-wiki-setting-i ()
  "."
  (interactive)
  (let* ((wiki-alias (read-string "Wiki alias: "))
         (xmlrpc-endpoint (read-string "XML-RPC endpoint: "))
         (username (read-string "Username: ")))
    (moinrpc-create-wiki-setting wiki-alias
                                 xmlrpc-endpoint
                                 username)))


(defun moinrpc-wikilink-at-point ()
  "."
  (cond ((moinrpc-bracket-wikilink-p)
         (let ((wikilink-bracket (buffer-substring (match-beginning 0)
                                                   (match-end 0))))
           (substring wikilink-bracket 2 -2)))
        ((moinrpc-wikilink-p)
         (buffer-substring (match-beginning 0)
                           (match-end 0)))))


(defun moinrpc-rel-wikilink-to-abs (wikilink parent)
  (if (or (s-starts-with? "/" wikilink)
          (s-starts-with? ".." wikilink))
      (format "%s%s" parent wikilink)
    wikilink))


(defun moinrpc-open-wikilink-at-point ()
  "."
  (interactive)
  (let ((wikilink (moinrpc-wikilink-at-point))
        (pagename nil))
    (when wikilink
      (moinrpc-open-page (moinrpc-rel-wikilink-to-abs
                          wikilink
                          moinrpc-current-pagename)))))


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


(defun moinrpc-recent-changes (&optional last-modified)
  (interactive)
  (let* ((wiki moinrpc-current-wiki)
         (content (moinrpc-xmlrpc-get-recent-changes wiki last-modified))
         (buffer (moinrpc-buffer-name "RecentChanges" wiki)))
    (switch-to-buffer buffer)
    (moinrpc-render-recent-changes buffer content wiki)))


(defun moinrpc-list-attachments ()
  (interactive)
  (let* ((wiki moinrpc-current-wiki)
         (pagename moinrpc-current-pagename)
         (content (moinrpc-xmlrpc-list-attachments wiki pagename))
         (buffer-name (moinrpc-buffer-name (format "%s:attachments"
                                                   pagename) wiki))
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (moinrpc-render-list-attachment buffer pagename content wiki)))


(defun moinrpc-open-page (pagename)
  (let* ((wiki moinrpc-current-wiki)
         (buffer-name (moinrpc-buffer-name pagename wiki))
         (buffer (get-buffer-create buffer-name))
         (content (moinrpc-xmlrpc-get-page wiki
                                           pagename)))
    (switch-to-buffer buffer)
    (moinrpc-render-page buffer pagename content wiki)))


(defun moinrpc-save-page ()
  "Save current buffer to remote wiki."
  (interactive)
  (moinrpc-xmlrpc-put-page moinrpc-current-wiki
                           moinrpc-current-pagename
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
    (moinrpc-xmlrpc-put-attachment moinrpc-current-wiki
                            moinrpc-current-pagename
                            name
                            content)
    (moinrpc-list-attachments)))


(defun moinrpc-delete-attachment ()
  (let* ((overlay (car (overlays-at (point))))
         (name (moinrpc-get-overlay-text overlay)))
    (moinrpc-xmlrpc-delete-attachment moinrpc-current-wiki
                                      moinrpc-current-pagename
                                      name)
    (moinrpc-list-attachments)))


(defun moinrpc-find-page ()
  "Find a page with name."
  (interactive)
  (let
      ((pagename (read-string "Open page: ")))
    (moinrpc-open-page pagename)))


(defun moinrpc-search-backlinks ()
  (interactive)
  (let* ((wiki moinrpc-current-wiki)
         (pagename moinrpc-current-pagename)
         (content (moinrpc-xmlrpc-search-backlinks wiki pagename))
         (buffer (moinrpc-buffer-name (format "Search [linkto:%s]" pagename) wiki)))
    (switch-to-buffer buffer)
    (moinrpc-render-search buffer pagename content wiki)))


(defun moinrpc-helm-find-page ()
  "Find page using helm."
  (interactive)
  (let
      ((all-pages (moinrpc-xmlrpc-get-all-pages moinrpc-current-wiki)))
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


(defun moinrpc-insert-wikilink ()
  (interactive)
  (let
      ((all-pages (moinrpc-xmlrpc-get-all-pages moinrpc-current-wiki)))
    (helm :sources
          '(((name . "All wiki pages")
             (candidates . all-pages)
             (action . (("Insert" . moinrpc-render-insert-link))))
            ((name . "fallback")
             (dummy)
             (action . (("Insert" . moinrpc-render-insert-link)))))
          :prompt "Select Page: "
          :buffer "*helm-moinrpc-find-pages*")))


(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
