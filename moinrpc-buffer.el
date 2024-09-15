;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Buffer related

(require 'moinrpc-common)
(require 'moinrpc-conf)
(require 'moinrpc-xmlrpc)
(require 'moinrpc-render)
(require 'subr-x)


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


(defun moinrpc-upload-attachment (&optional filename show-list-page)
  (interactive)
  (let* ((filename (if filename filename
                     (read-file-name "Select a file to upload:")))
         (name (file-name-nondirectory filename))
         (content (moinrpc-read-file filename)))
    (moinrpc-xmlrpc-put-attachment moinrpc-current-wiki
                            moinrpc-current-pagename
                            name
                            content)
    (when show-list-page
      (moinrpc-list-attachments))))


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


(defun moinrpc-search-pages ()
  (interactive)
  (let
      ((query-string (read-string "Search: ")))
    (let* ((wiki moinrpc-current-wiki)
           (content (moinrpc-xmlrpc-search-pages wiki query-string))
           (buffer (moinrpc-buffer-name (format "Search [%s]" query-string) wiki)))
      (switch-to-buffer buffer)
      (moinrpc-render-search buffer query-string content wiki))))


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


(defun moinrpc-table-find-edge (&optional backward)
  (let ((m (point-marker))
        (position nil)
        (moves 0)
        (delta (if backward 1 -1))
        (line-function (if backward
                           #'end-of-line
                         #'beginning-of-line)))
    (while (and (= moves 0)
                (moinrpc-table-p))
      (funcall line-function)
      (setq position (point-marker))
      (setq moves (forward-line delta)))
    (goto-char m)
    position))


(defun moinrpc-table-range ()
  (cons (moinrpc-table-find-edge)
        (moinrpc-table-find-edge t)))


(defun moinrpc-table-p ()
  (let ((m (point-marker)))
    (beginning-of-line)
    (let ((result (looking-at (format "^%s$"
                                      moinrpc-regex-table))))
      (goto-char m)
      result)))


(defun moinrpc-table-parse ()
  (let* ((range (moinrpc-table-range))
         (start (car range))
         (end (cdr range)))
    (map 'list
         #'moinrpc-table-parse-line
         (split-string (buffer-substring-no-properties start end)
                       "\n"))))


(defun moinrpc-table-parse-line (line)
  (mapcar #'string-trim
          (butlast (cdr (split-string line "||")))))


(defun moinrpc-table-columns-width (table)
  (let* ((dim (moinrpc-table-dimension table))
         (x (cdr dim))
         (widths (make-list x 0)))
    (dolist (row table)
      (dotimes (col-idx (length row))
        (setf (nth col-idx widths)
              (max (nth col-idx widths)
                   (string-width (string-trim (nth col-idx row)))))))
    widths))


(defun moinrpc-table-dimension (table)
  (let ((x (length table))
        (y 0))
    (dolist (row table)
      (when (< y (length row))
        (setq y (length row))))
    (cons x y)))


(defun moinrpc-table-delete ()
  (let* ((range (moinrpc-table-range))
         (start (car range))
         (end (cdr range)))
    (delete-region start end)))


(defun moinrpc-table-render (table)
  (let* ((widths (moinrpc-table-columns-width table))
         (dim (moinrpc-table-dimension table)))
      (dolist (row table)
     (insert "||")
     (dotimes (idx-col (cdr dim))
       (insert (format (format "%%-%ds" (nth idx-col widths))
                       (or (nth idx-col row) "")))
       (insert "||"))
     (newline))))


(defun moinrpc-table-format ()
  (when (moinrpc-table-p)
    (let ((table (moinrpc-table-parse)))
      (moinrpc-table-delete)
      (moinrpc-table-render table))))


(defun moinrpc-cycle ()
  (interactive)
  (if (moinrpc-table-p)
      (moinrpc-table-format)
    (indent-for-tab-command)))


(defun moinrpc-get-clipboard-image-target ()
  (let* ((targets (gui-get-selection 'CLIPBOARD 'TARGETS))
         (image-targets (seq-filter '(lambda (x)
                                       (or (equal x 'image/png)
					   (equal x 'image/tiff)
                                           (equal x 'PNG)))
                                    targets)))
    (cond (image-targets
           (first image-targets))
          ((and (not targets) (eq system-type 'darwin)) t))))


(defun moinrpc-clipboard-image-p ()
  (not (equal (moinrpc-get-clipboard-image-target) nil)))


(defvar powershell-cmd "powershell")

(defvar powershell-cmd "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")

(defun moinrpc-save-clipboard-image-to-file ()
  "Save the image content in the clipboard to a temporary file and return the file path."
  (interactive)
  (if (moinrpc-clipboard-image-p)
      (let* ((temp-file (make-temp-file "moinrpc-clipboard-image-" nil ".png"))
             (shell-command nil)
             (shell-command-string nil))
        (cond ((eq system-type 'darwin)
               (setq shell-command "pngpaste"
                     shell-command-string
                     (format "pngpaste %s" temp-file)))
              ((eq system-type 'gnu/linux)
               (setq shell-command "xclip"
                     shell-command-string
                     (format "xclip -selection clipboard -t image/png -o | convert - %s" temp-file)))
              ((eq system-type 'windows-nt)
               (setq shell-command powershell-cmd
                     shell-command-string
                     (format "%s -command \"$img = Get-Clipboard -Format Image; $img.save(\\\"%s\\\");\"" powershell-cmd temp-file)))
              (t
               (message "Unsupported system type: %s" system-type)
               nil))
        (when shell-command-string
          (unless (executable-find shell-command)
            (error (format "Can not find screenshot executable. Please install it first: %s" shell-command)))
          (message (format "Execute clipboard save command: %s" shell-command-string))
          (shell-command shell-command-string
                         "*moinrpc-clipboard-shell-output*"
                         "*moinrpc-clipboard-shell-error*")
          (message "Saved clipboard image to %s" temp-file)
          temp-file))
    (message "No image in clipboard")
    nil))


(defun moinrpc-yank ()
  (interactive)
  (if (moinrpc-clipboard-image-p)
      (let* ((filename (moinrpc-save-clipboard-image-to-file))
             (basename (file-name-nondirectory filename)))
        (moinrpc-upload-attachment filename nil)
        (insert (format "{{attachment:%s}}" basename))
        (when filename
          (delete-file filename)))
    (yank)))


(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
