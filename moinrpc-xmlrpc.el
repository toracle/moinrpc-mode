;;; package --- moinmoin xml-rpc client
;;; Commentary: 

;;; Code: 

(require 'moinrpc-conf)

;; Raw wiki-xmlrpc functions

(defvar *moinrpc-error-causes*
  '(("No such page was found." . :NOT-FOUND)
    ("Invalid token." . :INVALID-TOKEN)
    ("Empty token." . :EMPTY_TOKEN)))

(defun moinrpc-xmlrpc-error-cause-to-type (s)
  "Convert S to type."
  (assoc-default s *moinrpc-error-causes* #'string=))

(defun moinrpc-xmlrpc-response-valid-p (response)
  "Return whether RESPONSE is valid or not."
  (let* ((response-status (car response))
	 (unwrapped (car response-status)))
    (string= unwrapped "SUCCESS")))

;; (defun moinrpc-response-error-p (response)
;;   "Return whether RESPONSE is for error or not."
;;   (let* ((response-status (cdr response))
;; 	 (fault-code (assoc "faultCode" (car response-status))))
;;     (if (and fault-code
;; 	     (= 1 (cdr fault-code)))
;; 	t)))

(defun moinrpc-xmlrpc-response-error-type (response)
  "Fetch error cause from RESPONSE."
  (let* ((response-status (cdr response))
	 (fault-string (cdr (assoc "faultString" (car response-status)))))
    (moinrpc-xmlrpc-error-cause-to-type fault-string)))

(defun moinrpc-ask-token-and-save (response wiki)
  "Get auth token of WIKI and save it to file."
  (moinrpc-xmlrpc-get-auth-token wiki)
  (moinrpc-save-wiki-settings))

(defun moinrpc-xmlrpc-check-response (response wiki on-error)
  ""
  (if (moinrpc-xmlrpc-response-valid-p response)
      (let ((error-type (moinrpc-xmlrpc-response-error-type response)))
	(pcase error-type
	  (:INVALID-TOKEN (apply on-error response wiki))
	  (_ t)))))

(defun moinrpc-xmlrpc-encode-multi-each-method (method-name &rest params)
  (list
   (cons "methodName" method-name)
   (cons "params" (vconcat params))))

(defun moinrpc-xmlrpc-encode-multi-method (wiki method-name &rest params)
  "Construct and encode multi method call type to WIKI with a METHOD-NAME and PARAMS."
  (list
   (moinrpc-xmlrpc-encode-multi-each-method "applyAuthToken"
					     (moinrpc-get-wiki-conf wiki 'xmlrpc-api-token))
   (apply 'moinrpc-xmlrpc-encode-multi-each-method method-name params)))


(defun moinrpc-xmlrpc-unwrap-response (response)
  (caar (cdr response)))


(defun moinrpc-xmlrpc-multi-method-call (wiki method-name &rest params)
  "XML-RPC method call to WIKI with a METHOD-NAME and PARAMS."
  (let ((response nil)
	(url (moinrpc-get-wiki-conf wiki 'xmlrpc-endpoint ))
	(call-message (apply 'moinrpc-xmlrpc-encode-multi-method
			     wiki
			     method-name
			     params)))
    (setq response
	  (xml-rpc-method-call url
			       'system.multicall
			       call-message))

    (moinrpc-xmlrpc-check-response response wiki #'moinrpc-ask-token-and-save)
    (moinrpc-xmlrpc-unwrap-response response)))

(defun moinrpc-set-auth-token-to-current (token wiki-setting)
  "Set access TOKEN to WIKI-SETTING."
    (setq wiki-setting (assq-delete-all 'xmlrpc-api-token wiki-setting))
    (add-to-list 'wiki-setting (cons 'xmlrpc-api-token token))
    )

(defun moinrpc-xmlrpc-get-auth-token (wiki)
  "Prompt password and get access token of given WIKI using it."
  (let
      ((password (read-passwd "Password: ")))
    (xml-rpc-method-call (moinrpc-get-wiki-conf wiki 'xmlrpc-endpoint)
			 'getAuthToken
			 (moinrpc-get-wiki-conf wiki 'username)
			 password)))
    
(defun moinrpc-xmlrpc-get-page (wiki pagename)
  "Return raw wiki content string of a page.
Specify WIKI with a PAGENAME."
  (moinrpc-xmlrpc-multi-method-call wiki
				  "getPage"
				  pagename))

(defun moinrpc-xmlrpc-put-page (wiki pagename content)
  "Save WIKI content with a PAGENAME and CONTENT."
  (moinrpc-xmlrpc-multi-method-call wiki
				  "putPage"
				  pagename
				  content))

(defun moinrpc-xmlrpc-get-all-pages (wiki)
  "Return a list of all page names from WIKI."
  (let*
      ((content (moinrpc-xmlrpc-multi-method-call wiki "getAllPages"))
       (sorted-content (sort content 'string<)))
    sorted-content))


(defun moinrpc-xmlrpc-get-recent-changes (wiki &optional timestamp)
  (let ((since timestamp))
    (unless timestamp
      (setq since (time-subtract (current-time) (* 3600 24 7))))
    (moinrpc-xmlrpc-multi-method-call wiki
                                       "getRecentChanges"
                                       (list :datetime since))))


(defun moinrpc-xmlrpc-list-attachments (wiki pagename)
  "Return attachment list of a page has a PAGENAME from WIKI."
  (moinrpc-xmlrpc-multi-method-call wiki
                                     "listAttachments"
                                     pagename))


(defun moinrpc-xmlrpc-put-attachment (wiki pagename name content)
  "Add an attachment FILE to a page has a PAGENAME from WIKI."
  (moinrpc-xmlrpc-multi-method-call wiki
                                     "putAttachment"
                                     pagename
                                     name
                                     (list :base64 content)))


(defun moinrpc-xmlrpc-delete-attachment (wiki pagename name)
  "Delete an attachment has NAME from a page has a PAGENAME from WIKI."
  (moinrpc-xmlrpc-multi-method-call wiki
                                     "deleteAttachment"
                                     pagename
                                     name))


(provide 'moinrpc-xmlrpc)
;;; moinrpc-xmlrpc.el ends here
