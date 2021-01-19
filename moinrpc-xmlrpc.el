;;; package --- moinmoin xml-rpc client
;;; Commentary: 

;;; Code: 

(require 'moinrpc-conf)

;; Raw wiki-xmlrpc functions

(defvar *moinrpc-error-causes*
  '(("No such page was found." . :NOT-FOUND)
    ("Invalid token." . :INVALID-TOKEN)
    ("Empty token." . :EMPTY_TOKEN)))

(defun moinrpc-error-cause-to-type (s)
  "Convert S to type."
  (assoc-default s *moinrpc-error-causes* #'string=))

(defun moinrpc-response-valid-p (response)
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

(defun moinrpc-response-error-type (response)
  "Fetch error cause from RESPONSE."
  (let* ((response-status (cdr response))
	 (fault-string (cdr (assoc "faultString" (car response-status)))))
    (moinrpc-error-cause-to-type fault-string)))

(defun moinrpc-ask-token-and-save (response wiki)
  "Get auth token of WIKI and save it to file."
  (moinrpc-get-auth-token wiki)
  (moinrpc-save-wiki-settings))

(defun moinrpc-check-xmlrpc-response (response wiki on-error)
  ""
  (if (moinrpc-response-valid-p response)
      (let ((error-type (moinrpc-response-error-type response)))
	(case error-type
	  (:INVALID-TOKEN (apply on-error response wiki))
	  (t t)))))

(defun moinrpc-encode-xml-rpc-multi-each-method (method-name &rest params)
  (list
   (cons "methodName" method-name)
   (cons "params" (vconcat params))))

(defun moinrpc-encode-xml-rpc-multi-method (wiki method-name &rest params)
  "Construct and encode multi method call type to WIKI with a METHOD-NAME and PARAMS."
  (list
   (moinrpc-encode-xml-rpc-multi-each-method "applyAuthToken"
					     (moinrpc-get-wiki-conf wiki 'xmlrpc-api-token))
   (apply 'moinrpc-encode-xml-rpc-multi-each-method method-name params)))

(defun moinrpc-xml-rpc-multi-method-call (wiki method-name &rest params)
  "XML-RPC method call to WIKI with a METHOD-NAME and PARAMS."
  (let ((response nil)
	(url (moinrpc-get-wiki-conf wiki 'xmlrpc-endpoint ))
	(call-message (apply 'moinrpc-encode-xml-rpc-multi-method
			     wiki
			     method-name
			     params)))
    (setq response
	  (xml-rpc-method-call url
			       'system.multicall
			       call-message))

    (moinrpc-check-xmlrpc-response response wiki #'moinrpc-ask-token-and-save)
    (caar (cdr response))))

(defun moinrpc-set-auth-token-to-current (token wiki-setting)
  "Set access TOKEN to WIKI-SETTING."
    (setq wiki-setting (assq-delete-all 'xmlrpc-api-token wiki-setting))
    (add-to-list 'wiki-setting (cons 'xmlrpc-api-token token))
    )

(defun moinrpc-get-auth-token (wiki)
  "Prompt password and get access token of given WIKI using it."
  (let
      ((password (read-passwd "Password: ")))
    (xml-rpc-method-call (moinrpc-get-wiki-conf wiki 'xmlrpc-endpoint)
			 'getAuthToken
			 (moinrpc-get-wiki-conf wiki 'username)
			 password)))
    
(defun moinrpc-get-page-content (wiki pagename)
  "Return raw wiki content string of a page.
Specify WIKI with a PAGENAME."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "getPage"
				  pagename))

(defun moinrpc-save-page-content (wiki pagename content)
  "Save WIKI content with a PAGENAME and CONTENT."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "putPage"
				  pagename
				  content))

(defun moinrpc-get-list-content (wiki)
  "Return a list of all page names from WIKI."
  (let*
      ((content (moinrpc-xml-rpc-multi-method-call wiki "getAllPages"))
       (sorted-content (sort content 'string<)))
    sorted-content))


(defun moinrpc-get-recent-changes (wiki &optional timestamp)
  (let ((since timestamp))
    (unless timestamp
      (setq since (time-subtract (current-time) (* 3600 24 90))))
    (moinrpc-xml-rpc-multi-method-call wiki
                                       "getRecentChanges"
                                       (format-time-string "%F"
                                                           since))))


(defun moinrpc-get-attachment-list (wiki pagename)
  "Return attachment list of a page has a PAGENAME from WIKI."
  (moinrpc-xml-rpc-multi-method-call wiki
                                     "listAttachments"
                                     pagename))


(provide 'moinrpc-xmlrpc)
;;; moinrpc-xmlrpc.el ends here
