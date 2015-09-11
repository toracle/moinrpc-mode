;;; package --- moinmoin xml-rpc client
;;; Commentary: 

;;; Code: 


;; Raw wiki-xmlrpc functions

(defun moinrpc-check-xmlrpc-response (response)
  (let
      ((response-status (car response)))
    (when
	(not (string= (car response-status) "SUCCESS"))
      (when
	  (and
	   (string= "INVALID" (cdr (assoc "faultCode" response-status)))
	   (string= "Invalid token." (cdr (assoc "faultString" response-status))))
	(moinrpc-get-auth-token)
	(moinrpc-save-wiki-settings)))))
	
(defun moinrpc-encode-xml-rpc-multi-each-method (method-name &rest params)
  (list
   (cons "methodName" method-name)
   (cons "params" (vconcat params))))

(defun moinrpc-encode-xml-rpc-multi-method (wiki method-name &rest params)
  (list
    (moinrpc-encode-xml-rpc-multi-each-method "applyAuthToken" (cdr (assoc 'xmlrpc-api-token wiki)))
    (apply 'moinrpc-encode-xml-rpc-multi-each-method method-name params)))

(defun moinrpc-xml-rpc-multi-method-call (wiki method-name &rest params)
  (let
      ((response nil)
       (url (cdr (assoc 'xmlrpc-endpoint wiki)))
       (call-message (apply 'moinrpc-encode-xml-rpc-multi-method
			    wiki
			    method-name
			    params)))
    (setq response
	  (xml-rpc-method-call url
			       'system.multicall
			       call-message))
    (moinrpc-check-xmlrpc-response response)
    (caar (cdr response))))

(defun moinrpc-set-auth-token-to-current (token wiki-setting)
    (setq wiki-setting (assq-delete-all 'xmlrpc-api-token wiki-setting))
    (add-to-list 'wiki-setting (cons 'xmlrpc-api-token token))
    )

(defun moinrpc-get-auth-token (wiki)
  (let
      ((token nil)
       (password (read-passwd "Password: ")))
    (setq token
	  (xml-rpc-method-call (cdr (assoc 'xmlrpc-endpoint wiki))
			       'getAuthToken
			       (cdr (assoc 'username wiki))
			       password))
    ))
    
(defun moinrpc-get-page-content (wiki pagename)
  "Return raw wiki content string of a page.
Specify WIKI with a PAGENAME."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "getPage"
				  pagename))

(defun moinrpc-save-page-content (wiki pagename content)
  "Save WIKI content with a PAGENAME."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "putPage"
				  pagename
				  content))

(defun moinrpc-get-list-content (wiki)
  "Return a list of all page names from WIKI."
  (let
      ((content nil))
    (setq content (moinrpc-xml-rpc-multi-method-call wiki
						     "getAllPages"))
    (sort content 'string<)))

(provide 'moinrpc-xmlrpc)
;;; moinrpc-xmlrpc.el ends here
