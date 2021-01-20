(require 'moinrpc-xmlrpc)
(require 'moinrpc-test-fixtures)

(defvar *moinrpc-xmlrpc-test-call-history* nil)

(defvar *fixture-wiki*
  '((wiki-alias . "Wooridle")
    (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
    (username . "testuser")
    (xmlrpc-api-token . "api-token")))


(ert-deftest moinrpc-check-xmlrpc-response ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-check-xmlrpc-response
                     *moinrpc-fixture-response-get-pages*
		     *moinrpc-fixture-wiki*
		     nil)
		    t))
     (should (equal (moinrpc-check-xmlrpc-response
                     *moinrpc-fixture-response-error-invalid-token*
		     *moinrpc-fixture-wiki*
		     #'moinrpc-on-error-mockup)
		    nil)))))


(ert-deftest moinrpc-encode-xml-rpc-multi-each-method ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-encode-xml-rpc-multi-each-method 'getPage "TestPage")
		    '(("methodName" . getPage) ("params" . ["TestPage"])))))))


(defun dummy-xml-rpc-method-call (server-url method &rest params)
  (add-to-list '*moinrpc-xmlrpc-test-call-history*
               (list :server-url server-url
                     :method method
                     :params params)))


(defmacro with-dummy-xml-rpc-call (&rest body)
  `(progn
     (advice-add 'xml-rpc-method-call :override 'dummy-xml-rpc-method-call)
     (setf *moinrpc-xmlrpc-test-call-history* nil)
     ,@body
     (advice-remove 'xml-rpc-method-call 'dummy-xml-rpc-method-call)))


(ert-deftest moinrpc-get-page-content-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-get-page-content *fixture-wiki* "TestPage")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getPage")
                                ("params" . ["TestPage"]))))))))))


(ert-deftest moinrpc-save-page-content-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-save-page-content *fixture-wiki* "TestPage" "Test content")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "putPage")
                                ("params" . ["TestPage" "Test content"]))))))))))


(ert-deftest moinrpc-get-list-content-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-get-list-content *fixture-wiki*)
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getAllPages")
                                ("params" . []))))))))))


(ert-deftest moinrpc-get-recent-changes-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-get-recent-changes *fixture-wiki* 1610852783.2646885)
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getRecentChanges")
                                ("params" . ["2021-01-17"]))))))))))


(ert-deftest moinrpc-list-attachments-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-get-attachment-list *fixture-wiki* "TestPage")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "listAttachments")
                                ("params" . ["TestPage"]))))))))))


(ert-deftest moinrpc-put-attachment-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-put-attachment *fixture-wiki* "TestPage" "a.jpg" "JPG content")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "putAttachment")
                                ("params" . ["TestPage" "a.jpg" (:base64 "JPG content")]))))))))))


(ert-deftest moinrpc-xmlrpc-delete-attachment-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-delete-attachment *fixture-wiki* "TestPage" "a.jpg")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "deleteAttachment")
                                ("params" . ["TestPage" "a.jpg"]))))))))))


(provide 'moinrpc-xmlrpc-test)
