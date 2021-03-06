(require 'moinrpc-xmlrpc)
(require 'moinrpc-test-fixtures)

(defvar *moinrpc-xmlrpc-test-call-history* nil)

(defvar *fixture-wiki*
  '((wiki-alias . "Wooridle")
    (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
    (username . "testuser")
    (xmlrpc-api-token . "api-token")))


(ert-deftest moinrpc-xmlrpc-check-response ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-xmlrpc-check-response
                     *moinrpc-fixture-response-get-pages*
		     *moinrpc-fixture-wiki*
		     nil)
		    t))
     (should (equal (moinrpc-xmlrpc-check-response
                     *moinrpc-fixture-response-error-invalid-token*
		     *moinrpc-fixture-wiki*
		     #'moinrpc-on-error-mockup)
		    nil)))))


(ert-deftest moinrpc-xml-encode-rpc-multi-each-method ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-xml-encode-rpc-multi-each-method 'getPage "TestPage")
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


(ert-deftest moinrpc-xmlrpc-get-page-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-get-page *fixture-wiki* "TestPage")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getPage")
                                ("params" . ["TestPage"]))))))))))


(ert-deftest moinrpc-xmlrpc-put-page-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-put-page *fixture-wiki* "TestPage" "Test content")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "putPage")
                                ("params" . ["TestPage" "Test content"]))))))))))


(ert-deftest moinrpc-xmlrpc-get-all-pages-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-get-all-pages *fixture-wiki*)
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getAllPages")
                                ("params" . []))))))))))


(ert-deftest moinrpc-xmlrpc-get-recent-changes-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-get-recent-changes *fixture-wiki* '(24587 43080))
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getRecentChanges")
                                ("params" . [(:datetime (24587 43080))]))))))))))


(ert-deftest moinrpc-xmlrpc-list-attachments-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-list-attachments *fixture-wiki* "TestPage")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "listAttachments")
                                ("params" . ["TestPage"]))))))))))


(ert-deftest moinrpc-xmlrpc-put-attachment-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-put-attachment *fixture-wiki* "TestPage" "a.jpg" "JPG content")
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


(ert-deftest moinrpc-xmlrpc-get-page-html-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-get-page-html *fixture-wiki* "TestPage")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getPageHTML")
                                ("params" . ["TestPage"]))))))))))


(ert-deftest moinrpc-xmlrpc-search-pages-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-search-pages *fixture-wiki* "TestPage")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "searchPages")
                                ("params" . ["TestPage"]))))))))))


(ert-deftest moinrpc-xmlrpc-search-backlinks-should-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-xmlrpc-search-backlinks *fixture-wiki* "TestPage")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "searchPages")
                                ("params" . ["linkto:TestPage"]))))))))))


(provide 'moinrpc-xmlrpc-test)
