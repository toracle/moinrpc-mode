(require 'moinrpc-mode)
(require 'moinrpc-common)
(require 'moinrpc-conf)


(defvar *moinrpc-fixture-response-get-pages* nil)

(defvar *moinrpc-fixture-response-error-not-found* nil)

(defvar *moinrpc-fixture-response-error-invalid-token* nil)

(defvar *moinrpc-fixture-wiki* nil)

(defvar *moinrpc-fixture-wiki-setting* nil)


(defun my-fixture (body)
  (unwind-protect
      (let ((*moinrpc-fixture-response-get-pages*
             '(("SUCCESS")
               (("Page1" "Page2" "Page3"))))

             (*moinrpc-fixture-response-error-not-found*
               '(("SUCCESS")
                 (("faultCode" . 1) ("faultString" . "No such page was found."))))

             (*moinrpc-fixture-response-error-invalid-token*
               '(("SUCCESS")
                 (("faultCode" . 1) ("faultString" . "Invalid token."))))

             (*moinrpc-fixture-wiki*
               '(("testwiki"
                  (wiki-alias . "wooridle")
                  (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
                  (xmlrpc-api-token . "testtoken")
                  (username . "myuser"))))

             (*moinrpc-fixture-wiki-setting*
               '((wiki-alias . "wooridle")
                 (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
                 (xmlrpc-api-token . "testtoken")
                 (username . "myuser")))
             (funcall body)))))


(ert-deftest moinrpc-get-keys ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-get-keys '((key1 . value1) (key2 . value2) (key3 . value3)))
                    '(key1 key2 key3))))))


(ert-deftest moinrpc-response-valid-p ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-response-valid-p *moinrpc-fixture-response-get-pages*) t))
     (should (equal (moinrpc-response-valid-p *moinrpc-fixture-response-error-not-found*) t)))))


(ert-deftest moinrpc-error-cause-to-type ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-error-cause-to-type "No such page was found.") :NOT-FOUND))
     (should (equal (moinrpc-error-cause-to-type "Invalid token.") :INVALID-TOKEN))
     (should (equal (moinrpc-error-cause-to-type "Unknown error.") nil)))))


(ert-deftest moinrpc-response-error-type ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-response-error-type *moinrpc-fixture-response-error-not-found*) :NOT-FOUND))
     (should (equal (moinrpc-response-error-type *moinrpc-fixture-response-error-invalid-token*) :INVALID-TOKEN))
     (should (equal (moinrpc-response-error-type *moinrpc-fixture-response-get-pages*) nil)))))


(defun moinrpc-on-error-mockup (response wiki)
  nil)


(ert-deftest moinrpc-check-xmlrpc-response ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-check-xmlrpc-response *moinrpc-fixture-response-get-pages*
						   *moinrpc-fixture-wiki*
						   nil)
		    t))
     (should (equal (moinrpc-check-xmlrpc-response *moinrpc-fixture-response-error-invalid-token*
						   *moinrpc-fixture-wiki*
						   #'moinrpc-on-error-mockup)
		    nil)))))


(ert-deftest moinrpc-encode-xml-rpc-multi-each-method ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-encode-xml-rpc-multi-each-method 'getPage "TestPage")
		    '(("methodName" . getPage) ("params" . ["TestPage"])))))))


(ert-deftest moinrpc-make-wiki-conf ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-make-wiki-conf "testwiki")
		    '((wiki-alias . "testwiki")
		      (xmlrpc-endpoint . nil)
		      (username . nil)
		      (xmlrpc-api-token . nil))))
     (should (equal (moinrpc-make-wiki-conf "testwiki" "https://mywiki.com/xmlrpc2")
		    '((wiki-alias . "testwiki")
		      (xmlrpc-endpoint . "https://mywiki.com/xmlrpc2")
		      (username . nil)
		      (xmlrpc-api-token . nil))))
     (should (equal (moinrpc-make-wiki-conf "testwiki" "https://mywiki.com/xmlrpc2" "testuser")
		    '((wiki-alias . "testwiki")
		      (xmlrpc-endpoint . "https://mywiki.com/xmlrpc2")
		      (username . "testuser")
		      (xmlrpc-api-token . nil)))))))


(ert-deftest moinrpc-get-wiki-conf ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-get-wiki-conf *moinrpc-fixture-wiki-setting* 'xmlrpc-api-token)
		    "testtoken")))))


(ert-deftest moinrpc-set-wiki-conf ()
  (my-fixture
   (lambda ()
     (should
      (let ((wiki-setting (copy-list *moinrpc-fixture-wiki-setting*)))
        (moinrpc-set-wiki-conf wiki-setting 'xmlrpc-api-token "testtoken2")
        (equal wiki-setting
	       '((wiki-alias . "wooridle")
	         (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
	         (xmlrpc-api-token . "testtoken2")
	         (username . "myuser"))))))))
