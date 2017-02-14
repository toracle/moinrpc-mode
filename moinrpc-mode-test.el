(require 'moinrpc-mode)
(require 'moinrpc-common)
(require 'moinrpc-conf)


(defvar *moinrpc-fixture-response-get-pages*
  '(("SUCCESS")
    (("Page1" "Page2" "Page3"))))

(defvar *moinrpc-fixture-response-error-not-found*
  '(("SUCCESS")
    (("faultCode" . 1) ("faultString" . "No such page was found."))))

(defvar *moinrpc-fixture-response-error-invalid-token*
  '(("SUCCESS")
    (("faultCode" . 1) ("faultString" . "Invalid token."))))

(defvar *moinrpc-fixture-wiki*
  '(("testwiki"
     (wiki-alias . "wooridle")
     (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
     (xmlrpc-api-token . "testtoken")
     (username . "myuser"))))

(defvar *moinrpc-fixture-wiki-setting*
  '((wiki-alias . "wooridle")
    (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
    (xmlrpc-api-token . "testtoken")
    (username . "myuser")))


(ert-deftest moinrpc-get-keys ()
  (should (equal (moinrpc-get-keys '((key1 . value1) (key2 . value2) (key3 . value3)))
		 '(key3 key2 key1))))

(ert-deftest moinrpc-response-valid-p ()
  (should (equal (moinrpc-response-valid-p *moinrpc-fixture-response-get-pages*) t))
  (should (equal (moinrpc-response-valid-p *moinrpc-fixture-response-error-not-found*) t)))

(ert-deftest moinrpc-error-cause-to-type ()
  (should (equal (moinrpc-error-cause-to-type "No such page was found.") :NOT-FOUND))
  (should (equal (moinrpc-error-cause-to-type "Invalid token.") :INVALID-TOKEN))
  (should (equal (moinrpc-error-cause-to-type "Unknown error.") nil)))

(ert-deftest moinrpc-response-error-type ()
  (should (equal (moinrpc-response-error-type *moinrpc-fixture-response-error-not-found*) :NOT-FOUND))
  (should (equal (moinrpc-response-error-type *moinrpc-fixture-response-error-invalid-token*) :INVALID-TOKEN))
  (should (equal (moinrpc-response-error-type *moinrpc-fixture-response-get-pages*) nil)))

(defun moinrpc-on-error-mockup (response wiki)
  nil)

(ert-deftest moinrpc-check-xmlrpc-response ()
  (should (equal (moinrpc-check-xmlrpc-response *moinrpc-fixture-response-get-pages*
						*moinrpc-fixture-wiki*
						nil)
		 t))
  (should (equal (moinrpc-check-xmlrpc-response *moinrpc-fixture-response-error-invalid-token*
						*moinrpc-fixture-wiki*
						#'moinrpc-on-error-mockup)
		 nil)))

(ert-deftest moinrpc-make-wiki ()
  (should (equal (moinrpc-make-wiki "testwiki")
		 '((wiki-alias . "testwiki")
		   (xmlrpc-endpoint . nil)
		   (username . nil)
		   (xmlrpc-api-token . nil))))
  (should (equal (moinrpc-make-wiki "testwiki" "https://mywiki.com/xmlrpc2")
		 '((wiki-alias . "testwiki")
		   (xmlrpc-endpoint . "https://mywiki.com/xmlrpc2")
		   (username . nil)
		   (xmlrpc-api-token . nil))))
  (should (equal (moinrpc-make-wiki "testwiki" "https://mywiki.com/xmlrpc2" "testuser")
		 '((wiki-alias . "testwiki")
		   (xmlrpc-endpoint . "https://mywiki.com/xmlrpc2")
		   (username . "testuser")
		   (xmlrpc-api-token . nil)))))

(ert-deftest moinrpc-set-wiki-conf ()
  (should
   (let ((wiki-setting (copy-list *moinrpc-fixture-wiki-setting*)))
     (moinrpc-set-wiki-conf wiki-setting 'xmlrpc-api-token "testtoken2")
     (equal wiki-setting
	    '((wiki-alias . "wooridle")
	      (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
	      (xmlrpc-api-token . "testtoken2")
	      (username . "myuser"))))))

(ert-deftest moinrpc-get-wiki-conf ()
  (should (equal (moinrpc-get-wiki-conf *moinrpc-fixture-wiki-setting* 'xmlrpc-api-token)
		 "testtoken")))



;; (let ((wiki (moinrpc-make-wiki "wikiname")))
;;   (moinrpc-set-wiki-conf wiki 'xmlrpc-endpoint "https://")
;;   (moinrpc-set-wiki-conf wiki 'xmlrpc-api-token "fake-token"))


;; (moinrpc-set-wiki *moinrpc-fixture-wiki-setting* 'xmlrpc-api-token "testtoken2")
