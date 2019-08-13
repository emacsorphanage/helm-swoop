;;; cort-test.el --- Simplify extended unit test framework -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Naoya Yamashita <conao3@gmail.com>

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: test lisp
;; Version: 6.0.5
;; URL: https://github.com/conao3/cort.el
;; Package-Requires: ((emacs "24.0"))

;;   Abobe declared this package requires Emacs-24, but it's for warning
;;   suppression, and will actually work from Emacs-22.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
;; GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simplify extended unit test framework

;;; Code:

(defgroup cort-test nil
  "Simplify elisp test framework."
  :group 'lisp)

(defvar cort-test-test-cases nil
  "Test list such as ((TEST-NAME VALUE) (TEST-NAME VALUE) ...).")

(defcustom cort-test-debug nil
  "If non nil, turn on debug mode.

- don't throw annoying error when test fail, just output message."
  :type 'boolean
  :group 'cort-test)

(defcustom cort-test-show-backtrace nil
  "If non nil, show backtrace when fail test case."
  :type 'boolean
  :group 'cort-test)

(defcustom cort-test-enable-color noninteractive
  "If non nil, enable color message to output with meta character.
Default, enable color if run test on CUI.
`noninteractive' returns t on --batch mode"
  :type 'boolean
  :group 'cort-test)

(defcustom cort-test-header-message
  (if cort-test-enable-color
      "\n\e[33mRunning %d tests...\e[m\n"
    "\nRunning %d tests...\n")
  "Header message."
  :type 'string
  :group 'cort-test)

(defcustom cort-test-passed-label
  (if cort-test-enable-color
      "\e[36m[PASSED] \e[m"
    "[PASSED] ")
  "Passed label."
  :type 'string
  :group 'cort-test)

(defcustom cort-test-fail-label
  (if cort-test-enable-color
      "\e[31m[FAILED] \e[m"
    "[FAILED] ")
  "Fail label."
  :type 'string
  :group 'cort-test)

(defcustom cort-test-error-label
  (if cort-test-enable-color
      "\e[35m<ERROR>  \e[m"
    "<<ERROR>>")
  "Fail label."
  :type 'string
  :group 'cort-test)

(defcustom cort-test-error-message
  (if cort-test-enable-color
      "\e[31m===== Run %2d Tests, %2d Expected, %2d Failed, %2d Errored on Emacs-%s =====\e[m\n\n"
    "===== Run %2d Tests, %2d Expected, %2d Failed, %2d Errored on Emacs-%s =====\n\n")
  "Error message."
  :type 'string
  :group 'cort-test)

(defcustom cort-test-passed-message
  (if cort-test-enable-color
      "\e[34m===== Run %2d Tests, %2d Expected, %2d Failed, %2d Errored on Emacs-%s =====\e[m\n\n"
    "===== Run %2d Tests, %2d Expected, %2d Failed, %2d Errored on Emacs-%s =====\n\n")
  "Error message."
  :type 'string
  :group 'cort-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  for old Emacs
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  small functions
;;

(defsubst cort-test-pp (sexp)
  "Return pretty printed SEXP string."
  (replace-regexp-in-string "\n+$" "" (pp-to-string sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support functions
;;

(defun cort-test-test (test)
  "Actually execute TEST.  TEST expect (METHOD EXPECT GIVEN).
Evaluate GIVEN to check it match EXPECT.
If match, return t, otherwise return nil."
  (let ((_name  (nth 0 test))
        (method (nth 1 test))
        (expect (nth 2 test))
        (given  (nth 3 test)))
    (if (eq method :cort-error)
        (eval
         `(condition-case err
              (eval ,given)
            (,expect t)))
      (funcall (intern
                (substring (symbol-name method) 1))
               (eval given) (eval expect)))))

(defun cort-test-testpass (test)
  "Output messages for passed TEST."
  (let* ((name    (nth 0 test))
         (_method (nth 1 test))
         (_expect (nth 2 test))
         (_given  (nth 3 test)))
    (princ (format "%s %s\n" cort-test-passed-label name))))

(defun cort-test-testfail (test &optional err)
  "Output messages for failed TEST.
ERR is error message."
  (let ((name    (nth 0 test))
        (method  (nth 1 test))
        (expect  (nth 2 test))
        (given   (nth 3 test)))
    (let* ((failp           (not err))
           (errorp          (not failp))
           (method-errorp   (eq method :cort-error))
           (method-defaultp (not method-errorp)))
      (let ((mesheader) (mesmethod) (mesgiven) (mesreturned) (mesexpect)
            (meserror) (mesbacktrace))
        (setq mesgiven  (format "Given:\n%s\n" (cort-test-pp given)))
        (setq mesbacktrace (format "Backtrace:\n%s\n"
                                   (with-output-to-string
                                     (backtrace))))
        (progn
          (when errorp
            (setq mesheader (format "%s %s\n" cort-test-error-label name))
            (setq meserror  (format "Unexpected-error: %s\n" (cort-test-pp err))))
          (when failp
            (setq mesheader (format "%s %s\n" cort-test-fail-label name))))

        (progn
          (when method-defaultp
            (setq mesmethod (format "< Tested with %s >\n" method))
            (setq mesexpect (format "Expected:\n%s\n" (cort-test-pp expect)))
            (when failp
              (setq mesreturned (format "Returned:\n%s\n" (cort-test-pp (eval given))))))
          (when method-errorp
            (setq meserror  (format "Unexpected-error: %s\n" (cort-test-pp err)))
            (setq mesexpect (format "Expected-error:   %s\n" (cort-test-pp expect)))))

        (princ (concat mesheader
                       (when mesmethod   mesmethod)
                       (when mesexpect   mesexpect)
                       (when mesgiven    mesgiven)
                       (when mesreturned mesreturned)
                       (when meserror    meserror)
                       (when cort-test-show-backtrace
                         (when mesbacktrace mesbacktrace))
                       "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Define test phase
;;

(defmacro cort-deftest (name testlst)
  "Define a test case with the NAME.
TESTLST is list of forms as below.

basic         : (:COMPFUN EXPECT GIVEN)
error testcase: (:cort-error EXPECTED-ERROR:ROR-TYPE FORM)"
  (declare (indent 1))
  (let ((count 0)
        (suffixp (< 1 (length (cadr testlst)))))
    `(progn
       ,@(mapcar (lambda (test)
                   (setq count (1+ count))
                   `(add-to-list 'cort-test-test-cases
                                 '(,(if suffixp
                                         (make-symbol
                                          (format "%s-%s" (symbol-name name) count))
                                       name)
                                    ,@test)))
                 (eval testlst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Run test phase
;;

(defun cort-test-prune ()
  "Prune all test."
  (interactive)
  (setq cort-test-test-cases nil)
  (message "prune tests completed."))

(defun cort-test-run ()
  "Run all test."
  (let ((testc  (length cort-test-test-cases))
        (failc  0)
        (errorc 0))
    (princ (format cort-test-header-message testc))
    (princ (format "%s\n" (emacs-version)))

    (dolist (test (reverse cort-test-test-cases))
      (condition-case err
          (if (cort-test-test test)
              (cort-test-testpass test)
            (cort-test-testfail test)
            (setq failc (1+ failc)))
        (error
         (cort-test-testfail test err)
         (setq errorc (1+ errorc)))))

    (princ "\n\n")
    (if (or (< 0 failc) (< 0 errorc))
        (if cort-test-debug
            (princ "Test failed!!\n")
          (error (format cort-test-error-message
                         testc (- testc failc errorc) failc errorc emacs-version)))
      (princ (format cort-test-passed-message
                     testc (- testc failc errorc) failc errorc emacs-version)))))

;; (provide 'cort-test)
;;; cort-test.el ends here
