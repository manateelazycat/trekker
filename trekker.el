;;; trekker.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: trekker.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2023-08-12 21:08:48
;;           By: Andy Stewart
;; URL: https://github.com/manateelazycat/trekker
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Trekker
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET trekker RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'trekker-epc)

(defgroup trekker nil
  "Trekker group."
  :group 'applications)

(defvar trekker-server nil
  "The Trekker Server.")

(defvar trekker-python-file (expand-file-name "trekker.py" (if load-file-name
                                                                           (file-name-directory load-file-name)
                                                                         default-directory)))

(defvar trekker-server-port nil)

(defun trekker--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p trekker-server)
    (setq trekker-server
          (trekker-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (trekker-epc-define-method mngr 'eval-in-emacs 'trekker--eval-in-emacs-func)
               (trekker-epc-define-method mngr 'get-emacs-var 'trekker--get-emacs-var-func)
               (trekker-epc-define-method mngr 'get-emacs-vars 'trekker--get-emacs-vars-func)
               (trekker-epc-define-method mngr 'get-user-emacs-directory 'trekker--user-emacs-directory)
               ))))
    (if trekker-server
        (setq trekker-server-port (process-contact trekker-server :service))
      (error "[Trekker] trekker-server failed to start")))
  trekker-server)

(defun trekker--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun trekker--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun trekker--get-emacs-vars-func (&rest vars)
  (mapcar #'trekker--get-emacs-var-func vars))

(defvar trekker-epc-process nil)

(defvar trekker-internal-process nil)
(defvar trekker-internal-process-prog nil)
(defvar trekker-internal-process-args nil)

(defcustom trekker-name "*trekker*"
  "Name of Trekker buffer."
  :type 'string)

(defcustom trekker-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run trekker.py."
  :type 'string)

(defcustom trekker-enable-debug nil
  "If you got segfault error, please turn this option.
Then Trekker will start by gdb, please send new issue with `*trekker*' buffer content when next crash."
  :type 'boolean)

(defcustom trekker-enable-log nil
  "Enable this option to print log message in `*trekker*' buffer, default only print message header."
  :type 'boolean)

(defcustom trekker-enable-profile nil
  "Enable this option to output performance data to ~/trekker.prof."
  :type 'boolean)

(defun trekker--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun trekker-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (trekker-epc-live-p trekker-epc-process)
      (trekker-deferred-chain
       (trekker-epc-call-deferred trekker-epc-process (read method) args))
    (setq trekker-first-call-method method)
    (setq trekker-first-call-args args)
    ))

(defvar trekker-first-call-method nil)
(defvar trekker-first-call-args nil)

(defun trekker-restart-process ()
  "Stop and restart Trekker process."
  (interactive)
  (trekker-kill-process)
  (trekker-start-process)
  (message "[Trekker] Process restarted."))

(defun trekker-start-process ()
  "Start Trekker process if it isn't started."
  (if (trekker-epc-live-p trekker-epc-process)
      (remove-hook 'post-command-hook #'trekker-start-process)
    ;; start epc server and set `trekker-server-port'
    (trekker--start-epc-server)
    (let* ((trekker-args (append
                                (list trekker-python-file)
                                (list (number-to-string trekker-server-port))
                                (when trekker-enable-profile
                                  (list "profile"))
                                )))

      ;; Set process arguments.
      (if trekker-enable-debug
          (progn
            (setq trekker-internal-process-prog "gdb")
            (setq trekker-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" trekker-python-command) trekker-args)))
        (setq trekker-internal-process-prog trekker-python-command)
        (setq trekker-internal-process-args trekker-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq trekker-internal-process
              (apply 'start-process
                     trekker-name trekker-name
                     trekker-internal-process-prog trekker-internal-process-args)))
      (set-process-query-on-exit-flag trekker-internal-process nil))))

(defvar trekker-stop-process-hook nil)

(defun trekker-kill-process ()
  "Stop Trekker process and kill all Trekker buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'trekker-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (trekker--kill-python-process))

(add-hook 'kill-emacs-hook #'trekker-kill-process)

(defun trekker--kill-python-process ()
  "Kill Trekker background python process."
  (when (trekker-epc-live-p trekker-epc-process)
    ;; Cleanup before exit Trekker server process.
    (trekker-call-async "cleanup")
    ;; Delete Trekker server process.
    (trekker-epc-stop-epc trekker-epc-process)
    ;; Kill *trekker* buffer.
    (when (get-buffer trekker-name)
      (kill-buffer trekker-name))
    (setq trekker-epc-process nil)
    (message "[Trekker] Process terminated.")))

(defun trekker--first-start (trekker-epc-port)
  "Call `trekker--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq trekker-epc-process (make-trekker-epc-manager
                                   :server-process trekker-internal-process
                                   :commands (cons trekker-internal-process-prog trekker-internal-process-args)
                                   :title (mapconcat 'identity (cons trekker-internal-process-prog trekker-internal-process-args) " ")
                                   :port trekker-epc-port
                                   :connection (trekker-epc-connect "127.0.0.1" trekker-epc-port)
                                   ))
  (trekker-epc-init-epc-layer trekker-epc-process)

  (when (and trekker-first-call-method
             trekker-first-call-args)
    (trekker-deferred-chain
     (trekker-epc-call-deferred trekker-epc-process
                                      (read trekker-first-call-method)
                                      trekker-first-call-args)
     (setq trekker-first-call-method nil)
     (setq trekker-first-call-args nil)
     ))

  (message "*******"))

(defun trekker-enable ()
  (add-hook 'post-command-hook #'trekker-start-process))

(provide 'trekker)

;;; trekker.el ends here
