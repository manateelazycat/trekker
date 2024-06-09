;;; trekker.el --- Trekker  -*- lexical-binding: t -*-

;; Filename: trekker.el
;; Description: Trekker
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2023, Andy Stewart, all rights reserved.
;; Created: 2023-02-26 14:00
;; Version: 0.1
;; Last-Updated: 2023-02-27 09:36
;;           By: Andy Stewart
;; URL: https://github.com/manateelazycat/trekker
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28")
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
                          (trekker-get-render-size)
                          (list (number-to-string trekker-server-port))
                          (when trekker-enable-profile
                            (list "profile"))))
           environments)

      ;; Folow system DPI.
      (setq environments (trekker--build-process-environment))

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
  (setq trekker-emacs-frame (window-frame))

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
      )))

(defun trekker-enable ()
  (add-hook 'post-command-hook #'trekker-start-process))

(defcustom trekker-mode-line-format mode-line-format
  "`mode-line-format' used by trekker-mode.")

(defcustom trekker-kill-process-after-last-buffer-closed nil
  "Kill trekker process when last trekker buffer closed, default is nil.

If you don't want TREKKER process exist when all TREKKER buffer closed, turn on this option.

Turn off this option will improve TREKKER new page creation speed."
  :type 'boolean)

(defcustom trekker-frame-title-format frame-title-format
  "`frame-title-format' used by trekker-mode.")

(defvar trekker--monitor-configuration-p t
  "When this variable is non-nil, `trekker-monitor-configuration-change' executes.
This variable is used to open buffer in backend and avoid graphics blink.

TREKKER call python method `new_buffer' to create TREKKER application buffer.
TREKKER call python method `update_views' to create TREKKER application view.

Python process only create application view when Emacs window or buffer state change.")

(defvar trekker-fullscreen-p nil
  "When non-nil, TREKKER will intelligently hide modeline as necessray.")

(defvar trekker-last-frame-width 0)

(defvar trekker-last-frame-height 0)

(defvar-local trekker--buffer-map-alist nil
  "TREKKER buffer-local map alist.")

(defvar-local trekker--buffer-map-alist-order 1
  "Order of TREKKER buffer-local map alist in `emulation-mode-map-alists'.")

(defun trekker--monitor-buffer-kill ()
  "A function monitoring when an TREKKER buffer is killed."
  (ignore-errors
    (trekker-call-async "kill_buffer" trekker--buffer-id))

  ;; Kill trekker process when last trekker buffer closed.
  ;; We need add timer to avoid the last web page kill when terminal is exited.
  (when trekker-kill-process-after-last-buffer-closed
    (run-at-time
     5 nil
     (lambda ()
       (when (equal (length (trekker--get-trekker-buffers)) 0)
         (trekker--kill-python-process))
       ))))

(defun trekker--kill-python-process ()
  "Kill TREKKER background python process."
  (interactive)
  (when (trekker-epc-live-p trekker-epc-process)
    ;; Cleanup before exit TREKKER server process.
    (trekker-call-async "cleanup")
    ;; Delete TREKKER server process.
    (trekker-epc-stop-epc trekker-epc-process)
    ;; Kill *trekker* buffer.
    (when (get-buffer trekker-name)
      (kill-buffer trekker-name))
    (message "[TREKKER] Process terminated.")))

(defun trekker--generate-id ()
  "Randomly generate a seven digit id used for TREKKER buffers."
  (format "%04x-%04x-%04x-%04x-%04x-%04x-%04x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))))

(define-derived-mode trekker-mode fundamental-mode "TREKKER"
  "Major mode for Emacs Application Framework buffers.

This mode is used by all apps. The mode map `trekker-mode-map' is
created dynamically for each app and should not be changed
manually. See `trekker-bind-key' for customization of app bindings."
  (if (require 'holo-layer nil t)
      ;; Don't show mode-line if `holo-layer' is installed.
      (setq-local mode-line-format nil)
    ;; Let trekker can set its mode-line and frame-title.
    (setq-local mode-line-format trekker-mode-line-format))

  (setq-local frame-title-format trekker-frame-title-format)
  ;; Split window combinations proportionally.
  (setq-local window-combination-resize t)
  ;; Disable cursor in trekker buffer.
  (setq-local cursor-type nil)

  (set (make-local-variable 'trekker--buffer-id) (trekker--generate-id))

  ;; Copy default value in case user already has bindings there
  (setq-local emulation-mode-map-alists
              (copy-alist (default-value 'emulation-mode-map-alists)))
  ;; Construct map alist
  (setq-local trekker--buffer-map-alist (list (cons t trekker-mode-map)))
  ;; Eanble mode map and make it the first priority
  (add-to-ordered-list
   'emulation-mode-map-alists
   'trekker--buffer-map-alist
   'trekker--buffer-map-alist-order)

  (add-hook 'kill-buffer-hook #'trekker--monitor-buffer-kill nil t)
  (add-hook 'kill-emacs-hook #'trekker--monitor-emacs-kill))

(defun trekker--monitor-emacs-kill ()
  "Function monitoring when Emacs is killed."
  (trekker-call-async "kill_emacs"))

(defun trekker--called-from-wsl-on-windows-p ()
  "Check whether trekker is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" trekker-python-command)))

(defun trekker-get-emacs-xid (frame)
  "Get Emacs FRAME xid."
  (if (trekker--called-from-wsl-on-windows-p)
      (trekker-call-sync "get_emacs_wsl_window_id")
    (frame-parameter frame 'window-id)))

(defun trekker-get-window-allocation (&optional window)
  "Get WINDOW allocation."
  (let* ((window-edges (window-pixel-edges window))
         (x (nth 0 window-edges))
         (y (+ (nth 1 window-edges)
               (if (version< emacs-version "27.0")
                   (window-header-line-height window)
                 (window-tab-line-height window))))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) (window-mode-line-height window) y)))
    (list x y w h)))

(defun trekker-emacs-running-in-wayland-native ()
  (eq window-system 'pgtk))

(defun trekker--get-titlebar-height ()
  "We need fetch height of window titlebar to adjust y coordinate of TREKKER when Emacs is not fullscreen."
  (cond ((trekker-emacs-running-in-wayland-native)
         (let ((is-fullscreen-p (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))))
           (if is-fullscreen-p
               0
             ;; `32' is titlebar of Gnome3, we need change this value in other environment.
             (cond ((trekker--on-hyprland-p)
                    0)
                   ((trekker--on-sway-p)
                    (string-to-number (shell-command-to-string
                                       (format "swaymsg -t get_tree | jq -r '..|try select(.pid == %d).deco_rect|.height'" (emacs-pid)))))
                   (t
                    32)))))
        (t
         0)))

(defun trekker--on-sway-p ()
  (string-equal (getenv "XDG_SESSION_DESKTOP") "sway"))

(defun trekker--on-hyprland-p ()
  (string-equal (getenv "XDG_CURRENT_DESKTOP") "Hyprland"))

(defun trekker--split-number (string)
  (mapcar #'string-to-number (split-string string)))

(defun trekker--get-frame-coordinate ()
  "We need fetch Emacs coordinate to adjust coordinate of TREKKER if it running on system not support cross-process reparent technology.

Such as, wayland native, macOS etc."
  (if (trekker-emacs-running-in-wayland-native)
      (cond ((trekker--on-sway-p)
             (trekker--split-number (shell-command-to-string
				                     (format "swaymsg -t get_tree | jq -r '..|try select(.pid == %d).deco_rect|.x,.y'" (emacs-pid)))))
            ((trekker--on-hyprland-p)
             (let ((clients (json-parse-string (shell-command-to-string "hyprctl -j clients")))
		           (coordinate))
               (dotimes (i (length clients))
		         (when (equal (gethash "pid" (aref clients i)) (emacs-pid))
		           (setq coordinate (gethash "at" (aref clients i)))))
               (list (aref coordinate 0) (aref coordinate 1))))
            (t
             (require 'dbus)
             (let* ((coordinate (trekker--split-number
				                 (dbus-call-method :session "org.gnome.Shell" "/org/trekker/wayland" "org.trekker.wayland" "get_emacs_window_coordinate" :timeout 1000)
				                 ","))
                    ;; HiDPI need except by `frame-scale-factor'.
                    (frame-x (truncate (/ (car coordinate) (frame-scale-factor))))
                    (frame-y (truncate (/ (cadr coordinate) (frame-scale-factor)))))
               (list frame-x frame-y))))
    (list 0 0)))

(defun trekker--frame-top (frame)
  "Return outer top position."
  (let ((top (frame-parameter frame 'top)))
    (if (listp top) (nth 1 top) top)))

(defun trekker--frame-left (frame)
  "Return outer left position"
  (let ((left (frame-parameter frame 'left)))
    (if (listp left) (nth 1 left) left)))

(defun trekker--frame-internal-height (frame)
  "Height of internal objects.
Including title-bar, menu-bar, offset depends on window system, and border."
  (let ((geometry (frame-geometry frame)))
    (+ (cdr (alist-get 'title-bar-size geometry))
       (cdr (alist-get 'tool-bar-size geometry)))))

(defun trekker--buffer-x-position-adjust (frame)
  "Adjust the x position of TREKKER buffers for macOS"
  (if (eq system-type 'darwin)
      (trekker--frame-left frame)
    0))

(defun trekker--buffer-y-position-adjust (frame)
  "Adjust the y position of TREKKER buffers for macOS"
  (if (eq system-type 'darwin)
      (+ (trekker--frame-top frame) (trekker--frame-internal-height frame))
    0))

(defun trekker-monitor-configuration-change (&rest _)
  "TREKKER function to respond when detecting a window configuration change."
  (when (and trekker--monitor-configuration-p
             (trekker-epc-live-p trekker-epc-process)
             ;; When current frame is same with `trekker-emacs-frame'.
             (equal (window-frame) trekker-emacs-frame))
    (ignore-errors
      (let (view-infos)
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (with-current-buffer (window-buffer window)
              (when (derived-mode-p 'trekker-mode)
                ;; When `trekker-fullscreen-p' is non-nil, and only the TREKKER window is present, use frame size
                (if (and trekker-fullscreen-p
                         (equal (length (cl-remove-if #'window-dedicated-p (window-list frame))) 1))
                    (push (format "%s:%s:%s:%s:%s:%s"
                                  trekker--buffer-id
                                  (trekker-get-emacs-xid frame)
                                  0 0 (frame-pixel-width frame) (frame-pixel-height frame))
                          view-infos)
                  (let* ((window-allocation (trekker-get-window-allocation window))
                         (window-divider-right-padding (if window-divider-mode window-divider-default-right-width 0))
                         (window-divider-bottom-padding (if window-divider-mode window-divider-default-bottom-width 0))
                         (titlebar-height (trekker--get-titlebar-height))
                         (frame-coordinate (trekker--get-frame-coordinate))
                         (frame-x (car frame-coordinate))
                         (frame-y (cadr frame-coordinate))
                         (x (+ (trekker--buffer-x-position-adjust frame) (nth 0 window-allocation)))
                         (y (+ (trekker--buffer-y-position-adjust frame) (nth 1 window-allocation)))
                         (w (nth 2 window-allocation))
                         (h (nth 3 window-allocation)))
                    (push (format "%s:%s:%s:%s:%s:%s"
                                  trekker--buffer-id
                                  (trekker-get-emacs-xid frame)
                                  (+ x frame-x)
                                  (+ y titlebar-height frame-y)
                                  (- w window-divider-right-padding)
                                  (- h window-divider-bottom-padding))
                          view-infos)))))))
        (trekker-call-async "update_views" (mapconcat #'identity view-infos ","))))))

(defun trekker-monitor-window-size-change (frame)
  "Delay some time and run `trekker-try-adjust-view-with-frame-size' to compare with Emacs FRAME size."
  (when (trekker-epc-live-p trekker-epc-process)
    (setq trekker-last-frame-width (frame-pixel-width frame))
    (setq trekker-last-frame-height (frame-pixel-height frame))
    (run-with-timer 1 nil (lambda () (trekker-try-adjust-view-with-frame-size frame)))))

(defun trekker-try-adjust-view-with-frame-size (frame)
  "Update TREKKER view once Emacs window size of the FRAME is changed."
  (unless (and (equal (frame-pixel-width frame) trekker-last-frame-width)
               (equal (frame-pixel-height frame) trekker-last-frame-height))
    (trekker-monitor-configuration-change)))

(defun trekker--get-trekker-buffers ()
  "A function that return a list of TREKKER buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'trekker-mode)))
   (buffer-list)))

(defmacro trekker-for-each-trekker-buffer (&rest body)
  "A syntactic sugar to loop through each TREKKER buffer and evaluat BODY.

Within BODY, `buffer' can be used to"
  `(dolist (buffer (trekker--get-trekker-buffers))
     (with-current-buffer buffer
       ,@body)))

(defun trekker-get-buffer (url)
  "Find the buffer given the url."
  (catch 'found-trekker
    (trekker-for-each-trekker-buffer
     (when (string= trekker--buffer-url url)
       (throw 'found-trekker buffer))
     nil)))

(defun trekker-get-render-size ()
  "Get allocation for render application in backend.
We need calcuate render allocation to make sure no black border around render content."
  (let* (;; We use `window-inside-pixel-edges' and `window-absolute-pixel-edges' calcuate height of window header, such as tabbar.
         (window-header-height (- (nth 1 (window-inside-pixel-edges)) (nth 1 (window-absolute-pixel-edges))))
         (width (frame-pixel-width))
         ;; Render height should minus mode-line height, minibuffer height, header height.
         (height (- (frame-pixel-height) (window-mode-line-height) (window-pixel-height (minibuffer-window)) window-header-height)))
    (mapcar (lambda (x) (format "%s" x)) (list width height))))

(defun trekker--build-process-environment ()
  ;; Turn on DEBUG info when `trekker-enable-debug' is non-nil.
  (let ((environments (seq-filter
                       (lambda (var)
                         (and (not (string-match-p "QT_SCALE_FACTOR" var))
                              (not (string-match-p "QT_SCREEN_SCALE_FACTOR" var))))
                       process-environment)))
    (when trekker-enable-debug
      (add-to-list 'environments "QT_DEBUG_PLUGINS=1" t))

    (unless (eq system-type 'darwin)
      (add-to-list 'environments
                   (cond
                    ((trekker-emacs-running-in-wayland-native)
                     ;; Wayland native need to set QT_AUTO_SCREEN_SCALE_FACTOR=1
                     ;; otherwise Qt window only have half of screen.
                     "QT_AUTO_SCREEN_SCALE_FACTOR=1")
                    (t
                     ;; XWayland need to set QT_AUTO_SCREEN_SCALE_FACTOR=0
                     ;; otherwise Qt which explicitly force high DPI enabling get scaled TWICE.
                     "QT_AUTO_SCREEN_SCALE_FACTOR=0"))
                   t)

      (add-to-list 'environments "QT_FONT_DPI=96" t)

      ;; Make sure Trekker application scale support 4k screen.
      (add-to-list 'environments "QT_SCALE_FACTOR=1" t)

      ;; Fix CORS problem.
      (add-to-list 'environments "QTWEBENGINE_CHROMIUM_FLAGS=--disable-web-security" t)

      ;; Use XCB for input event transfer.
      ;; Only enable this option on Linux platform.
      (when (and (eq system-type 'gnu/linux)
                 (not (trekker-emacs-running-in-wayland-native)))
        (add-to-list 'environments "QT_QPA_PLATFORM=xcb" t)))
    environments))

(defun trekker-open-url (url)
  (interactive "MOpen URL: ")
  (let* ((buf (trekker-get-buffer url)))
    (unless buf
      (setq buf (generate-new-buffer url))
      (with-current-buffer buf
        (trekker-mode)

        (trekker-call-async "create_buffer" trekker--buffer-id url)

        (setq-local confirm-kill-processes nil)

        (set (make-local-variable 'trekker--buffer-url) url)

        (setq mode-name "Trekker")
        ))
    (add-hook 'window-size-change-functions #'trekker-monitor-window-size-change)
    (add-hook 'window-configuration-change-hook #'trekker-monitor-configuration-change)

    (switch-to-buffer buf))

  (message "Open url %s" url))

(provide 'trekker)

;;; trekker.el ends here
