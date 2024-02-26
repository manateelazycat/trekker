;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro trekker-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar trekker-deferred-debug nil
  "Debug output switch.")

(defvar trekker-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun trekker-deferred-log (&rest args)
  "[internal] Debug log function."
  (when trekker-deferred-debug
    (with-current-buffer (get-buffer-create "*trekker-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" trekker-deferred-debug-count (apply #'format args)))))
    (cl-incf trekker-deferred-debug-count)))

(defvar trekker-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro trekker-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`trekker-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal trekker-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar trekker-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar trekker-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `trekker-deferred-post-task' and `trekker-deferred-worker'.")

(defun trekker-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`trekker-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack trekker-deferred-queue)
    (trekker-deferred-log "QUEUE-POST [%s]: %s" (length trekker-deferred-queue) pack)
    (run-at-time trekker-deferred-tick-time nil 'trekker-deferred-worker)
    d))

(defun trekker-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when trekker-deferred-queue
    (let* ((pack (car (last trekker-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq trekker-deferred-queue (nbutlast trekker-deferred-queue))
      (condition-case err
          (setq value (trekker-deferred-exec-task d which arg))
        (error
         (trekker-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: trekker-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `trekker-deferred-resignal')
;; cancel      : a canceling function (default `trekker-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct trekker-deferred-object
  (callback 'identity)
  (errorback 'trekker-deferred-resignal)
  (cancel 'trekker-deferred-default-cancel)
  next status value)

(defun trekker-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun trekker-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (trekker-deferred-log "CANCEL : %s" d)
  (setf (trekker-deferred-object-callback d) 'identity)
  (setf (trekker-deferred-object-errorback d) 'trekker-deferred-resignal)
  (setf (trekker-deferred-object-next d) nil)
  d)

(defun trekker-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (trekker-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "trekker-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (trekker-deferred-object-callback d)
                    (trekker-deferred-object-errorback d)))
        (next-deferred (trekker-deferred-object-next d)))
    (cond
     (callback
      (trekker-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((trekker-deferred-object-p value)
                                             (trekker-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (trekker-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (trekker-deferred-post-task next-deferred 'ok value)
                                               (setf (trekker-deferred-object-status d) 'ok)
                                               (setf (trekker-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (trekker-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (trekker-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (trekker-deferred-object-status d) 'ng)
                                            (setf (trekker-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (trekker-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (trekker-deferred-resignal arg)))))))

(defun trekker-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (trekker-deferred-object-next prev) next)
  (cond
   ((eq 'ok (trekker-deferred-object-status prev))
    (setf (trekker-deferred-object-status prev) nil)
    (let ((ret (trekker-deferred-exec-task
                next 'ok (trekker-deferred-object-value prev))))
      (if (trekker-deferred-object-p ret) ret
        next)))
   ((eq 'ng (trekker-deferred-object-status prev))
    (setf (trekker-deferred-object-status prev) nil)
    (let ((ret (trekker-deferred-exec-task next 'ng (trekker-deferred-object-value prev))))
      (if (trekker-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun trekker-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-trekker-deferred-object :callback callback)
    (make-trekker-deferred-object)))

(defun trekker-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (trekker-deferred-exec-task d 'ok arg))

(defun trekker-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (trekker-deferred-exec-task d 'ng arg))

(defun trekker-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (trekker-deferred-post-task d 'ok arg))

(defun trekker-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (trekker-deferred-callback-post (trekker-deferred-new callback))."
  (let ((d (if callback
               (make-trekker-deferred-object :callback callback)
             (make-trekker-deferred-object))))
    (trekker-deferred-callback-post d arg)
    d))

(defun trekker-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-trekker-deferred-object :callback callback)))
    (trekker-deferred-set-next d nd)))

(defun trekker-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-trekker-deferred-object :errorback callback)))
    (trekker-deferred-set-next d nd)))

(defvar trekker-epc-debug nil)

(defun trekker-epc-log (&rest args)
  (when trekker-epc-debug
    (with-current-buffer (get-buffer-create "*trekker-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun trekker-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar trekker-epc-uid 1)

(defun trekker-epc-uid ()
  (cl-incf trekker-epc-uid))

(defvar trekker-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct trekker-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun trekker-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return trekker-epc-connection object."
  (trekker-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (trekker-epc-uid))
         (connection-name (format "trekker-epc con %s" connection-id))
         (connection-buf (trekker-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-trekker-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (trekker-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (trekker-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (trekker-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun trekker-epc-process-sentinel (connection process msg)
  (trekker-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (trekker-epc-connection-name connection) process msg)
  (trekker-epc-disconnect connection))

(defun trekker-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (trekker-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (trekker-epc-connection-process connection)))
    (trekker-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun trekker-epc-disconnect (connection)
  (let ((process (trekker-epc-connection-process connection))
        (buf (trekker-epc-connection-buffer connection))
        (name (trekker-epc-connection-name connection)))
    (trekker-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (trekker-epc-log "!! Disconnected finished [%s]" name)))

(defun trekker-epc-process-filter (connection process message)
  (trekker-epc-log "INCOMING: [%s] [%S]" (trekker-epc-connection-name connection) message)
  (with-current-buffer (trekker-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (trekker-epc-process-available-input connection process)))

(defun trekker-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (trekker-deferred-new callback)
             (trekker-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun trekker-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (trekker-deferred-callback-post d event))))

(defun trekker-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (trekker-epc-net-have-input-p)
      (let ((event (trekker-epc-net-read-or-lose process))
            (ok nil))
        (trekker-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'trekker-epc-signal-send
                         (cons (trekker-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (trekker-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (trekker-epc-process-available-input connection process)))))))

(defun trekker-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (trekker-epc-net-decode-length))))

(defun trekker-epc-net-read-or-lose (_process)
  (condition-case error
      (trekker-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun trekker-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (trekker-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun trekker-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun trekker-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct trekker-epc-manager
  "Root object that holds all information related to an EPC activity.

`trekker-epc-start-epc' returns this object.

title          : instance name for displaying on the `trekker-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : trekker-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct trekker-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar trekker-epc-live-connections nil
  "[internal] A list of `trekker-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun trekker-epc-server-process-name (uid)
  (format "trekker-epc-server:%s" uid))

(defun trekker-epc-server-buffer-name (uid)
  (format " *%s*" (trekker-epc-server-process-name uid)))

(defun trekker-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (trekker-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (trekker-epc-disconnect (trekker-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 trekker-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq trekker-epc-live-connections (delete mngr trekker-epc-live-connections))
    ))

(defun trekker-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun trekker-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an trekker-epc-connection instance."
  (let* ((mngr mngr)
         (conn (trekker-epc-manager-connection mngr))
         (channel (trekker-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (trekker-epc-log "SIG CALL: %S" args)
                    (apply 'trekker-epc-handler-called-method ,mngr (trekker-epc-args args))))
               (return
                . (lambda (args)
                    (trekker-epc-log "SIG RET: %S" args)
                    (apply 'trekker-epc-handler-return ,mngr (trekker-epc-args args))))
               (return-error
                . (lambda (args)
                    (trekker-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'trekker-epc-handler-return-error ,mngr (trekker-epc-args args))))
               (epc-error
                . (lambda (args)
                    (trekker-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'trekker-epc-handler-epc-error ,mngr (trekker-epc-args args))))
               (methods
                . (lambda (args)
                    (trekker-epc-log "SIG METHODS: %S" args)
                    (trekker-epc-handler-methods ,mngr (caadr args))))
               ) do
             (trekker-epc-signal-connect channel method body))
    (push mngr trekker-epc-live-connections)
    mngr))

(defun trekker-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (trekker-epc-manager-connection mngr)))
    (trekker-epc-net-send conn (cons method messages))))

(defun trekker-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (trekker-epc-manager-methods mngr)
           if (eq method-name (trekker-epc-method-name i))
           do (cl-return i)))

(defun trekker-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (trekker-epc-manager-methods mngr)
                  collect
                  (list
                   (trekker-epc-method-name i)
                   (or (trekker-epc-method-arg-specs i) "")
                   (or (trekker-epc-method-docstring i) "")))))
    (trekker-epc-manager-send mngr 'return uid info)))

(defun trekker-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (trekker-epc-manager-methods mngr))
           (method (trekker-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (trekker-epc-log "ERR: No such method : %s" name)
        (trekker-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (trekker-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((trekker-deferred-object-p ret)
                (trekker-deferred-nextc ret
                                          (lambda (xx) (trekker-epc-manager-send mngr 'return uid xx))))
               (t (trekker-epc-manager-send mngr 'return uid ret))))
          (error
           (trekker-epc-log "ERROR : %S" err)
           (trekker-epc-manager-send mngr 'return-error uid err))))))))

(defun trekker-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (trekker-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (trekker-epc-manager-sessions mngr) ret)))

(defun trekker-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (trekker-epc-manager-sessions mngr))))
    (cond
     (pair
      (trekker-epc-log "RET: id:%s [%S]" uid args)
      (trekker-epc-manager-remove-session mngr uid)
      (trekker-deferred-callback (cdr pair) args))
     (t                                 ; error
      (trekker-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun trekker-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (trekker-epc-manager-sessions mngr))))
    (cond
     (pair
      (trekker-epc-log "RET-ERR: id:%s [%S]" uid args)
      (trekker-epc-manager-remove-session mngr uid)
      (trekker-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (trekker-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun trekker-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (trekker-epc-manager-sessions mngr))))
    (cond
     (pair
      (trekker-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (trekker-epc-manager-remove-session mngr uid)
      (trekker-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (trekker-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun trekker-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (trekker-epc-uid))
        (sessions (trekker-epc-manager-sessions mngr))
        (d (trekker-deferred-new)))
    (push (cons uid d) sessions)
    (setf (trekker-epc-manager-sessions mngr) sessions)
    (trekker-epc-manager-send mngr 'call uid method-name args)
    d))

(defun trekker-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-trekker-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (trekker-epc-manager-methods mngr))))
    (setf (trekker-epc-manager-methods mngr) methods)
    method))

(defun trekker-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'trekker-epc-nothing))
    (trekker-deferred-chain
     d
     (trekker-deferred-nextc it
                               (lambda (x) (setq result x)))
     (trekker-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'trekker-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (trekker-epc-connection-process (trekker-epc-manager-connection mngr))
         0 trekker-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun trekker-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (trekker-epc-sync mngr (trekker-epc-call-deferred mngr method-name args)))

(defun trekker-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (trekker-epc-connection-process (trekker-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar trekker-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`trekker-epc-manager' instance]).
When the server process accepts the client connection, the
`trekker-epc-manager' instance is created and stored in this variable
`trekker-epc-server-client-processes'. This variable is used for the management
purpose.")

;; trekker-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `trekker-epc-manager' instances
(cl-defstruct trekker-epc-server name process port connect-function)

(defvar trekker-epc-server-processes nil
  "[internal] A list of ([process object] . [`trekker-epc-server' instance]).
This variable is used for the management purpose.")

(defun trekker-epc-server-get-manager-by-process (proc)
  "[internal] Return the trekker-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in trekker-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun trekker-epc-server-accept (process)
  "[internal] Initialize the process and return trekker-epc-manager object."
  (trekker-epc-log "TREKKER-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (trekker-epc-uid))
         (connection-name (format "trekker-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-trekker-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (trekker-epc-log "TREKKER-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (trekker-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (trekker-epc-process-sentinel connection p e)))
    (make-trekker-epc-manager :server-process process :port t
                                :connection connection)))

(defun trekker-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (trekker-epc-log "TREKKER-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (trekker-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (trekker-epc-server-accept process)))
            (push (cons process mngr) trekker-epc-server-client-processes)
            (trekker-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (trekker-epc-log "TREKKER-EPC-SERVER- Protocol error: %S" err)
         (trekker-epc-log "TREKKER-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process trekker-epc-server-client-processes)) _d)
        (when pair
          (trekker-epc-log "TREKKER-EPC-SERVER- DISCONNECT %S" process)
          (trekker-epc-stop-epc (cdr pair))
          (setq trekker-epc-server-client-processes
                (assq-delete-all process trekker-epc-server-client-processes))
          ))
      nil))))

(defun trekker-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "TREKKER EPC Server %s" (trekker-epc-uid)))
       (buf (trekker-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (trekker-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-trekker-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          trekker-epc-server-processes)
    main-process))

(provide 'trekker-epc)
;;; trekker-epc.el ends here
