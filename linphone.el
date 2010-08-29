;;; -*- show-trailing-whitespace: t -*-
;;; linphone.el --- Emacs interface to Linphone

;; Copyright (C) 2010 Yoni Rabkin
;;
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;

;;; Code:

(defvar linphcsh-binary "/usr/local/bin/linphonecsh")

(defvar linph-already-running-string
  (concat "A running linphonec has been"
	  " found, not spawning a second one."
	  "\n"))

(defvar linph-sip-accounts nil
  "List of lists. Each sub-list contains exactly three elements
as strings. First the SIP identity of the user, then the SIP
proxy and finally the password.")

(defun linph-wait (message sec)
  (if (and (not (stringp message))
	   (not (integerp sec)))
      (error "bad arguments: %s %s" message sec)
    (let ((ellipsis "."))
      (dotimes (c sec)
	(message "%s%s" message ellipsis)
	(sleep-for 1)
	(setq ellipsis (concat ellipsis "."))))))

(defun linph-command (&rest args)
  (with-temp-buffer
    (condition-case nil
	(apply 'call-process (append
			      `(,linphcsh-binary)
			      `(nil t nil)
			      args))
      (file-error (error "linphonecsh binary not found")))
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun linph-parse-status (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (if (re-search-forward "^\\(.*\\)=\\(.*\\)$" (point-max) t)
	(let ((command (match-string-no-properties 1))
	      (status  (match-string-no-properties 2)))
	  (if (and command status)
	      (cons command status)
	    (error "could not parse: %s" str)))
      (error "could not parse: %s" str))))

(defun linph-assert-alive ()
  "Throw an error if linphonec isn't running."
  (when (not (linph-command-alive-p))
    (error "linphonec isn't running")))

(defun linph-get-status (&rest commands)
  (linph-assert-alive)
  (linph-parse-status
   (apply 'linph-command commands)))

(defun linph-register (sip-identity sip-proxy password)
  (when (not (and (stringp sip-identity)
		  (stringp sip-proxy)
		  (stringp password)))
    (error "bad arguments: %s" sip-identity sip-proxy password))
  (when (not (linph-command-alive-p))
    (error "linphonec not running"))
  (let ((response
	 (linph-command "generic" "register"
			sip-identity sip-proxy password)))
    response))

(defun linph-command-init ()
  "Start a background instance of linphonec."
  (when (string= (linph-command "init")
		 linph-already-running-string)
    (error "linphonec already running."))
  (linph-wait "waiting for linphone to start" 2)
  (if (linph-command-alive-p)
      (message "linphonec successfully started")
    (error "could not start linphone")))

(defun linph-command-alive-p ()
  "Return a truth value if linphone is running."
  (let ((output (linph-command "status" "hook")))
    (cond ((string= (substring output 0 5) "hook=") t)
	  ((string= output
		    (concat "ERROR: Failed to connect"
			    " pipe: Connection refused\n")) nil)
	  (t (error "unhandled response: %s" output)))))

(defun linph-command-exit ()
  "Kill the running linphone."
  (linph-command "exit")
  (linph-wait "waiting for linphone to shut down" 2)
  (if (not (linph-command-alive-p))
      (message "linphonec successfully shut down")
    (error "failed to shut down linphonec")))

(provide 'linphone)

;;; linphone.el ends here.
