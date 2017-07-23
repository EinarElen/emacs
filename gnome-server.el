(require 'dbus)
;;; save & shutdown when we get an "end of session" signal on dbus 
(defun my-register-signals (client-path)
  "Register for the 'QueryEndSession' and 'EndSession' signals from
Gnome SessionManager.

When we receive 'QueryEndSession', we just respond with
'EndSessionResponse(true, \"\")'.  When we receive 'EndSession', we
append this EndSessionResponse to kill-emacs-hook, and then call
kill-emacs.  This way, we can shut down the Emacs daemon cleanly
before we send our 'ok' to the SessionManager."
  (setq my-gnome-client-path client-path)
  (let ( (end-session-response (lambda (&optional arg)
                                 (dbus-call-method-asynchronously
                                  :session "org.gnome.SessionManager" my-gnome-client-path
                                  "org.gnome.SessionManager.ClientPrivate" "EndSessionResponse" nil
                                  t "") ) ) )
    (dbus-register-signal
     :session "org.gnome.SessionManager" my-gnome-client-path
     "org.gnome.SessionManager.ClientPrivate" "QueryEndSession"
     end-session-response )
    (dbus-register-signal
     :session "org.gnome.SessionManager" my-gnome-client-path
     "org.gnome.SessionManager.ClientPrivate" "EndSession"
     `(lambda (arg)
        (add-hook 'kill-emacs-hook ,end-session-response t)
        (kill-emacs) ) ) ) )

;; DESKTOP_AUTOSTART_ID is set by the Gnome desktop manager when emacs
;; is autostarted.  We can use it to register as a client with gnome
;; SessionManager.
(dbus-call-method-asynchronously
 :session "org.gnome.SessionManager"
 "/org/gnome/SessionManager" 
 "org.gnome.SessionManager" "RegisterClient" 'my-register-signals
 "Emacs server" (getenv "DESKTOP_AUTOSTART_ID"))
