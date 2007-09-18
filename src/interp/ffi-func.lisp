(in-package "BOOT")
;;; (use-package "SB-ALIEN")
#+sbcl 
(progn
     (setf SB-ALIEN::*DEFAULT-C-STRING-EXTERNAL-FORMAT* :LATIN-1)
     (setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :LATIN-1)
     (SB-ALIEN::define-alien-routine
           ("writeablep" |writeablep|) SB-ALIEN::int
           (filename SB-ALIEN::c-string))
     (SB-ALIEN::define-alien-routine
           ("open_server" open_server) SB-ALIEN::int
           (server_name SB-ALIEN::c-string))
     (SB-ALIEN::define-alien-routine
          ("sock_get_int" sock_get_int) SB-ALIEN::int
          (purpose SB-ALIEN::int :in))
     (SB-ALIEN::define-alien-routine
          ("sock_send_int" sock_send_int) SB-ALIEN::int
          (purpose SB-ALIEN::int :in)
          (val SB-ALIEN::int :in))
     (SB-ALIEN::define-alien-routine
          ("sock_get_float" sock_get_float) SB-ALIEN::double
          (purpose SB-ALIEN::int :in))
     (SB-ALIEN::define-alien-routine
          ("sock_send_float" sock_send_float) SB-ALIEN::int
          (purpose SB-ALIEN::int :in)
          (num SB-ALIEN::double :in))

;;;     (defun sock_get_string_buf (purpose buf len))
     (defun |sockGetStringFrom| (purpose)
         ;;; (format t "sock_get_string_buf~%")
         ;;; (force-output)
         (let ((buf nil))
         (SB-ALIEN::with-alien ((tmp-buf (SB-ALIEN::array 
                                          SB-ALIEN::char 10000)))
             (SB-ALIEN::alien-funcall 
                  (SB-ALIEN::extern-alien 
                      "sock_get_string_buf" 
                           (SB-ALIEN::function SB-ALIEN::void
                               SB-ALIEN::int 
                               (SB-ALIEN::* SB-ALIEN::char) 
                               SB-ALIEN::int))
                 purpose
                 (SB-ALIEN::addr (SB-ALIEN::deref tmp-buf 0))
                 10000)
             ;;; (format t "sock_get_string_buf returned~%")
             ;;; (force-output)
             (prog ((len2 10000))
                 (dotimes (i 10000)
                      (if (eql 0 (SB-ALIEN::deref tmp-buf i))
                          (progn 
                                (setf len2 i)
                                (go nn1))))
                nn1
                 (setf buf (make-string len2))
                 ;;; (format t "len2 = ~A~%" len2)
                 ;;; (force-output)
                 (dotimes (i len2)
                      (setf (aref buf i) 
                          (code-char (SB-ALIEN::deref tmp-buf i))))
                 ;;; (setf (aref buf len2) (code-char 0))
             ;;; (format t "~S~%" buf)
             ;;; (force-output)
             )
             
             )
             buf))
          
     (SB-ALIEN::define-alien-routine
          ("sock_send_string" sock_send_string) SB-ALIEN::int
          (purpose SB-ALIEN::int :in)
          (str SB-ALIEN::c-string))
     (SB-ALIEN::define-alien-routine
          ("sock_send_string_len" sock_send_string_len) SB-ALIEN::int
          (purpose SB-ALIEN::int :in)
          (str SB-ALIEN::c-string)
          (len SB-ALIEN::int :in))
     (SB-ALIEN::define-alien-routine ("server_switch" server_switch)
          SB-ALIEN::int)
     (SB-ALIEN::define-alien-routine
          ("sock_send_signal" sock_send_signal) SB-ALIEN::int
          (purpose SB-ALIEN::int :in)
          (sig SB-ALIEN::int :in))
)
