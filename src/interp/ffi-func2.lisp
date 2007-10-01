(in-package "BOOT")
#+:clisp
(progn
     (FFI:DEFAULT-FOREIGN-LIBRARY *libspad_pathname*)
     (FFI:DEF-CALL-OUT |writeablep| 
          (:NAME "writeablep")
          (:arguments (filename FFI:C-STRING))
          (:return-type ffi:int)
          (:language :stdc))
     (FFI:DEF-CALL-OUT open_server
          (:NAME "open_server")
          (:arguments (server_name FFI:C-STRING))
          (:return-type ffi:int)
          (:language :stdc))
     (FFI:DEF-CALL-OUT sock_get_int
          (:NAME "sock_get_int")
          (:arguments (purpose ffi:int))
          (:return-type ffi:int)
          (:language :stdc))
     (FFI:DEF-CALL-OUT sock_send_int
          (:NAME "sock_send_int")
          (:arguments (purpose ffi:int) (val ffi:int))
          (:return-type ffi:int)
          (:language :stdc))
     (FFI:DEF-CALL-OUT sock_get_float
          (:NAME "sock_get_float")
          (:arguments (purpose ffi:int))
          (:return-type DOUBLE-FLOAT)
          (:language :stdc))
     (FFI:DEF-CALL-OUT sock_send_float_raw
          (:NAME "sock_send_float")
          (:arguments (purpose ffi:int) (num DOUBLE-FLOAT))
          (:return-type ffi:int)
          (:language :stdc))
     (defun sock_send_float (purpose num)
          (sock_send_float_raw purpose (float num 1.0d0)))
     (FFI:DEF-CALL-OUT sock_get_string_buf
          (:NAME "sock_get_string_buf")
          (:arguments (purpose ffi:int)
                      (buf (FFI:C-POINTER (FFI:C-ARRAY FFI::char 10000)))
                      (len ffi:int))
          (:return-type ffi:int)
          (:language :stdc))
     (defun |sockGetStringFrom| (purpose)
        (let ((buf nil))
        (FFI:WITH-C-VAR (tmp-buf '(FFI:C-ARRAY
                                           FFI::char 10000))
           (sock_get_string_buf purpose (FFI:C-VAR-ADDRESS tmp-buf) 10000)
             (prog ((len2 10000))
                 (dotimes (i 10000)
                      (if (eql 0 (FFI:ELEMENT tmp-buf i))
                          (progn
                                (setf len2 i)
                                (go nn1))))
                nn1
                 (setf buf (make-string len2))
                 ;;; (format t "len2 = ~A~%" len2)
                 ;;; (force-output)
                 (dotimes (i len2)
                      (setf (aref buf i)
                          (code-char 
                                        (FFI:ELEMENT tmp-buf i)))))

         )
        buf))
      (FFI:DEF-CALL-OUT sock_send_string
          (:NAME "sock_send_string")
          (:arguments (purpose ffi:int)
                      (str FFI:C-STRING))
          (:return-type ffi:int)
          (:language :stdc))
      (FFI:DEF-CALL-OUT sock_send_string_len
          (:NAME "sock_send_string_len")
          (:arguments (purpose ffi:int)
                      (str FFI:C-STRING)
                      (len ffi:int))
          (:return-type ffi:int)
          (:language :stdc))
      (FFI:DEF-CALL-OUT server_switch
          (:NAME "server_switch")
          (:return-type ffi:int)
          (:language :stdc))
      (FFI:DEF-CALL-OUT sock_send_signal
          (:NAME "sock_send_signal")
          (:arguments (purpose ffi:int) (sig ffi:int))
          (:return-type ffi:int)
          (:language :stdc))
)
