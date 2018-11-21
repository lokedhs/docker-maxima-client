(sb-ext:restrict-compiler-policy 'safety 3)
(sb-ext:restrict-compiler-policy 'debug 3)

(ql:quickload "maxima-client")

(defun start-maxima ()
  (let ((appdir-env (sb-ext:posix-getenv "APPDIR")))
    (unless appdir-env
      (error "APPDIR is not set"))
    (let ((path (make-pathname :directory appdir-env)))
      (setq maxima-client.common:*font-directory* (merge-pathnames "maxima-client/fonts/" path))
      (setq maxima-client.common:*image-directory* (merge-pathnames "maxima-client/images/" path))
      (setq freetype2:*library* (freetype2:make-freetype))
      (maxima-client:maxima-client))))

(sb-ext:save-lisp-and-die "clim-maxima" :executable t :toplevel #'start-maxima)
