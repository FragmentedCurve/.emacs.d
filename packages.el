(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "caa92f1d64fc25480551757d854b4b49981dfa6b") ; 2.4.1
 ("elfeed" "https://github.com/skeeto/elfeed"
  "362bbe5b38353d033c5299f621fea39e2c75a5e0"  ; 3.4.0
  :removal '("^web$"))
 ("youtube-dl" "https://github.com/skeeto/youtube-dl-emacs"
  "af877b5bc4f01c04fccfa7d47a2c328926f20ef4") ; 2018-10-12T15:08:06Z
 ("evil" "https://github.com/emacs-evil/evil"
  "067a29214d9d4e1fc4b4d29fc596ad4cefe7e492"  ; 2022-04-03T14:59:29Z
  :removal '("evil-tests.el" "^ert.el"))
 ("fasm-mode" "https://github.com/the-little-language-designer/fasm-mode.git"
  "cb4b9bf48c7f05530e140456eb1f4a54716470f9")

 ;; magit & deps
 ("compat" "https://github.com/emacs-compat/compat.git"
  "74300f16a1630a33a86710aa20c1fc26f5f89f75") ; 29.1.4.2
 ("dash" "https://github.com/magnars/dash.el.git"
  "39d067b9fbb2db65fc7a6938bfb21489ad990cb4") ; 2.19.1
 ("transient" "https://github.com/magit/transient.git"
  "8cf1238181d57504e68f42fa0d4ef66784b197a9") ; 0.4.3
 ("with-editor" "https://github.com/magit/with-editor.git"
  "d5f3f06cb830311e60c58f656988ef37c05a99e0") ; 3.3.2
 ("magit" "https://github.com/magit/magit.git"
  "f44f6c14500476d918e9c01de8449edb20af4113")) ; 3.3.0


;; Set up some extra load-path directories
(add-to-list 'load-path (gpkg-path "evil" "lib"))
(add-to-list 'load-path (gpkg-path "dash"))
(add-to-list 'load-path (gpkg-path "transient" "lisp"))
(add-to-list 'load-path (gpkg-path "with-editor"))
(add-to-list 'load-path (gpkg-path "magit" "lisp"))
