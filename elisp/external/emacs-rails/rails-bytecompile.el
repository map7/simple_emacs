(add-to-list 'load-path default-directory)

(require 'rails)

(mapcar
 #'byte-compile-file
 (directory-files "./" t "\\.el$"))