pack.el
=======

Pack and unpack archive files


Overview
--------

This library provides some commands and functions to pack and unpack
archives in a simple way.

Commands to pack/unpack archive files can be defined by setting
`pack-program-alist' variable.


Use from Dired
--------------

To pack/unpack files from dired buffers, add following to your init.el:

    (with-eval-after-load 'dired
      (define-key dired-mode-map "P" 'pack-dired-dwim))

This command creates an archive file from marked files, or unpack the file when
only one file is selected and that has an extension for archive.

License
-------

This software is unlicensed.
