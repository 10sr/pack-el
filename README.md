pack.el
=======

Pack and unpack archive files


Overview
--------

This library provides some commands and functions to pack and unpack
archives.

Commands to pack/unpack archive files are defined in `pack-program-alist'.

Use from Dired
--------------

To pack/unpack files from dired buffers, add following to your init.el:

    (with-eval-after-load 'dired
      (define-key dired-mode-map "P" 'pack-dired-dwim))

Now you can create an archive file from marked files, or unpack the file when
only one file is selected and that seems to be an archive.

License
-------

This software is unlicensed.
