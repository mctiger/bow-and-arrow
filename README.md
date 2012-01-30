This game is a remake of the old Win95 game named Bow & Arrow.
This remake is entireley written in Common Lisp. 
This package depends on lispbuilder-sdl.
You can get lispbuilder-sdl with [Quicklisp](http://www.quicklisp.org).

## Usage :
```
$ git clone https://github.com/ckairaba/bow-and-arrow.git
$ cd bow-and-arrow
$ sbcl
* (pushnew "./" asdf:*central-registry*)
* (ql:quickload "bow-and-arrow")
* (bow-and-arrow:play)
```

## Play :
```
right click to armed
left click to stand (maintain)
```

<img src="https://github.com/downloads/ckairaba/bow-and-arrow/screenshot.png" height="250" width="450" align="center" />


This package is theoretically portable. We just tested the package on
sbcl/linux, ccl/linux, clisp/linux and cmucl/linux.
We have implemented 3 levels. So, the next levels will come
soon.
