Novel Speaker Mode
==================

* Author : Xiang Wang (wxjeacen@gmail.com)
* Date :  2014.08.01

Description
-----------

* Novel Speaker Mode is a free open source project.
* Novel Speaker Mode is a minor mode for eyes free novel reading.
* Novel Speaker Mode is based on Google TTS Service.
* Novel Speaker Mode uses mplayer to play the vioce. Also, you can specify mplayer location path.
* HTTP Proxy can be set for novel speaker mode for accessing Google TTS.
* You can contribute it.


Version
-------

1.0

Installation
------------

---
    git clone [git-repo-url] novel-speaker-mode
    cd novel-speaker-mode
    cp novel-speaker-mode [your emacs load path]
---

User Novel Speaker Mode
-----------------------

---
    (require 'novel-speaker-mode)
---

##### Configure HTTP Proxy for Novel Speaker Mode
* Novel Speaker Mode only supports HTTP proxy.
* If you have a socks5 proxy, you can use polipo to change it as http proxy.
* All the Novel Speaker Mode command is using "novel-speaker-" as prefix.
