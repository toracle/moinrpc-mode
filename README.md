# MoinRPC-mode

This mode enables to handle moinmoin wiki using Emacs.

Currently this is alpha stage, so not works well.


```
(setq wiki-setting
	  (moinrpc-create-wiki-setting
		"wooridle"
		"https://wiki.wooridle.net/?action=xmlrpc2"
		"<username here>"))

(moinrpc-create-page-buffer wiki-setting "FrontPage")
```

Then you can find a new buffer opened with FrontPage content. From here you can navigate 


## Features

 - Create or Edit wiki page
 - List of all pages


## Keys

 - C-x C-f: Open page or create a new one (using Helm)
 - C-x C-s: Save page

## Alternatives

There are some other modes for moinmoin supports:

 - moomin-el
 - moinmoin-mode


## To-do

 - C-c C-o to follow a WikiLink
 - Find reverse links of a page
 - User friendly wiki setting
