<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: buffer-move.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=buffer-move.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: buffer-move.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=buffer-move.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for buffer-move.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=buffer-move.el" />
<script type="text/javascript">
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-2101513-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
</script>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/Glossary">Glossary</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22buffer-move.el%22">buffer-move.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="http://www.emacswiki.org/emacs/download/buffer-move.el">Download</a></p><pre class="code"><span class="linecomment">;;; buffer-move.el --- Swap buffers without typing C-x b on each window</span>

<span class="linecomment">;; Copyright (C) 2004  Lucas Bonnet &lt;lukhas@free.fr&gt;</span>

<span class="linecomment">;; Author: Lucas Bonnet &lt;lucas@rincevent.net&gt;</span>
<span class="linecomment">;; Keywords: lisp,convenience</span>
<span class="linecomment">;; Version: 0.4</span>
<span class="linecomment">;; URL : http://lukhas.free. fr/emacs/elisp/buffer-move.el</span>

<span class="linecomment">;; This program is free software; you can redistribute it and/or</span>
<span class="linecomment">;; modify it under the terms of the GNU General Public License</span>
<span class="linecomment">;; as published by the Free Software Foundation; either version 2</span>
<span class="linecomment">;; of the License, or (at your option) any later version.</span>

<span class="linecomment">;; This program is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>

<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with this program; if not, write to the Free Software</span>
<span class="linecomment">;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA</span>
<span class="linecomment">;; 02111-1307, USA.</span>

<span class="linecomment">;;; Commentary:</span>

<span class="linecomment">;; This file is for lazy people wanting to swap buffers without</span>
<span class="linecomment">;; typing C-x b on each window. This is useful when you have :</span>

<span class="linecomment">;; +--------------+-------------+</span>
<span class="linecomment">;; |              |             |</span>
<span class="linecomment">;; |    #emacs    |    #gnus    |</span>
<span class="linecomment">;; |              |             |</span>
<span class="linecomment">;; +--------------+-------------+</span>
<span class="linecomment">;; |                            |</span>
<span class="linecomment">;; |           .emacs           |</span>
<span class="linecomment">;; |                            |</span>
<span class="linecomment">;; +----------------------------+</span>

<span class="linecomment">;; and you want to have :</span>

<span class="linecomment">;; +--------------+-------------+</span>
<span class="linecomment">;; |              |             |</span>
<span class="linecomment">;; |    #gnus     |   .emacs    |</span>
<span class="linecomment">;; |              |             |</span>
<span class="linecomment">;; +--------------+-------------+</span>
<span class="linecomment">;; |                            |</span>
<span class="linecomment">;; |           #emacs           |</span>
<span class="linecomment">;; |                            |</span>
<span class="linecomment">;; +----------------------------+</span>

<span class="linecomment">;; With buffer-move, just go in #gnus, do buf-move-left, go to #emacs</span>
<span class="linecomment">;; (which now should be on top right) and do buf-move-down.</span>

<span class="linecomment">;; To use it, simply put a (require 'buffer-move) in your ~/.emacs and</span>
<span class="linecomment">;; define some keybindings. For example, i use :</span>

<span class="linecomment">;; (global-set-key (kbd "&lt;C-S-up&gt;")     'buf-move-up)</span>
<span class="linecomment">;; (global-set-key (kbd "&lt;C-S-down&gt;")   'buf-move-down)</span>
<span class="linecomment">;; (global-set-key (kbd "&lt;C-S-left&gt;")   'buf-move-left)</span>
<span class="linecomment">;; (global-set-key (kbd "&lt;C-S-right&gt;")  'buf-move-right)</span>


<span class="linecomment">;;; Code:</span>


(require 'windmove)


<span class="linecomment">;;;###autoload</span>
(defun buf-move-up ()
  "<span class="quote">Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled.</span>"
<span class="linecomment">;;  "Switches between the current buffer, and the buffer above the</span>
<span class="linecomment">;;  split, if possible."</span>
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "<span class="quote">No window above this one</span>")
      <span class="linecomment">;; swap top with this one</span>
      (set-window-buffer (selected-window) (window-buffer other-win))
      <span class="linecomment">;; move this one to top</span>
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

<span class="linecomment">;;;###autoload</span>
(defun buf-move-down ()
"<span class="quote">Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled.</span>"
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "<span class="quote">^ \\*Minibuf</span>" (buffer-name (window-buffer other-win))))
        (error "<span class="quote">No window under this one</span>")
      <span class="linecomment">;; swap top with this one</span>
      (set-window-buffer (selected-window) (window-buffer other-win))
      <span class="linecomment">;; move this one to top</span>
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

<span class="linecomment">;;;###autoload</span>
(defun buf-move-left ()
"<span class="quote">Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled.</span>"
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "<span class="quote">No left split</span>")
      <span class="linecomment">;; swap top with this one</span>
      (set-window-buffer (selected-window) (window-buffer other-win))
      <span class="linecomment">;; move this one to top</span>
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

<span class="linecomment">;;;###autoload</span>
(defun buf-move-right ()
"<span class="quote">Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled.</span>"
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "<span class="quote">No right split</span>")
      <span class="linecomment">;; swap top with this one</span>
      (set-window-buffer (selected-window) (window-buffer other-win))
      <span class="linecomment">;; move this one to top</span>
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


(provide 'buffer-move)
<span class="linecomment">;;; buffer-move.el ends here</span></span></pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/Glossary">Glossary</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=buffer-move.el;missing=de_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="comment local" href="http://www.emacswiki.org/emacs/Comments_on_buffer-move.el">Talk</a> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=buffer-move.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=buffer-move.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=buffer-move.el">Administration</a></span><!-- test --><span class="time"><br /> Last edited 2011-03-05 02:49 UTC by <a class="author" title="from 96-24-39-95.tpa.clearwire-wmx.net" href="http://www.emacswiki.org/emacs/K.AdamChristensen">K.AdamChristensen</a> <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=buffer-move.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
