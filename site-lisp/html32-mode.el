;;; -------------------------------- CONTENTS --------------------------------
;;;
;;; HTML 3.2 mode, based on text mode
;;; file: html32-mode.el
;;;
;;; Major mode for editing HTML 2.x, 3.x documents.
;;; (This is no browsing mode.)
;;; Revision: 1.02 beta
;;; 
;;; ------------------------------- COPYRIGHT --------------------------------
;;;
;;; Copyright (C) 1985 Free Software Foundation, Inc. 
;;; Copyright (C) 1992, 1993 National Center for Supercomputing Applications.
;;; Copyright (C) 1995, 1997 University of Saarland (Germany), 
;;;                          Dept. of Computer Science.
;;; NCSA modifications by Marc Andreessen (marca@ncsa.uiuc.edu).
;;; Uni-SB modifications by Axel Beckert (abe@cs.uni-sb.de),
;;;                         Eva Stopp (eva@cs.uni-sb.de).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; -------------------------------- REQUIRES --------------------------------
;;;
;;; HTML32 mode requires easymenu.el for providing the menu. One
;;; version of easymenu.el is part of GNU Emacs. It supports at least
;;; GNU Emacs. But there exists another easymenu.el version (by Per
;;; Abrahamsen) which supports also other emacs version than GNU
;;; Emacs. You can disable the easymenu menu by setting the variable
;;; html32-using-easymenu to nil.
;;;
;;; You are assumed to be at least somewhat familiar with the HTML
;;; format.  If you aren't, read about it first.
;;;
;;; ------------------------------ INSTRUCTIONS ------------------------------
;;;
;;; Put the following code in your .emacs file:
;;;
;;; (autoload 'html32-mode "html32-mode" "HTML major mode." t)
;;; (or (assoc "\\.html$" auto-mode-alist)
;;;   (setq auto-mode-alist (cons '("\\.html$" . html32-mode) 
;;;                               auto-mode-alist)))
;;; (or (assoc "\\.shtml$" auto-mode-alist)
;;;   (setq auto-mode-alist (cons '("\\.shtml$" . html32-mode) 
;;;                               auto-mode-alist)))
;;; (or (assoc "\\.htm$" auto-mode-alist)
;;;   (setq auto-mode-alist (cons '("\\.htm$" . html32-mode) 
;;;                               auto-mode-alist)))
;;;
;;; Emacs will detect ".html", ".shtml" and ".htm" suffixes and
;;; activate html32-mode appropriately.
;;;
;;; ------------------------------- KNOWN BUGS -------------------------------
;;;
;;; <TITLE>Emacs HTML 3.2 Mode - Source Code</TITLE>
;;; (The line above is needed by our site map script, because it
;;;  always found the "..." in the example beyond as title of the
;;;  document when generating the site map. :-)
;;;
;;; The mode is a little bit slow on some machines when fontifying big
;;; files.
;;;  
;;; ------------------------------ WHAT HTML IS ------------------------------
;;;
;;; HTML (HyperText Markup Language) is a format for hypertext
;;; documents, particularly in the World Wide Web (WWW).  For more
;;; information on HTML go to http://www.w3.org/MarkUp/ at the
;;; World Wide Web Consortium.
;;;
;;; If you know HTML quite well and only need to look up some specific
;;; tag, have a look at the very compact HTML 3.2 Short Reference at
;;; http://fsinfo.cs.uni-sb.de/~lynx/help/html32.html
;;; 
;;; ------------------------- WHAT HTML32-MODE IS NOT ------------------------
;;;
;;; html32-mode is not a mode for *browsing* HTML documents.  In
;;; particular, html32-mode provides no hypertext or World Wide Web
;;; capabilities.
;;;
;;; See ftp://ftp.cs.indiana.edu/pub/elisp/w3/ for w3.el, which is
;;; an Elisp World Wide Web browser written by William Perry.
;;;
;;; The World Wide Web browser NCSA (naturally) recommend is NCSA
;;; Mosaic, which can be found at ftp.ncsa.uiuc.edu in /Mosaic.
;;;
;;; I don't want to recommend any browser here, because it's
;;; dependent of someone's flavour. What's more important, is that you
;;; shouldn't write HTML documents, which fit perfectly on your
;;; favourite browser but looks horrible (or is unreadable) on every
;;; other browser. Especially they should be writeen, so that they can
;;; be read by text browsers, such as lynx. This is why
;;; html32-add-image always prompts for an alternative text.
;;; 
;;; For more details on that topic have a look at the "Campaign For A
;;; Browser-Independent Web": http://www.anybrowser.org/campaign/
;;; 
;;; ---------------------------- ACKNOWLEDGEMENTS ----------------------------
;;;
;;; Some code herein provided by:
;;;   Dan Connolly (connolly@pixel.convex.com),
;;;   Markus Bolz (paulchen@cs.uni-sb.de)
;;;                         
;;; --------------------------- LCD Archive Entry ----------------------------
;;; 
;;; LCD Archive Entry:
;;; html32-mode|Axel Beckert|abe@cs.uni-sb.de|
;;; Major mode for editing HTML 2.x and 3.x documents.|
;;; 23-Mar-1998|1.02 beta||
;;; 
;;; ------------------------- HTML 3.2 Mode Web Site -------------------------
;;; 
;;; The actual beta version of this mode (possibly unstable) can be
;;; found at ftp://fsinfo.cs.uni-sb.de/pub/abe/emacs/. The last stable
;;; version can be found at ftp://fsinfo.cs.uni-sb.de/pub/abe/emacs/stable/.
;;;
;;; A complete online documentation will be available soon at
;;; http://fsinfo.cs.uni-sb.de/~abe/htmlmode/
;;; 
;;; -------------------------------- GOTCHAS ---------------------------------
;;;
;;; HTML documents can be tricky.  html32-mode is not smart enough to
;;; enforce correctness or sanity, so you have to do that yourself.
;;;
;;; In particular, html32-mode is smart enough to generate unique
;;; numeric NAME id's for all links that were (1) created via an
;;; html32-mode command or (2) present in the file when it was loaded.
;;; Any other links (e.g. links added via Emacs cut and paste) may
;;; have ID's that conflict with ID's html32-mode generates.  You must
;;; watch for this and fix it when appropriate; otherwise, your
;;; hypertext document will not work correctly under some browsers.
;;;
;;; html32-reorder-numeric-names can be used to reset all of the NAME
;;; id's in a document to an ordered sequence; this will also give
;;; html32-mode a chance to look over the document and figure out what
;;; new links should be named to be unique.  However, note that doing
;;; so may confuse references to named anchors from other HTML
;;; documents.  Beeeeeeee careful.






;;; -------------------------------- COMMANDS --------------------------------
;;; 
;;; Here are key sequences and corresponding commands:
;;; Most time the following rule matches: "C-c character" are logical
;;; elements like CITE, ADDRESS, CITE, etc. "C-c C-character" are
;;; formating elements like B, I, U, STRIKE, TT, etc.
;;; 
;;; All logical and font formating elements (except lists)
;;; parenthesize (sp?) the region with begin and end tag if a region
;;; was marked when the function was called.
;;; 
;;; COMMANDS FOR THE BASIC HTML SKELETON:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; 
;;; C-c M-a       html32-add-basics
;;;   Generates or completes the basic skeleton of an HTML 3.2
;;;   document. Asks for the HTML version and for a document title.
;;;   <!DOCTYPE ...>
;;;   <HTML>
;;;   <HEAD>
;;;   <TITLE>...</TITLE>
;;;      .
;;;      :
;;;   </HEAD>
;;;   <BODY>
;;;      .
;;;      :
;;;   </BODY>
;;;   </HTML>
;;;   
;;; C-c M-b       html32-add-body
;;;   Add or complete document body element.
;;;   <BODY>...</BODY>
;;;   
;;; C-c M-d       html32-add-doctype
;;;   Add or modify the doctype string. You will be prompted for the
;;;   HTML version.  If a doctype string already exists at the
;;;   beginning of the document, the existing contents will be
;;;   replaced. If you give the empty string as parameter the actual
;;;   version (at the moment "3.2 Final") will be inserted. The
;;;   underlined part will be prompted. 
;;;   <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">
;;;                                           ~~~~~~~~~
;;;
;;; C-c M-h       html32-add-head
;;;   Add or complete document head element.
;;;   <HEAD>...</HEAD>
;;;   
;;; C-c M-l       html32-add-html
;;;   Add or complete document html element.
;;;   <HTML>...</HTML>
;;;   
;;; C-c M-t       html32-add-title
;;;   Add or modify the title of the document.  You will be prompted
;;;   for the contents of the title.  If a title already exists in the
;;;   head of the document, the existing contents will be replaced.
;;;   <TITLE>...</TITLE>
;;;
;;; NORMAL COMMANDS:
;;; ~~~~~~~~~~~~~~~~
;;; 
;;; C-c a         html32-add-address
;;;   Open an address element.
;;;   <ADDRESS>...</ADDRESS>
;;;
;;; C-c b         html32-add-blockquote
;;;   Open a block-quoted area.
;;;   <BLOCKQUOTE>...</BLOCKQUOTE>
;;;
;;; C-c C-b       html32-add-bold
;;;   Open a bold tag.
;;;   <B>...</B>
;;;
;;; C-c c       html32-add-citation
;;;   Open a citation element.
;;;   <CITE>...</CITE>
;;;
;;; C-c C-c         html32-add-code
;;;   Open a 'code' (fixed-font) element.
;;;   <CODE>...</CODE>
;;;
;;; C-c d         html32-add-description-list
;;;   Open a definition list.  The initial entry is created for you.
;;;   To create subsequent entries, use 'C-c e'.
;;;   <DL>
;;;      <DT>...
;;;         <DD>...
;;;      .
;;;      :
;;;   </DL>
;;;
;;; C-c e         html32-add-description-entry
;;;   Add a new definition entry in a definition list.  You are
;;;   assumed to be inside a definition list (specifically, at the end
;;;   of another definition entry).
;;;
;;; C-c C-e       html32-add-emphasized
;;;   Open an emphasized element.
;;;   <EM>...</EM>
;;;   
;;; C-c C-f       html32-add-fixed
;;;   Add a fixed font tag.
;;;   <TT>...</TT>
;;;   
;;; C-c g         html32-add-img
;;;   Add an IMG element (inlined image or graphic). You will be
;;;   prompted for the URL of the image you wish to inline into the
;;;   document and an alternative text for it.
;;;   <IMG SRC="..." ALT="...">
;;;
;;; C-c h         html32-add-header
;;;   Add a header.  You are prompted for size (1 is biggest, 6 is 
;;;   smallest) and header contents. The region command does not
;;;   prompt for the header contents, it only prompts for size.
;;;   <H1>...</H1> up to <H6>...</H6>
;;;   
;;; C-c i         html32-add-list-or-menu-item
;;;   Add a new list or menu item in a list or menu.  You are assumed
;;;   to be inside a list or menu (specifically, at the end of another
;;;   item).
;;;   <LI>
;;;
;;; C-c C-i       html32-add-italic
;;;   Add an italic tag.
;;;   <I>...</I>
;;;
;;; C-c k         html32-add-keyboard
;;;   Open an keyboard-typing element.
;;;   <KBD>...</KBD>
;;; 
;;; C-c l         html32-add-link
;;;   Add a link.  You will be prompted for the link (any string;
;;;   e.g. an absolute URL as "http://foo.bar/argh/blagh" or a
;;;   relative path).  The cursor will be left where you can type the
;;;   text that will represent the link in the document.
;;;   <A HREF="...">...</A>
;;;   
;;; C-c C-l       html32-add-listing
;;;   Open a listning area.
;;;   <LISTING>...</LISTING>
;;;   
;;; C-c m         html32-add-menu
;;;   Open a menu.  The initial item is created for you.  To create
;;;   additional items, use 'C-c i'.
;;;   <MENU>
;;;      <LI>...
;;;      .
;;;      :
;;;   </MENU>
;;;
;;; C-c C-m       html32-add-sample
;;;   Open an example element.
;;;   <SAMP>...</SAMP>
;;;   
;;; C-c n         html32-add-numbered-list
;;;   Open a ordered list. The initial item is created for you.  You
;;;   will be prompted for a type. This could be a "1" for decimal
;;;   numbering, an "a" for small letters, an "A" for captial letters,
;;;   an "i" for roman numbering with small letters or an "I" for
;;;   roman numbering with capital letters. To create additional
;;;   items, use 'C-c i'. 
;;;   <OL TYPE="...">
;;;      <LI>...
;;;      .
;;;      :
;;;   </OL>
;;;
;;; C-c p         html32-add-paragraph
;;;   Open a paragraph.
;;;   <P>...</P>
;;; 
;;; C-c C-p       html32-add-preformatted
;;;   Open a pre-formatted area.
;;;   <PRE>...</PRE>
;;;
;;; C-c q         html32-add-quotify-hrefs
;;;   Add quotation marks before and after all URLs in HREF-, USEMAP-
;;;   and SRC- parameters - if they haven't some yet.
;;; 
;;; C-c C-q       html32-add-quotify-all
;;;   Add quotation marks before and after all parameter-values.
;;; 
;;; C-c r         html32-add-reference
;;;   Add a reference mark element.
;;;   <A NAME="...">...</A>
;;;
;;; C-c s         html32-add-list
;;;   Open a unordered list.  The initial item is created for you.  To
;;;   create additional items, use 'C-c i'.
;;;   <UL>
;;;      <LI>...
;;;      .
;;;      :
;;;   </UL>
;;;
;;; C-c C-s       html32-add-strong
;;;   Open a strong emphasized element.
;;;   <STRONG>...</STRONG>
;;;
;;; C-c t         html32-add-table
;;;   Add a HTML 3.2 table.
;;;
;;;   You will be prompted for 
;;;   - the size of the table border,
;;;   - a caption and
;;;   - the caption's position. 
;;;   
;;;   If you enter the empty string or nil to one of the parameters,
;;;   it's default value is used:
;;;   
;;;   - border size default: zero. 
;;;   - caption default: No caption will be specified. (But if
;;;     html32-add-table is called interactive you will be prompted
;;;     for the position, though. Just press enter then.)
;;;   - caption position: Browser will use their default
;;;     position. (means: No position will be specified.)
;;;     
;;;   <TABLE BORDER="...">
;;;   <CAPTION ALIGN="...">
;;;   	...
;;;   </CAPTION>
;;;   <TR>
;;;     <TD>
;;;       .
;;;       :
;;;   	</TD>
;;;     .
;;;     :
;;;   </TR>
;;;   .
;;;   :
;;;   </TABLE>
;;;   
;;; C-c v         html32-add-variable
;;;   Open a variable element.
;;;   <VAR>...</VAR>
;;;
;;; C-c x         html32-add-plaintext
;;;   Add plaintext.  The cursor will be positioned where you can type
;;;   plaintext (or insert another file, or whatever).
;;;   <PLAINTEXT>...</PLAINTEXT>
;;;
;;; C-c C-x       html32-add-xmp
;;;   Add an HTML example.  The cursor will be positioned where you
;;;   can type HTML source code (or insert another HTML file, or
;;;   whatever). 
;;;   <XMP>...</XMP>
;;;
;;; C-c z         html32-preview-document
;;;   Direct lynx output of the actual HTML file to a buffer. 
;;;
;;; C-c _         html32-add-underline
;;;   Add an underline tag.
;;;   <U>...</U>
;;;
;;; C-c =         html32-add-strike
;;;   Add a strike tag.
;;;   <STRIKE>...</STRIKE>
;;;
;;; C-c C-=       html32-add-horizontal-rule
;;;   Add a horizontal rule. You will be prompted for it's width
;;;   and alignment. Defaults are WIDTH="100%" and ALIGN="center".
;;;   
;;;   If you enter the empty string or nil to one of the parameters,
;;;   it's default value is used:
;;;   
;;;   - width default: No width will be specified. 
;;;   - alignment: Browser will use the actual alignment (means: No
;;;     alignment will be specified.)
;;;   <HR WIDTH="..." ALIGN="...">
;;;
;;; C-c -         html32-add-small
;;;   Add a small element. If you give an integer as parameter, you
;;;   can decide, how small the font should be.
;;;   <SMALL>...</SMALL>
;;;   <FONT SIZE="-...">...</FONT>
;;;
;;; C-c C--       html32-add-sub
;;;   Add a subscript tag.
;;;   <SUB>...</SUB>
;;;
;;; C-c +         html32-add-big
;;;   Add a big element. If you give an integer as parameter, you
;;;   can decide, how big the font should be.
;;;   <SMALL>...</SMALL>
;;;   <FONT SIZE="+...">...</FONT>
;;;
;;; C-c C-+       html32-add-sup
;;;   Add a superscript tag.
;;;   <SUP>...</SUP>
;;;
;;; C-c #         html32-add-font-size
;;;   Add an absolute font size tag. You will be prompted for a font
;;;   size (1 = smallest).
;;;   <FONT SIZE="...">...</FONT>
;;;   
;;; C-c :         html32-add-definition
;;;   Open a definition element.
;;;   <DFN>...</DFN>
;;;   
;;; C-c .         html32-add-dir
;;;   Open a directory list.  The initial item is created for you.  To
;;;   create additional items, use 'C-c i'.
;;;   <DIR>
;;;      <LI>...
;;;      .
;;;      :
;;;   </DIR>
;;;
;;; C-c !         html32-comment-region
;;;   Comment or uncomment region or insert comment tags.
;;;   
;;;   If no region is marked, the actual comment point is in will be
;;;   uncommented or - if point is not inside a comment - comment
;;;   parenthesis will be inserted.
;;;
;;;   If a region is marked, it will be commented or - if region is
;;;   completely inside a region, this comment and all comment inside
;;;   the region will be uncomment. 
;;;   <!-- ... -->
;;;   
;;; ---------------------------- NOTES ON HTML 3.2 ----------------------------
;;; 
;;; Modifications for HTML 2.x to 3.x done by Axel Beckert, Eva Stopp
;;; and Markus Bolz. The code was mostly derived from htmlmode.el
;;; version by NCSA. Because of html32-mode differs quite a lot from
;;; the original htmlmode, the mode name is changed.
;;; 
;;; All characters, which have special meaning in HTML, like "<", ">"
;;; and "\"" can be typed normaly. If you want the corresponding HTML
;;; entity, you need to type the prefix C-c before (e.g. C-c < for the
;;; HTML entity for less than). So you can also use tags which are not
;;; implemented, e.g. non-standard tags like those for frames or tags
;;; from the new standard 4.0.
;;;
;;; Accents ("'", "`", "^") can be typed as usually as a prefix
;;; command, but instead of a character with accent they insert the
;;; corresponding HTML entities or in some cases the entities for some
;;; other special character, like fractions or currency signs.
;;;
;;; The HTML special character ampersand ("&") has also a prefix
;;; function which is used for the umlaut characters (characters with
;;; two dots above) and most other special characters.
;;;
;;; The HTML entities for other non-standard accents (e.g. the
;;; Scandinavian ring) or the for URL's often needed tilde ("~") can
;;; be reached by using the prefix "C-c o" for the ring and "C-c ~"
;;; for the tilde.
;;;
;;; After typing one of these prefix characters, by pressing C-h you
;;; can get a list of all HTML entities, which can be inserted by this
;;; prefix. 
;;;
;;; COMPATIBILITY and CHANGES to "htmlmode.el":
;;;
;;; <, >
;;;   In comparison to standard html-mode (htmlmode.el), they have no
;;;   special function in the mode anymore. Because of new or
;;;   non-stanard HTML tags coming up very often the need for typing
;;;   this characters normaly became bigger than the need of a real
;;;   short short cut for the HTML Entities &lt; and &gt;. So typing
;;;   ">" will insert a ">" and a "<" will insert a "<".
;;;
;;; C-c <, C-c >,  html32-real-less-than, html32-real-greater-than
;;; C-c "          html32-quotation-mark
;;;   Because of "<" and ">" having no special meaning any more, these
;;;   two short cuts insert the HTML entities &lt; resp. &gt;. See
;;;   above. 'C-c "' inserts the HTML entity &quot;.
;;;
;;; SPECIAL COMMANDS
;;; 
;;; &, ', `, ^    html32-ampersand, html32-accent-aigu,
;;;               html32-accent-grave, html32-accent-circumflex
;;;   These characters are overridden by functions to output the HTML
;;;   3.2 Entities. 
;;;   After typing one of them you will be asked for a character to
;;;   indicate which entity you want to insert. By typing C-h instead
;;;   of a character you will get a list of all entities, which and
;;;   with which character can be inserted. If you type a character,
;;;   which is not mentioned in this list, a &, ', ` repsective ^
;;;   itself and the type character will be inserted. If you type
;;;   Backspace instead of a character, nothing will be inserted
;;;   (Quit).
;;;
;;; C-c &, C-c ', html32-real-ampersand, html32-real-accent-aigu,
;;; C-c `, C-c ^  html32-real-accent-grave,
;;;               html32-real-accent-circumflex 
;;;   Inserts a sole ampersand ("&"), accent aigu ("'"), accent grave
;;;   ("`") resp. accent circumflex ("^").
;;;
;;; C-c o, C-c ~  html32-ring, html32-tilde
;;;   After typing one of them you will be asked for a character to
;;;   indicate which entity you want to insert. By typing C-h instead
;;;   of a character you will get a list of all entities, which and
;;;   with which character can be inserted. If you type a character,
;;;   which is not mentioned in this list, a &deg; respective a ~
;;;   itself and the typed character will be inserted. The ~ still can
;;;   be typed normally, it still inserts itself, because the ~ is
;;;   needed very often, while typing URLs.
;;;
;;; ---------------------------- ADDITIONAL NOTES ----------------------------
;;;
;;; If you are running Epoch or Lucid Emacs, highlighting will be used
;;; to deemphasize HTML message elements as they are created.  You can
;;; turn this off; see the variable 'html32-use-highlighting'.
;;;
;;; To reorder all of the link NAME fields in your message (in order
;;; of their occurrence in the text), use:
;;;
;;; html32-reorder-numeric-names
;;;   Reorder the NAME fields for links in the current buffer.  The
;;;   new ordering starts at 1 and increases monotonically through the
;;;   buffer.  If optional arg REORDER-NON-NUMERIC is non-nil, then
;;;   non-numeric NAME's will also be numbered, else they won't.
;;;
;;; HREF arguments in anchors and ALT texts of inline images should
;;; always be quoted.  In some existing HTML documents, they are not.
;;; html32-mode will automatically quotify all such unquoted arguments
;;; when it encounters them.  The following variables affect this
;;; behavior.
;;;
;;; html32-quotify-hrefs-on-find       (variable, default t)
;;;   If this is non-nil, all HREF arguments will be quotified
;;;   automatically when a HTML document is loaded into Emacs
;;;   (actually when html32-mode is entered).






;;; ---------------------------- emacs variations ----------------------------

(defvar html32-running-lemacs (string-match "Lucid" emacs-version)
  "Non-nil if running Lucid Emacs.")

(defvar html32-running-gnuemacs (string-match "GNU" (version))
  "Non-nil if running GNU Emacs.")

(defvar html32-running-epoch (boundp 'epoch::version)
  "Non-nil if running Epoch.")

;;; ------------------------------- variables --------------------------------

(defvar html32-using-easymenu (and html32-running-gnuemacs t)
  "*Using easymenu if non-nil.")

(defvar html32-using-tex-umlauts () ;;; still buggy!
  "*Using keyboard-macros, which replaces umlauts, written like '\"a'
by the corresponding HTML entities. if non-nil.")

(defvar html32-using-latin1-input t ;;; seems to work properly at
				    ;;; least under M$ Windows.
  "*Using keyboard-macros, which replaces directly typed special
character, e.g. 'ä', 'ð', 'æ' and 'é', by the corresponding HTML
entity if non-nil. This should be used, if working under Microsoft
Windows or most Unixes.")

(defvar html32-using-ascii-input (string-match "GNU" (version))
  "*Using keyboard-macros, which replaces directly typed special
character, e.g. '„' and '‚', by the corresponding HTML entity if
non-nil. This should be used, if working under MS-DOS and other OSes,
working with an 8-bit ASCII character set.

Note: Not all Latin-1 (ISO-8859-1) characters are available in ASCII,
so they can't be typed directly and need to be typed with one of the
special character prefixes: \"&\", \"´\", \"`\", \"^\", \"C-c o\" or
\"C-c ~\"") 

(defvar html32-closing-td nil
  "*Using closing tag for table fields and headers if non-nil.")

(defvar html32-closing-tr t
  "*Using closing tag for table rows if non-nil.")

(defvar html32-closing-li nil
  "*Using closing tag for list items if non-nil.")

(defvar html32-tab "\t" 
  "*What should be insert, if you use indented areas or lists. Could
also be any string, which consists of blanks or tabs.")

(defvar html32-alignment-at-end t
  "*If non-nil, alignment-parameters are inserted at the end of a tag,
else at next position after point.")

(defvar html32-quotify-on-find t
  "*If non-nil, all parameter values in a file will be automatically
quotified when the file is loaded.  This is necessary for some values,
but recommend for all.")

(defvar html32-quotify-alts-on-find t
  "*If non-nil, all ALT's in a file will be automatically quotified
when the file is loaded.  This is necessary for ALT texts with more
than one word and recommend for ALT texts with one word. This should
always be T.")

(defvar html32-quotify-hrefs-on-find t 
  "*If non-nil, all HREF's and SRC's in a file will be automatically
quotified when the file is loaded.  This is useful for converting
ancient HTML documents to SGML-compatible syntax, which mandates
quoted HREF's. Therefore html32-quotify-alts will be executed, too.")

(defvar html32-highlight-non-standard-url t
  "*Flag to use highlighting of URL parameters, which are not standard
in HTML 3.2 (e.g. LOWSRC and MAP).")

(defvar html32-highlight-non-standard-key-tags t
  "*Flag to use highlighting of key tags, which are not standard
in HTML 3.2 (e.g. FRAMESET and SCRIPT).")

(defvar html32-highlight-ssi t
  "*Flag to use highlighting of Server Side Includes (SSI).")

(defvar html32-highlight-input-tags t
  "*Flag to use highlighting of <INPUT ...>, <
TEXTAREA ...>...</TEXTAREA> and <SELECT ...>...</SELECT> tags.)")

(defvar html32-location-lynx "/usr/bin/lynx"
  "*Local path to lynx which is used to preview HTML documents.
Program is assumed to accept a single argument, a filename containing
a file to view.")

(defvar html32-lynx-args "-dump"
  "*Arguments to be given to lynx; NIL if none should be given.")

(defvar html32-sigusr1-signal-value 16
  "*Value for the SIGUSR1 signal on your system.  See, usually,
/usr/include/sys/signal.h.")

;;; --------------------------------- setup ----------------------------------

(defvar html32-menu
  '("HTML3.2"
   ; HTML main menu
    "Links and Inline Images"
    "----"
    ["Add hypertext link ..." html32-add-link t]
    ["Add mark ..." html32-add-reference t]
    ["Add image ..." html32-add-img t]
    ["Quotify URLs" html32-quotify-hrefs t]
    "----"
    "Text Formatting"
    "----"
    ("Logical formats"
     ; Logical formats menu
     ["Header ..." html32-add-header t]
     ["Address" html32-add-address t]
     ["Block quoted text" html32-add-blockquote t]
     ["Normal emphasized" html32-add-emphasized t]
     ["Strong emphasized" html32-add-strong t]
     ["Citation" html32-add-citation t]
     ["Code" html32-add-code t]
     ["Definition" html32-add-definition t]
     ["Keyboard input" html32-add-keyboard t]
     ["Variable" html32-add-variable t]
     ["Example" html32-add-sample t]
     )
    ("Physical formats"
     ; Physical formats menu
     ["Bold" html32-add-bold t]
     ["Italics" html32-add-italic t]
     ["Underlined" html32-add-underline t]
     ["Stroke out" html32-add-strike t]
     ["Typewriter (fixed)" html32-add-fixed t]
     ["Superscript" html32-add-sup t]
     ["Subscript" html32-add-sub t]
     ["Bigger" html32-add-big t]
     ["Smaller" html32-add-small t]
     )
    ("Preformatted Text"
     ["Standard" html32-add-preformatted t]
     ["Plain text" html32-add-plaintext t]
     ["Program Listing" html32-add-listing t]
     ["HTML Example" html32-add-xmp t]
     )
    "----"
    ("Alignment tags for text"
     ["left" html32-divalign-left t]
     ["right" html32-divalign-right t]
     ["center" html32-divalign-center t]
     )
    ("Alignment for existing tags"
     "horizontal alignment"
     "----"
     ["ALIGN=\"left\"" html32-align-left t]
     ["ALIGN=\"right\"" html32-align-right t]
     ["ALIGN=\"center\"" html32-align-center t]
     "----"
     "vertical alignment"
     "----"
     ["ALIGN=\"middle\"" html32-align-middle t]
     ["ALIGN=\"absmiddle\"" html32-align-absmiddle t]
     ["ALIGN=\"top\"" html32-align-top t]
     ["ALIGN=\"bottom\"" html32-align-bottom t]
     "----"
     "vertical alignment inside tables"
     "----"
     ["VALIGN=\"middle\"" html32-valign-middle t]
     ["VALIGN=\"top\"" html32-valign-top t]
     ["VALIGN=\"bottom\"" html32-valign-bottom t]
     )
    "----"
    "Lists and tables"
    "----"
    ("Create list"
     ["Unordered list" html32-add-list t]
     ["Ordered list ..." html32-add-numbered-list t]
     ["Menu" html32-add-menu t]
     ["Directory list" html32-add-dir t]
     "----"
     ["Definition list" html32-add-description-list t]
     )
    ["Normal list item" html32-add-list-or-menu-item t]
    ["Definition list entry" html32-add-description-entry t]
    "----"
    ["Create table ..." html32-add-table t]
    ["Table row" html32-add-table-row t]
    ["Table field" html32-add-table-field t]
    ["Table header" html32-add-table-head t]
    "----"
    "Document Basics"
    "----"
    ["DOCTYPE string ..."  html32-add-doctype t]
    ["Basic skeleton ..." html32-add-basics t]
    ["Document default font size ..." html32-add-basefont-size t]
    "----"
    "Other Commands"
    "----"
    ["(Un)comment region" html32-comment-region t]
    ["Quotify all parameters" html32-quotify-all t]
    ["New value for html32-tab" html32-set-html-tab t]
    ["Preview document" html32-preview-document t]
    ["Info about HTML 3.2 Mode" describe-mode t]
    )
  "*This is the menu for both, Easy menu and Lucid Emacs.")

(defvar html32-mode-syntax-table nil
  "Syntax table used while in html mode.")

(defvar html32-mode-abbrev-table nil
  "Abbrev table used while in html mode.")
(define-abbrev-table 'html32-mode-abbrev-table ())

(setq words-include-escapes t)

;(if html32-mode-syntax-table
;    ()
;  (setq html32-mode-syntax-table (make-syntax-table))
;  (modify-syntax-entry ?\" ".   " html32-mode-syntax-table)
;  (modify-syntax-entry ?\\ ".   " html32-mode-syntax-table)
;  (modify-syntax-entry ?' "w   " html32-mode-syntax-table)
;  (modify-syntax-entry ?&  "\\" html32-mode-syntax-table))

(if html32-mode-syntax-table
    ()
  (setq html32-mode-syntax-table (make-syntax-table))
  (let ((char 0))
    (while (< char ? )
      (modify-syntax-entry char "." html32-mode-syntax-table)
      (setq char (1+ char)))
    (modify-syntax-entry ?   "-" html32-mode-syntax-table)
    (modify-syntax-entry ?\t "-" html32-mode-syntax-table)
    (modify-syntax-entry ?\n "-" html32-mode-syntax-table)
    (modify-syntax-entry ?\f "-" html32-mode-syntax-table)
;    (modify-syntax-entry ?\" "$\"" html32-mode-syntax-table)
;    (modify-syntax-entry ?\" "." html32-mode-syntax-table)
    (modify-syntax-entry ?\" "\""   html32-mode-syntax-table)
    (modify-syntax-entry ?\\ "/" html32-mode-syntax-table)
;    (modify-syntax-entry ?& "/" html32-mode-syntax-table)
    (modify-syntax-entry ?&  "\\" html32-mode-syntax-table)
    (modify-syntax-entry ?_ "w" html32-mode-syntax-table)
    (modify-syntax-entry ?@ "_" html32-mode-syntax-table)
    (modify-syntax-entry ?~ "_" html32-mode-syntax-table)
    (modify-syntax-entry ?' "_" html32-mode-syntax-table)
  
;    (modify-syntax-entry ?\  " "    html32-mode-syntax-table)
;    (modify-syntax-entry ?<  "(>"   html32-mode-syntax-table)
;    (modify-syntax-entry ?>  ")<"   html32-mode-syntax-table)
;    (modify-syntax-entry ?\\ "/"    html32-mode-syntax-table)
    )
)

(defvar html32-mode-alignment-map nil 
  "Keymap for the C-c C-a prefix of the HTML 3.2 mode")
(cond ((not html32-mode-alignment-map)
       (setq html32-mode-alignment-map (make-sparse-keymap))
       (define-key html32-mode-alignment-map "a"    'html32-align-absmiddle)
       (define-key html32-mode-alignment-map "b"    'html32-align-bottom)
       (define-key html32-mode-alignment-map "\C-b" 'html32-valign-bottom)
       (define-key html32-mode-alignment-map "c"    'html32-align-center)
       (define-key html32-mode-alignment-map "\C-c" 'html32-divalign-center)
       (define-key html32-mode-alignment-map "l"    'html32-align-left)
       (define-key html32-mode-alignment-map "\C-l" 'html32-divalign-left)
       (define-key html32-mode-alignment-map "m"    'html32-align-middle)
       (define-key html32-mode-alignment-map "\C-m" 'html32-valign-middle)
       (define-key html32-mode-alignment-map "r"    'html32-align-right)
       (define-key html32-mode-alignment-map "\C-r" 'html32-divalign-right)
       (define-key html32-mode-alignment-map "t"    'html32-align-top)
       (define-key html32-mode-alignment-map "\C-t" 'html32-valign-top)
       ))

(defvar html32-mode-table-map nil 
  "Keymap for the C-c t prefix of the HTML 3.2 mode")
(cond ((not html32-mode-table-map)
       (setq html32-mode-table-map (make-sparse-keymap))
       (define-key html32-mode-table-map "t"    'html32-add-table)
       (define-key html32-mode-table-map "d"    'html32-add-table-field)
       (define-key html32-mode-table-map "r"    'html32-add-table-row)
       (define-key html32-mode-table-map "h"    'html32-add-table-head)
       ))

(defvar html32-mode-map nil 
  "Keymap for the HTML 3.2 mode")
(cond ((not html32-mode-map)
       (setq html32-mode-map (make-sparse-keymap))
       (define-key html32-mode-map "\t"       'tab-to-tab-stop)
       (define-key html32-mode-map "\M-j"     'html32-add-line-break)
; Prefixes
       (define-key html32-mode-map "\C-c\C-a" html32-mode-alignment-map)
       (define-key html32-mode-map "\C-ct"    html32-mode-table-map)
; Basic skeleton commands
       (define-key html32-mode-map "\C-c\M-a" 'html32-add-basics)
       (define-key html32-mode-map "\C-c\M-h" 'html32-add-head)
       (define-key html32-mode-map "\C-c\M-b" 'html32-add-body)
       (define-key html32-mode-map "\C-c\M-f" 'html32-add-basefont-size)
       (define-key html32-mode-map "\C-c\M-l" 'html32-add-html)
       (define-key html32-mode-map "\C-c\M-t" 'html32-add-title)
       (define-key html32-mode-map "\C-c\M-d" 'html32-add-doctype)
; Other commands
       (define-key html32-mode-map "\C-ca"    'html32-add-address)
                                 ; "\C-c\C-a" Prefix
       (define-key html32-mode-map "\C-cb"    'html32-add-blockquote)
       (define-key html32-mode-map "\C-c\C-b" 'html32-add-bold)
       (define-key html32-mode-map "\C-cc"    'html32-add-citation)
       (define-key html32-mode-map "\C-c\C-c" 'html32-add-code)
       (define-key html32-mode-map "\C-cd"    'html32-add-description-list)
       (define-key html32-mode-map "\C-c\C-d" 'html32-add-doctype)
       (define-key html32-mode-map "\C-ce"    'html32-add-description-entry)
       (define-key html32-mode-map "\C-c\C-e" 'html32-add-emphasized)
       (define-key html32-mode-map "\C-c\C-f" 'html32-add-fixed)
       (define-key html32-mode-map "\C-cg"    'html32-add-img)
                                 ; "\C-c\C-g" Quit
       (define-key html32-mode-map "\C-ch"    'html32-add-header)
                                 ; "\C-c\C-h" Help
       (define-key html32-mode-map "\C-ci"    'html32-add-list-or-menu-item)
       (define-key html32-mode-map "\C-c\C-i" 'html32-add-italic)
       (define-key html32-mode-map "\C-ck"    'html32-add-keyboard)
       (define-key html32-mode-map "\C-cl"    'html32-add-link)
       (define-key html32-mode-map "\C-c\C-l" 'html32-add-listing)
       (define-key html32-mode-map "\C-cm"    'html32-add-menu)
       (define-key html32-mode-map "\C-c\C-m" 'html32-add-sample)
       (define-key html32-mode-map "\C-cn"    'html32-add-numbered-list)
       (define-key html32-mode-map "\C-co"    'html32-ring)
       (define-key html32-mode-map "\C-cp"    'html32-add-paragraph)
       (define-key html32-mode-map "\C-c\C-p" 'html32-add-preformatted)
       (define-key html32-mode-map "\C-cq"    'html32-quotify-hrefs)
       (define-key html32-mode-map "\C-c\C-q" 'html32-quotify-all)
       (define-key html32-mode-map "\C-cr"    'html32-add-reference)
       (define-key html32-mode-map "\C-cs"    'html32-add-list)
       (define-key html32-mode-map "\C-c\C-s" 'html32-add-strong)
                                 ; "\C-ct"    Prefix
       (define-key html32-mode-map "\C-c\C-t" 'html32-set-html-tab)
       (define-key html32-mode-map "\C-cv"    'html32-add-variable)
       (define-key html32-mode-map "\C-cx"    'html32-add-plaintext)
       (define-key html32-mode-map "\C-c\C-x" 'html32-add-xmp)
       (define-key html32-mode-map "\C-c_"    'html32-add-underline)
       (define-key html32-mode-map "\C-c="    'html32-add-strike)
       (define-key html32-mode-map [?\C-c ?\C-=] 'html32-add-horizontal-rule)
       (define-key html32-mode-map "\C-cz"       'html32-preview-document)
       (define-key html32-mode-map "\C-c-"       'html32-add-small)
       (define-key html32-mode-map [?\C-c ?\C--] 'html32-add-sub)
       (define-key html32-mode-map "\C-c+"       'html32-add-big)
       (define-key html32-mode-map [?\C-c ?\C-+] 'html32-add-sup)
       (define-key html32-mode-map "\C-c#"       'html32-add-font-size)
       (define-key html32-mode-map "\C-c!"       'html32-comment-region)
       (define-key html32-mode-map "\C-c:"       'html32-add-definition)
       (define-key html32-mode-map "\C-c."       'html32-add-dir)
; Special character commands
       (define-key html32-mode-map "&"        'html32-ampersand)
       (define-key html32-mode-map "'"        'html32-accent-aigu) 
       (define-key html32-mode-map "`"        'html32-accent-grave) 
       (define-key html32-mode-map "^"        'html32-accent-circumflex) 
       (define-key html32-mode-map "\C-c~"    'html32-tilde) 
       (define-key html32-mode-map "\C-c\""   'html32-quotation-mark)
       (define-key html32-mode-map "\C-c&"    'html32-real-ampersand)
       (define-key html32-mode-map "\C-c'"    'html32-real-accent-aigu)
       (define-key html32-mode-map "\C-c`"    'html32-real-accent-grave)
       (define-key html32-mode-map "\C-c^"    'html32-real-accent-cirumflex)
; TeX umlauts
       (cond (html32-using-tex-umlauts
	      (define-key html32-mode-map "\"" 'html32-tex-kbd-macro)))
; iso-8859-1 / ansi special characters
       (cond (html32-using-latin1-input
; umlauts
	      (define-key html32-mode-map [?ä] 'html32-add-auml) ; C-q M-d
	      (define-key html32-mode-map [?ë] 'html32-add-euml) ; C-q M-k
	      (define-key html32-mode-map [?ï] 'html32-add-uuml) ; C-q M-o
	      (define-key html32-mode-map [?ö] 'html32-add-ouml) ; C-q M-v
	      (define-key html32-mode-map [?ü] 'html32-add-uuml) ; C-q M-|
	      (define-key html32-mode-map [?ÿ] 'html32-add-yuml)
	      (define-key html32-mode-map [?Ë] 'html32-add-cap-euml)
	      (define-key html32-mode-map [?Ï] 'html32-add-cap-iuml)
	      (define-key html32-mode-map [?Ä] 'html32-add-cap-auml)
	      (define-key html32-mode-map [?Ö] 'html32-add-cap-ouml)
	      (define-key html32-mode-map [?Ü] 'html32-add-cap-uuml)
; accent aigu
	      (define-key html32-mode-map [?á] 'html32-add-aacute) ; C-q M-a
	      (define-key html32-mode-map [?é] 'html32-add-eacute) ; C-q M-i
	      (define-key html32-mode-map [?í] 'html32-add-iacute) ; C-q M-m
	      (define-key html32-mode-map [?ó] 'html32-add-oacute) ; C-q M-s
	      (define-key html32-mode-map [?ú] 'html32-add-uacute) ; C-q M-z
	      (define-key html32-mode-map [?ý] 'html32-add-yacute)
	      (define-key html32-mode-map [?Á] 'html32-add-cap-aacute) 
	      (define-key html32-mode-map [?É] 'html32-add-cap-eacute) 
	      (define-key html32-mode-map [?Í] 'html32-add-cap-iacute) 
	      (define-key html32-mode-map [?Ó] 'html32-add-cap-oacute) 
	      (define-key html32-mode-map [?Ú] 'html32-add-cap-uacute) 
; accent grave
	      (define-key html32-mode-map [?à] 'html32-add-agrave) 
	      (define-key html32-mode-map [?è] 'html32-add-egrave) ; C-q M-h
	      (define-key html32-mode-map [?ì] 'html32-add-igrave) ; C-q M-l
	      (define-key html32-mode-map [?ò] 'html32-add-ograve) ; C-q M-r
	      (define-key html32-mode-map [?ù] 'html32-add-ugrave) ; C-q M-y
	      (define-key html32-mode-map [?À] 'html32-add-cap-agrave) 
	      (define-key html32-mode-map [?È] 'html32-add-cap-egrave)
	      (define-key html32-mode-map [?Ì] 'html32-add-cap-igrave)
	      (define-key html32-mode-map [?Ò] 'html32-add-cap-ograve)
	      (define-key html32-mode-map [?Ù] 'html32-add-cap-ugrave)
; accent circonflexe
	      (define-key html32-mode-map [?â] 'html32-add-acirc) ; C-q M-b
	      (define-key html32-mode-map [?ê] 'html32-add-ecirc) ; C-q M-j
	      (define-key html32-mode-map [?î] 'html32-add-icirc) ; C-q M-n
	      (define-key html32-mode-map [?ô] 'html32-add-ocirc) ; C-q M-t
	      (define-key html32-mode-map [?û] 'html32-add-ucirc) 
	      (define-key html32-mode-map [?Â] 'html32-add-cap-acirc) 
	      (define-key html32-mode-map [?Ê] 'html32-add-cap-ecirc) 
	      (define-key html32-mode-map [?Î] 'html32-add-cap-icirc) 
	      (define-key html32-mode-map [?Ô] 'html32-add-cap-ocirc) 
	      (define-key html32-mode-map [?Û] 'html32-add-cap-ucirc)
; tilde, ring, cedille, slash
	      (define-key html32-mode-map [?ã] 'html32-add-atilde) ; C-q M-c
	      (define-key html32-mode-map [?õ] 'html32-add-otilde) ; C-q M-u
	      (define-key html32-mode-map [?ñ] 'html32-add-ntilde) ; C-q M-q
	      (define-key html32-mode-map [?å] 'html32-add-aring) ; C-q M-e
	      (define-key html32-mode-map [?ç] 'html32-add-ccedil) ; C-q M-g
	      (define-key html32-mode-map [?ø] 'html32-add-oslash) ; C-q M-x
	      (define-key html32-mode-map [?Â] 'html32-add-cap-atilde) 
	      (define-key html32-mode-map [?Ô] 'html32-add-cap-otilde) 
	      (define-key html32-mode-map [?Ñ] 'html32-add-cap-ntilde)
	      (define-key html32-mode-map [?Å] 'html32-add-cap-Aring) 
	      (define-key html32-mode-map [?Ç] 'html32-add-cap-ccedil)
	      (define-key html32-mode-map [?Ø] 'html32-add-cap-oslash)
; national special characters
	      (define-key html32-mode-map [?ß] 'html32-add-szlig) ; C-q M-_
	      (define-key html32-mode-map [?ð] 'html32-add-eth) ; C-q M-p
	      (define-key html32-mode-map [?Ð] 'html32-add-cap-eth)
	      (define-key html32-mode-map [?þ] 'html32-add-thorn)
	      (define-key html32-mode-map [?Þ] 'html32-add-cap-thorn)
	      (define-key html32-mode-map [?æ] 'html32-add-aelig) ; C-q M-f
	      (define-key html32-mode-map [?Æ] 'html32-add-cap-aelig)
; sign characters
	      (define-key html32-mode-map [?¡] 'html32-add-iexcl)
	      (define-key html32-mode-map [?¢] 'html32-add-cent)
	      (define-key html32-mode-map [?£] 'html32-add-pound)
	      (define-key html32-mode-map [?¤] 'html32-add-curren)
	      (define-key html32-mode-map [?¥] 'html32-add-yen)
	      (define-key html32-mode-map [?¦] 'html32-add-brvbar)
;	      (define-key html32-mode-map [?|] 'html32-add-brvbar)
	      (define-key html32-mode-map [?§] 'html32-add-sect)
	      (define-key html32-mode-map [?¨] 'html32-add-uml)
	      (define-key html32-mode-map [?©] 'html32-add-copy)
	      (define-key html32-mode-map [?ª] 'html32-add-ordf)
	      (define-key html32-mode-map [?«] 'html32-add-laquo)
	      (define-key html32-mode-map [?¬] 'html32-add-not)
	      (define-key html32-mode-map [?­] 'html32-add-shy) 
	        ;;; AFAIR the character behind the question mark is
		;;; no normal minus.
	      (define-key html32-mode-map [?®] 'html32-add-reg)
	      (define-key html32-mode-map [?¯] 'html32-add-macr)
	      (define-key html32-mode-map [?°] 'html32-add-deg)
	      (define-key html32-mode-map [?±] 'html32-add-plusmn)
	      (define-key html32-mode-map [?²] 'html32-add-sup2)
	      (define-key html32-mode-map [?³] 'html32-add-sup3)
	      (define-key html32-mode-map [?µ] 'html32-add-micro)
	      (define-key html32-mode-map [?¶] 'html32-add-para)
	      (define-key html32-mode-map [?·] 'html32-add-middot)
	      (define-key html32-mode-map [?¸] 'html32-add-cedil)
	      (define-key html32-mode-map [?¹] 'html32-add-sup1)
	      (define-key html32-mode-map [?º] 'html32-add-ordm)
	      (define-key html32-mode-map [?»] 'html32-add-raquo)
	      (define-key html32-mode-map [?¼] 'html32-add-frac14)
	      (define-key html32-mode-map [?½] 'html32-add-frac12)
	      (define-key html32-mode-map [?¾] 'html32-add-frac34)
	      (define-key html32-mode-map [?¿] 'html32-add-iquest)
	      (define-key html32-mode-map [?×] 'html32-add-times)
	      (define-key html32-mode-map [?÷] 'html32-add-divide))
; ASCII special characters
	     (html32-using-ascii-input
; umlauts
	      (define-key html32-mode-map [?„] 'html32-add-auml)
	      (define-key html32-mode-map [?‰] 'html32-add-euml)
	      (define-key html32-mode-map [?‹] 'html32-add-uuml)
	      (define-key html32-mode-map [?”] 'html32-add-ouml)
	      (define-key html32-mode-map [?] 'html32-add-uuml)
	      (define-key html32-mode-map [?˜] 'html32-add-yuml)
	      (define-key html32-mode-map [?Ž] 'html32-add-cap-auml)
	      (define-key html32-mode-map [?™] 'html32-add-cap-ouml)
	      (define-key html32-mode-map [?š] 'html32-add-cap-uuml)
; accent aigu
	      (define-key html32-mode-map [? ] 'html32-add-aacute)
	        ;;; Be careful with the line above: The character
		;;; behind the question mark is *NO* blank, it is the
		;;; character #240(oct) (#A0(hex)/#160(dec)), which
		;;; is the "a accent aigu" / "'a" in the ASCII
		;;; character set and looks like a blank in the ANSI
		;;; character set!
	      (define-key html32-mode-map [?‚] 'html32-add-eacute)
	      (define-key html32-mode-map [?¡] 'html32-add-iacute)
	      (define-key html32-mode-map [?¢] 'html32-add-oacute)
	      (define-key html32-mode-map [?£] 'html32-add-uacute)
	      (define-key html32-mode-map [?] 'html32-add-cap-eacute) 
; accent grave
	      (define-key html32-mode-map [?…] 'html32-add-agrave) 
	      (define-key html32-mode-map [?Š] 'html32-add-egrave)
	      (define-key html32-mode-map [?] 'html32-add-igrave)
	      (define-key html32-mode-map [?•] 'html32-add-ograve)
	      (define-key html32-mode-map [?—] 'html32-add-ugrave)
; accent circonflexe
	      (define-key html32-mode-map [?ƒ] 'html32-add-acirc)
	      (define-key html32-mode-map [?ˆ] 'html32-add-ecirc)
	      (define-key html32-mode-map [?Œ] 'html32-add-icirc)
	      (define-key html32-mode-map [?“] 'html32-add-ocirc)
	      (define-key html32-mode-map [?–] 'html32-add-ucirc) 
; tilde, ring, cedille, slash
	      (define-key html32-mode-map [?¤] 'html32-add-ntilde) 
	      (define-key html32-mode-map [?†] 'html32-add-aring) 
	      (define-key html32-mode-map [?‡] 'html32-add-ccedil) 
	      (define-key html32-mode-map [?¥] 'html32-add-cap-ntilde)
	      (define-key html32-mode-map [?] 'html32-add-cap-Aring) 
	      (define-key html32-mode-map [?€] 'html32-add-cap-ccedil)
; national special characters
	      (define-key html32-mode-map [?á] 'html32-add-szlig) 
	      (define-key html32-mode-map [?‘] 'html32-add-aelig)
	      (define-key html32-mode-map [?’] 'html32-add-cap-aelig) 
; sign characters
	      (define-key html32-mode-map [?­] 'html32-add-iexcl)
	        ;;; AFAIR the character behind the question mark is
		;;; no normal minus.
	      (define-key html32-mode-map [?›] 'html32-add-cent)
	      (define-key html32-mode-map [?œ] 'html32-add-pound)
	      (define-key html32-mode-map [?] 'html32-add-curren)
	      (define-key html32-mode-map [?] 'html32-add-yen)
	      (define-key html32-mode-map [?Ý] 'html32-add-brvbar)
	      (define-key html32-mode-map [?] 'html32-add-sect)
	      (define-key html32-mode-map [?¦] 'html32-add-ordf)
	      (define-key html32-mode-map [?®] 'html32-add-laquo)
	      (define-key html32-mode-map [?ª] 'html32-add-not)
	      (define-key html32-mode-map [?ø] 'html32-add-deg)
	      (define-key html32-mode-map [?ñ] 'html32-add-plusmn)
	      (define-key html32-mode-map [?ý] 'html32-add-sup2)
	      (define-key html32-mode-map [?ü] 'html32-add-sup3)
	      (define-key html32-mode-map [?æ] 'html32-add-micro)
	      (define-key html32-mode-map [?] 'html32-add-para)
	      (define-key html32-mode-map [?ú] 'html32-add-middot)
	      (define-key html32-mode-map [?§] 'html32-add-ordm)
	      (define-key html32-mode-map [?¯] 'html32-add-raquo)
	        ;;; AFAIR the character behind the question mark is
		;;; no normal blank.
	      (define-key html32-mode-map [?¬] 'html32-add-frac14)
	      (define-key html32-mode-map [?«] 'html32-add-frac12)
	      (define-key html32-mode-map [?¨] 'html32-add-iquest)
	      (define-key html32-mode-map [?ö] 'html32-add-divide)
	      )
	     )
       )
      )

;;; ----------------------- Easymenu (GNU Emacs) menu -------------------------

(cond (html32-using-easymenu 
       (require 'easymenu)
       (easy-menu-define html32-mode-menu html32-mode-map 
			 "HTML 3.2 Menu." html32-menu)))

;;; ---------------------------- highlighting -------------------------------
;;; --------------------------- Font Lock Mode  -----------------------------

(defconst html32-font-lock-keywords
  (list
;;; First display all tags in the light color of variables.
   '("<\\([ \t\n][a-zA-Z0-9]+=\"[^\"]*\"\\|[^>]\\)+>" 0 font-lock-variable-name-face t)

;;; Then display all parameter strings as strings..
   '("=\\(\"[^\"]*\"\\)" 1 font-lock-string-face t)

;;; Highlight all anchors.
   '("<[^>]*[ \t\n]\\(NAME=\"[^\"]*\"\\)"
     1 font-lock-function-name-face t)

;;; Highlight all URL parameters.
   (list (concat "<[^>]*[ \t\n]\\(\\("
		 "ACTION\\|"  ; Forms
		 "CODE\\|"    ; Applets
		 "HREF\\|"    ; Links
		 "SRC"        ; Images
		 "\\)=\"[^\"]*\"\\|"
		 "NOHREF\\)") ; Image maps
	 1 'font-lock-function-name-face t)

;;; This URL parameters need to be matched in a second grepping,
;;; because they may appear together in one tag with one of the
;;; above parameters.
   (list (concat "<[^>]*[ \t\n]\\(\\("
		 "USEMAP\\|"   ; Images
		 "CODEBASE\\|" ; Applets
		 "DATA"        ; Objects
		 "\\)=\"[^\"]*\"\\|"
		 "ISMAP\\)")   ; Images with CGI links
	 1 'font-lock-function-name-face t)

;;; Non-standard parameters (Netscape or Mircosoft extensions),
;;; need to be matched in a further grepping, because they may appear
;;; together in one tag with one or two parameters both groups above.
   (cond (html32-highlight-non-standard-url
	  (list (concat "<[^>]*[ \t\n]\\(\\("
			"DYN\\|"    ; Microsoft movies
			"LOW\\)SRC" ; Netscape low-res image previews
			"=\"[^\"]*\"\\)")
	    1 'font-lock-function-name-face t)))

;;; Highlight all important document structuring tags (including HTML
;;; 4.0 OBJECT tag) as keywords. (First: Elements with opening and
;;; closing tag.
   (list (concat "<\\(/?\\("
		 "H\\(TML\\|EAD\\|[1-6]\\)\\|" 
		 "T\\(ABLE\\|ITLE\\)\\|"
		 "BODY\\|"
		 "APPLET\\|"
		 "FORM\\|"
		 "OBJECT\\|"
		 "LISTNING\\|"
		 "P\\(RE\\|LAINTEXT\\)\\|"
		 "\\(XM\\|MA\\)P"
                 ;;; Input and textarea tags for using in forms.
		 (cond (html32-highlight-input-tags
			"\\|TEXTAREA\\|SELECT"))
		 ;;; Non-standard tags (Netscape or Mircosoft extensions)
		 (cond (html32-highlight-non-standard-key-tags
			(concat "\\|"
				"S\\(CRIPT\\|TYLE\\)\\|"  ; Reserved
							  ; tags since
							  ; HTML 3.2
				"FRAMESET\\|NOFRAME\\|"   ; Frames (M+N)
				"MULTICOL\\|"             ; Multi-columns
				                          ; (M)
				"I\\(FRAME\\|LAYER\\)"))) ; (M xor N?)
		 "\\)\\)")
		 1 'font-lock-keyword-face t)

;;; Highlight all important document structuring tags (including HTML
;;; 4.0 OBJECT tag) as keywords. (Second: Elements with only opening
;;; tag.
   (list (concat "<\\(I\\(MG\\|SINDEX\\)\\|"
		 "BASE\\(FONT\\)?\\|"
		 "!DOCTYPE[^>]*"
                 ;;; Input and textarea tags for using in forms.
		 (cond (html32-highlight-input-tags
			"\\|INPUT"))
 		 ;;; Non-standard tags (Sun, Netscape or Mircosoft extensions)
		 (cond (html32-highlight-non-standard-key-tags
			(concat "APP\\|" ; Applet tag (S)
				"\\(BGSOUN\\|EMBE\\)D\\|"
					 ; Background sound (M or N?) and
					 ; Multimedia object (N)
				"FRAME\\|" ; Frames (M+N)
				"\\(LAY\\|SPAC\\)ER")))
				         ; Layer (M or N?)
				         ; Spacer (N)
		 "\\)")
	 1 'font-lock-keyword-face t)

;;; Displaying all comments as comments (Quite nice regexp, huh? ;-)
   '("<!--\\([^->]\\|[^-]\\(>\\|->\\)\\|-[^>]\\)+[^>]?-->" 
     0 font-lock-comment-face t)

;;; Hightlight HTML entities a little bit.
   '("&[a-zA-Z0-9#]*;" 0 font-lock-type-face t)

;;; Displaying all HTML code inside TEXTAREAs as normal text. Works
;;; not in Emacs 20.x!
;   (list
;    (concat "<XMP[^>]*>\\(\\("
;	    "[^<]\\|<\\([^/]\\|/\\([^X]\\|X\\([^M]\\|M[^P]\\)\\)\\)"
;	    "\\)*\\)</XMP>")
;     1 'font-lock-standard-face t)

;;; Displaying all HTML examples as normal text. Works not in Emacs
;;; 20.x! 
;   (list 
;    (concat "<TEXTAREA[^>]*>\\(\\("
;	        "[^<]\\|"
;	    "<\\([^/]\\|"
;	    "/\\([^T]\\|"
;	    "T\\([^E]\\|"
;	    "E\\([^X]\\|"
;	    "X\\([^T]\\|"
;	    "T\\([^A]\\|"
;	    "A\\([^R]\\|"
;	    "R\\([^E]\\|"
;	       "E[^A]"
;	    "\\)\\)\\)\\)\\)\\)\\)\\)\\)*\\)</TEXTAREA>")
;    1 'font-lock-standard-face t)

;;; Highlight Server-Side-Includes (aka SSI) against comments
   (cond (html32-highlight-ssi
	  '("<!--\\(#[a-zA-Z0-9]+ .*\\) -->" 
	    (0 font-lock-comment-face t) 
	    (1 font-lock-function-name-face t))))
   )
  "Patterns to highlight in HTML buffers.

Because of HTML having no functions or variables and other constructs
like a programming languages, I choosed a fontifying that all tags
appear a bit lighter than the normal text. All parameter, which get an
URL as value and all document structuring tags are highlighted against
the other tags. Also HTML entities are highlighted, but not so strong,
so that you can read the text in the source code without problems.

Suggestion for some other fontifiying schemes are welcome at
abe@cs.uni-sb.de.

May be the fontifying scheme will be redone in further versions of
HTML 3.2 mode.") 

;(defun html32-font-lock-make-additional-faces ()
;  (interactive)
;  (font-lock-make-face (list 'font-lock-standard-face "Black")))
;
;(add-hook 'font-lock-mode-hook
;	  'html32-font-lock-make-additional-faces)


(defun html32-fontify ()
  "Loads the (possibly new defined) html32-font-lock-keywords into
font-lock-keywords.

Note: This function being interactive is necessary only during
developing and testing regexps for the highlighting. So if it's still
interactive when you read this, you've got a beta version or I forgot
to remove the \"(interactive)\" in the final version. ;-)" 
  (interactive) ;;; Comment this line for final version!
  (make-local-variable 'font-lock-keywords) 
  (setq font-lock-keywords html32-font-lock-keywords)
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
)

;;; --------------------------- buffer-local vars ----------------------------

(defvar html32-link-counter-default 0)
(defvar html32-link-counter nil)
(make-variable-buffer-local 'html32-link-counter)
(setq-default html32-link-counter html32-link-counter-default)

(defun html32-set-html-tab (tab)
  "Set a new value for html32-tab."
  (interactive "sNew tab (Enter leaves old value): ")
  (if (not (or (null tab) (equal "" tab)))
      (setq html32-tab tab)))

;;; --------------------------------------------------------------------------
;;; ------------------------ command support routines ------------------------
;;; --------------------------------------------------------------------------

(defun html32-add-list-internal (type &optional param)
  "Set up a given type of list by opening the list start/end pair
and creating an initial element.  Single argument TYPE is a string,
assumed to be a valid HTML list type (\"UL\", \"OL\", \"DIR\" or
\"MENU\"). Mark is set after list."
  (let ((start (point)))
    (insert "<" type)
    (if param (insert " " param))
    (insert ">\n")
;    (html32-maybe-deemphasize-region start (1- (point)))
    (insert html32-tab "<LI>")
    ;; Point goes right there.
    (save-excursion
      (if html32-closing-li (insert "</LI>"))
      (insert "\n")
      (setq start (point))
      (insert "</" type ">\n")
;      (html32-maybe-deemphasize-region start (1- (point)))
      ;; Reuse start to set mark.
      (setq start (point)))
    (push-mark start t)))

(defun html32-open-area (tag &optional tabulator)
  "Open an area for entering text such as PRE, XMP, BLOCKQUOTE or
LISTING. If a second argument (strings only) is given, it is used to 
indent the items"
  (let ((start (point)))
    (insert "<" tag ">\n")
    (if tabulator (insert tabulator))
;    (html32-maybe-deemphasize-region start (1- (point)))
    (save-excursion
      (insert "\n")
      (setq start (point))
      (insert "</" tag ">\n")
;      (html32-maybe-deemphasize-region start (1- (point)))
      ;; Reuse start to set mark.
      (setq start (point)))
    (push-mark start t)))

(defun html32-open-field (tag &optional param)
  (cond (mark-active
	 (let ((start (region-beginning))
	       (end   (region-end)))
	   (goto-char end)
	   (save-excursion
	     (goto-char start)
	     (insert "<" tag)
	     (if param (insert " " param))
	     (insert ">")
;	     (html32-maybe-deemphasize-region start (1- (point)))
	     )
	   (insert "</" tag ">")
;	   (html32-maybe-deemphasize-region (1+ end) (point))
	   ))
	(t
	 (let ((start (point)))
	   (insert "<" tag)
	   (if param (insert " " param))
	   (insert ">")
;	   (html32-maybe-deemphasize-region start (1- (point)))
	   (setq start (point))
	   (insert "</" tag ">")
;	   (html32-maybe-deemphasize-region (1+ start) (point))
	   (push-mark)
	   (goto-char start)))))

(defun html32-not-inside-tagp (&optional point)
  "Tests, if point is inside a HTML tag."
  (interactive "d")
  (save-excursion
    (let ((point (if point point (point)))
	  (ff t)
	  (fb t)
	  )
      (goto-char point)
      (cond ((looking-at "<") (setq fb ()))
	    (t
	     (if (search-backward "<" (point-min) t)
		 (setq fb (search-forward ">" point t)))
	     (goto-char point)))
      (if (search-forward ">" (point-max) t)
	  (setq ff (search-backward "<" (1+ point) t)))
      (or fb ff))))
      
(defun html32-inside-closing-tagp (&optional point)
  "Tests, if point is inside a closing HTML tag."
  (interactive "d")
  (save-excursion
    (let ((point (if point point (point)))
	  (beg ()))
      
	  
      (goto-char point)
      (setq beg (looking-at "</"))
      (search-backward "<" (point-min) t)
      (and (not (html32-not-inside-tagp (point))) 
	   (or (looking-at "</") beg)))))
      
(defun html32-add-alignment (align &optional valign)
  "Need to be started inside a opening HTML tag."
  (if (looking-at "<") (forward-char 1))
  (if (looking-at ">") (forward-char -1))
  (if (html32-not-inside-tagp (point))
      (error "You aren't inside a HTML tag."))
  (if (html32-inside-closing-tagp (point))
      (error "This is a closing HTML tag."))
  (if html32-alignment-at-end
      (search-forward ">" (point-max) t)
    (re-search-forward "[ \n\t]" (point-max) t))
  (save-excursion
    (forward-char -1)
    (insert " ")
    (if valign (insert "V"))
    (insert "ALIGN=\"" align "\"")))

;;; --------------------------------------------------------------------------
;;; -------------------------------- commands --------------------------------
;;; --------------------------------------------------------------------------

(defun html32-add-basefont-size (size)
  "Add a default font size to the document. If a default font size is
already set, replace old setting. If no BODY, HTML or HEAD tag exists,
they will be inserted."
  (interactive "sDefault font size for this document (1..7) [3]: ")
  (cond ((or (equal size "") (null size))
	 (setq size "3"))
	((not (stringp size))
	 (error "Parameter SIZE needs to be a string.")))
  (save-excursion 
    (html32-add-body)
    (goto-char (point-min))
    (cond ((re-search-forward 
	    "<[bB][aA][sS][eE][fF][oO][nN][tT]"
	    (point-max) t)
	   (let ((end-of-bf (point))
		 (end-of-tag (save-excursion
				(search-forward ">" (point-max))
				(point))))
	     (cond ((re-search-forward 
		     "[sS][iI][zZ][eE]=\"?[0-9]+\"?" end-of-tag)
		    (replace-match (concat "SIZE=\"" size "\"")))
		   (t
		    (goto-char end-of-bf)
		    (insert " SIZE=\"" size "\"")))))
	  (t 
	   (goto-char (point-min))
	   (re-search-forward "<[bB][oO][dD][yY]" (point-max) t)
	   (search-forward ">" (point-max) t)
	   (insert "\n<BASEFONT SIZE=\"" size "\">")))))

;; C-c C-a a
(defun html32-align-absmiddle ()
  "Add \" ALIGN=absmiddle\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "absmiddle"))

;; C-c C-a b
(defun html32-align-bottom ()
  "Add \" ALIGN=bottom\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "bottom"))

;; C-c C-a C-b
(defun html32-valign-bottom ()
  "Add \" VALIGN=bottom\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "bottom" t))

;; C-c C-a c
(defun html32-align-center ()
  "Add \" ALIGN=center\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "center"))

;; C-c C-a l
(defun html32-align-left ()
  "Add \" ALIGN=left\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "left"))

;; C-c C-a m
(defun html32-align-middle ()
  "Add \" ALIGN=middle\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "middle"))

;; C-c C-a C-m
(defun html32-valign-middle ()
  "Add \" VALIGN=middle\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "middle" t))

;; C-c C-a r
(defun html32-align-right ()
  "Add \" ALIGN=right\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "right"))

;; C-c C-a t
(defun html32-align-top ()
  "Add \" ALIGN=top\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "top"))

;; C-c C-a C-t
(defun html32-valign-top ()
  "Add \" VALIGN=top\" to an existing HTML tag at point."
  (interactive)
  (html32-add-alignment "top" t))

;; C-c C-a C-l
(defun html32-divalign-left ()
  "Add an tag for left bound text."
  (interactive)
  (html32-open-field "DIV" "ALIGN=\"left\""))

;; C-c C-a C-r
(defun html32-divalign-right ()
  "Add an tag for right bound text."
  (interactive)
  (html32-open-field "DIV" "ALIGN=\"right\""))

;; C-c C-a C-c
(defun html32-divalign-center ()
  "Add an tag for centered text."
  (interactive)
  (html32-open-field "CENTER"))

;; C-c a
(defun html32-add-address ()
  "Open an address element."
  (interactive)
  (html32-open-field "ADDRESS"))

;; C-c b
(defun html32-add-blockquote ()
  "Add a block-quoted area."
  (interactive)
  (html32-open-area "BLOCKQUOTE" "\t"))

;; C-c C-b
(defun html32-add-bold ()
  "Add a bold tag."
  (interactive)
  (html32-open-field "B"))

;; C-c c
(defun html32-add-citation ()
  "Add a citation element"
  (interactive)
  (html32-open-field "CITE"))

;; C-c C-c
(defun html32-add-code ()
  "Add a code element"
  (interactive)
  (html32-open-field "CODE"))

;; C-c d
(defun html32-add-description-list ()
  "Add a definition list."
  (interactive)
  (let ((start (point)))
    (insert "<DL>\n")
;    (html32-maybe-deemphasize-region start (1- (point)))
    (insert html32-tab "<DT>")
    ;; Point goes right there.
    (save-excursion
      (insert "\n" html32-tab html32-tab "<DD>\n")
      (setq start (point))
      (insert "</DL>\n")
;      (html32-maybe-deemphasize-region start (1- (point)))
      ;; Reuse start to set mark.
      (setq start (point)))
    (push-mark start t)))

;; C-c t
(defun html32-add-table (border caption &optional align)
  "Add a HTML 3.2 table. 

You will be prompted for 
- the size of the table border,
- a caption and
- the caption's position. 

If you enter the empty string or nil to one of the parameters, it's
default value is used:

- border size default: zero. 
- caption default: No caption will be specified. (But if
  html32-add-table is called interactive you will be prompted for the
  position, though. Just press enter then.)
- caption position: Browser will use their default position. (means:
  No position will be specified.)
"
  (interactive 
   "sBorder size [0]: \nsCaption [none]: \nsPosition (top or bottom) [none]: ")
  (if (equal caption "") (setq caption ()))
  (if (equal align "") (setq align ()))
  (if (or (null border) (equal border "")) (setq border "0"))
  (let ((start (point)))
    (insert "<TABLE BORDER=\"" border "\">\n")
;    (html32-maybe-deemphasize-region start (1- (point)))
    (cond (caption (insert "<CAPTION")
		   (if align (insert " ALIGN=\"" align))
		   (insert "\">\n" html32-tab caption "\n</CAPTION>\n")))
    (insert "<TR>\n" html32-tab "<TD>")
    ;; Point goes right there.
    (save-excursion
      (if html32-closing-td (insert "\n" html32-tab "</TD>\n"))
      (if html32-closing-tr (insert "</TR>\n"))
      (setq start (point))
      (insert "</TABLE>\n")
;      (html32-maybe-deemphasize-region start (1- (point)))
      ;; Reuse start to set mark.
      (setq start (point)))
    (push-mark start t)))

;; C-c e
(defun html32-add-description-entry ()
  "Add a definition entry.  Assume we're at the end of a previous
entry."
  (interactive)
  (let ((start (point)))
    (insert "\n" html32-tab "<DT> ")
    (save-excursion
      (insert "\n" html32-tab html32-tab "<DD> "))))

;; C-c C-e
(defun html32-add-emphasized ()
  "Add an emphasized element."
  (interactive)
  (html32-open-field "EM"))

;; C-c C-f
(defun html32-add-fixed ()
  "Add a non-proportional-font (typewriter font) tag."
  (interactive)
  (html32-open-field "TT"))

;; C-c g
(defun html32-add-img (href alt)
  "Add an image tag"
  (interactive "sImage URL: \nsAlternative text: ")
  (let ((start (point)))
    (insert "<IMG\ SRC=\"" href "\" ALT=\"" alt "\">")
    ; The "\\ " is necessary, because it would be replaced by
    ; "\"\\\"", when you byte-compile and load this file. :-/
;    (html32-maybe-deemphasize-region (1+ start) (1- (point)))
    ))

;; C-c h
(defun html32-add-header (size header)
  "Add a header."
  (interactive "sSize (1-6; 1 biggest): \nsHeader: ")
  (let ((start (point)))
    (insert "<H" size ">")
;    (html32-maybe-deemphasize-region start (1- (point)))
    (insert header)
    (setq start (point))
    (insert "</H" size ">\n")
;    (html32-maybe-deemphasize-region (1+ start) (1- (point)))
    ))

;; C-c C-h
(defun html32-add-head ()
  "Add document head, if none exists."
  (interactive)
  (html32-add-html)
  (save-excursion
    (goto-char (point-min))
    (cond
     ((null (re-search-forward
	    "<[hH][eE][aA][dD]>"
	    (point-max) t))
      (goto-char (point-min))
      (re-search-forward "<[hH][tT][mM][lL]>" (point-max) t)
      (insert "\n<HEAD>")))
    (cond
     ((null (re-search-forward
	    "</[hH][eE][aA][dD]>"
	    (point-max) t))
      (while 
	  (re-search-forward
	   "\\(<\\([mM][eE][tT][aA]\\|[bB][aA][sS][eE]\\|[iI][sS][iI][nN][dD][eE][xX]\\|[lL][iI][nN][kK]\\)\\([ \n\t]+[a-zA-Z]+=\"?[^\"]*\"?\\)>\\|<[tT][iI][tT][lL][eE]>.*</[tT][iI][tT][lL][eE]>\\)" (point-max) t))
      (insert "\n</HEAD>\n")))))

;; C-c i
(defun html32-add-list-or-menu-item ()
  "Add a list or menu item. Assume we're at the end of the
last item."
  (interactive)
  (insert "\n" html32-tab "<LI>")
  (if html32-closing-li (save-excursion (insert "</LI>"))))

;; C-c t r
(defun html32-add-table-row ()
  "Add a table row. Assume we're at the end of a previous entry."
  (interactive)
  (insert "\n<TR>\n" html32-tab)
  (if html32-closing-tr (save-excursion (insert "\n</TR>"))))

;; C-c t f
(defun html32-add-table-field ()
  "Add a table field. Assume we're at the end of a previous entry."
  (interactive)
  (insert "\n" html32-tab "<TD>")
  (if html32-closing-td (save-excursion (insert "</TD>"))))

;; C-c t h
(defun html32-add-table-head ()
  "Add a table header field. Assume we're at the end of a previous entry."
  (interactive)
  (insert "\n" html32-tab "<TH>")
  (if html32-closing-td (save-excursion (insert "</TH>"))))

;; C-c C-i
(defun html32-add-italic ()
  "Add an italic tag."
  (interactive)
  (html32-open-field "I"))

;; C-c k
(defun html32-add-keyboard ()
  "Add a keyboard-input-like element."
  (interactive)
  (html32-open-field "KBD"))

;; C-c l
(defun html32-add-link (link)
  "Make a hypertext link.  (There is no completion of any kind yet.)"
  (interactive "sLink to: ") 
     ;;; "F" instead of "s" does *NOT* work, because it doesn't accept
     ;;; "//" and "~" as they are and converts them. Also would
     ;;; relative links be expanded to absolute ones. :-(((
  (html32-open-field "A" (concat "HREF=\"" link "\"")))

;; C-c C-l
(defun html32-add-listing ()
  "Add a listing area."
  (interactive)
  (html32-open-area "LISTING"))

;; C-c m
(defun html32-add-menu ()
  "Add a menu."
  (interactive)
  (html32-add-list-internal "MENU"))

;; C-c C-m
(defun html32-add-sample ()
  "Add an example element."
  (interactive)
  (html32-open-field "SAMP"))

;; C-c n
(defun html32-add-numbered-list (type)
  "Add a numbered list."
  (interactive "sHow shoul it be numbered? (1, a, A, i, I): ")
  (html32-add-list-internal "OL" (concat "TYPE=\"" type "\"")))

;; C-c p
(defun html32-add-paragraph ()
  "Open a paragraph element. (Replaces the former
html32-add-paragraph-separator function.)"
  (interactive)
  (html32-open-field "P"))

;; C-c C-p
(defun html32-add-preformatted ()
  "Add a pre-formatted text area."
  (interactive)
  (html32-open-area "PRE"))

;; C-c r
(defun html32-add-reference (name)
  "Add a reference point (a link with no reference of its own) to
the current region."
  (interactive "sName (or RET for numeric): ")
  (cond ((not (stringp name))
	 (error "Parameter NAME needs to be a string."))
	((string= name "")
         (setq html32-link-counter (1+ html32-link-counter))
         (setq name (format "%d" html32-link-counter))))
  (html32-open-field "A" (concat "NAME=\"" name "\"")))

;; C-c C-=
(defun html32-add-horizontal-rule (width align)
  "Adds a horizontal rule into the document and asks for it's width
and alignment. Defaults are WIDTH=\"100%\" and ALIGN=\"center\"."
  (interactive "sWidth (either percent or pixel) [100%%]: \nsAlignment (center, left or right) [center]")
  (insert "<HR")
  (if (not (equal "" width))
      (insert " WIDTH=\"" width "\""))
  (if (not (equal "" align))
      (insert "ALIGN=\"" align "\""))
  (insert ">"))

;; C-c s
(defun html32-add-list ()
  "Add a unordered list."
  (interactive)
  (html32-add-list-internal "UL"))

;; C-c C-s
(defun html32-add-strong ()
  "Add a strong emphasized element."
  (interactive)
  (html32-open-field "STRONG"))

;; C-c v
(defun html32-add-variable () 
  (interactive)
  (html32-open-field "VAR"))

;; C-c x
(defun html32-add-plaintext ()
  "Add plaintext."
  (interactive)
  (html32-open-area "PLAINTEXT"))

;; C-c C-x
(defun html32-add-xmp ()
  "Add an HTML example."
  (interactive)
  (html32-open-area "XMP"))

;; C-c _
(defun html32-add-underline ()
  "Add an underline tag."
  (interactive)
  (html32-open-field "U"))

;; C-c =
(defun html32-add-strike ()
  "Add an strike tag."
  (interactive)
  (html32-open-field "STRIKE"))

;; C-c #
(defun html32-add-font-size (n)
  "Add an element with font size n."
  (interactive "sFont size: (1 = smallest): ")
  (if (not (stringp n)) (error "Parameter N needs to be a string."))
  (html32-open-field "FONT" (concat "SIZE=\"" n "\"")))

;; C-c -
(defun html32-add-small (&optional n)
  "Add an small font element. If you give an optional 
parameter (1..6), you can define \"how small\" the font should
be. Default value is one." 
  (interactive "sHow small? (integer) [1]: ")
  (cond ((or (null n) (equal n 1)) (html32-open-field "SMALL"))
	((not (stringp n)) (error "Parameter N needs to be a string."))
	((or (string= n "") (string= n "1")) (html32-open-field "SMALL"))
	(t (html32-open-field "FONT" (concat "SIZE=\"-" 
					     (int-to-string n) 
					     "\"")))))

;; C-c +
(defun html32-add-big (&optional n)
  "Add an big font element. If you give an optional 
parameter (1..6), you can define \"how big\" the font should
be. Default value is one." 
  (interactive "sHow big? (integer) [1]")
  (cond ((or (null n) (equal n 1)) (html32-open-field "BIG"))
	((not (stringp n)) (error "Parameter N needs to be a string."))
	((or (string= n "") (string= n "1")) (html32-open-field "BIG"))
	(t (html32-open-field "FONT" (concat "SIZE=\"+" 
					     (int-to-string n) 
					     "\"")))))

;; C-c C--
(defun html32-add-sub ()
  "Add an subscript tag."
  (interactive)
  (html32-open-field "SUB"))

;; C-c C-+
(defun html32-add-sup ()
  "Add an superscript tag."
  (interactive)
  (html32-open-field "SUP"))

;; C-c .
(defun html32-add-dir ()
  "Add a directory list."
  (interactive)
  (html32-add-list-internal "DIR"))

;; C-c :
(defun html32-add-definition ()
  "Open a definition element."
  (interactive)
  (html32-open-field "DFN"))

;; C-c M-a
(defun html32-add-basics (ver title)
  "Adds the basic skeleton of an HTML 3.2 document. Asks for the HTML
version and for a document title."
  (interactive "sHTML version [3.2 Final]: \nsTitle: ")
  (html32-add-doctype ver)
  (html32-add-html)
  (html32-add-head)
  (html32-add-title title)
  (html32-add-body))

;; C-c M-b
(defun html32-add-body ()
  "Add document body, if none exists."
  (interactive)
  (html32-add-head)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     "</[hH][eE][aA][dD]>"
     (point-max) t)
    (cond
     ((null (re-search-forward
	     "<[bB][oO][dD][yY][ \t\n>]"
	     (point-max) t))
      (insert "\n<BODY>")))
    (cond
     ((null (re-search-forward
	     "</[bB][oO][dD][yY]>"
	     (point-max) t))
      (re-search-forward
       "</[hH][tT][mM][lL]>"
       (point-max) t)
      (forward-char -7)
      (insert "</BODY>\n")))))

;; C-c M-d

(defun html32-add-doctype (ver)
  "Add DOCTYPE string at the beginning of the document. Uses HTML
version 3.2 final, if the empty string is given as version."
  (interactive "sHTML version [3.2 Final]: ")
  (if (equal "" ver) (setq ver "3.2 Final"))
  (setq ver (concat "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML " ver
		    "//EN\">"))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward 
	 "<![dD][oO][cC][tT][yY][pP][eE].*>"
	 (save-excursion (end-of-line) (point))
	 t)
	(replace-match ver t)
      (insert ver "\n"))))

;; C-c M-l
(defun html32-add-html () 
  "Add document begin and end (<HTML>...</HTML>), if none exists."
  (interactive) 
  (save-excursion
    (goto-char (point-min))
    (cond
     ((null (re-search-forward
	     "<[hH][tT][mM][lL]>"
	     (point-max) t))
      (if (re-search-forward
	   "<![dD][oO][cC][tT][yY][pP][eE].*>"
	   (point-max) t)
	  (insert "\n"))
      (insert "<HTML>\n")))
    (cond
     ((null (re-search-forward
	    "</[hH][tT][mM][lL]>"
	    (point-max) t))
      (goto-char (point-max))
      (insert "\n</HTML>")))))

;; C-c t
(defun html32-add-title (title)
  "Add or modify a title. Calls html32-add-head before adding or
modifying the title."
  (interactive "sTitle: ")
  (html32-add-head)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "<[hH][eE][aA][dD]>" (point-max) t)
    (cond 
     ((re-search-forward 
       "<[tT][iI][tT][lL][eE]>"
	 (point-max) t)
      (re-search-forward "[^<]*" 
			 (save-excursion (end-of-line) (point)) 
			 t)
	;; Plop the new title in its place.
        (replace-match title t))
     (t
      (insert "\n<TITLE>")
;      (html32-maybe-deemphasize-region (point-min) (1- (point)))
      (insert title)
      (insert "</TITLE>")
;      (html32-maybe-deemphasize-region (- (point) 7) (point))
      ))))

;;; --------------------------------------------------------------------------
;;; ---------------------------- special commands ----------------------------
;;; --------------------------------------------------------------------------

;;; Inserting special characters

; umlauts

(defun html32-add-auml ()
  (interactive)
  (insert "&auml;"))

(defun html32-add-cap-auml ()
  (interactive)
  (insert "&Auml;"))

(defun html32-add-euml ()
  (interactive)
  (insert "&euml;"))

(defun html32-add-cap-euml ()
  (interactive)
  (insert "&Euml;"))

(defun html32-add-iuml ()
  (interactive)
  (insert "&iuml;"))

(defun html32-add-cap-iuml ()
  (interactive)
  (insert "&Iuml;"))

(defun html32-add-ouml ()
  (interactive)
  (insert "&ouml;"))

(defun html32-add-cap-ouml ()
  (interactive)
  (insert "&Ouml;"))

(defun html32-add-uuml ()
  (interactive)
  (insert "&uuml;"))

(defun html32-add-cap-uuml ()
  (interactive)
  (insert "&Uuml;"))

(defun html32-add-yuml ()
  (interactive)
  (insert "&yuml;"))

; accent acute

(defun html32-add-aacute ()
  (interactive)
  (insert "&aacute;"))

(defun html32-add-cap-aacute ()
  (interactive)
  (insert "&Aacute;"))

(defun html32-add-eacute ()
  (interactive)
  (insert "&eacute;"))

(defun html32-add-cap-eacute ()
  (interactive)
  (insert "&Eacute;"))

(defun html32-add-iacute ()
  (interactive)
  (insert "&iacute;"))

(defun html32-add-cap-iacute ()
  (interactive)
  (insert "&Iacute;"))

(defun html32-add-oacute ()
  (interactive)
  (insert "&oacute;"))

(defun html32-add-cap-oacute ()
  (interactive)
  (insert "&Oacute;"))

(defun html32-add-uacute ()
  (interactive)
  (insert "&uacute;"))

(defun html32-add-cap-uacute ()
  (interactive)
  (insert "&Uacute;"))

(defun html32-add-yacute ()
  (interactive)
  (insert "&yacute;"))

; accent grave

(defun html32-add-agrave ()
  (interactive)
  (insert "&agrave;"))

(defun html32-add-cap-agrave ()
  (interactive)
  (insert "&Agrave;"))

(defun html32-add-egrave ()
  (interactive)
  (insert "&egrave;"))

(defun html32-add-cap-egrave ()
  (interactive)
  (insert "&Egrave;"))

(defun html32-add-igrave ()
  (interactive)
  (insert "&igrave;"))

(defun html32-add-cap-igrave ()
  (interactive)
  (insert "&Igrave;"))

(defun html32-add-ograve ()
  (interactive)
  (insert "&ograve;"))

(defun html32-add-cap-ograve ()
  (interactive)
  (insert "&Ograve;"))

(defun html32-add-ugrave ()
  (interactive)
  (insert "&ugrave;"))

(defun html32-add-cap-ugrave ()
  (interactive)
  (insert "&Ugrave;"))

; accent circonflexe

(defun html32-add-acirc ()
  (interactive)
  (insert "&acirc;"))

(defun html32-add-cap-acirc ()
  (interactive)
  (insert "&Acirc;"))

(defun html32-add-ecirc ()
  (interactive)
  (insert "&ecirc;"))

(defun html32-add-cap-ecirc ()
  (interactive)
  (insert "&Ecirc;"))

(defun html32-add-icirc ()
  (interactive)
  (insert "&icirc;"))

(defun html32-add-cap-icirc ()
  (interactive)
  (insert "&Icirc;"))

(defun html32-add-ocirc ()
  (interactive)
  (insert "&ocirc;"))

(defun html32-add-cap-ocirc ()
  (interactive)
  (insert "&Ocirc;"))

(defun html32-add-ucirc ()
  (interactive)
  (insert "&ucirc;"))

(defun html32-add-cap-ucirc ()
  (interactive)
  (insert "&Ucirc;"))

; tilde, ring, cedille, slash

(defun html32-add-atilde ()
  (interactive)
  (insert "&atilde;"))

(defun html32-add-cap-atilde ()
  (interactive)
  (insert "&Atilde;"))

(defun html32-add-otilde ()
  (interactive)
  (insert "&otilde;"))

(defun html32-add-cap-otilde ()
  (interactive)
  (insert "&Otilde;"))

(defun html32-add-ntilde ()
  (interactive)
  (insert "&ntilde;"))

(defun html32-add-cap-ntilde ()
  (interactive)
  (insert "&Ntilde;"))

(defun html32-add-aring ()
  (interactive)
  (insert "&aring;"))

(defun html32-add-cap-aring ()
  (interactive)
  (insert "&Aring;"))

(defun html32-add-ccedil ()
  (interactive)
  (insert "&ccedil;"))

(defun html32-add-cap-ccedil ()
  (interactive)
  (insert "&Ccedil;"))

(defun html32-add-oslash ()
  (interactive)
  (insert "&oslash;"))

(defun html32-add-cap-oslash ()
  (interactive)
  (insert "&Oslash;"))

; national special characters

(defun html32-add-szlig ()
  (interactive)
  (insert "&szlig;"))

(defun html32-add-eth ()
  (interactive)
  (insert "&eth;"))

(defun html32-add-cap-eth ()
  (interactive)
  (insert "&ETH;"))

(defun html32-add-thorn ()
  (interactive)
  (insert "&thorn;"))

(defun html32-add-cap-thorn ()
  (interactive)
  (insert "&THORN;"))

(defun html32-add-aelig ()
  (interactive)
  (insert "&aelig;"))

(defun html32-add-cap-aelig ()
  (interactive)
  (insert "&AElig;"))

; HTML entities for sign characters

(defun html32-add-iexcl ()
  (interactive)
  (insert "&iexcl;"))

(defun html32-add-cent ()
  (interactive)
  (insert "&cent;"))

(defun html32-add-pound ()
  (interactive)
  (insert "&pound;"))

(defun html32-add-curren ()
  (interactive)
  (insert "&curren;"))

(defun html32-add-yen ()
  (interactive)
  (insert "&yen;"))

(defun html32-add-brvbar ()
  (interactive)
  (insert "&brvbar;"))

(defun html32-add-sect ()
  (interactive)
  (insert "&sect;"))

(defun html32-add-uml ()
  (interactive)
  (insert "&uml;"))

(defun html32-add-copy ()
  (interactive)
  (insert "&copy;"))

(defun html32-add-ordf ()
  (interactive)
  (insert "&ordf;"))

(defun html32-add-laquo ()
  (interactive)
  (insert "&laquo;"))

(defun html32-add-not ()
  (interactive)
  (insert "&not;"))

(defun html32-add-shy ()
  (interactive)
  (insert "&shy;"))

(defun html32-add-reg ()
  (interactive)
  (insert "&reg;"))

(defun html32-add-macr ()
  (interactive)
  (insert "&macr;"))

(defun html32-add-deg ()
  (interactive)
  (insert "&deg;"))

(defun html32-add-plusmn ()
  (interactive)
  (insert "&plusmn;"))

(defun html32-add-sup2 ()
  (interactive)
  (insert "&sup2;"))

(defun html32-add-sup3 ()
  (interactive)
  (insert "&sup3;"))

(defun html32-add-micro ()
  (interactive)
  (insert "&micro;"))

(defun html32-add-para ()
  (interactive)
  (insert "&para;"))

(defun html32-add-middot ()
  (interactive)
  (insert "&middot;"))

(defun html32-add-cedil ()
  (interactive)
  (insert "&cedil;"))

(defun html32-add-sup1 ()
  (interactive)
  (insert "&sup1;"))

(defun html32-add-ordm ()
  (interactive)
  (insert "&ordm;"))

(defun html32-add-raquo ()
  (interactive)
  (insert "&raquo;"))

(defun html32-add-frac14 ()
  (interactive)
  (insert "&frac14;"))

(defun html32-add-frac12 ()
  (interactive)
  (insert "&frac12;"))

(defun html32-add-frac34 ()
  (interactive)
  (insert "&frac34;"))

(defun html32-add-iquest ()
  (interactive)
  (insert "&iquest;"))

(defun html32-add-times ()
  (interactive)
  (insert "&times;"))

(defun html32-add-divide ()
  (interactive)
  (insert "&divide;"))

; HTML key characters

(defun html32-less-than ()
  (interactive)
  (insert "&lt;"))

(defun html32-greater-than ()
  (interactive)
  (insert "&gt;"))

(defun html32-quotation-mark ()
  (interactive)
  (insert "&quot;"))

; Special character prefixes

(defun html32-ampersand ( special )
  "Used as prefix to get the most of the HTML 3.2 Entities. 
Type \"& C-h\" to get a list of all entities possible with the 
\"&\" prefix."
  (interactive "cWhat special char? (C-h for help) ")
  (let ((case-fold-search nil)) ;; Casesensitive
    (cond ((char-equal special ?a) ;; a umlaut
	   (insert "&auml;"))
	  ((char-equal special ?A) ;; A umlaut
	   (insert "&Auml;"))
	  ((char-equal special ?o) ;; o umlaut
	   (insert "&ouml;"))
	  ((char-equal special ?O) ;; O umlaut
	   (insert "&Ouml;"))
	  ((char-equal special ?e) ;; e dieresis
	   (insert "&euml;"))
	  ((char-equal special ?E) ;; E dieresis
	   (insert "&Euml;"))
	  ((char-equal special ?i) ;; i dieresis
	   (insert "&iuml;"))
	  ((char-equal special ?I) ;; I dieresis
	   (insert "&Iuml;"))
	  ((char-equal special ?u) ;; u umlaut
	   (insert "&uuml;"))
	  ((char-equal special ?U) ;; U umlaut
	   (insert "&Uuml;"))
	  ((char-equal special ?s) ;; sharp s (German)
	   (insert "&szlig;"))
	  ((char-equal special ?&) ;; ampersand
	   (insert "&amp;"))
	  ((char-equal special ?l) ;; less than
	   (insert "&lt;"))
	  ((char-equal special ?g) ;; greater than
	   (insert "&gt;"))
	  ((char-equal special ?<) ;; less than
	   (insert "&lt;"))
	  ((char-equal special ?>) ;; greater than
	   (insert "&gt;"))
	  ((char-equal special ?!) ;; inverted exclamation mark 
	   (insert "&iexcl;"))
	  ((char-equal special ??) ;; inverted question mark 
	   (insert "&iquest;"))
	  ((char-equal special ?$) ;; cent sign
	   (insert "&cent;"))
	  ((char-equal special ?L) ;; pound sign
	   (insert "&pound;"))
	  ((char-equal special ?P) ;; section sign
	   (insert "&sect;"))
	  ((char-equal special ?S) ;; section sign
	   (insert "&sect;"))
	  ((char-equal special ?W) ;; general currency sign 
	   (insert "&curren;"))
	  ((char-equal special ?") ;; Hochkommas
	   (insert "&quot;"))
	  ((char-equal special ?Y) ;; Yen
	   (insert "&yen;"))
	  ((char-equal special ?|) ;; Pipe
	   (insert "&brvbar;"))
	  ((char-equal special ?C) ;; (c)
	   (insert "&copy;"))
	  ((char-equal special ?c) ;; (c)
	   (insert "&copy;"))
	  ((char-equal special ?R) ;; (r)
	   (insert "&reg;"))
	  ((char-equal special ?r) ;; (r)
	   (insert "&reg;"))
	  ((char-equal special ?T) ;; TM
	   (insert "<SUP>TM</SUP>"))
	  ((char-equal special ?t) ;; tm
	   (insert "<SUP>tm</SUP>"))
	  ((char-equal special ?\;) ;; <<
	   (insert "&laquo;"))
	  ((char-equal special ?:) ;; >>
	   (insert "&raquo;"))
	  ((char-equal special ?=) ;; Weiche Trennung, was auch immer das 
	   (insert "&shy;"))       ;; heissen soll.... :-)
	  ((char-equal special ?_) ;; Macron
	   (insert "&macr;"))
	  ((char-equal special ?*) ;; Grad-Zeichen
	   (insert "&deg;"))
	  ((char-equal special ?~) ;; Grad-Zeichen
	   (insert "&deg;"))
	  ((char-equal special ?+) ;; Plus-Minus
	   (insert "&plusmn;"))
	  ((char-equal special ?1) ;; Hoch 1
	   (insert "&sup1;"))
	  ((char-equal special ?2) ;; Hoch 2
	   (insert "&sup2;"))
	  ((char-equal special ?3) ;; Hoch 3
	   (insert "&sup3;"))
	  ((char-equal special ?m) ;; micro
	   (insert "&micro;"))
	  ((char-equal special ?.) ;; Punkt in der Mitte
	   (insert "&middot;"))
	  ((char-equal special ?,) ;; Cedille
	   (insert "&cedil;"))
	  ((char-equal special ?x) ;; Multiplikations-X
	   (insert "&times;"))
	  ((char-equal special ?X) ;; Multiplikations-X
	   (insert "&times;"))
	  ((char-equal special ?%) ;; Divisionszeichen
	   (insert "&divide;"))
	  ((char-equal special ?/) ;; Divisionszeichen
	   (insert "&divide;"))
	  ((char-equal special ?b) ;; thorn (islandic)
	   (insert "&thorn;"))
	  ((char-equal special ?B) ;; THORN (islandic)
	   (insert "&THORN;"))
	  ((char-equal special ?d) ;; eth (islandic)
	   (insert "&eth;"))
	  ((char-equal special ?D) ;; ETH (islandic)
	   (insert "&ETH;"))
	  ((char-equal special ?n) ;; n Tilde
	   (insert "&ntilde;"))
	  ((char-equal special ?N) ;; N Tilde
	   (insert "&Ntilde;"))
	  ((char-equal special ?y) ;; y Umlaut
	   (insert "&yuml;"))
	  ((char-equal special ?-) ;; Not
	   (insert "&not;"))
	  ((char-equal special ?M) ;; ordinal indicator, maskuline
	   (insert "&ordm;"))
	  ((char-equal special ?F) ;; ordinal indicator, feminine
	   (insert "&ordf;"))
	  ((char-equal special ?^) ;; Umlaut-Puenktchen
	   (insert "&uml;"))
	  ((char-equal special ?0) ;; Grad
	   (insert "&deg;"))
	  ((char-equal special ?') ;; Accent aigu
	   (insert "&acute;"))
	  ((char-equal special ? ) ;; non-breaking space
	   (insert "&nbsp;"))
	  ((char-equal special ?\C-?) ;; Delete / Undo "&" / C-g
	   (insert ""))
	  ((char-equal special ?\C-m) ;; Line Break
	   (insert "<BR>"))
	  ((char-equal special ?\C-h) ;; Help
	   (with-output-to-temp-buffer "*HTML32-Mode-Specials*"
	     (princ
"        
          Special Characters to get with &-Prefix

          C-? (Backspace) inserts nothing.
          
          a   Character: a umlaut                       HTML: &auml;
	  A   Character: A umlaut                       HTML: &Auml;
	  o   Character: o umlaut                       HTML: &ouml;
	  O   Character: O umlaut                       HTML: &Ouml;
	  e   Character: e dieresis                     HTML: &euml;
	  E   Character: E dieresis                     HTML: &Euml;
	  i   Character: i dieresis                     HTML: &iuml;
	  I   Character: I dieresis                     HTML: &Iuml;
	  u   Character: u umlaut                       HTML: &uuml;
	  U   Character: U umlaut                       HTML: &Uuml;
	  s   Character: sharp s (German)               HTML: &szlig;
	  &   Character: ampersand                      HTML: &amp;
	  l   Character: lesser than                    HTML: &lt;
	  g   Character: greater than                   HTML: &gt;
	  <   Character: lesser than                    HTML: &lt;
	  >   Character: greater than                   HTML: &gt;
	  !   Character: inverted exclamation mark      HTML: &iexcl;
	  ?   Character: inverted question mark         HTML: &iquest;
	  $   Character: cent sign                      HTML: &cent;
	  L   Character: pound sign                     HTML: &pound;
	  P   Character: section sign                   HTML: &sect;
	  S   Character: section sign                   HTML: &sect;
	  W   Character: general currency sign          HTML: &curren;
	  \"  Character: quotation mark                 HTML: &quot;
	  Y   Character: yen sign                       HTML: &yen;
	  |   Character: broken vertical bar            HTML: &brvbar;
	  C   Character: copyright sign                 HTML: &copy;
	  c   Character: copyright sign                 HTML: &copy;
	  R   Character: registered sign                HTML: &reg;
	  r   Character: registered sign                HTML: &reg;
	  T   Character: trademark sign (upper case)    HTML: <SUP>TM</SUP>
	  t   Character: trademark sign (lower case)    HTML: <SUP>tm</SUP>
	  ;   Character: angle quotation mark, left     HTML: &laquo;
	  :   Character: angle quotation mark, right    HTML: &raquo;
	  =   Character: soft hyphen                    HTML: &shy;
	  _   Character: macron                         HTML: &macr;
	  *   Character: degree sign                    HTML: &deg;
	  ~   Character: degree sign                    HTML: &deg;
	  0   Character: degree sign                    HTML: &deg;
	  +   Character: plus-or-minus sign             HTML: &plusmn;
	  1   Character: superscript one                HTML: &sup1;
	  2   Character: superscript two                HTML: &sup2;
	  3   Character: superscript three              HTML: &sup3;
	  m   Character: micro sign                     HTML: &micro;
	  .   Character: middle dot                     HTML: &middot;
	  ,   Character: cedilla                        HTML: &cedil; 
	  x   Character: multiply sign                  HTML: &times;
	  X   Character: multiply sign                  HTML: &times;
	  %   Character: divide sign                    HTML: &divide;
	  /   Character: divide sign                    HTML: &divide;
	  b   Character: small thorn (Islandic)         HTML: &thorn;
	  B   Character: capital THORN (Islandic)       HTML: &THORN;
	  d   Character: small eth (Islandic)           HTML: &eth;
	  D   Character: capital ETH (Islandic)         HTML: &ETH;
	  n   Character: n tilde                        HTML: &ntilde;
	  N   Character: N tilde                        HTML: &Ntilde;
	  y   Character: y umlaut                       HTML: &yuml;
	  -   Character: not sign                       HTML: &not;
	  M   Character: ordinal indicator, masculine   HTML: &ordm;
	  F   Character: ordinal indicator, feminine    HTML: &ordf;
	  ^   Character: umlaut dots                    HTML: &uml;
	  '   Character: acute accent                   HTML: &acute;
	      Character: non-breaking space             HTML: &nbsp;
	  ^M  Character: forced line break              HTML: <BR>
         " )))
	  (t (insert "&" special)))))
  
(defun html32-accent-aigu ( special )
  "Von Axel eingefuehrt am 5. Juni 1997."
  (interactive "cWhat special char? (C-h for help) ")
  (let ((case-fold-search nil)) ;; Casesensitive
    (cond ((char-equal special ?") ;; ' itself
	   (insert "'"))
	  ((char-equal special ?') ;; ' HTML
	   (insert "&acute;"))
	  ((char-equal special ?a) ;; 'a
	   (insert "&aacute;"))
	  ((char-equal special ?A) ;; 'A
	   (insert "&Aacute;"))
	  ((char-equal special ?e) ;; 'e
	   (insert "&eacute;"))
	  ((char-equal special ?E) ;; 'E
	   (insert "&Eacute;"))
	  ((char-equal special ?i) ;; 'i
	   (insert "&iacute;"))
	  ((char-equal special ?I) ;; 'I
	   (insert "&Iacute;"))
	  ((char-equal special ?o) ;; 'o
	   (insert "&oacute;"))
	  ((char-equal special ?O) ;; 'O
	   (insert "&Oacute;"))
	  ((char-equal special ?u) ;; 'u
	   (insert "&uacute;"))
	  ((char-equal special ?U) ;; 'U
	   (insert "&Uacute;"))
	  ((char-equal special ?y) ;; 'y
	   (insert "&yacute;"))
	  ((char-equal special ?Y) ;; 'Y
	   (insert "&Yacute;"))
	  ((char-equal special ?c) ;; ,c
	   (insert "&ccedil;"))
	  ((char-equal special ?C) ;; ,C
	   (insert "&Ccedil;"))
	  ((char-equal special ?d) ;; eth
	   (insert "&eth;"))
	  ((char-equal special ?D) ;; ETH
	   (insert "&ETH;"))
	  ((char-equal special ?4) ;; 1/4
	   (insert "&frac14;"))
	  ((char-equal special ?3) ;; 3/4
	   (insert "&frac34;"))
	  ((char-equal special ?2) ;; 1/2
	   (insert "&frac12;"))
	  ((char-equal special ?0) ;; /o
+	   (insert "&oslash;"))
	  ((char-equal special ?-) ;; Not
	   (insert "&not;"))
	  ((char-equal special ?\C-?) ;; Delete / Undo "&"
	   (insert ""))
	  ((char-equal special ?\C-h) ;; Help
	   (with-output-to-temp-buffer "*HTML32-Mode-Specials*"
	     (princ 
"        
          Special Characters to get with '-Prefix
          
          C-? (Backspace) inserts nothing.
          
              Character: '                              HTML: &acute;
          a   Character: 'a                             HTML: &aacute;
          A   Character: 'A                             HTML: &Aacute;
	  e   Character: 'e                             HTML: &eacute;
	  E   Character: 'E                             HTML: &Eacute;
	  i   Character: 'i                             HTML: &iacute;
	  I   Character: 'I                             HTML: &Iacute;
	  o   Character: 'o                             HTML: &oacute;
	  O   Character: 'O                             HTML: &Oacute;
	  u   Character: 'u                             HTML: &uacute;
	  U   Character: 'U                             HTML: &Uacute;
	  y   Character: 'y                             HTML: &yacute;
	  Y   Character: 'Y                             HTML: &Yacute;
	  c   Character: ,c                             HTML: &ccedil;
	  C   Character: ,C                             HTML: &Ccedil;
	  d   Character: eth                            HTML: &eth;
	  D   Character: ETH                            HTML: &ETH;
	  4   Character: 1/4                            HTML: &frac14;
	  3   Character: 3/4                            HTML: &frac34;
	  2   Character: 1/2                            HTML: &frac12;
	  0   Character: /o                             HTML: &oslash;
	  -   Character: Not                            HTML: &not;
	  '   Character: '                              ' itself
         " )))
	  (t (insert "'" special)))))

(defun html32-accent-grave ( special )
  "Von Axel eingefuehrt am 5. Juni 1997."
  (interactive "cWhat special char? (C-h for help) ")
  (let ((case-fold-search nil)) ;; Casesensitive
    (cond ((char-equal special ?`) ;; `
	   (insert "`"))
	  ((char-equal special ?a) ;; `a
	   (insert "&agrave;"))
	  ((char-equal special ?A) ;; `A
	   (insert "&Agrave;"))
	  ((char-equal special ?e) ;; `e
	   (insert "&egrave;"))
	  ((char-equal special ?E) ;; `E
	   (insert "&Egrave;"))
	  ((char-equal special ?i) ;; `i
	   (insert "&igrave;"))
	  ((char-equal special ?I) ;; `I
	   (insert "&Igrave;"))
	  ((char-equal special ?o) ;; `o
	   (insert "&ograve;"))
	  ((char-equal special ?O) ;; `O
	   (insert "&Ograve;"))
	  ((char-equal special ?u) ;; `u
	   (insert "&ugrave;"))
	  ((char-equal special ?U) ;; `U
	   (insert "&Ugrave;"))
	  ((char-equal special ?d) ;; eth
	   (insert "&eth;"))
	  ((char-equal special ?D) ;; ETH
	   (insert "&ETH;"))
	  ((char-equal special ?4) ;; 1/4
	   (insert "&frac14;"))
	  ((char-equal special ?3) ;; 3/4
	   (insert "&frac34;"))
	  ((char-equal special ?2) ;; 1/2
	   (insert "&frac12;"))
	  ((char-equal special ?0) ;; /O
	   (insert "&Oslash;"))
	  ((char-equal special ?-) ;; Not
	   (insert "&not;"))
	  ((char-equal special ?\C-?) ;; Delete / Undo "&"
	   (insert ""))
	  ((char-equal special ?\C-h) ;; Help
	   (with-output-to-temp-buffer "*HTML32-Mode-Specials*"
	     (princ 
"        
          Special Characters to get with `-Prefix
          
          C-? (Backspace) inserts nothing.
          
          a   Character: `a                             HTML: &agrave;
	  A   Character: `A                             HTML: &Agrave;
	  e   Character: `e                             HTML: &egrave;
	  E   Character: `E                             HTML: &Egrave;
	  i   Character: `i                             HTML: &igrave;
	  I   Character: `I                             HTML: &Igrave;
	  o   Character: `o                             HTML: &ograve;
	  O   Character: `O                             HTML: &Ograve;
	  u   Character: `u                             HTML: &ugrave;
	  U   Character: `U                             HTML: &Ugrave;
	  d   Character: eth                            HTML: &eth;
	  D   Character: ETH                            HTML: &ETH;
	  4   Character: 1/4                            HTML: &frac14;
	  3   Character: 3/4                            HTML: &frac34;
	  2   Character: 1/2                            HTML: &frac12;
	  0   Character: /O                             HTML: &Oslash;
	  -   Character: Not                            HTML: &not;
	  `   Character: `                              ` itself
         " )))
	  (t (insert "`" special)))))

(defun html32-accent-circumflex ( special )
  "Von Axel eingefuehrt am 5. Juni 1997."
  (interactive "cWhat special char? (C-h for help) ")
  (let ((case-fold-search nil)) ;; Casesensitive
    (cond ((char-equal special ?^) ;; ^
	   (insert "^"))
	  ((char-equal special ?a) ;; ^a
	   (insert "&acirc;"))
	  ((char-equal special ?A) ;; ^A
	   (insert "&Acirc;"))
	  ((char-equal special ?e) ;; ^e
	   (insert "&ecirc;"))
	  ((char-equal special ?E) ;; ^E
	   (insert "&Ecirc;"))
	  ((char-equal special ?i) ;; ^i
	   (insert "&icirc;"))
	  ((char-equal special ?I) ;; ^I
	   (insert "&Icirc;"))
	  ((char-equal special ?o) ;; ^o
	   (insert "&ocirc;"))
	  ((char-equal special ?O) ;; ^O
	   (insert "&Ocirc;"))
	  ((char-equal special ?u) ;; ^u
	   (insert "&ucirc;"))
	  ((char-equal special ?U) ;; ^U
	   (insert "&Ucirc;"))
	  ((char-equal special ?d) ;; eth
	   (insert "&eth;"))
	  ((char-equal special ?D) ;; ETH
	   (insert "&ETH;"))
	  ((char-equal special ?4) ;; 1/4
	   (insert "&frac14;"))
	  ((char-equal special ?3) ;; 3/4
	   (insert "&frac34;"))
	  ((char-equal special ?2) ;; 1/2
	   (insert "&frac12;"))
	  ((char-equal special ?0) ;; /O
	   (insert "&Oslash;"))
	  ((char-equal special ?-) ;; Not
	   (insert "&not;"))
	  ((char-equal special ?\C-?) ;; Delete / Undo "&"
	   (insert ""))
	  ((char-equal special ?\C-h) ;; Help
	   (with-output-to-temp-buffer "*HTML32-Mode-Specials*"
	     (princ 
"        
          Special Characters to get with ^-Prefix
          
          C-? (Backspace) inserts nothing.
          
          a   Character: ^a                             HTML: &acirc;
	  A   Character: ^A                             HTML: &Acirc;
	  e   Character: ^e                             HTML: &ecirc;
	  E   Character: ^E                             HTML: &Ecirc;
	  i   Character: ^i                             HTML: &icirc;
	  I   Character: ^I                             HTML: &Icirc;
	  o   Character: ^o                             HTML: &ocirc;
	  O   Character: ^O                             HTML: &Ocirc;
	  u   Character: ^u                             HTML: &ucirc;
	  U   Character: ^U                             HTML: &Ucirc;
	  d   Character: eth                            HTML: &eth;
	  D   Character: ETH                            HTML: &ETH;
	  4   Character: 1/4                            HTML: &frac14;
	  3   Character: 3/4                            HTML: &frac34;
	  2   Character: 1/2                            HTML: &frac12;
	  0   Character: /O                             HTML: &Oslash;
	  -   Character: Not                            HTML: &not;
	  ^   Character: ^                              ^ itself
         " )))
	  (t (insert "^" special)))))

(defun html32-tex-kbd-macro ( special )
  "Converts in TeX-style typed umlauts on the fly in HTML entities."
  (interactive "cWhat special char? (C-h for help) ")
  (let ((case-fold-search nil)) ;; Casesensitive
    (cond ((char-equal special ?a) ;; "a
	   (insert "&auml;"))
	  ((char-equal special ?A) ;; "A
	   (insert "&Auml;"))
	  ((char-equal special ?u) ;; "u
	   (insert "&uuml;"))
	  ((char-equal special ?U) ;; "U
	   (insert "&Uuml;"))
	  ((char-equal special ?o) ;; "o
	   (insert "&ouml;"))
	  ((char-equal special ?O) ;; "O
	   (insert "&Ouml;"))
	  ((char-equal special ?e) ;; "e
	   (insert "&euml;"))
	  ((char-equal special ?E) ;; "E
	   (insert "&Euml;"))
	  ((char-equal special ?i) ;; "i
	   (insert "&iuml;"))
	  ((char-equal special ?I) ;; "I
	   (insert "&Iuml;"))
	  ((char-equal special ?y) ;; "y
	   (insert "&yuml;"))
	  ((char-equal special ?s) ;; "s
	   (insert "&szlig;"))
	  ((char-equal special ?\C-?) ;; Delete / Undo "&"
	   (insert ""))
	  ((char-equal special ?\C-h) ;; Help
	   (with-output-to-temp-buffer "*HTML32-Mode-Specials*"
	     (princ
"        
          Special Characters to get with \"-Prefix

          This prefix is a special prefix for TeX users to type
          umlauts in TeX style. It is activated if the variable
          html32-using-tex-kbd-macro is non-nil.
          
          C-? (Backspace) inserts nothing.
          
          a   Character: \\\"a                            HTML: &auml;
	  A   Character: \\\"A                            HTML: &Auml;
	  e   Character: \\\"e                            HTML: &euml;
	  E   Character: \\\"E                            HTML: &Euml;
	  i   Character: \\\"i                            HTML: &iuml;
	  I   Character: \\\"I                            HTML: &Iuml;
	  o   Character: \\\"o                            HTML: &ouml;
	  O   Character: \\\"O                            HTML: &Ouml;
	  u   Character: \\\"u                            HTML: &uuml;
	  U   Character: \\\"U                            HTML: &Uuml;
	  y   Character: \\\"y                            HTML: &yuml;
	  s   Character: \\ss                            HTML: &szlig;
" )
))
	  (t (insert "\"" special)))))

(defun html32-tilde ( special )
  "Von Axel eingefuehrt am 5. Juni 1997."
  (interactive "cWhat special char? (C-h for help) ")
  (let ((case-fold-search nil)) ;; Casesensitive
    (cond ((char-equal special ?a) ;; ~a
	   (insert "&atilde;"))
	  ((char-equal special ?A) ;; ~A
	   (insert "&Atilde;"))
	  ((char-equal special ?e) ;; ae Ligatur
	   (insert "&aelig;"))
	  ((char-equal special ?E) ;; AE Ligatur
	   (insert "&AElig;"))
	  ((char-equal special ?o) ;; ~o
	   (insert "&otilde;"))
	  ((char-equal special ?O) ;; ~O
	   (insert "&Otilde;"))
	  ((char-equal special ?n) ;; ~n
	   (insert "&ntilde;"))
	  ((char-equal special ?N) ;; ~N
	   (insert "&Ntilde;"))
	  ((char-equal special ?\C-?) ;; Delete / Undo "&"
	   (insert ""))
	  ((char-equal special ?\C-h) ;; non-breaking space
	   (with-output-to-temp-buffer "*HTML32-Mode-Specials*"
	     (princ 
"        
          Special Characters to get with C-c~-Prefix
          
          C-? (Backspace) inserts nothing.
          
          a   Character: ~a                             HTML: &atilde;
	  A   Character: ~A                             HTML: &Atilde;
	  e   Character: ae Ligatur                     HTML: &aelig;
	  E   Character: AE Ligatur                     HTML: &AElig;
	  o   Character: ~o                             HTML: &otilde;
	  O   Character: ~O                             HTML: &Otilde;
	  n   Character: ~n                             HTML: &ntilde;
	  N   Character: ~N                             HTML: &Ntilde;
         " )))
	  (t (insert "~" special)))))

(defun html32-ring ( special )
  "Von Axel eingefuehrt am 5. Juni 1997."
  (interactive "cWhat special char? (C-h for help) ")
  (let ((case-fold-search nil)) ;; Casesensitive
    (cond ((char-equal special ?a) ;; oa
	   (insert "&aring;"))
	  ((char-equal special ?A) ;; oA
	   (insert "&Aring;"))
	  ((char-equal special ?e) ;; ae Ligatur
	   (insert "&aelig;"))
	  ((char-equal special ?E) ;; AE Ligatur
	   (insert "&AElig;"))
	  ((char-equal special ?c) ;; (c)
	   (insert "&copy;"))
	  ((char-equal special ?C) ;; (c)
	   (insert "&copy;"))
	  ((char-equal special ?r) ;; (r)
	   (insert "&reg;"))
	  ((char-equal special ?R) ;; (r)
	   (insert "&reg;"))
	  ((char-equal special ?\C-?) ;; Delete / Undo "&"
	   (insert ""))
	  ((char-equal special ?\C-h) ;; non-breaking space
	   (with-output-to-temp-buffer "*HTML32-Mode-Specials*"
	     (princ 
"        
          Special Characters to get with C-co-Prefix
          (similiar to C-cO-Prefix and C-c0-Prefix)
          
          C-? (Backspace) inserts nothing.
          
          a   Character: oa                             HTML: &aring;
	  A   Character: oA                             HTML: &Aring;
	  e   Character: ae Ligatur                     HTML: &aelig;
	  E   Character: AE Ligatur                     HTML: &AElig;
	  c   Character: (c)                            HTML: &copy;
	  C   Character: (c)                            HTML: &copy;
	  r   Character: (r)                            HTML: &reg;
	  R   Character: (r)                            HTML: &reg;
         " )))
	  (t (insert "&deg;" special)))))

(defun html32-real-ampersand ()
  (interactive)
  (insert "&"))

(defun html32-real-accent-aigu ()
  (interactive)
  (insert "'"))

(defun html32-real-accent-grave ()
  (interactive)
  (insert "`"))

(defun html32-real-accent-circumflex ()
  (interactive)
  (insert "^"))

(defun html32-add-line-break ()
  (interactive)
  (insert "<BR>\n"))

;;; --------------------------------------------------------------------------
;;; ---------------------------- Lynx previewing -----------------------------
;;; --------------------------------------------------------------------------

(defun html32-write-buffer-to-tmp-file ()
  "Write the current buffer to a temp file and return the name
of the tmp file."
  (let ((filename (concat "/tmp/" (make-temp-name "html32-mode") ".html")))
    (write-region (point-min) (point-max) filename nil 'foo)
    filename))

(defun html32-preview-document ()
  "Direct lynx output of the actual HTML file to a buffer."
  (interactive)
  (let ((tmp-file (html32-write-buffer-to-tmp-file)))
    (if html32-lynx-args
	(shell-command (concat html32-location-lynx " " 
			       html32-lynx-args " " 
			       tmp-file)
		       "*html32-previewing*")
      (shell-command (concat html32-location-lynx " " 
			     html32-lynx-args " " 
			     tmp-file)
		     "*html32-previewing*"))))

;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------

;;; ----------------------- html32-reorder-numeric-names -----------------------

(defun html32-replace-string-in-buffer (start end newstring)
  (save-excursion
    (goto-char start)
    (delete-char (1+ (- end start)))
    (insert newstring)))

(defun html32-reorder-numeric-names (&optional reorder-non-numeric)
  "Reorder the NAME fields for links in the current buffer.  The
new ordering starts at 1 and increases monotonically through the buffer.
If optional arg REORDER-NON-NUMERIC is non-nil, then non-numeric NAME's
will also be numbered, else they won't.

Beware that doing this will possibly mess up references to specific
links within this document (e.g., HREF=\"#12\") or by other documents.
This command is mainly intended for use during the initial creation
stage of a document, especially when this creation involves cutting
and pasting from other documents (which it shouldn't, since this is
hypertext :-)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq html32-link-counter 0)
    (while (re-search-forward "<A[ \t\n]+NAME=" (point-max) t)
      (let* ((start (match-end 0))
             (end (save-excursion
                    (re-search-forward "[ \t\n>]" 
                                       (point-max) 
                                       t)
                    (match-beginning 0)))
             (subst (buffer-substring start end)))
        (and subst
             ;; Proceed only if we reorder non-numeric links or
             ;; this is in fact numeric (i.e. > 0).
             (or reorder-non-numeric (> (string-to-int subst) 0))
             (progn
               (setq html32-link-counter (1+ html32-link-counter))
               (html32-replace-string-in-buffer start (1- end)
                (format "%d" html32-link-counter))))))))

;;; ----------------------------- html32-quotify ------------------------------

(defun html32-quotify-hrefs ()
  "Insert quotes around all HREF, USEMAP and SRC attribute value literals.

This remedies the problem with old HTML files that can't be processed
by SGML parsers. That is, changes <A\ HREF=foo> to <A\ HREF=\"foo\">. To
work correctly on the SRC's, it is necessary, that all ALT texts are
quotified. Use html32-quotify-alts for guaranting that. (Works only on
the first appearance of HREF resp. USEMAP and SRC inside one A
resp. IMG tag.)

Known bug: The function does not work, if any non-standard parameters
are used in an tag before the HREF, USEMAP or SRC parameter. Bug for
JavaScript handlers (onMouseover, etc.) fixed."
; The "\\ " is necessary, because it would be replaced by "\"\\\"",
; when you byte-compile and load this file. :-/
  (interactive)
  (save-excursion
    (let ((count 0))
; HREF
      (goto-char (point-min))
      (while 
	  (re-search-forward
	   "<[aA][ \t\n]+\\([oO][nN][a-zA-Z]+=\".*\"[ \t\n]+\\|\\([nN][aA][mM][eE]\\|[tT][aA][rR][gG][eE][tT]\\)=\"?[-a-zA-Z0-9_]+\"?[ \t\n]+\\|[tT][iI][tT][lL][eE]=\"?[^ \t\n]+\"?[ \t\n]+\\|[rR][eE][lLvV]=\"?[a-zA-Z]\"?\\)*[hH][rR][eE][fF]="
	   (point-max)
	   t)
	(cond
	 ((null (looking-at "\""))
	  (insert "\"")
	  (re-search-forward "[ \t\n>]" (point-max) t)
	  (forward-char -1)
	  (insert "\"")
	  (setq count (1+ count)))))
; SRC
      (goto-char (point-min))
      (while 
	  (re-search-forward
	   "<[iI][mM][gG][ \t\n]+\\([aA][lL][tT]=\".*\"[ \t\n]+\\|\\([wW][iI][dD][tT][hH]\\|[hH][eE][iI][gG][hH][tT]\\)=\"?[0-9]+%?\"?[ \t\n]+\\|\\([bB][oO][rR][dD][eE][rR]\\|[hHvV][sS][pP][aA][cC][eE]\\)=\"?[0-9]+\"?[ \t\n]+\\|[iI][sS][mM][aA][pP][ \t\n]+\\|[uU][sS][eE][mM][aA][pP]=\"?[-?a-zA-Z0-9_:/~%#]+\"?[ \t\n]+\\|[aA][lL][iI][gG][nN]=\"?[-_a-zA-Z]\"?[ \t\n]+\\)*[sS][rR][cC]="
	   (point-max)
	   t)
	(cond
	 ((null (looking-at "\""))
	  (insert "\"")
	  (re-search-forward "[ \t\n>]" (point-max) t)
	  (forward-char -1)
	  (insert "\"")
	  (setq count (1+ count)))))
; USEMAP
    (goto-char (point-min))
    (while 
        (re-search-forward
         "<[iI][mM][gG][ \t\n]+\\([aA][lL][tT]=\".*\"[ \t\n]+\\|\\([wW][iI][dD][tT][hH]\\|[hH][eE][iI][gG][hH][tT]\\)=\"?[0-9]+%?\"?[ \t\n]+\\|\\([bB][oO][rR][dD][eE][rR]\\|[hHvV][sS][pP][aA][cC][eE]\\)=\"?[0-9]+\"?[ \t\n]+\\|[iI][sS][mM][aA][pP][ \t\n]+\\|[sS][rR][cC]=\"?[-?a-zA-Z0-9_:/~%#]+\"?[ \t\n]+\\|[aA][lL][iI][gG][nN]=\"?[-_a-zA-Z]\"?[ \t\n]+\\)*[uU][sS][eE][mM][aA][pP]="
         (point-max)
         t)
      (cond
       ((null (looking-at "\""))
        (insert "\"")
        (re-search-forward "[ \t\n>]" (point-max) t)
        (forward-char -1)
        (insert "\"")
	(setq count (1+ count)))))
    (message (concat "Added " count " quotation mark pairs.")))))

(defun html32-quotify-alts ()
  "Insert quotes around all ALT attribute value literals."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while 
	  (re-search-forward
	   "<[iI][mM][gG][ \t\n]+\\([sS][rR][cC]=\"?[-a-zA-Z0-9_:/~%#]+\"?[ \t\n]+\\|\\([wW][iI][dD][tT][hH]\\|[hH][eE][iI][gG][hH][tT]\\)=\"?[0-9]+%?\"?[ \t\n]+\\|\\([bB][oO][rR][dD][eE][rR]\\|[hHvV][sS][pP][aA][cC][eE]\\)=\"?[0-9]+\"?[ \t\n]+\\|[iI][sS][mM][aA][pP][ \t\n]+\\|[uU][sS][eE][mM][aA][pP]=\"?[-a-zA-Z0-9_:/~%#]+\"?[ \t\n]+\\|[aA][lL][iI][gG][nN]=\"?[-_a-zA-Z]\"?[ \t\n]+\\)*[aA][lL][tT]=" ; (quite nice REGEXP, huh? ;-)
	   (point-max)
	   t)
	(cond
	 ((null (looking-at "\""))
	  (insert "\"")
	  (re-search-forward "[ >\t\n]" (point-max) t) ; 
	  (forward-char -1)
	  (insert "\"")
	  (setq count (1+ count)))))
	(message (concat "Added " count " quotation mark pairs.")))))

(defun html32-quotify-all ()
  "Insert quotes around all parameter attribute value literals."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while 
	  (re-search-forward
	   "<[a-zA-Z0-9]+[ \t\n]+\\([a-zA-Z]+\\(=\".*\"\\)?[ \t\n]+\\)*[a-zA-Z]+=[^\"]"
	   (point-max)
	   t)
	(forward-char -1)
	(insert "\"")
	(re-search-forward "[ >\t\n]" (point-max) t)
	(forward-char -1)
	(insert "\"")
	(re-search-backward "<[a-zA-Z]" (point-min) t)
	(forward-char -3)
	(setq count (1+ count)))
      (message (concat "Added " count " quotation mark pairs.")))))

;;; --------------------------- html32-comment-region -------------------------

(defun html32-inside-commentp (&optional beg end)
  "Tests, if point (or region is inside a HTML comment. (Equal if
inside are other comment tags.)"
  (interactive)
  (cond ((and beg end))                 ; If both parameter given, use
					; them, means here: do nothing. 

	((and beg (null end))           ; If only one parameter is 
	 (setq end beg))                ; set the other to the same
					; value.

	(mark-active                    ; If a region is still marked
	 (setq beg (region-beginning)   ; use it's beginning and end.
	       end (1- (region-end))))

        (t                              ; If no parameter is given,
	 (setq beg (point)		; set both variables to 
	       end (point))))           ; (point).

  (cond ((save-excursion
	   (goto-char (+ 5 beg))
	   (null (re-search-backward "<!--[ \t\n]" (point-min) t)))
	 nil)                           ; False, because no open tag
					; can be found before beginning
	((save-excursion
	   (goto-char (- end 3))
	   (null (re-search-forward "[ \t\n]-->" (point-max) t)))
	 nil)                           ; False, because no closing
					; tag can be found after end.
	((save-excursion
	   (goto-char (+ 5 beg))
	   (re-search-backward "<!--[ \t\n]" (point-min) t)
	   (re-search-forward "[ \t\n]-->" beg t))
	 nil)                           ; False, because opening tag
					; found before is also closed
					; before.
	((save-excursion
	   (goto-char (- end 3))
	   (re-search-forward "[ \t\n]-->" (point-max) t)
	   (re-search-backward "<!--[ \t\n]" (+ end 1) t))
	 nil)                           ; False, because next closing
					; tag is opened after, too.

	(t t)))                         ; Else: Must be inside - say -
					; true. ;->
	     
(defun html32-comment-region (&optional beg end)
  "Comment out a region or delete comment parenthesis of comment,
point is in. If the mark is active, all comment parenthesises 
inside (or at begin or end of) the region - equal if opening or
closing part - will be removed.

In comparison to comment-region - which is also useable in html32 mode
- it only adds comment parenthesis at the begin and and of the region
and not at beginning and end of every line inside the region."

  (interactive)
  (cond ((and beg end))                 ; If both parameter given, use
					; them, means here: do nothing. 

	((and beg (null end))           ; If only one parameter is 
	 (setq end beg))                ; set the other to the same
					; value.

	(mark-active                    ; If a region is still marked
	 (setq beg (region-beginning)   ; use it's beginning and end.
	       end (1- (region-end))))

        (t                              ; If no parameter is given,
	 (setq beg (point)		; set both variables to 
	       end (point))))           ; (point).

  (let ((inside (html32-inside-commentp beg end)) 
	(point (= beg end)))            ; Are beg and end equal?
					; (means: Are they
					; representing a single point?)

    (cond ((and point (not inside))     ; If no region is given and
	   (message "No region active... Adding comment tag at point.")
	   (goto-char beg)              ; point isn't inside a comment. 
	   (insert "<!-- ")
	   (setq beg (point))
	   (insert " -->")
	   (push-mark)
	   (goto-char beg))

	  ((not inside)                 ; If a region outside a
	   (message "Setting region as comment...")
	     (goto-char (1+ end))	; comment is marked, add
	     (insert " -->")	        ; comment parenthesis around 
	     (goto-char beg)            ; it.
	     (insert "<!-- "))

	  (point
	   (message "Uncommenting actual comment...")
	   (forward-char 5)
	   (re-search-backward "<!--[ \t\n]" (point-min) t)
	   (forward-char 4)
	   (search-backward "<!--" (point-min) t)
	   (replace-match "")
	   (forward-char -1)
	   (re-search-forward "[ \t\n]-->" (point-max) t)
	   (forward-char -4)
	   (search-forward "-->" (point-max) t)
	   (replace-match ""))

	  (t                            ; Else...
	   (message "Uncommenting all comments inside region...")
	   (save-excursion
	     (goto-char (- end 4))
	     (re-search-forward "[ \t\n]-->" (point-max) t)
	     (forward-char -4) 
	     (re-search-forward "-->" (point-max) t)
	     (replace-match "")
	     (while (re-search-backward "<!--[ \t\n]\\|[ \t\n]-->" 
					(1+ beg) t)
	       (forward-char 4)
	       (re-search-backward "<!--\\|-->" (1+ beg) )
	       (replace-match ""))
	     (re-search-backward "<!--[ \t\n]" (point-min) )
	     (forward-char 4)
	     (search-backward "<!--" (point-min) )
	     (replace-match ""))))))

;;; ------------------------------- html32-mode -------------------------------

(defun html32-mode ()
  "Major mode for editing HTML 2.0 and 3.2 documents. Special
commands:

\\{html32-mode-map}
Turning on html32-mode calls the value of the variable html32-mode-hook,
if that value is non-nil.

More extensive documentation is available in the file 'html32-mode.el'.
The latest (possibly unstable) version of this file will always be available
on anonymous FTP server fsinfo.cs.uni-sb.de in /pub/abe/emacs/."
  (interactive)
  (kill-all-local-variables)
  (use-local-map html32-mode-map)
  (setq mode-name "HTML 3.2")
  (setq major-mode 'html32-mode)
  (setq local-abbrev-table html32-mode-abbrev-table)
  (set-syntax-table html32-mode-syntax-table)
  (transient-mark-mode 1)
  (make-local-variable 'mark-even-if-inactive)
  (setq mark-even-if-inactive ())
  (html32-fontify)
  (run-hooks 'html32-mode-hook)
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-start-skip)

;  (setq comment-start-skip "<!--+[ \t\n]*") ;;; Test!
;  (make-local-variable 'comment-column)
;  (setq comment-column 32) ;;; Test!  
;  (make-local-variable 'comment-multi-line)
;  (setq comment-multi-line t) ;;; Test
  )

;;; ------------------------------- Our hooks --------------------------------

(defun html32-html32-mode-hook ()
  "Hook called from html32-mode-hook.  Set html32-link-counter to 
the highest link value in the document (the next link created will
be one greater than that) to insure unique (numeric) link ID's.
Also run htlm-quotify-hrefs if html32-quotify-hrefs-on-find is non-nil."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<A[ \t\n]+NAME=" (point-max) t)
      (let* ((start (match-end 0))
             (end (save-excursion
                    (re-search-forward "[ \t\n>]"
                                       (point-max)
                                       t)
                    (match-beginning 0)))
             (subst (buffer-substring start end)))
        (and subst
             ;; Safe to do compare, since string-to-int passed a non-number
             ;; returns 0.
             (> (string-to-int subst) html32-link-counter)
             (setq html32-link-counter (string-to-int subst))))))
  ;; Quotify existing HREF's, USEMAP's and SRC's if
  ;; html32-quotify-hrefs-on-find is non-nil. 
  (cond (html32-quotify-on-find  
	 (html32-quotify-all))
	(html32-quotify-hrefs-on-find 
	 (html32-quotify-alts)
	 (html32-quotify-hrefs))
	(html32-quotify-alts-on-find
	 (html32-quotify-alts))))

;;; ------------------------------- hook setup -------------------------------

;; Author: Daniel LaLiberte (liberte@cs.uiuc.edu).
(defun html32-postpend-unique-hook (hook-var hook-function)
  "Postpend HOOK-VAR with HOOK-FUNCTION, if it is not already an element.
hook-var's value may be a single function or a list of functions."
  (if (boundp hook-var)
      (let ((value (symbol-value hook-var)))
        (if (and (listp value) (not (eq (car value) 'lambda)))
            (and (not (memq hook-function value))
                 (set hook-var (append value (list hook-function))))
          (and (not (eq hook-function value))
               (set hook-var (append value (list hook-function))))))
    (set hook-var (list hook-function))))

(html32-postpend-unique-hook 'html32-mode-hook 'html32-html32-mode-hook)
(html32-postpend-unique-hook 'html32-mode-hook 'html32-fontify)

;;; ------------------------------ final setup -------------------------------

(or (assoc "\\.html$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.html$" . html32-mode) auto-mode-alist)))
(or (assoc "\\.htm$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.htm$" . html32-mode) auto-mode-alist)))
(or (assoc "\\.shtml$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.shtml$" . html32-mode) auto-mode-alist)))

(provide 'html32-mode)

;;; html32-mode.el ends here.
