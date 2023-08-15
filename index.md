---
layout: page
title: Notes
menubar_toc: true
toc_title: Table of contents
---


# Dala's playground

[Fixing org-mode coding assistance](pages/fixing-org-mode-coding-assistance.md)

[Spacemacs notes (recipes)](pages/spacemacs-notes.md)

[Optics notes](pages/optics-notes.md)

[bibtex](pages/bibtex.md)

[Magit quick start](pages/magit-quick-start.md)


# This blog

This blog used the [mkdocs-jekyll](https://github.com/vsoch/mkdocs-jekyll) (remote) theme.

In order to use it while writing my blogs in `org-mode`, I'm using the
[ox-jekyll-md](https://github.com/gonsie/ox-jekyll-md) package. I am adding the 'yaml frontmatter manually to my org
files' (see those files), and I have a `.dir-locals` file in the root folder of
the repo containing the line

{% highlight emacs-lisp %}
((org-mode . ((org-jekyll-md-include-yaml-front-matter . nil)
              (org-jekyll-md-use-todays-date . nil)
              (after-save-hook . org-jekyll-md-export-to-md))))
{% endhighlight %}

to make sure that the org-file get exported correctly on save.
