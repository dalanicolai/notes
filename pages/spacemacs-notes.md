---
layout: page
title: Spacemacs notes
menubar_toc: true
toc_title: Table of contents
---


# Spacemacs notes (recipes)


## Images


### Create a XBM image

{% highlight emacs-lisp %}
(save-im (create-image (vconcat
                               (make-vector 50 (make-bool-vector 100 t))
                               (make-vector 50 (make-bool-vector 100 nil)))
                              'xbm t
                              :width 100 :height 100
                              :foreground "blue" :background "orange"
                              :help-echo "this does not work")
                :data)
{% endhighlight %}


### Create a PBM image

{% highlight emacs-lisp %}
(create-image
 (concat "P1 "
         "120 "
         "200 "
         "000010000010000010000010000010000010100010011100000000000000")
 'pbm t)

(image-property
 (create-image
  (concat "P1 " "120 " "200 "
          (mapconcat 'identity
                     (mapcar (lambda (row)
                               (mapconcat 'identity
                                          (make-list 20
                                                     (mapconcat (lambda (c)
                                                                  (make-string 20 c))
                                                                row
                                                                ""))
                                          ""))
                             '("000010" "000010" "000010" "000010" "000010"
                               "000010" "100010" "011100" "000000" "000000"))
                     ""))
  'pbm t)
 :data)

{% endhighlight %}


### Create a help-echo

{% highlight emacs-lisp %}
(create-image "~/testt.png" nil nil
              :map '(((rect . ((0 . 0) . (200 . 200)))
                      hello
                      (pointer arrow help-echo "hello"))))
{% endhighlight %}