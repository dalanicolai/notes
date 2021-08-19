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
{% endhighlight %}

{% highlight emacs-lisp %}
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


### Create a help-echo over an image (part)

{% highlight emacs-lisp %}
(create-image "~/testt.png" nil nil
              :map '(((rect . ((0 . 0) . (200 . 200)))
                      hello
                      (pointer arrow help-echo "hello"))))
{% endhighlight %}


## Keyboard macro's


### Reformat to columns

There was the following question on Gitter:

> Siva @sprasanth Aug 18 19:04 Hi! I am trying to learn how to align code in
> spacemacs. I have similar piece of code everywhere in my file and they are all
> messed up. How can I align them ? I would like to have State name start from
> column 6, City name start from Column 8 and Restuarant name start from
> Column 10.
> 
> California begin  
> SanJose  
> SanFransisco  
> SantaCruz  
> Restaurants begin  
> Subway  
> Starbucks  
> end  
> end

As a solution I suggested to use a keyboard macro. The following macro will
work fine after you first 'create enough space' for the rectangular blocks
(using `(string-replace "begin" " begin")`:

{% highlight emacs-lisp %}
(fset 'to-columns
   (kmacro-lambda-form [?m ?m ?/ ?b ?e ?g ?i ?n return
                           ?d ?$ ?j ?0 ?\C-v ?n ?h ?d ?k ?A S-insert escape
                           ?n ?d ?d ?\C-v ?/ ?e ?n ?d return
                           ?k ?$ ?d ?\' ?m ?A ?
                           ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?
                           ?  S-insert escape ?n ?d ?d ?d ?d] 0 "%d"))
{% endhighlight %}

For it to work on a block of any form, I probably had to insert some spaces
after *Starbucks* before selecting and deleting the rectangular block.
