---
layout: page
title: Magit quick start
menubar_toc: true
toc_title: Table of contents
---

Hello there, hello. I'm a software tester and for my work I need to run a lot of scripts many times a day.


# Emacs transient

A transient in emacs provides a way to


# A first basic transient

<div class="article" id="org9749169">
<div class="mb" id="org3e92f8a">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #4f97d7;">(</span><span style="color: #4f97d7; font-weight: bold;">transient-define-prefix</span>  <span style="color: #bc6ec5; font-weight: bold;">tutorial-transient</span> <span style="color: #bc6ec5;">()</span>
  <span style="color: #9f8766;">"Some Emacs magic"</span>
  <span style="color: #4f97d7;">:info-manual</span> <span style="color: #2d9574;">"Surf system-test transient"</span>
  <span style="color: #bc6ec5;">[</span><span style="color: #2d9574;">"Not defined yet"</span>
    <span style="color: #2d9574;">(</span><span style="color: #2d9574;">"p"</span> <span style="color: #2d9574;">"print message"</span>      tutorial-print-message<span style="color: #2d9574;">)</span><span style="color: #bc6ec5;">]</span>
  <span style="color: #bc6ec5;">[</span><span style="color: #2d9574;">(</span><span style="color: #2d9574;">"q"</span> <span style="color: #2d9574;">"Quit"</span>           transient-quit-one<span style="color: #2d9574;">)</span><span style="color: #bc6ec5;">]</span><span style="color: #4f97d7;">)</span>

<span style="color: #4f97d7;">(</span><span style="color: #4f97d7; font-weight: bold;">defun</span> <span style="color: #bc6ec5; font-weight: bold;">tutorial-print-message</span> <span style="color: #bc6ec5;">(</span><span style="color: #ce537a; font-weight: bold;">&amp;optional</span> args<span style="color: #bc6ec5;">)</span>
  <span style="color: #bc6ec5;">(</span><span style="color: #4f97d7; font-weight: bold;">interactive</span><span style="color: #bc6ec5;">)</span>
  <span style="color: #bc6ec5;">(</span>print <span style="color: #2d9574;">"hello world"</span><span style="color: #bc6ec5;">)</span><span style="color: #4f97d7;">)</span>
</pre>
</div>

</div>

</div>

The transient-define-prefix is the command we use in order to define a new transient. tutorial-transient is the name of the transient (and the name of the command that we use to invoke the transient).

"info-manual" is a &#x2026;


## Transient layout

Next we define the sections/groups of the transient. Groups can be nested. By default the outer most groups define the "vertical" section of the transient while the inner groups define the columns within the virtual section.

In our examples we have two rows because we defined two seperate groups as follwoing:

    [...]        group1 (in this case row one)
    [...]        group2 (in this case row two)

Inside each row we can define multiple columns as following:

[[&#x2026;][&#x2026;]\\]           first row separated into two columns
[[&#x2026;][&#x2026;][&#x2026;]\\]      second row separated into three columns

In a group we can define either suffixes or string (incl. the empty string to create an empty line)
["Commands"         ;; first row with only some text
 ""                 :: empty line (just as for the sake of the example)
 ("p" "print message"      tutorial-print-message)]

At the moment of writing, a group can contain either subgroups or a group can contain string and suffixes. Subgroups and string/suffixes can not be combined within a single group)


# Creating a shortcut to acces your new transient

Now that we have defined the transient we can invoke it by "calling" the command with the name of the transient (tutorial-transient). In emacs we can call interactive functions with M-x. In Spacemacs we can simply run a interactive command through SPC SPC.

However, instead of invoking the command through M-x or SPC SPC every time you might probably want to bind your new transient to a shortcut.

In spacemacs we can do this using by defining a &#x2026;.. By default 'SPC o' is  reserved for "(o)ther applications". So for a custom (non mainstream) application/transient I would suggest to specify it somewhere under the SPC o prefix (but of course, if you prefer something else you're free do do anything you like).


# Adding and infix (in this case a switch/boolen to start with)

OK, now that we have explained a little bit about how we can control the layout of our transient it is time to add some extra features to our transient. Let's start with adding an option to set a flag. With the flag enabled we will print "hello BIG word". Without the flag we simply print "hellow world".

{% highlight emacs-lisp %}
(transient-define-prefix  tutorial-transient ()
  "Some Emacs magic"
  :info-manual "Tutroial transient"
  ["Arguments"
   ("-b" "A flag that can be set or not set" "--print-big")]
  ["Commands"
   ("p" "print message"      tutorial-print-message)]
  [("q" "Quit"           transient-quit-one)])

(defun tutorial-print-message (&optional args)
  (interactive (list (transient-args 'tutorial-transient)))
  (if (member "--print-big" args)
      (print "hello BIG enabled")
    (print "hello world"))
  )
{% endhighlight %}


# Adding a second infix (transient option, similar to a cli-option, text prompt for value)

{% highlight emacs-lisp %}
(transient-define-prefix  tutorial-transient ()
  "Some Emacs magic"
  :info-manual "Tutroial transient"
  ["Arguments"
   ("-b" "A flag that can be set or not set" "--print-big")
   (transient-text-option)]
  ["Commands"
   ("p" "print message"      tutorial-print-message)
   ("a" "print args"         tutorial-print-args)]
  [("q" "Quit"           transient-quit-one)])

(defun tutorial-print-message (&optional args)
  (interactive (list (transient-args 'tutorial-transient)))
  (print args)
  (if (member "--print-big" args)
      (print "hello BIG enabled")
    (print "hello world"))
  )

(defun tutorial-print-args (&optional args)
  (interactive (list (transient-args 'tutorial-transient)))
  (print args)
  )

(transient-define-infix transient-text-option ()     # one could also use the "transient-define-argument" function
  :description "Option with text"                    # which is an alias for the "transient-define-argument" funct
  :class 'transient-option                           # how the latter is the more generic name. The first should o
  :shortarg "-i"                                     # be used in when you're actually defining an argument
  :argument "--include-text=")
{% endhighlight %}

The first notable difference here is that we did not define the "transient-text-optin" infix directly in the "transient-define-prefix" function. For very basic infixes it is possible to define them directly in the under the "transient-define-prefix" command. Like we did in the previous section for the "&#x2013;print-big flag". However, when an option becomes a bit more complex and we we want more control about how it should be defined then we can also define the prefix with the "transient-define-infix" or "transient-define-argument" functions. In the previous section we defined the full infix by simply Provding a list with some configuration options to inside "transient-define-prefix" function (we could simply define this list ("-b" "A flag that can be set or not set" "&#x2013;print-big") to define the full &#x2013;print-big infix).

Since now we want to define a little bit more complex infix we will define the infix in a seperate function. Instead of providing the "configuration list" we simply call the "configuration function" in the "transient-define-prefix" function.

Thus instead of providing a list like this:

("-b" "A flag that can be set or not set" "&#x2013;print-big")

we are now simply calling the "configuration function":

(transient-text-option)

Additonally we define the "transient-text-option" configuration function which looks as follwing:

{% highlight emacs-lisp %}
(transient-define-argument transient-text-option ()
  :description "Option with text"
  :class 'transient-option
  :shortarg "-i"
  :argument "--include-text=")
{% endhighlight %}


# Adding a third infix (transient option, similar to a cli-option, list prompt for value)

{% highlight emacs-lisp %}
  (transient-define-prefix  tutorial-transient ()
    "Some Emacs magic"
    :info-manual "Tutroial transient"
    ["Arguments"
     ("-b" "A flag that can be set or not set" "--print-big")
     (transient-text-option)
     (transient-list-option)]
    ["Commands"
     ("p" "print message"      tutorial-print-message)
     ("a" "print args"         tutorial-print-args)]
    [("q" "Quit"           transient-quit-one)])

  (defun tutorial-print-message (&optional args)
    (interactive (list (transient-args 'tutorial-transient)))
    (print args)
    (if (member "--print-big" args)
        (print "hello BIG enabled")
      (print "hello world"))
    )

  (defun tutorial-print-args (&optional args)
    (interactive (list (transient-args 'tutorial-transient)))
    (print args)
    )

  (transient-define-infix transient-text-option ()
    :description "Option with text"
    :class 'transient-option
    :shortarg "-i"
    :argument "--include-text=")

(transient-define-infix transient-list-option ()
  :description "Option with list"
  :class 'transient-option
  :shortarg "-l"
  :argument "--include-list-option="
  :choices '("true" "false" "bit-true" "bit-false" "in-between"))
{% endhighlight %}


# Adding a fourth infix (transient option, similar to a cli-option, cycled list)

Instead of prompting the user with a list of choices we can also print the choices in the transient window and let the user cycle through the choices. This is not (yet) by defulat included in the transient package. However, the author/maintainer of both the transient and magit packages has implemented this alternative in magit. Therefore we can copy (or at least get inspriation from) the magit source code.

In order to define the suffix not much more should be necessary then what we needed to specify the infix that let's you choose an option from the prompt. However, two aspect of the "cycled list" infix type are different compared to the "list prompt" suffix:

1.  Instead of prompting the user and make the user select an option from a list we will print the list inside the transient window. The user can select the value from the list by typing the key for the infix a &#x2026;. amount of times. Each time that the user enter the key the next item in the list will be highlighted. In order to present the suffix in a different way in the transient window we need to modify the formatting for this suffix type.
2.  Instead of the list at the prompt where the user can select a value with the arrow keys the value should now be selected by cycling through a list in a different way. Each time the user presses the key for the specific suffix the value "cycles" to the next value in the list. Therefore we need to change the "set value" function for the prefix. The new value that should be set is depending on the current value. However in order to get the current value from the object we also need to change the "read value" function a bit.

{% highlight emacs-lisp %}
  (defclass tutorial-variable:choices (transient-variable)
    ((choices     :initarg :choices)
     (fallback    :initarg :fallback    :initform nil)
     (default     :initarg :default     :initform nil)))

  (cl-defmethod transient-infix-read ((obj tutorial-variable:choices))
    (print obj)
    (let ((choices (oref obj choices)))
      (if-let ((value (oref obj value)))
          (cadr (print (member value choices)))
        (car choices))))

  (cl-defmethod transient-infix-value ((obj tutorial-variable:choices))
    "Return the value of OBJ's `value' slot if not nil,
     else return value of OBJ's `default' slot if not nil,
     else return nil"
    (let ((default (oref obj default)))
      (if-let ((value (oref obj value)))
          (concat (oref obj argument) value)
        (when default
          (concat (oref obj argument) default)))))

  (cl-defmethod transient-format-value ((obj tutorial-variable:choices))
    (let ((value (oref obj value))
          (choices (oref obj choices))
          (default  (oref obj default)))
      (concat
       (propertize "[" 'face 'transient-inactive-value)
       (mapconcat (lambda (choice)
                    (propertize choice 'face (if (equal choice value)
                                                 (if (member choice choices)
                                                     'transient-value
                                                   'font-lock-warning-face)
                                               'transient-inactive-value)))
                  choices
                  (propertize "|" 'face 'transient-inactive-value))
       (and (or default)
            (concat
             (propertize "|" 'face 'transient-inactive-value)
             (cond
                   (default
                     (propertize (concat "default:" default)
                                 'face
                                 (if (print value)
                                     'transient-inactive-value
                                   'transient-value))))))
       (propertize "]" 'face 'transient-inactive-value))))

  (transient-define-prefix  tutorial-transient ()
    "Some Emacs magic"
    :info-manual "Tutroial transient"
    ["Arguments"
     ("-b" "A flag that can be set or not set" "--print-big")
     (transient-text-option)
     ("-c" "cycle" transient-cycle-option)]
    ["Commands"
     ("p" "print message"      tutorial-print-message)
     ("a" "print args"         tutorial-print-args)]
    [("q" "Quit"           transient-quit-one)])

  (defun tutorial-print-message (&optional args)
    (interactive (list (transient-args 'tutorial-transient)))
    (print args)
    (if (member "--print-big" args)
        (print "hello BIG enabled")
      (print "hello world"))
    )

  (defun tutorial-print-args (&optional args)
    (interactive (list (transient-args 'tutorial-transient)))
    (print args)
    )

  (transient-define-infix transient-text-option ()
    :description "Option with text"
    :class 'transient-option
    :shortarg "-i"
    :argument "--include-text=")

(transient-define-infix transient-list-option ()
  :description "Option with list"
  :class 'transient-option
  :shortarg "-l"
  :argument "--include-list-option="
  :choices '("true" "false" "bit-true" "bit-false" "in-between"))

(transient-define-infix transient-cycle-option ()
  :description "Option with list"
  :class 'tutorial-variable:choices
  :shortarg "-c"
  :argument "--include-cycle-option="
  :choices '("true" "false" "bit-true" "bit-false" "in-between")
  :default "bit-false")
{% endhighlight %}


# Adding a fourth infix (transient option, similar to a cli-option, cycled list with default)

{% highlight emacs-lisp %}
(defclass tutorial-variable:choices (transient-variable)
  ((choices     :initarg :choices)
   (fallback    :initarg :fallback    :initform nil)
   (default     :initarg :default     :initform nil)))

(cl-defmethod transient-infix-read ((obj tutorial-variable:choices))
  (let ((choices (oref obj choices)))
    (if-let ((value (oref obj value)))
        (cadr (print (member value choices)))
      (car choices))))

(cl-defmethod transient-infix-value ((obj tutorial-variable:choices))
  "Return the value of OBJ's `value' slot if not nil,
     else return value of OBJ's `default' slot if not nil,
     else return nil"
  (let ((default (oref obj default)))
    (if-let ((value (oref obj value)))
        (concat (oref obj argument) value)
      (when default
        (concat (oref obj argument) default)))))

(cl-defmethod transient-format-value ((obj tutorial-variable:choices))
  (let (
        ;; (variable (oref obj variable))
        (value (oref obj value))
        ;; (local    (magit-git-string "config" "--local"  variable))
        (choices (oref obj choices))
        (default  (oref obj default)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (choice)
                  (propertize choice 'face (if (equal choice value)
                                               (if (member choice choices)
                                                   'transient-value
                                                 'font-lock-warning-face)
                                             'transient-inactive-value)))
                choices
                (propertize "|" 'face 'transient-inactive-value))


     (and (or default)
          (concat
           (propertize "|" 'face 'transient-inactive-value)
           (cond
            (default
              (propertize (concat "default:" default)
                          'face
                          (if (print value)
                              'transient-inactive-value
                            'transient-value))))))

     (propertize "]" 'face 'transient-inactive-value))))

(transient-define-prefix  tutorial-transient ()
  "Some Emacs magic"
  :info-manual "Tutroial transient"
  ["Arguments"
   ("-b" "A flag that can be set or not set" "--print-big")
   (transient-text-option)
   ("-c" "cycle" transient-cycle-option)
   ("-d" "cycle-default" transient-cycle-with-default-option)]
  ["Commands"
   ("p" "print message"      tutorial-print-message)
   ("a" "print args"         tutorial-print-args)]
  [("q" "Quit"           transient-quit-one)])

(defun tutorial-print-message (&optional args)
  (interactive (list (transient-args 'tutorial-transient)))
  (if (member "--print-big" args)
      (print "hello BIG enabled")
    (print "hello world"))
  )

(defun tutorial-print-args (&optional args)
  (interactive (list (transient-args 'tutorial-transient)))
  (print args)
  )

(transient-define-infix transient-text-option ()
  :description "Option with text"
  :class 'transient-option
  :shortarg "-i"
  :argument "--include-text=")

(transient-define-infix transient-list-option ()
  :description "Option with list"
  :class 'transient-option
  :shortarg "-l"
  :argument "--include-list-option="
  :choices '("true" "false" "bit-true" "bit-false" "in-between"))

(transient-define-infix transient-cycle-option ()
  :description "Option with list"
  :class 'tutorial-variable:choices
  :argument "--include-cycle-option="
  :choices '("true" "false" "bit-true" "bit-false" "in-between"))

(transient-define-infix transient-cycle-with-default-option ()
  :description "Option with list"
  :class 'tutorial-variable:choices
  :argument "--include-cycle-with-default-option="
  :choices '("test" "with" "default-value")
  :default "default-value")
{% endhighlight %}


# Adding a multi-option (we seperate it with commands)


# Adding an "argument" ()


# Adding a second argument ()


# Defining all suffices with the use of "define-suffix" functions

{% highlight emacs-lisp %}
(transient-define-prefix  tutorial-transient ()
  "Some Emacs magic"
  :info-manual "Tutroial transient"
  ["Arguments"
   (print-big-option)
   (transient-text-option)]
  ["Commands"
   ("p" "print message"      tutorial-print-message)
   ("a" "print args"         tutorial-print-args)]
  [("q" "Quit"           transient-quit-one)])

(defun tutorial-print-message (&optional args)
  (interactive (list (transient-args 'tutorial-transient)))
  (print args)
  (if (member "--print-big" args)
      (print "hello BIG enabled")
    (print "hello world"))
  )

(defun tutorial-print-args (&optional args)
  (interactive (list (transient-args 'tutorial-transient)))
  (print args)
  )

(transient-define-infix print-big-option ()
  :description "A flag that can be set or not set"
  :class 'transient-switch                        
  :shortarg "-b"                                  
  :argument "--print-big")

(transient-define-infix transient-text-option ()     # one could also use the "transient-define-argument" function
  :description "Option with text"                    # which is an alias for the "transient-define-argument" funct
  :class 'transient-option                           # how the latter is the more generic name. The first should o
  :shortarg "-i"                                     # be used in when you're actually defining an argument
  :argument "--include-text=")
{% endhighlight %}


# Running a Python script in a new buffer

The transient is a (in my humble opinion) superior alternative for run commands from the commandline. Personally I write Python commandline script on a regular basis. Very often I find it very convenient to run these script directly from Emacs. The typical and less convenient way is to open up a terminal and subsequently type the command in the terminal. Sometimes if you use a specific command often it can be convenient to create an alias. But what if you want to use slightly different parameters then those that you have defined in your alias. In that case you need to type the full command again on the terminal.
