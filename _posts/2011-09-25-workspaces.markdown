---
tite: Workspaces
summary: Perspective is a great emacs package which gives you tagged workspaces and offers the possibility to working in a separated set of buffers.
hint: 2
layout: post
---
Perspective is an emacs package by [Nathan Weizenbaum](http://nex-3.com/), which offers the
possibility of working with different workspaces. If you are used to a
windows manager (like Fluxbox, Gnome, KDE etc. for instance), you
know the advantages of switching between several desktops or setting up
different desktops for your purposes.

In my daily grind I switch often between projects, fixing a bug
there or reading some source codes here. This possibility of working this way is extremely helpful.
It's hard imagining not to be able to use it.

Perspective gives you tagged workspaces and because each perspective
(workspace) is composed of a window configuration and a set of
buffers, switching to a perspective will only make the buffers
available which belong to it.

### Here's how to use it

First you have to install perspective, you get the source on
[Github](http://github.com/nex3/perspective-el).

Then you need to put the perspective.el file somewhere in your `load-path`
and add the following to your `~/.emacs` file.

{% highlight scheme %}
;; Load Perspective
(require 'perspective)
;; Toggle the perspective mode
(persp-mode)
{% endhighlight %}

From now on you are ready to go. Perspective is easy to use and you
will be familiar with its functionality in no time.

What you will use mostly are `persp-switch`. With this function you
switch to the perspective you want, if the specified perspective
doesn't exist, it will create a new one and switch to it.

Sometimes you need to rename a perspective and for this action use the function
 `persp-rename`.

Another useful function is `persp-add-buffer`, just
add a buffer to your chosen perspective. The vice-versa function of
it is `persp-remove-buffer`.

### More functions

`pesp-kill` just kill a not anymore used perspective.

`persp-import` import a perspective from another frame.

### Some customization

Yves Senn, a friend of mine did a great job customizing
perspective. He defined a macro which takes two arguments : the `name` for the
perspective and the `body` which contains the code that will be evaluated after creating
the perspective. So what you can do now is to define your custom
function using the macro.

{% highlight scheme %}
(defmacro custom-persp (name &rest body)
       `(let ((initialize (not (gethash ,name perspectives-hash)))
              (current-perspective persp-curr))
          (persp-switch ,name)
          (when initialize ,@body)
          (setq persp-last current-perspective)))
{% endhighlight %}

For instance, create a `@org`
named perspective and open the first agenda file.

{% highlight scheme %}
(defun custom-persp/org ()
  (interactive)
  (custom-persp "@org"
  (find-file (first org-agenda-files))))
{% endhighlight %}

At last, just create a key binding for the function.

{% highlight scheme %}
(global-set-key (kbd "C-p o") 'custom-persp/org)
{% endhighlight %}

You find the whole source in the [emacs-configs](https://github.com/senny/emacs-configs/blob/master/private/extensions/perspectives.el) repository from Yves Senn.

### Summary

Perspective is definitly a well written package

I recommend reading through the Perspective source and make yourself
more familiar with whats going on beyond all these functions.
