---
tite: Handle Ruby versions
summary: The Emacs package rvm.el brings the excellent skills from the RVM tool to Emacs and adds some very useful extra features to work with it under Emacs.
hint: 3
layout: post
---
[RVM](http://beginrescueend.com/) it's an extremly usefull tool if you
have to work with Ruby, different Ruby projects and specific
Ruby Gemsets for your projects. RVM offers the solution for these
needs by creating environments bound to a specific Ruby version and a
set of Ruby Gems. This possibility avoids Gem version conflicts between
your projects and also makes it easy to create test environments.

Now what we want is to bring this into our beloved Emacs. And here comes rvm.el
along, an Emacs package by [Yves Senn](https://github.com/senny). The rvm.el package integrates
Emacs with RVM and gives us a few nifty functions.

One of the great advantages which rvm.el brings is that packages like
`inf-ruby` and `mode-compile` automatically benefit from it and use the
precise Ruby version.

### How to use it straightforward?

First you have to install rvm.el; you get the source on
[Github](https://github.com/senny/rvm.el).

Then you need to put the rvm.el somewhere in your `load-path`
and add the following to your `~/.emacs` file.

{% highlight scheme %}
;; Load rvm.el
(require 'rvm)
;; use rvm’s default ruby for the current Emacs session
(rvm-use-default)
{% endhighlight %}

Done, now you're ready to use rvm.el.

The first function you want to know is `rvm-activate-corresponding-ruby`,
this interactive function activates the corresponding Ruby version for
the file in the current buffer. What the function does is, to search
for an [.rvmrc](http://beginrescueend.com/workflow/rvmrc/) file and activate the selected Ruby version. If there
is no `.rvmrc` file, the default ruby will be used instead.

Maybe you want to switch manually to a specific Ruby version, then you
could use the `rvm-use` function, which is also as an interactive
function available.

If you're working with Ruby on a daily basis and have to rummage through
different Gem source code from time to time, then you will definitely
need the interactive `rvm-open-gem` function. This function gives you a
list of all the Gems which are installed for the currently activated
Ruby version and shows them in the minibuffer. Now you just have to decide
which one to open.

In addition to that, if you have [Perspective](/2011/09/25/workspaces/) installed, `rvm-open-gem` will
open the chosen Gem directory in a new perspective, labeled with the
Gem's name. Now it's easy to switch back and forth between your project and
the Gem source.

### More functions

`rvm-autodetect-ruby` will add a `ruby-mode-hook` which will call
`rvm-activate-corresponding-ruby` to detect the Ruby version.

`rvm-autodetect-ruby-stop` is the vice-versa function, it will remove
the `ruby-mode-hook` which calls the `rvm-activate-corresponding-ruby` function.

`rvm-use-default` uses the rvm default configured Ruby as the
current Ruby version.

### Some customization

You can use rvm.el in your own Emacs scripts easily. Here an
example how `rspec-mode` uses it:

{% highlight scheme %}
(defcustom rspec-use-rvm nil
  "t when RVM in is in use. (Requires rvm.el)"
  :type 'boolean
  :group 'rspec-mode)
{% endhighlight %}

And in the `rspec-compile` function it uses the
`rvm-activate-corresponding-ruby` before it runs the specs.

The following example code is simplified.

{% highlight scheme %}
(defun rspec-compile ()
  ;; some code.....
  (if rspec-use-rvm
      (rvm-activate-corresponding-ruby))
  ;; more code ..
  )
{% endhighlight %}

### Summary

Rvm.el is a great package and if you're a Ruby programmer it
will make your daily adventures with Ruby a lot more exciting.

The source code is available on the [rvm.el](https://github.com/senny/rvm.el) repository on Github.

