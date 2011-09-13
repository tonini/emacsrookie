---
title: Do you remember?
summary: Through the day there are many things which pop up in your head. How to catch them on the fly or get back if needed? In this article you will see how Remember will help you with these kind of tasks.
hint: 1
layout: post
---
Remember is a mode for Emacs, to simpy remember data.
I'm a big fan of [org-mode](http://orgmode.org/), but to create notes on the fly
without having to switch to my org file, I prefer Remember. Since
Emacs version 23.1 Remember is built in and it's doing a good job
without any special configurations needed.

### How to use straight ahead?

To jump in the note Remember buffer, simpy call the
`remember` function. You will see an empty buffer if you are using it for the
first time and there is no `~/.notes` file.
Below your current cursor position you will see the file path from
which you called the `remember` function.

{% highlight text %}
# The cursor will be positioned above the file path
/path/to/your/file.rb
{% endhighlight %}

Add your notes and after you're finished, just call the `remember-finalize` function,
or use the default key-binding `C-c C-c`. By default the note will be
stored in `<your_home_directory>/.notes`.

{% highlight text %}
** Mon Sep 12 16:02:03 2011 (Refactor)

Refactor

I should refactor some code smell in this file...

/path/to/your/file.rb
{% endhighlight %}

Done. That's it! That's the easy way to use it without any magic
and store your notes on the fly.

### More functions

There are a few other functions which are also very useful.

`remember-destroy` just destroy the current Remember buffer you're in.

`remember-region` without opening the selected region in the Remember buffer - it saves the text directly into your notes file.

`remember-clipboard` writes your current clipboard contents to your notes file.

I would recommend that you just inspect the source of Remember on your own to get
an overview of the full functionality.

### Some customization

To change the file name in which you want to store your notes from
`.todos` to `.notes` just overwrite the `remember-data-file` variable.

{% highlight scheme %}
(defcustom remember-data-file (convert-standard-filename "~/.todos")
  "The file in which to store unprocessed data."
  :type 'file
  :group 'remember)
{% endhighlight %}

Or the leading text, which is used to begin each created note.

{% highlight scheme %}
(defcustom remember-leader-text "** "
  "The text used to begin each remember item."
  :type 'string
  :group 'remember)
{% endhighlight %}

### Further information

A lot of people use Remember with org-mode ([org-remember](http://orgmode.org/org-remember.pdf) documentation).
But since version 6.36, org-mode has its own Remember-like feature
implemented (see [Capture](http://orgmode.org/manual/Capture.html)),
If you're using org-mode already, I would recommend to use that instead of Remember.

In a further article I will show you how to use Capture, instead of Remember and how org-mode serves you some cool features with Capture.
