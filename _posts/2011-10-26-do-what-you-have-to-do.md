---
title: Do what you have to do
summary: Manage your tasks through the day could be difficult, but with the power of Org Mode's task ability you get a powerful feature to handle it.
hint: 6
layout: post
---
In the article [Capture the monkey](http://emacsrookie.com/2011/10/17/capture-the-monkey/) I wrote about the Capture feature
from Org Mode, which gives you the ability to create task items on the
fly. Now I will show you how I use this feature to create
so-called TODO items.

You will see that the introduction I give you is enough to start and
use the TODO feature to be productive in your task management
daily grind. If you're looking for more detailed information about the
TODO items, please consider to read the Org Mode [manual](http://orgmode.org/manual/TODO-Items.html).

### Here's how to use it

Ok, let's create our first TODO item.

To structure an Org Mode document you use headlines, and these start
with one or more stars:

{% highlight text %}
* Top level
** Second level
*** Third level
{% endhighlight %}

Now you see how to create headlines, to make a headline into a
TODO item, just add the word `TODO` after the stars and you get your
first TODO item:

{% highlight text %}
** TODO Create your first todo item
{% endhighlight %}

By default, a TODO item has two states, `TODO` and `DONE` and to
rotate through the different states use `(org-shiftright)` or
`(org-shiftleft)` functions. At this time it just switches between `TODO` and
`DONE`, but later when you have more keywords, it will also iterate
through these.

Example of a `TODO` item and a finished one marked as `DONE`:

{% highlight text %}
** TODO Not done yet!
** DONE I'm done!
   CLOSED: [<Timestamp>]
{% endhighlight %}

You see that in the `DONE` marked task item is a `CLOSED` keyword with a
timestamp beside it. By default, if you finish a task item Org
Mode will add a timestamp and a `CLOSED` keyword, to inform you about
the finished time of the task. This is very helpful to get more
information in the agenda view.

By default the variable `org-log-done` is set to `'time`, which means
that after you finish a task a timestamp will be added, as already
mentioned above. But you could also add a note. Just assign `'note`
to the `org-log-done` variable and Org Mode will prompt you for an
additional note along with the timestamp.

One of my favorite features I couldn't work without are the [clock
commands](http://orgmode.org/manual/Clocking-commands.html#Clocking-commands). Imagine you're starting with a new task, you have to fix a
bug in your code and have to manage the time you need to fix it. Org
Mode comes along with punch-clock powers, which makes this kind of
job very easy.

The function `(org-clock-in)` starts the clock on the current item
you're in and the vice-versa function `(org-clock-out)` stops the
clock.

Check out what this looks like:

{% highlight text %}
** TODO what's the amount of time needed
   CLOCK: [2011-10-26 Wed 14:31]--[2011-10-26 Wed 15:10] =>  0:39
{% endhighlight %}

In the example above I started the clock and stopped the clock 39
minutes later. This feature is really great, because in the agenda
view of Org Mode this clock information will give you an overview
over how long we needed to fix our bugs.

For now you know enough to manage your task items. Go ahead and try this
out.

Next I will show you how easy it is to create your own `TODO` like
keywords, for example something like `FEATURE`, `BUG` or `SUPPORT`.

### Your own keywords

In my daily grind I use a few more keywords than just a simple `TODO`
for my tasks. Org Mode gives you the possibility to add
our own keywords really easily. The variable `org-todo-keywords` gets
all the task types we want to have:

{% highlight scheme %}
(setq org-todo-keywords
       '((sequence "TODO" "|" "DONE")
       (sequence "FEATURE" "|" "COMPLETED")
       (sequence "BUG" "|" "FIXED")
       (sequence "|" "CANCELED")))
{% endhighlight %}

Great, now you're ready to use this just like `TODO` before.

What I really like is the possibility to create fast access `TODO`
states by just adding single keys in parentheses after a keyword.

Fast access state keys:

{% highlight scheme %}
(setq org-todo-keywords
       '((sequence "TODO(t)" "|" "DONE(d)")
       (sequence "FEATURE(f)" "|" "COMPLETED(c)")
       (sequence "BUG(b)" "|" "FIXED(x)")
       (sequence "|" "CANCELED(a)")))
{% endhighlight %}

If you now use the `(org-todo)` function, Org Mode prompts you for the kind of
task key which you want to use, so you don't have to cycle through the
task types with the `(org-shiftleft)` or `(org-shiftright)` functions
anymore.

### Functions and default keybindings

<table class="function-list">
    <tr>
        <th class="functions">Function</th>
        <th>Keybinding</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><span class="code">org-shiftright</span></td>
        <td><span class="code">S-RIGHT</span></td>
        <td>Cycling through the item state forwardly.</td>
    </tr>
    <tr>
        <td><span class="code">org-shiftleft</span></td>
        <td><span class="code">S-LEFT</span></td>
        <td>Cycling through the item state backwards.</td>
    </tr>
    <tr>
        <td><span class="code">org-clock-in</span></td>
        <td><span class="code">C-c C-x C-i</span></td>
        <td>Start the clock in the current task item.</td>
    </tr>
    <tr>
        <td><span class="code">org-clock-out</span></td>
        <td><span class="code">C-c C-x C-o</span></td>
        <td>Stop the clock in the current task item.</td>
    </tr>
    <tr>
        <td><span class="code">org-todo</span></td>
        <td><span class="code">C-c C-t</span></td>
        <td>Change the TODO state of an item. If other keywords are
        set it prompts for one of these..</td>
    </tr>
</table>

### Summary

You see Org Mode's TODO feature is very powerful and still easy to
use. You get in touch with it in no time and you can use it for your
daily programming routine.

There is certainly a lot of more stuff about the TODO Org Mode
feature and it would be wise of you to check it out. [Org Mode TODO manual](http://orgmode.org/manual/TODO-Items.html#TODO-Items)
