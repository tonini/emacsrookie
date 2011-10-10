---
tite: Enter the Dired dungeon
summary: Emacs comes with a very powerful tool called Dired, which is way more than just a directory navigator.
hint: 4
layout: post
---
When I talk with people about Emacs, they often complain about
the missing project navigator.

Dired has a lot to offer that doesn't fit in just one article, so
I decided to split it into several parts. I start by showing the
navigation abilities of Dired.

Throughout the article I won't refer to keybindings but instead to the
function names. At the end there will be a list of the functions
including the corresponding default keybindings.

### How to use it straightforward?

Dired is built into Emacs, so you don't have to install anything.

Enter Dired by calling the function `dired`. The minibuffer opens and prompts you to choose a directory to open.

Example of an open Dired buffer:

{% highlight text %}
  /Users/samuel/Projects/basement:
  total used in directory 48 available 151364824
  drwxr-xr-x  10 samuel  staff   340 24 Aug 16:16 .
  drwxr-xr-x  37 samuel  staff  1258  5 Okt 13:54 ..
  drwxr-xr-x  12 samuel  staff   408  5 Okt 22:29 .git
  -rw-r--r--   1 samuel  staff    33 24 Aug 15:55 .gitignore
  -rw-r--r--   1 samuel  staff    92 24 Aug 15:55 Gemfile
  -rw-r--r--   1 samuel  staff   194 24 Aug 16:16 Gemfile.lock
  -rw-r--r--   1 samuel  staff    71 24 Aug 15:57 README.markdown
  -rw-r--r--   1 samuel  staff    28 24 Aug 15:55 Rakefile
  -rw-r--r--   1 samuel  staff   757 24 Aug 16:04 basement.gemspec
  drwxr-xr-x   4 samuel  staff   136 24 Aug 15:55 lib
{% endhighlight %}

Dired creates a read-only buffer and lists all the files
from your chosen directory.

In the Dired buffer you're able to move around with the usual
movement functions. Only the `next-line` and `previous-line` functions are
redefined. Generally when you use these functions, the cursor will be
positioned at the beginning of the line, but in the Dired buffer it
goes to the beginning of the file name.

Next what you want to know is the `dired-goto-file` function, which
prompts for a file name in the minibuffer. It moves the cursor in the
Dired buffer to the file you've chosen. Most of the time, you want to
visit the file of the currently selected line. In this case you use
`dired-find-file` which opens the file straight away without prompting
for a file name input.

Maybe you're asking yourself now, but what about
sub directories? You have seen how easy it is to navigate in a single
directory and visit specific files, maybe you found out that when you
use `dired-goto-file` on a sub directory it will list the files from
this directory. But you want a solution with a better overview,
something that gives you the parent and the sub directory at one glance.
So lets check out what Dired has in store for that scenario.

As you know, Dired just displays one directory at the time. But with
the `dired-maybe-insert-subdir` function, Dired takes the directory
on the current line and lists the content of the directory below
the parent directory in the current Dired buffer.

Let's see how this looks like:

{% highlight text %}
  /Users/samuel/Projects/basement:
  total used in directory 48 available 151364824
  drwxr-xr-x  10 samuel  staff   340 24 Aug 16:16 .
  drwxr-xr-x  37 samuel  staff  1258  5 Okt 13:54 ..
  drwxr-xr-x  12 samuel  staff   408  5 Okt 22:29 .git
  -rw-r--r--   1 samuel  staff    33 24 Aug 15:55 .gitignore
  -rw-r--r--   1 samuel  staff    92 24 Aug 15:55 Gemfile
  -rw-r--r--   1 samuel  staff   194 24 Aug 16:16 Gemfile.lock
  -rw-r--r--   1 samuel  staff    71 24 Aug 15:57 README.markdown
  -rw-r--r--   1 samuel  staff    28 24 Aug 15:55 Rakefile
  -rw-r--r--   1 samuel  staff   757 24 Aug 16:04 basement.gemspec
  drwxr-xr-x   4 samuel  staff   136 24 Aug 15:55 lib

  /Users/samuel/Projects/basement/lib:
  total used in directory 8 available 151364824
  drwxr-xr-x   4 samuel  staff  136 24 Aug 15:55 .
  drwxr-xr-x  10 samuel  staff  340 24 Aug 16:16 ..
  drwxr-xr-x   3 samuel  staff  102 24 Aug 15:55 basement
  -rw-r--r--   1 samuel  staff   75 24 Aug 15:55 basement.rb
{% endhighlight %}

You see, the parent directory is on top and the sub directory you jumped
to is listed below. It's an easy and great overview. At this point
Dired gives you a few functions to move between the
directories. `dired-prev-subdir` to move up in the directory tree and the
vice-versa function `dired-next-subdir`. Also useful is
the function `dired-up-directory` which runs Dired in the parent
directory. It searches in the current buffer for the parent directory or
in another buffer, or creates a new one if needed.

### Some customization

The variable `dired-listing-switches` contains the options which are
used when `ls` is called. That means you could define how the command
`ls` should be called when you open a new directory in Dired.

Example:

{% highlight scheme %}
(setq dired-listing-switches "-lRS")
{% endhighlight %}

The `-` in the `dired-listing-switches` variable is a must or Dired
wont work. I also recommend to add the `l` option to the variable, as
by default just the single file and directory names would be listed.
Just fire up the `ls` man page if you want to see all available
options for `ls`.

### Functions and default keybindings

<table class="function-list">
    <tr>
        <th class="functions">Function</th>
        <th>Keybinding</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><span class="code">dired</span></td>
        <td><span class="code">C-x d</span></td>
        <td>Enter Dired.</td>
    </tr>
    <tr>
        <td><span class="code">dired-next-line</span></td>
        <td><span class="code">C-n / SPC / n</span></td>
        <td>Move to the next line.</td>
    </tr>
    <tr>
        <td><span class="code">dired-previous-line</span></td>
        <td><span class="code">C-p / p / DEL</span></td>
        <td>Move to the previous line. (<span class="code">DEL</span> also unflags.)</td>
    </tr>
    <tr>
        <td><span class="code">dired-goto-file</span></td>
        <td><span class="code">j</span></td>
        <td>Prompts for a filename and moves cursor to choosen file in
    the Dired buffer.</td>
    </tr>
    <tr>
        <td><span class="code">dired-find-file</span></td>
        <td><span class="code">f / e / RET</span></td>
        <td>Open the file on the current line.</td>
    </tr>
    <tr>
        <td><span class="code">dired-next-subdir</span></td>
        <td><span class="code">C-M-n</span></td>
        <td>Jumps to the next sub directory.</td>
    </tr>
    <tr>
        <td><span class="code">dired-prev-subdir</span></td>
        <td><span class="code">C-M-p</span></td>
        <td>Jumps to the previous sub directory.</td>
    </tr>
    <tr>
        <td><span class="code">dired-prev-subdir</span></td>
        <td><span class="code">^</span></td>
        <td>Run Dired on parent directory of current directory.</td>
    </tr>
    <tr>
        <td><span class="code">dired-maybe-insert-subdir</span></td>
        <td><span class="code">i</span></td>
        <td>Opens the content of the directory on the current line in the Dired buffer.</td>
    </tr>
</table>

### Summary

Dired has everything you need to navigate through your daily projects
and a lot more. I just scratched the surface and recommend the
official [Emacs Dired manual](http://www.gnu.org/s/libtool/manual/emacs/Dired.html#Dired)
for further information.
