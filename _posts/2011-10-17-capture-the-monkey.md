---
title: Capture the monkey
summary: Org Mode serves a great feature called Capture, to create task items on the fly. It has a few very handy capabilities which I will show you.
hint: 5
layout: post
---
The first Emacsrookie article was about
[Remember](http://emacsrookie.com/2011/09/13/do-you-remember/), an
Emacs built in package to create new tasks on the fly. There I
promised to write a new article about how to do the same in a more
recent way, with Capture.

I will show now how to use Capture, the way how
[Org Mode](http://orgmode.org/) handles capturing new tasks on the
fly. Capture is highly inspired by the Remember package and I
recommend to switch to it if you still use Remember.

But why it's so great? It's really easy to use, it's built in Org Mode
and it brings powerful abilities to create and customize new task
templates.

Let's check it out!

### Here's how to use it

Org Mode ships in with Emacs. But if you like to get a more recent
version of Org Mode, please check the
[install](http://orgmode.org/manual/Installation.html#Installation)
section in the Org Mode manual.

To create your first task, you have to define a default file in
which the task should be added.

Just leave the path to the file in the following variable:

{% highlight scheme %}
(setq org-default-notes-file (concat org-directory "/tasks.org"))
{% endhighlight %}

Done, now you're ready to go!

Org Mode gives you the function `org-capture` to open the `*Org
select*` buffer. At first, you just have three points to choose from,
create a new task item, open customize (for create new Capture
templates) or quit the buffer.

You should create a new task item at the outset, to get a feeling how
it works. You shouldn't have any problems to create a task, it's very
self-explinatory.

Maybe at this point you ask yourself what's about this Capture
template stuff you read about in the `*Org select*` buffer.

Let's see what this template stuff is all about.

### Templates

Capture gives you the ability to create your own templates for new
task items. You just have to define a list for each of your templates
and assign them to the `org-capture-templates` variable.

{% highlight scheme %}
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
             "* TODO %?\n  %t")))
{% endhighlight %}

Ok, let's see what kind of a template we have defined. When you create
a template, you need to set the following items in the list:

<table class="function-list">
    <tr>
        <th class="functions">Item value</th>
        <th>Description</th>
    </tr>

    <tr>
        <td><span class="item">keys</span></td>
        <td>A string which is used as a key for the template selection.</td>
    </tr>

    <tr>
        <td><span class="item">description</span></td>
        <td>The description of the template.</td>
    </tr>

    <tr>
        <td><span class="item">type</span></td>
        <td>The type of a new item.</td>
    </tr>

    <tr>
        <td><span class="item">target</span></td>
        <td>The specification where the new item should
        be placed.</td>
    </tr>

    <tr>
        <td><span class="item">template</span></td>
        <td>The template for creating the capture item.</td>
    </tr>

    <tr>
        <td><span class="item">properties</span></td>
        <td>Additional properties.</td>
    </tr>
</table>

Check the [manuel](http://orgmode.org/manual/Template-elements.html#Template-elements) for detailed information.

Capture has some nice [template expansion](http://orgmode.org/manual/Template-expansion.html#Template-expansion) and you should read through
the list.

In the above template definition I just used `%?` and `%t`. I mostly
use them because I'm not a fancy boy. `%?` means that after completing
the template, the cursor will be positioned there. And the `%t` is a
simple date only timestamp which I use because I want that my new
`TODO` item will be shown up in the agenda view of Org Mode.

Let's define a new keybinding to your new Capture template, so you
don’t have to go through the selecting process when you want to create a new
task item:

{% highlight scheme %}
(define-key global-map "C-c t"
  (lambda () (interactive) (org-capture nil "t")))
{% endhighlight %}

So now you could create a new task item with `C-c t` without going
trough the template selection.

### Some customization

I use Org Mode for a lot of things, also these article are written in Org
mode. I manage also all my project with it, and because I have to
switch between them through the day, I have written some code to
create new items for a specific project on the fly.

First a variable which holds the path to the projects org files:

{% highlight scheme %}
(defvar custom--org-project-file-path
  "~/org/projects/")
{% endhighlight %}

Second I defined a method who returns all the files which are in the
org projects path:

{% highlight scheme %}
(defun custom-org-project-files
  (directory-files custom--org-project-file-path nil "\\.org$"))
{% endhighlight %}

Then I created a method which prompts for one of my project org files
and I used the `completing-read` function to get a nice file
completing on my org project files:

{% highlight scheme %}
(defun custom-org-project-chooser ()
  (let ((completing-read-func (if (null ido-mode)
                                  'completing-read
                                'ido-completing-read)))
    (setq project-file
          (funcall completing-read-func
                   "Project: "
                   (custom-org-project-files)
                   nil
                   t))))
{% endhighlight %}

You can see in the code example above that I decided to use the
function `ido-completing-read` instead of `completing-read` if
`ido-mode` available. (Check out [ido-mode](http://www.emacswiki.org/emacs/InteractivelyDoThings) for further
informations.)


After that I put my `custom-org-project-chooser` function in a helper
function where I define in which file the new task item should be
placed and where in the file the task should be positioned.

I used in the `re-search-forward` function with a speficied regulary
expression to find the position in the file where I want to put the
task.

In my case I always put it after the project title header (like: `* My Project`).

{% highlight scheme %}
(defun custom-org-project-file ()
  (find-file (concat custom--org-project-file-path (custom-org-projects-chooser)))
  (goto-char (point-min))
  (re-search-forward "^\*\s*.+\n" nil t)
  (newline 1))
{% endhighlight %}

At last I created a Capture template from the type `function` which
calls my `custom-org-project-file` function when a new project item
will be created:

{% highlight scheme %}
(setq org-capture-templates
      '(("p" "Project Todo Files" entry (function custom-org-project-file)
         "** TODO %?\n  %T")))
{% endhighlight %}

Thats it! Bind this Capture template call to a key you like and you're
ready to go and create your own project tasks on the fly.

You find the whole source in my emacs configs [repository](https://github.com/tonini/dotfiles/blob/master/emacs.d/private/org-mode.el).

### Summary

If you like to manage your task through the day I really
recommend Capture and all his capabilites. Check the Capture [manual](http://orgmode.org/manual/Capture.html#Capture)
out and get yourself a better overview.
