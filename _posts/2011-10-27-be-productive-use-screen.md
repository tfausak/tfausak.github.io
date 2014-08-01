---
title: 'Be Productive: Use Screen'
---

My development machine exists in "the cloud", so to speak. At
[Famigo][1], [Amazon EC2][2] powers both our development and
production environments. I still connect to EC2 using good old-fashioned
hardware, but the only thing I know about my development instance
is that it gives me a shell prompt.

Growing up, my dad introduced me to Linux, but not in the usual
client-server fashion. I frequently used Linux, sitting directly
in front of the machine, connected with a keyboard.  More often
than not, I set up a GUI so I could see more than 80 columns and
24 rows.

By the time I got to high school, I was better acquainted with the
client-server model. I routinely connected to servers to start or
administrate long-running tasks. In particular, I ran a Counter-Strike
server for me and my friends. It didn't take me long to realize
that I needed decouple the terminal from the commands running in
it.

My first attempt was to push the server into the background. I don't
remember the exact command, but it was something like `./server &`.
That didn't work too well. It spammed the console with the server's
output. After a bit of searching I found [a tutorial][3] explaining
how to run it in a "screen". It was a revelation.

By "screen", I mean [GNU Screen][4]. I use it all day, every day.
It makes working from home a cinch. I can leave work without doing
anything other than closing my terminal and pick it up at home
without doing anything other than opening a new terminal. Screen
is a powerful tool, but you don't need to know much about it for
it to be useful.

As soon as I SSH into my development box, the first thing I do is
`screen -dR`. That command detaches the screen from any other
terminals and connects it to mine. If a screen doesn't exist, it
creates one.

Now that I'm in the screen, I hit `C-a "` to see all my windows.
The output typically looks like this:

    Num Name    Flags
    0 server      $
    1 bash        $
    2 test        $
    3 git         $

As you can see, not a whole lot going on. I'm running a server in
screen 0. I can easily go to it (with `C-a 0`) to catch Django's
output. Screen 1 is where I do all of my actual work; it's usually
running [Vim][5]. I run all of my unit tests in screen 2. This is
handy because I can flip back and forth (with `C-a C-a`) between
the test output and the code that broke it. Finally, I handle all
my git commands in screen 3, which lets me commit frequently while
still keeping all my files open and tests running.

So there you have it. That's how I use screen to be productive at work.

[1]: http://www.famigo.com
[2]: http://aws.amazon.com/ec2/
[3]: http://www.srcds.com/db/engine.php?&id=1098643920
[4]: http://en.wikipedia.org/wiki/GNU_Screen
[5]: http://en.wikipedia.org/wiki/Vim_(text_editor)
