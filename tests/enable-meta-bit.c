#include <ncurses.h>
#include <stdio.h>
#include <string.h>
#include <term.h>
#include <termios.h>
#include <unistd.h>

static struct termios save_termios;

int
tty_raw (int fd)
{
        struct termios buf;

        if (tcgetattr (fd, &save_termios) < 0)
        return -1;
        buf = save_termios;

        buf.c_lflag &= ~ (ECHO | ICANON | IEXTEN | ISIG);
        buf.c_iflag &= ~ (BRKINT | ICRNL | INPCK | ISTRIP | IXON);
        buf.c_cflag &= ~ (CSIZE | PARENB);
        buf.c_cflag |= CS8;

        buf.c_oflag &= ~ OPOST;

        buf.c_cc [VMIN]  = 1;
        buf.c_cc [VTIME] = 0;

        tcsetattr (fd, TCSAFLUSH, &buf);

        return 0;
}

int
tty_cbreak (int fd)
{
        struct termios buf;
        tcgetattr (fd, &save_termios);

        buf = save_termios;

        buf.c_lflag &= ~ (ECHO | ICANON);

        buf.c_cc [VMIN]  = 1;
        buf.c_cc [VTIME] = 0;

        tcsetattr (fd, TCSAFLUSH, &buf);

        return 0;
}

int
tty_reset (int fd)
{
        if (tcsetattr (fd, TCSAFLUSH, &save_termios) < 0)
                return -1;

        return 0;
}

void
read_char ()
{
        int i = 0;
        while ((i = getc (stdin)) != 'q')
                printf (" %i ", i);
}

void
new_line ()
{
        putp (carriage_return);
        putp (newline);
}

int
main (void)
{
        char *term = getenv ("TERM");
        int status;

        setupterm (term, 1, &status);
        printf ("has_meta_key: ");
        puts (has_meta_key == TRUE ? "true" : "false");
        printf ("gnu_has_meta_key: ");
        puts (gnu_has_meta_key == TRUE ? "true" : "false");

        tty_raw (STDIN_FILENO);

        puts ("Normal");
        read_char (); new_line ();

        putp (meta_on);  puts ("Meta mode");
        read_char (); new_line ();

        tty_reset (STDIN_FILENO);

        return 0;
}
